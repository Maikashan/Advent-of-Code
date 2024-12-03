#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <err.h>
#include <fstream>
#include <raft/core/device_resources.hpp>
#include <raft/core/device_span.hpp>
#include <raft/core/handle.hpp>
#include <raft/util/cudart_utils.hpp>
#include <rmm/device_scalar.hpp>
#include <rmm/device_uvector.hpp>
#include <rmm/device_vector.hpp>
#include <rmm/mr/device/cuda_async_memory_resource.hpp>
#include <rmm/mr/device/owning_wrapper.hpp>
#include <rmm/mr/device/pool_memory_resource.hpp>
#include <sstream>
#include <vector>

#include "rmm/cuda_stream_view.hpp"

#define CUDA_CHECK_ERROR(call)                                                 \
    do                                                                         \
    {                                                                          \
        cudaError_t err = call;                                                \
        if (err != cudaSuccess)                                                \
        {                                                                      \
            std::cerr << "CUDA error in " << __FILE__ << " at line "           \
                      << __LINE__ << ": " << cudaGetErrorString(err)           \
                      << std::endl;                                            \
            std::exit(EXIT_FAILURE);                                           \
        }                                                                      \
    } while (0)

static auto make_async()
{
    return std::make_shared<rmm::mr::cuda_async_memory_resource>();
}

static auto make_pool()
{
    // Allocate 0.05 Go
    size_t initial_pool_size = std::pow(2, 26);
    return rmm::mr::make_owning_wrapper<rmm::mr::pool_memory_resource>(
        make_async(), initial_pool_size);
}

__inline__ __device__ int warp_reduce(int val)
{
#pragma unroll
    for (int offset = warpSize / 2; offset > 0; offset /= 2)
        val += __shfl_down_sync(~0, val, offset);
    return val;
}

__global__ void level_handling_block(raft::device_span<int> data,
                                     raft::device_span<int> result)
{
    extern __shared__ int sdata[];
    // The second part of the shared memory is allocated for the flags
    int* flags = (sdata + blockDim.x);

    unsigned int tid = threadIdx.x;
    // We should never have more than a block
    unsigned int i = blockDim.x * blockIdx.x + tid;
    if (i >= data.size())
        return;

    sdata[tid] = data[tid];
    __syncthreads();

    flags[tid] = tid != 0 && sdata[tid] > sdata[tid - 1] ? 1 : 0;
    __syncthreads();

    // Block level reduce, without one value (the first in reality)
    for (int s = blockDim.x / 2; s > 32; s /= 2)
    {
        if (tid < s && i + s < data.size())
            flags[tid] += flags[tid + s];
        __syncthreads();
    }

    int val = 0;

    if (tid < 32)
        val = warp_reduce(flags[tid]);

    if (tid == 0)
        flags[0] = val;

    __syncthreads();

    // The reduced value must be equal to 0 or the size - 1 if the data is
    // sorted
    if (flags[0] != 0 && flags[0] < data.size() - 1)
    {
        return;
    }
    __syncthreads();

    // Checking if the distance if in the said boundaries
    if (tid > 0)
    {
        int dist = abs(sdata[tid] - sdata[tid - 1]);
        flags[tid] = dist >= 1 && dist <= 3 ? 1 : 0;
    }
    else
    {
        flags[tid] = 0;
    }
    __syncthreads();

    // Block level Reduce (once again)
    for (int s = blockDim.x / 2; s > 32; s /= 2)
    {
        if (tid < s && i + s < data.size() - 1)
            flags[tid] += flags[tid + s];
        __syncthreads();
    }

    val = 0;
    if (tid < 32)
        val = warp_reduce(flags[tid]);

    if (tid == 0 && val == data.size() - 1)
        atomicAdd(&result[0], 1);
}

static int part_1(const std::string& filename)
{
    // reading the input
    std::ifstream input(filename);
    std::string line;
    int* result;
    CUDA_CHECK_ERROR(cudaMalloc(&result, sizeof(int)));
    CUDA_CHECK_ERROR(cudaMemset(result, 0, sizeof(int)));

    CUDA_CHECK_ERROR(cudaStreamSynchronize(0));

    // If every line was of the same size, we could use MPI to read each line
    // with fseek and make a better use of the streams
    std::vector<cudaStream_t> streams;
    while (getline(input, line))
    {
        cudaStream_t s;
        CUDA_CHECK_ERROR(cudaStreamCreate(&s));

        // If we were not using GPU, we could handle most of the logic here
        std::vector<int> level;
        std::stringstream ss(line);
        int first = 0;
        while (ss >> first)
        {
            level.push_back(first);
        }
        // Putting every line on GPU
        rmm::device_uvector<int> dlevel(level.size(), s);
        raft::copy(dlevel.data(), level.data(), level.size(), s);
        streams.emplace_back(s);

        // I have multiple issue with this code.
        // - The rows do not all have the same amount of columns -> Challenging
        // to efficiently prepare my data for the GPU, and will have a lot of
        // work imbalance.
        // - Each row is very short, not very worth it to use my gpu on one line
        // Since it is only the second day and i am late, i will do something
        // neither optimized nor smart

        // We assume the length of a line will never be more than 1024 (maximum
        // size for a block)
        level_handling_block<<<1, level.size(),
                               dlevel.size() * 2 * sizeof(int) - 1, s>>>(
            raft::device_span<int>(dlevel.data(), dlevel.size()),
            raft::device_span<int>(result, 1));
    }

    for (cudaStream_t& s : streams)
    {
        CUDA_CHECK_ERROR(cudaStreamSynchronize(s));
        CUDA_CHECK_ERROR(cudaStreamDestroy(s));
    }

    int final_result;
    CUDA_CHECK_ERROR(
        cudaMemcpy(&final_result, result, sizeof(int), cudaMemcpyDeviceToHost));

    CUDA_CHECK_ERROR(cudaFree(result));

    return final_result;
}

__global__ void level_handling_block_unified(raft::device_span<int> data,
                                             raft::device_span<int> result)
{
    extern __shared__ int sdata[];
    // The second part of the shared memory is allocated for the flags
    int* flags = (sdata + blockDim.x);

    unsigned int tid = threadIdx.x;
    // We should never have more than a block
    unsigned int i = blockDim.x * blockIdx.x + tid;
    if (i >= data.size())
        return;

    sdata[tid] = data[tid];
    __syncthreads();

    flags[tid] = tid != 0 && sdata[tid] > sdata[tid - 1] ? 1 : 0;
    __syncthreads();

    // Block level reduce, without one value (the first in reality)
    for (int s = blockDim.x / 2; s > 32; s /= 2)
    {
        if (tid < s && i + s < data.size())
            flags[tid] += flags[tid + s];
        __syncthreads();
    }

    int val = 0;

    if (tid < 32)
        val = warp_reduce(flags[tid]);

    if (tid == 0)
        flags[0] = val;

    __syncthreads();

    // The reduced value must be equal to 0 or the size - 1 if the data is
    // sorted
    if (flags[0] != 0 && flags[0] < data.size() - 1)
    {
        return;
    }
    __syncthreads();

    // Checking if the distance if in the said boundaries
    if (tid > 0)
    {
        int dist = abs(sdata[tid] - sdata[tid - 1]);
        flags[tid] = dist >= 1 && dist <= 3 ? 1 : 0;
    }
    else
    {
        flags[tid] = 0;
    }
    __syncthreads();

    // Block level Reduce (once again)
    for (int s = blockDim.x / 2; s > 32; s /= 2)
    {
        if (tid < s && i + s < data.size() - 1)
            flags[tid] += flags[tid + s];
        __syncthreads();
    }

    val = 0;
    if (tid < 32)
        val = warp_reduce(flags[tid]);

    if (tid == 0 && val == data.size() - 1)
        result[0]++;
}

static int part_2(const std::string& filename)
{
    std::ifstream input(filename);
    std::string line;
    int tot_result = 0;

    // If every line was of the same size, we could use MPI to read each line
    // with fseek and make a better use of the streams
    while (getline(input, line))
    {
        // If we were not using GPU, we could handle most of the logic here
        std::vector<int> level;
        std::stringstream ss(line);
        int first = 0;
        while (ss >> first)
        {
            level.push_back(first);
        }
        int* result;
        CUDA_CHECK_ERROR(cudaMallocManaged(&result, sizeof(int)));
        CUDA_CHECK_ERROR(cudaMemset(result, 0, sizeof(int)));

        {
            rmm::device_uvector<int> dlevel(level.size(),
                                            rmm::cuda_stream_default);
            raft::copy(dlevel.data(), level.data(), level.size(),
                       rmm::cuda_stream_default);

            level_handling_block<<<1, level.size(),
                                   dlevel.size() * 2 * sizeof(int) - 1>>>(
                raft::device_span<int>(dlevel.data(), dlevel.size()),
                raft::device_span<int>(result, 1));
            cudaDeviceSynchronize();
        }

        // Well... bruteforce is a way...
        size_t i = 0;
        while (*result == 0 && i < level.size())
        {
            std::vector<int> short_level;
            for (int j = 0; j < level.size(); j++)
            {
                if (j == i)
                    continue;
                short_level.push_back(level[j]);
            }
            rmm::device_uvector<int> dlevel(short_level.size(),
                                            rmm::cuda_stream_default);
            raft::copy(dlevel.data(), short_level.data(), short_level.size(),
                       rmm::cuda_stream_default);

            level_handling_block_unified<<<
                1, short_level.size(), dlevel.size() * 2 * sizeof(int) - 1>>>(
                raft::device_span<int>(dlevel.data(), dlevel.size()),
                raft::device_span<int>(result, 1));
            cudaDeviceSynchronize();
            i++;
        }
        tot_result += *result;
        CUDA_CHECK_ERROR(cudaFree(result));
    }

    return tot_result;
}

int main(int argc, char* argv[])
{
    if (argc < 2)
    {
        errx(1, "Error: please provide a filename");
    }

    // RMM Setup
    auto memory_resource = make_pool();
    rmm::mr::set_current_device_resource(memory_resource.get());

    unsigned long long part1_res = part_1(argv[1]);

    std::cout << "q1: " << part1_res << "\n";

    unsigned long long part2_res = part_2(argv[1]);

    std::cout << "q2: " << part2_res << std::endl;

    return 0;
}
