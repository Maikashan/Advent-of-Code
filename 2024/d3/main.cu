#include <cstdio>
#include <cuda/atomic>
#include <err.h>
#include <fstream>
#include <raft/core/device_resources.hpp>
#include <raft/core/device_span.hpp>
#include <raft/util/cudart_utils.hpp>
#include <rmm/device_uvector.hpp>
#include <rmm/mr/device/cuda_async_memory_resource.hpp>
#include <rmm/mr/device/owning_wrapper.hpp>
#include <rmm/mr/device/pool_memory_resource.hpp>
#include <vector>
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

// This code is absolutely awfull: We have warp divergence everywhere : at least
// 11 thread will be doing nothing per warp.
// Unfortunately, I do not have enough time to put in the AOC to do better.
__global__ void compute_mul(raft::device_span<char> line,
                            raft::device_span<unsigned long long> result)
{
    extern __shared__ char sline[];
    unsigned int tid = threadIdx.x;
    unsigned int i = blockDim.x * blockIdx.x + tid;
    if (i >= line.size())
        return;

    sline[tid] = line[i];
    __syncthreads();

    int current = 0;
    char rgx[] = { 'm', 'u', 'l', '(', '*', ',', '*', ')' };
    int nb1 = 0;
    int nb2 = 0;
    int index = 0;
    while (tid + index < blockDim.x && current < 8)
    {
        // One of the characters
        if (rgx[current] != '*' && rgx[current] == sline[tid + index])
        {
            current++;
            index++;
        }
        // We are on a number
        else if (rgx[current] == '*')
        {
            if (sline[tid + index] <= '9' && sline[tid + index] >= '0')
            {
                if (current == 4)
                {
                    nb1 = nb1 * 10 + (sline[tid + index] - '0');

                    // We have more than 3 characters
                    if (nb1 >= 1000)
                        break;
                }
                if (current == 6)
                {
                    nb2 = nb2 * 10 + (sline[tid + index] - '0');

                    // We have more than 3 characters
                    if (nb2 >= 1000)
                        break;
                }
                index++;
            }
            else
            {
                // We are just after
                current++;
            }
        }
        else
        {
            break;
        }
    }

    // We succeded in finding a mul
    if (current == 8)
    {
        atomicAdd(&result[0], nb1 * nb2);
        return;
    }
    // We reached the end of the shared
    if (tid + index == blockDim.x)
    {
        while (i + index < line.size() && current < 8)
        {
            // One of the characters
            if (rgx[current] != '*' && rgx[current] == line[i + index])
            {
                current++;
                index++;
            }
            // We are on a number
            else if (rgx[current] == '*')
            {
                if (line[i + index] <= '9' && line[i + index] >= '0')
                {
                    if (current == 4)
                    {
                        nb1 = nb1 * 10 + (line[i + index] - '0');

                        // We have more than 3 characters
                        if (nb1 >= 1000)
                            break;
                    }
                    if (current == 6)
                    {
                        nb2 = nb2 * 10 + (line[i + index] - '0');

                        // We have more than 3 characters
                        if (nb2 >= 1000)
                            break;
                    }
                    index++;
                }
                else
                {
                    // We are just after
                    current++;
                }
            }
            else
            {
                break;
            }
        }
        // We read a value which overflowed
        if (current == 8)
        {
            atomicAdd(&result[0], nb1 * nb2);
            return;
        }
    }
}

static size_t part_1(const std::string& filename)
{
    // reading the input
    std::vector<std::string> tot_input;
    std::ifstream input(filename);
    std::string line;
    while (getline(input, line))
    {
        tot_input.push_back(line);
    }
    unsigned long long* result;
    CUDA_CHECK_ERROR(cudaMallocManaged(&result, sizeof(unsigned long long)));
    result[0] = 0;

    constexpr unsigned int blockSize = 256;
    for (const std::string& l : tot_input)
    {
        const int gridSize = (blockSize - 1 + l.size()) / blockSize;
        // Putting the current line on GPU
        rmm::device_uvector<char> dline(l.size(), rmm::cuda_stream_default);
        raft::copy(dline.data(), l.data(), l.size(), dline.stream());
        compute_mul<<<gridSize, blockSize, blockSize * sizeof(char)>>>(
            raft::device_span<char>(dline.data(), dline.size()),
            raft::device_span<unsigned long long>(result, 1));
    }
    cudaDeviceSynchronize();
    return result[0];
}

static unsigned long long part_2(const std::string& filename)
{
    return 0;
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
