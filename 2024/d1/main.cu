#include <cstdio>
#include <err.h>
#include <fstream>
#include <raft/core/device_resources.hpp>
#include <raft/core/device_span.hpp>
#include <raft/util/cudart_utils.hpp>
#include <rmm/device_uvector.hpp>
#include <rmm/device_vector.hpp>
#include <rmm/mr/device/cuda_async_memory_resource.hpp>
#include <rmm/mr/device/owning_wrapper.hpp>
#include <rmm/mr/device/pool_memory_resource.hpp>
#include <sstream>
#include <thrust/iterator/transform_iterator.h>
#include <thrust/iterator/zip_iterator.h>
#include <thrust/tuple.h>
#include <vector>

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

static unsigned long long part_1(raft::device_resources& res,
                                 const std::string& filename)
{
    // reading the input
    std::vector<unsigned long long> left;
    std::vector<unsigned long long> right;
    std::ifstream input(filename);
    std::string line;
    while (getline(input, line))
    {
        // Inserting in a sorted manner (kind of cheating, since it is happening
        // on CPU...)
        std::stringstream ss(line);
        unsigned long long first = 0;
        ss >> first;
        auto lower = std::lower_bound(left.begin(), left.end(), first);
        left.insert(lower, first);
        ss >> first;
        lower = std::lower_bound(right.begin(), right.end(), first);
        right.insert(lower, first);
    }

    // Putting the data on GPU
    rmm::device_uvector<unsigned long long> dleft(left.size(),
                                                  res.get_stream());
    raft::copy(dleft.data(), left.data(), left.size(), dleft.stream());
    rmm::device_uvector<unsigned long long> dright(right.size(),
                                                   dleft.stream());
    raft::copy(dright.data(), right.data(), right.size(), dright.stream());

    // Creating some fancy iterators
    // Zipping both vectors
    const auto q1_iterator = thrust::make_zip_iterator(
        thrust::make_tuple(dleft.cbegin(), dright.cbegin()));
    // Preparing the distance computation using a transform iterator
    const auto distance = thrust::make_transform_iterator(
        q1_iterator,
        [] __device__(
            thrust::tuple<unsigned long long, unsigned long long> const& tpl)
            -> unsigned long long {
            return thrust::get<0>(tpl) > thrust::get<1>(tpl)
                ? thrust::get<0>(tpl) - thrust::get<1>(tpl)
                : thrust::get<1>(tpl) - thrust::get<0>(tpl);
        });
    unsigned long long init = 0;
    // Computing the reduce
    auto result = thrust::reduce(thrust::cuda::par.on(dleft.stream()), distance,
                                 distance + left.size(), init);
    return result;
}

__global__ void histogram(raft::device_span<unsigned long long> data,
                          raft::device_span<unsigned long long> bins)
{
    unsigned int tid = threadIdx.x;
    unsigned int i = blockDim.x * blockIdx.x + tid;

    if (i >= data.size())
        return;

    atomicAdd(&bins[data[i]], 1);
}

static unsigned long long part_2(raft::device_resources& res,
                                 const std::string& filename)
{
    // reading the input
    // If we were not using CUDA, we could use a map to count. Unfortunately, we
    // are on GPU.
    std::vector<unsigned long long> left;
    std::vector<unsigned long long> right;
    std::ifstream input(filename);
    std::string line;
    unsigned long long maximum = 0;
    while (getline(input, line))
    {
        std::stringstream ss(line);
        unsigned long long first = 0;
        ss >> first;
        left.push_back(first);
        ss >> first;
        if (first > maximum)
            maximum = first;
        right.push_back(first);
    }

    // Putting the data on GPU
    rmm::device_uvector<unsigned long long> dleft(left.size(),
                                                  res.get_stream());
    raft::copy(dleft.data(), left.data(), left.size(), dleft.stream());
    rmm::device_uvector<unsigned long long> dright(right.size(),
                                                   dleft.stream());
    raft::copy(dright.data(), right.data(), right.size(), dright.stream());

    // Creating a container to count the amount of occurence of each left values
    rmm::device_uvector<unsigned long long> bins(
        maximum * sizeof(unsigned long long), dleft.stream());
    thrust::uninitialized_fill(thrust::cuda::par.on(dleft.stream()),
                               bins.begin(), bins.end(), 0);

    // Since I am late for the first day, i did a very simple histogram which
    // write in global memroy with an atomic. The memory access pattern is
    // awfull.
    // I might want to replace this part of the code with a CUB histogram, but
    // once again, it is awfull
    constexpr unsigned int blockSize = 256;
    const unsigned int gridSize = (blockSize - 1 + dright.size()) / blockSize;
    histogram<<<gridSize, blockSize, 0, dright.stream()>>>(
        raft::device_span<unsigned long long>(dright.data(), dright.size()),
        raft::device_span<unsigned long long>(bins.data(), bins.size()));

    // Preparing the computation of the value
    unsigned long long* raw_bins = bins.data();
    const auto val_iterator = thrust::make_transform_iterator(
        dleft.begin(),
        [raw_bins, maximum] __device__(
            unsigned long long const& val) -> unsigned long long {
            return val >= maximum ? 0 : val * raw_bins[val];
        });
    unsigned long long init = 0;
    // Computing the reduce
    auto result =
        thrust::reduce(thrust::cuda::par.on(bins.stream()), val_iterator,
                       val_iterator + left.size(), init);
    return result;
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
    //
    // Raft Setup
    raft::device_resources res;

    unsigned long long part1_res = part_1(res, argv[1]);

    std::cout << "q1: " << part1_res << "\n";

    unsigned long long part2_res = part_2(res, argv[1]);

    std::cout << "q2: " << part2_res << std::endl;

    return 0;
}
