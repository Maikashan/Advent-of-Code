cmake_minimum_required(VERSION 3.26.4 FATAL_ERROR)

set(RAFT_NVTX ON)
include(fetch_rapids.cmake)
include(rapids-cmake)
include(rapids-cpm)
include(rapids-cuda)
include(rapids-find)


project(AOC_2024 LANGUAGES CXX CUDA)


rapids_cuda_init_architectures(aoc_2024)
rapids_cpm_init()
include(cmake/thirdparty/get_cccl.cmake)
include(cmake/thirdparty/get_rmm.cmake)
include(cmake/thirdparty/get_raft.cmake)

# Set the CUDA standard
set(CMAKE_CUDA_STANDARD 17)
set(CXX_STANDARD 17)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fconcepts ")

# To update depending of your cuda architecture
set(CUDA_ARCHITECTURES "75")

function(aoc_day day)
    add_executable(${day} ${ARGN})

    # Compile in debug mode by default
    target_compile_options(${day} PRIVATE $<$<COMPILE_LANGUAGE:CUDA>:-G -g>)

    # To update depending of your cuda architecture
    set_target_properties(${day} PROPERTIES CUDA_ARCHITECTURES "75")

    target_link_libraries(${day} PUBLIC rmm::rmm raft::raft)
endfunction()

aoc_day(d1 d1/main.cu)
