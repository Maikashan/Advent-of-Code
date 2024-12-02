if(NOT EXISTS ${CMAKE_CURRENT_BINARY_DIR}/AOC_2024_RAPIDS.cmake)
  file(DOWNLOAD https://raw.githubusercontent.com/rapidsai/rapids-cmake/branch-24.08/RAPIDS.cmake
      ${CMAKE_CURRENT_BINARY_DIR}/AOC_2024_RAPIDS.cmake
  )
endif()
include(${CMAKE_CURRENT_BINARY_DIR}/AOC_2024_RAPIDS.cmake)
