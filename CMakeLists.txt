cmake_minimum_required(VERSION 3.26)

project(
  modules-example
  VERSION 0.1
  LANGUAGES CXX
  HOMEPAGE_URL https://github.com/stevenvdschoot/containermanip)

set(CONTAINERMANIP_SUPPORT_MODULES_DEFAULT OFF)
if((CMAKE_CXX_COMPILER_ID STREQUAL "Clang") AND (CMAKE_CXX_COMPILER_VERSION
                                                 VERSION_GREATER_EQUAL "17.0"))
  set(CONTAINERMANIP_SUPPORT_MODULES_DEFAULT ON)
endif()

option(ENABLE_INCLUDE_WHAT_YOU_USE "Use include-what-you-use" OFF)
if(ENABLE_INCLUDE_WHAT_YOU_USE)
  find_program(iwyu_path NAMES include-what-you-use iwyu REQUIRED)
endif()

option(ENABLE_CLANG_TIDY "Use include-what-you-use" OFF)
if(ENABLE_CLANG_TIDY)
  find_program(clang_tidy_path NAMES clang-tidy REQUIRED)
endif()

option(CONTAINERMANIP_SUPPORT_MODULES "Turn on support for C++20 modules"
       ${CONTAINERMANIP_SUPPORT_MODULES_DEFAULT})

if(CONTAINERMANIP_SUPPORT_MODULES)
  set(CMAKE_EXPERIMENTAL_CXX_MODULE_CMAKE_API
      "2182bf5c-ef0d-489a-91da-49dbc3090d2a")
  set(CMAKE_EXPERIMENTAL_CXX_MODULE_DYNDEP ON)
endif()

set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

if(CONTAINERMANIP_SUPPORT_MODULES)
  add_library(ContainerManip_module)
  target_sources(
    ContainerManip_module
    PUBLIC FILE_SET
           containerManip_moduleFiles
           TYPE
           CXX_MODULES
           FILES
           src/module.cpp
           # src/range/module.cpp
           src/zip/module.cpp)
  set_property(TARGET ContainerManip_module PROPERTY CXX_STANDARD 20)
  if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    target_compile_options(
      ContainerManip_module PRIVATE -fmodules -fno-implicit-module-maps
                                    -fbuiltin-module-map)
  endif()
endif()

add_library(ContainerManip_nomodule INTERFACE src/range/impl.hpp src/range.hpp
            src/zip/impl.hpp src/zip.hpp)
target_include_directories(ContainerManip_nomodule
                           INTERFACE ${CMAKE_SOURCE_DIR}/src)
set_property(TARGET ContainerManip_nomodule PROPERTY CXX_STANDARD 17)
if(ENABLE_INCLUDE_WHAT_YOU_USE)
  set_property(
    TARGET ContainerManip_nomodule PROPERTY CXX_INCLUDE_WHAT_YOU_USE
                                            "${iwyu_path};-Xiwyu;--cxx17ns")
endif()
if(ENABLE_CLANG_TIDY)
  # set_property(TARGET ContainerManip_nomodule
  #              PROPERTY CXX_CLANG_TIDY "${clang_tidy_path};-header-filter=.*")
endif()

include(FetchContent)

FetchContent_Declare(
  Catch2
  GIT_REPOSITORY https://github.com/catchorg/Catch2.git
  GIT_TAG v3.4.0)

FetchContent_MakeAvailable(Catch2)

add_executable(tests test/range.cpp test/zip.cpp)
target_compile_definitions(tests PUBLIC CATCH_CONFIG_RUNTIME_STATIC_REQUIRE)
# target_compile_definitions(tests PUBLIC CATCH_CONFIG_NO_EXPERIMENTAL_STATIC_ANALYSIS_SUPPORT)
get_target_property(catch2_include_dirs Catch2::Catch2 
                    INTERFACE_INCLUDE_DIRECTORIES)
target_include_directories(tests SYSTEM PRIVATE "${catch2_include_dirs}")
target_link_libraries(tests PRIVATE ContainerManip_nomodule
                                    Catch2::Catch2WithMain)
set_property(TARGET tests PROPERTY CXX_STANDARD 17) # TODO: 20 version as
                                                    # well...
if(ENABLE_INCLUDE_WHAT_YOU_USE)
  set_property(TARGET tests PROPERTY CXX_INCLUDE_WHAT_YOU_USE
                                     "${iwyu_path};-Xiwyu;--cxx17ns")
endif()
if(ENABLE_CLANG_TIDY)
  # set_property(TARGET tests PROPERTY CXX_CLANG_TIDY
  #                                    "${clang_tidy_path};-header-filter=.*")
endif()

list(APPEND CMAKE_MODULE_PATH ${catch2_SOURCE_DIR}/extras)
include(CTest)
include(Catch)
catch_discover_tests(tests)
# catch_discover_tests(tests REPORTER XML)

if(CONTAINERMANIP_SUPPORT_MODULES) # TODO
  add_library(ContainerManip ALIAS ContainerManip_module)
else()
  add_library(ContainerManip ALIAS ContainerManip_nomodule)
endif()

set_property(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  PROPERTY VS_STARTUP_PROJECT
  test)
