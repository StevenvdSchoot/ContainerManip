Container Manip
===============

This library provides some basic container manipulation functions. They are intended as zero cost abstractions of common operations on containers.

Distribution
------------

The library is C++17 and C++20 compatible, with the intention to become C++23 compatible shortly after the standard is released. It can be used as a static library or as a C++20 module.

Usage of the library function should be trivial. The entire public API is designed to be easy to use right and hard to use wrong. Some examples that illustrate how and when to use this library are planned.

At the moment the latest stable version can be found in this git repo in the release branch. In the future releases will be shipped as conan and vcpkg packages as well.

TODO
----
 * Add support for executing test using C++20 modules
 * Add continues integration that runs all test after compiling:
    * As static library using GCC 9 to 12 in C++17 and C++20 mode
    * As static library using clang 8 to 16 in C++17 and C++20 mode
    * As module using clang 16 and cmake's (experimental) cxx module cmake api
 * Add usage examples
 * Ship releases as conan package
 * Ship releases as vcpkg package
