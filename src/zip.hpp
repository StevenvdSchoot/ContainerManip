#ifndef CONTAINER_MANIP_ZIP_H
#define CONTAINER_MANIP_ZIP_H

#include "zip/impl.hpp"   // IWYU pragma: export

#include <utility>

namespace ContainerManip {

/**
 * @brief Iterate over multiple containers
 *
 * Takes multiple containers and produces a container whose ith element is a tuple-like value
 * consisting of the ith elements of all containers. **If the size of all provided containers is not
 * the same, behaviour of the iterators produced by the produced container is undefined.**
 *
 * Example usage:
 * @code {.cpp}
 *   std::array<int, 6> container1{1, 2, 3, 4, 5, 6}, container2{};
 *   for (auto&& [elementContainer1, elementContainer2] : zip(container1, container2)) {
 *   	elementContainer2 = elementContainer1;
 *   }
 * @endcode
 *
 * This function, as well as the various begin and end operators and the operators on the produced
 * iterators are [noexcept](https://en.cppreference.com/w/cpp/language/noexcept) when the
 * corresponding operations on the underlying containers are noexcept as well.
 *
 * @param containers Collection of containers that need to be zipped
 * @return Container whose ith element is a tuple-like value consisting of the ith elements of all
 * containers
 */
template <typename... ContainerType>
static constexpr auto zip(ContainerType&&... containers) noexcept(
	noexcept(internal::zip(std::forward<ContainerType>(containers)...))) {
	return internal::zip(std::forward<ContainerType>(containers)...);
}

}   // namespace ContainerManip

#endif