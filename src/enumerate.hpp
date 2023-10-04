#ifndef CONTAINER_MANIP_ENUMERATE_H
#define CONTAINER_MANIP_ENUMERATE_H

#include "range.hpp"
#include "zip.hpp"

namespace ContainerManip {

template <typename ContainerType>
constexpr auto enumerate(ContainerType&& container) noexcept(
	noexcept(internal::zip(internal::range(container), std::forward<ContainerType>(container))))
	-> decltype(internal::zip(internal::range(container), std::forward<ContainerType>(container))) {
	return internal::zip(internal::range(container), std::forward<ContainerType>(container));
}

}   // namespace ContainerManip

#endif