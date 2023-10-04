#ifndef CONTAINER_MANIP_RANGE_H
#define CONTAINER_MANIP_RANGE_H

#include "range/impl.hpp"   // IWYU pragma: export

namespace ContainerManip {

/**
 * @brief
 *
 * @tparam ContainerType
 * @param container
 * @return A range container
 */
template <typename ContainerType>
constexpr auto range(ContainerType&& container) noexcept(
	noexcept(internal::range(std::forward<ContainerType>(container))))
	-> decltype(internal::range(std::forward<ContainerType>(container))) {
	return internal::range(std::forward<ContainerType>(container));
}

}   // namespace ContainerManip

#endif