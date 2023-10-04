#include <cstddef>
#include <iterator>
#include <sys/types.h>
#include <type_traits>

#include "GenericIndexBasedRangeContainer.hpp"   // IWYU pragma: export

namespace ContainerManip::internal {

template <typename ContainerType>
class RangeContainer {
	class Iterator;

	class EndSentinel {
	public:
		constexpr EndSentinel() = default;

		template <typename T,
				  // Prevent this constructor from becoming the copy or move constructor. The
				  // default copy and move constructor will do the right thing already, but they
				  // keep the object trivially movable/copyable
				  typename = std::enable_if_t<
					  !std::is_same_v<EndSentinel, std::remove_cv_t<std::remove_reference_t<T>>>>>
		explicit constexpr EndSentinel(T&& endSentinel)
			: endSentinel{std::forward<T>(endSentinel)} {}

		constexpr auto operator==(EndSentinel rhs) const
			noexcept(noexcept(endSentinel == rhs.endSentinel)) -> bool {
			return endSentinel == rhs.endSentinel;
		}
		constexpr auto operator!=(EndSentinel rhs) const
			noexcept(noexcept(endSentinel != rhs.endSentinel)) -> bool {
			return endSentinel != rhs.endSentinel;
		}
		constexpr auto operator<=(EndSentinel rhs) const
			noexcept(noexcept(endSentinel <= rhs.endSentinel)) -> bool {
			return endSentinel <= rhs.endSentinel;
		}
		constexpr auto operator>=(EndSentinel rhs) const
			noexcept(noexcept(endSentinel >= rhs.endSentinel)) -> bool {
			return endSentinel >= rhs.endSentinel;
		}
		constexpr auto operator<(EndSentinel rhs) const
			noexcept(noexcept(endSentinel < rhs.endSentinel)) -> bool {
			return endSentinel < rhs.endSentinel;
		}
		constexpr auto operator>(EndSentinel rhs) const
			noexcept(noexcept(endSentinel > rhs.endSentinel)) -> bool {
			return endSentinel > rhs.endSentinel;
		}

		constexpr auto operator==(const Iterator& rhs) const
			noexcept(noexcept(endSentinel == rhs.iterator)) -> bool {
			return endSentinel == rhs.iterator;
		}
		constexpr auto operator!=(const Iterator& rhs) const
			noexcept(noexcept(endSentinel != rhs.iterator)) -> bool {
			return endSentinel != rhs.iterator;
		}
		constexpr auto operator<=(const Iterator& rhs) const
			noexcept(noexcept(endSentinel <= rhs.iterator)) -> bool {
			return endSentinel <= rhs.iterator;
		}
		constexpr auto operator>=(const Iterator& rhs) const
			noexcept(noexcept(endSentinel >= rhs.iterator)) -> bool {
			return endSentinel >= rhs.iterator;
		}
		constexpr auto operator<(const Iterator& rhs) const
			noexcept(noexcept(endSentinel < rhs.iterator)) -> bool {
			return endSentinel < rhs.iterator;
		}
		constexpr auto operator>(const Iterator& rhs) const
			noexcept(noexcept(endSentinel > rhs.iterator)) -> bool {
			return endSentinel > rhs.iterator;
		}

	private:
		std::remove_cv_t<std::remove_reference_t<decltype(std::end(std::declval<ContainerType>()))>>
			endSentinel{};

		friend class Iterator;
	};

	class Iterator {
		using SubIteratorType = std::remove_cv_t<
			std::remove_reference_t<decltype(std::begin(std::declval<ContainerType>()))>>;

	public:
		using iterator_category = std::random_access_iterator_tag;   // TODO(root):

		using difference_type = typename std::iterator_traits<SubIteratorType>::difference_type;
		using value_type      = typename std::make_unsigned_t<difference_type>;
		using reference       = value_type;
		using const_reference = value_type;
		using pointer         = value_type;

		constexpr Iterator() = default;

		template <typename IteratorType,
				  // Prevent this constructor from becoming the copy or move constructor. The
				  // default copy and move constructor will do the right thing already, but they
				  // keep the object trivially movable/copyable
				  typename = std::enable_if_t<!std::is_same_v<
					  Iterator, std::remove_cv_t<std::remove_reference_t<IteratorType>>>>>
		explicit constexpr Iterator(IteratorType&& iterator, value_type index = 0U)
			: index{index}
			, iterator{std::forward<IteratorType>(iterator)} {}

		[[nodiscard]] constexpr auto operator*() const noexcept -> value_type { return index; }
		[[nodiscard]] constexpr auto operator->() const noexcept -> value_type { return index; }

		[[nodiscard]] constexpr auto operator==(const Iterator& rhs) const noexcept -> bool {
			return index == rhs.index;
		}
		[[nodiscard]] constexpr auto operator!=(const Iterator& rhs) const noexcept -> bool {
			return index != rhs.index;
		}
		[[nodiscard]] constexpr auto operator<=(const Iterator& rhs) const noexcept -> bool {
			return index <= rhs.index;
		}
		[[nodiscard]] constexpr auto operator>=(const Iterator& rhs) const noexcept -> bool {
			return index >= rhs.index;
		}
		[[nodiscard]] constexpr auto operator<(const Iterator& rhs) const noexcept -> bool {
			return index < rhs.index;
		}
		[[nodiscard]] constexpr auto operator>(const Iterator& rhs) const noexcept -> bool {
			return index > rhs.index;
		}

		[[nodiscard]] constexpr auto operator==(const EndSentinel& rhs) const
			noexcept(noexcept(iterator == rhs.endSentinel)) -> bool {
			return iterator == rhs.endSentinel;
		}
		[[nodiscard]] constexpr auto operator!=(const EndSentinel& rhs) const
			noexcept(noexcept(iterator != rhs.endSentinel)) -> bool {
			return iterator != rhs.endSentinel;
		}
		[[nodiscard]] constexpr auto operator<=(const EndSentinel& rhs) const
			noexcept(noexcept(iterator <= rhs.endSentinel)) -> bool {
			return iterator <= rhs.endSentinel;
		}
		[[nodiscard]] constexpr auto operator>=(const EndSentinel& rhs) const
			noexcept(noexcept(iterator >= rhs.endSentinel)) -> bool {
			return iterator >= rhs.endSentinel;
		}
		[[nodiscard]] constexpr auto operator<(const EndSentinel& rhs) const
			noexcept(noexcept(iterator < rhs.endSentinel)) -> bool {
			return iterator < rhs.endSentinel;
		}
		[[nodiscard]] constexpr auto operator>(const EndSentinel& rhs) const
			noexcept(noexcept(iterator > rhs.endSentinel)) -> bool {
			return iterator > rhs.endSentinel;
		}

		constexpr auto operator++() noexcept(noexcept(++iterator)) -> Iterator& {
			++iterator;
			++index;
			return *this;
		}
		constexpr auto operator++(int) noexcept(noexcept(++iterator)) -> Iterator {
			auto copy = *this;
			++iterator;
			++index;
			return copy;
		}
		constexpr auto operator--() noexcept(noexcept(--iterator)) -> Iterator& {
			--iterator;
			--index;
			return *this;
		}
		constexpr auto operator--(int) noexcept(noexcept(--iterator)) -> Iterator {
			auto copy = *this;
			--iterator;
			--index;
			return copy;
		}

		[[nodiscard]] constexpr auto operator+(difference_type rhs) const
			noexcept(noexcept(iterator + rhs)) -> Iterator {
			return Iterator{iterator + rhs, index + rhs};
		}
		[[nodiscard]] constexpr auto operator-(difference_type rhs) const
			noexcept(noexcept(iterator - rhs)) -> Iterator {
			return Iterator{iterator - rhs, index - rhs};
		}

		[[nodiscard]] constexpr auto
		operator+=(difference_type rhs) noexcept(noexcept(iterator += rhs)) -> Iterator& {
			iterator += rhs;
			index += rhs;
			return *this;
		}
		[[nodiscard]] constexpr auto
		operator-=(difference_type rhs) noexcept(noexcept(iterator -= rhs)) -> Iterator& {
			iterator -= rhs;
			index -= rhs;
			return *this;
		}

		[[nodiscard]] constexpr auto operator-(const Iterator& rhs) const noexcept
			-> difference_type {
			return index - rhs.index;
		}

		[[nodiscard]] constexpr auto operator[](difference_type offset) const noexcept
			-> value_type {
			return index + offset;
		}

	private:
		value_type      index{};
		SubIteratorType iterator{};

		friend class EndSentinel;

		[[nodiscard]] friend constexpr auto operator+(difference_type difference,
													  Iterator iterator) noexcept -> Iterator {
			return iterator + difference;
		};
	};

public:
	template <typename T,
			  // Prevent this constructor from becoming the copy or move constructor. The
			  // default copy and move constructor will do the right thing already, but they
			  // keep the object trivially movable/copyable
			  typename = std::enable_if_t<
				  !std::is_same_v<std::remove_cv_t<std::remove_reference_t<T>>, RangeContainer>>>
	explicit constexpr RangeContainer(T&& container)
		: container{std::forward<T>(container)} {}

	[[nodiscard]] constexpr auto begin() -> Iterator { return Iterator{std::begin(container)}; }
	[[nodiscard]] constexpr auto end() -> EndSentinel { return EndSentinel{std::end(container)}; }

	[[nodiscard]] constexpr auto begin() const -> Iterator {
		return Iterator{std::cbegin(container)};
	}
	[[nodiscard]] constexpr auto end() const -> EndSentinel {
		return EndSentinel{std::cend(container)};
	}
	[[nodiscard]] constexpr auto cbegin() const -> Iterator {
		return Iterator{std::cbegin(container)};
	}
	[[nodiscard]] constexpr auto cend() const -> EndSentinel {
		return EndSentinel{std::cend(container)};
	}

private:
	ContainerType container;   // NOLINT(cppcoreguidelines-avoid-const-or-ref-data-members)
};

template <typename T>
constexpr auto preferPrecomputeSizeHelper(int /*unused*/)
	-> std::enable_if_t<std::is_arithmetic_v<decltype(std::size(std::declval<T>()))>, bool> {
	return true;
}
template <typename T>
constexpr auto preferPrecomputeSizeHelper(float /*unused*/) -> bool {
	return false;
}

template <typename T>
inline constexpr auto preferPrecomputeSize_v = preferPrecomputeSizeHelper<T>(0);

template <typename ContainerType,
		  typename std::iterator_traits<std::remove_reference_t<
			  decltype(begin(std::declval<ContainerType>()))>>::iterator_category* = nullptr>
constexpr auto range(ContainerType&& container) noexcept(
	preferPrecomputeSize_v<ContainerType> /*&& noexcept(container.size())*/) {
	if constexpr (preferPrecomputeSize_v<ContainerType>) {
		using SizeType = std::remove_cv_t<std::remove_reference_t<decltype(container.size())>>;
		using DiffType =
			typename std::iterator_traits<decltype(std::begin(container))>::difference_type;

		struct StartIndexProviderType {
			constexpr auto operator()() const noexcept -> SizeType {
				return static_cast<SizeType>(0U);
			};
		};
		struct EndIndexProviderType {
			explicit constexpr EndIndexProviderType(SizeType endIndex) noexcept
				: endIndex{endIndex} {}

			constexpr auto operator()() const noexcept -> SizeType { return endIndex; };

		private:
			SizeType endIndex;
		};

		return GenericIndexBasedRangeContainer<StartIndexProviderType, EndIndexProviderType,
											   DiffType>{StartIndexProviderType{},
														 EndIndexProviderType{container.size()}};
	} else {
		return RangeContainer<ContainerType>{
			std::forward<ContainerType>(container)};   // TODO(root):
	}
}

template <typename T, size_t n>
constexpr auto range(T (& /*container*/)[n]) noexcept {   // NOLINT(*-avoid-c-arrays)
	using SizeType = size_t;
	using DiffType = ssize_t;

	struct StartIndexProviderType {
		constexpr auto operator()() const noexcept -> SizeType {
			return static_cast<SizeType>(0U);
		};
	};
	struct EndIndexProviderType {
		explicit constexpr EndIndexProviderType(SizeType endIndex) noexcept
			: endIndex{endIndex} {}

		constexpr auto operator()() const noexcept -> SizeType { return endIndex; };

	private:
		SizeType endIndex;
	};

	return GenericIndexBasedRangeContainer<StartIndexProviderType, EndIndexProviderType, DiffType>{
		StartIndexProviderType{}, EndIndexProviderType{n}};
}

template <typename T, typename = std::enable_if_t<std::is_integral_v<T>>>
constexpr auto range(T length) noexcept {
	using SizeType = typename std::remove_reference_t<decltype(length + 0U)>;
	using DiffType = typename std::make_signed_t<SizeType>;

	struct StartIndexProviderType {
		constexpr auto operator()() const noexcept -> SizeType {
			return static_cast<SizeType>(0U);
		};
	};
	struct EndIndexProviderType {
		explicit constexpr EndIndexProviderType(SizeType endIndex) noexcept
			: endIndex{endIndex} {}

		constexpr auto operator()() const noexcept -> SizeType { return endIndex; };

	private:
		SizeType endIndex;
	};

	return GenericIndexBasedRangeContainer<StartIndexProviderType, EndIndexProviderType, DiffType>{
		StartIndexProviderType{}, EndIndexProviderType{length}};
}

template <typename T, typename = std::enable_if_t<std::is_integral_v<T>>>
constexpr auto range(T start, T end) noexcept {
	using SizeType = typename std::remove_reference_t<decltype(start + 0U)>;
	using DiffType = typename std::make_signed_t<SizeType>;

	struct IndexProviderType {
		explicit constexpr IndexProviderType(SizeType endIndex) noexcept
			: endIndex{endIndex} {}

		constexpr auto operator()() const noexcept -> SizeType { return endIndex; };

	private:
		SizeType endIndex;
	};

	return GenericIndexBasedRangeContainer<IndexProviderType, IndexProviderType, DiffType>{
		IndexProviderType{start}, IndexProviderType{end}};
}

}   // namespace ContainerManip::internal