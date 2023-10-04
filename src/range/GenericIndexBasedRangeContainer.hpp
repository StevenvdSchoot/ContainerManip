
#include <iterator>
#include <type_traits>
#include <utility>

namespace ContainerManip::internal {

template <typename StartIndexProviderType, typename EndIndexProviderType, typename DiffType>
class GenericIndexBasedRangeContainer {
	StartIndexProviderType startIndexProvider;
	EndIndexProviderType   endIndexProvider;

	using IndexType = std::conditional_t<
		std::is_same_v<std::remove_cv_t<std::remove_reference_t<decltype(startIndexProvider())>>,
					   std::remove_cv_t<std::remove_reference_t<decltype(endIndexProvider())>>>,
		std::remove_cv_t<std::remove_reference_t<decltype(startIndexProvider())>>,
		std::remove_cv_t<
			std::remove_reference_t<decltype(startIndexProvider() + endIndexProvider())>>>;

	class Iterator {
	public:
		using iterator_category = std::random_access_iterator_tag;

		using value_type      = IndexType;
		using reference       = IndexType;
		using const_reference = IndexType;
		using pointer         = IndexType;   // TODO(root):
		using difference_type = DiffType;

		constexpr Iterator() noexcept
			: index{} {}

		explicit constexpr Iterator(IndexType index) noexcept
			: index{index} {}

		[[nodiscard]] constexpr auto operator*() const noexcept -> IndexType { return index; }
		[[nodiscard]] constexpr auto operator->() const noexcept -> IndexType { return index; }

		[[nodiscard]] constexpr auto operator==(Iterator rhs) const noexcept -> bool {
			return index == rhs.index;
		}
		[[nodiscard]] constexpr auto operator!=(Iterator rhs) const noexcept -> bool {
			return index != rhs.index;
		}
		[[nodiscard]] constexpr auto operator<=(Iterator rhs) const noexcept -> bool {
			return index <= rhs.index;
		}
		[[nodiscard]] constexpr auto operator>=(Iterator rhs) const noexcept -> bool {
			return index >= rhs.index;
		}
		[[nodiscard]] constexpr auto operator<(Iterator rhs) const noexcept -> bool {
			return index < rhs.index;
		}
		[[nodiscard]] constexpr auto operator>(Iterator rhs) const noexcept -> bool {
			return index > rhs.index;
		}

		constexpr auto operator++() noexcept -> Iterator& {
			++index;
			return *this;
		}
		constexpr auto operator++(int) noexcept -> Iterator {
			auto copy = *this;
			++index;
			return copy;
		}
		constexpr auto operator--() noexcept -> Iterator& {
			--index;
			return *this;
		}
		constexpr auto operator--(int) noexcept -> Iterator {
			auto copy = *this;
			--index;
			return copy;
		}

		constexpr auto operator+=(DiffType rhs) noexcept -> Iterator& {
			index += rhs;
			return *this;
		}
		constexpr auto operator-=(DiffType rhs) noexcept -> Iterator& {
			index -= rhs;
			return *this;
		}

		[[nodiscard]] constexpr auto operator+(DiffType rhs) const noexcept -> Iterator {
			return Iterator{index + rhs};
		}
		[[nodiscard]] constexpr auto operator-(DiffType rhs) const noexcept -> Iterator {
			return Iterator{index - rhs};
		}
		[[nodiscard]] constexpr auto operator-(Iterator rhs) const noexcept -> DiffType {
			return static_cast<DiffType>(index - rhs.index);
		}

		[[nodiscard]] constexpr auto operator[](DiffType offset) const noexcept -> IndexType {
			return index + offset;
		}

	private:
		IndexType index;
	};

	class ReverseIterator : private Iterator {
		static constexpr auto endValue =
			static_cast<IndexType>(IndexType{} - static_cast<IndexType>(1));

		explicit constexpr ReverseIterator(const Iterator& iter)
			: iterator{iter} {}
		explicit constexpr ReverseIterator(Iterator&& iter)
			: iterator{std::move(iter)} {}

	public:
		using iterator_category = typename Iterator::iterator_category;

		using const_reference = typename Iterator::const_reference;
		using difference_type = typename Iterator::difference_type;
		using pointer         = typename Iterator::pointer;
		using reference       = typename Iterator::reference;
		using value_type      = typename Iterator::value_type;

		using Iterator::Iterator;

		[[nodiscard]] constexpr auto operator*() const noexcept -> IndexType {
			return iterator::operator*() - 1U;
		}
		[[nodiscard]] constexpr auto operator->() const noexcept -> IndexType {
			return iterator::operator->() - 1U;
		}

		[[nodiscard]] constexpr auto operator==(ReverseIterator rhs) const noexcept -> bool {
			return iterator::operator==(static_cast<const iterator&>(rhs));
		}
		[[nodiscard]] constexpr auto operator!=(ReverseIterator rhs) const noexcept -> bool {
			return iterator::operator!=(static_cast<const iterator&>(rhs));
		}
		[[nodiscard]] constexpr auto operator<=(ReverseIterator rhs) const noexcept -> bool {
			return iterator::operator>=(static_cast<const iterator&>(rhs));
		}
		[[nodiscard]] constexpr auto operator>=(ReverseIterator rhs) const noexcept -> bool {
			return iterator::operator<=(static_cast<const iterator&>(rhs));
		}
		[[nodiscard]] constexpr auto operator<(ReverseIterator rhs) const noexcept -> bool {
			return iterator::operator>(static_cast<const iterator&>(rhs));
		}
		[[nodiscard]] constexpr auto operator>(ReverseIterator rhs) const noexcept -> bool {
			return iterator::operator<(static_cast<const iterator&>(rhs));
		}

		constexpr auto operator++() noexcept -> ReverseIterator& {
			return static_cast<ReverseIterator&>(iterator::operator--());
		}
		constexpr auto operator++(int integer) noexcept -> ReverseIterator {
			return ReverseIterator{iterator::operator--(integer)};
		}
		constexpr auto operator--() noexcept -> ReverseIterator& {
			return static_cast<ReverseIterator&>(iterator::operator++());
		}
		constexpr auto operator--(int integer) noexcept -> ReverseIterator {
			return ReverseIterator{iterator::operator++(integer)};
		}

		constexpr auto operator+=(DiffType rhs) noexcept -> ReverseIterator& {
			return static_cast<ReverseIterator&>(iterator::operator-=(rhs));
		};
		constexpr auto operator-=(DiffType rhs) noexcept -> ReverseIterator& {
			return static_cast<ReverseIterator&>(iterator::operator+=(rhs));
		};

		[[nodiscard]] constexpr auto operator+(DiffType rhs) const noexcept -> ReverseIterator {
			return ReverseIterator{iterator::operator-(rhs)};
		};
		[[nodiscard]] constexpr auto operator-(DiffType rhs) const noexcept -> ReverseIterator {
			return ReverseIterator{iterator::operator+(rhs)};
		};
		[[nodiscard]] constexpr auto operator-(ReverseIterator rhs) const noexcept -> DiffType {
			return static_cast<const iterator&>(rhs) - static_cast<const iterator&>(*this);
		};

		[[nodiscard]] constexpr auto operator[](DiffType offset) const noexcept -> IndexType {
			return iterator::operator[](-offset) - 1U;
		}
	};

	[[nodiscard]] friend constexpr auto operator+(DiffType difference, Iterator iterator) noexcept
		-> Iterator {
		return iterator + difference;
	};

	[[nodiscard]] friend constexpr auto operator+(DiffType        difference,
												  ReverseIterator iterator) noexcept
		-> ReverseIterator {
		return iterator + difference;
	};

public:
	using iterator       = Iterator;
	using const_iterator = iterator;

	using value_type      = typename iterator::value_type;
	using reference       = typename iterator::reference;
	using pointer         = typename iterator::pointer;
	using const_reference = typename iterator::const_reference;
	using difference_type = typename iterator::difference_type;

	constexpr GenericIndexBasedRangeContainer(StartIndexProviderType&& startIndexProvider,
											  EndIndexProviderType&&   endIndexProvider) noexcept
		: startIndexProvider{std::move(startIndexProvider)}
		, endIndexProvider{std::move(endIndexProvider)} {}

	[[nodiscard]] constexpr auto begin() const noexcept -> iterator {
		return iterator{static_cast<IndexType>(startIndexProvider())};
	}
	[[nodiscard]] constexpr auto cbegin() const noexcept -> iterator {
		return iterator{static_cast<IndexType>(startIndexProvider())};
	}
	[[nodiscard]] constexpr auto rbegin() const noexcept -> ReverseIterator {
		return ReverseIterator{static_cast<IndexType>(endIndexProvider())};
	}
	[[nodiscard]] constexpr auto crbegin() const noexcept -> ReverseIterator {
		return ReverseIterator{static_cast<IndexType>(endIndexProvider())};
	}
	[[nodiscard]] constexpr auto end() const noexcept -> iterator {
		return iterator{static_cast<IndexType>(endIndexProvider())};
	}
	[[nodiscard]] constexpr auto cend() const noexcept -> iterator {
		return iterator{static_cast<IndexType>(endIndexProvider())};
	}
	[[nodiscard]] constexpr auto rend() const noexcept -> ReverseIterator {
		return ReverseIterator{static_cast<IndexType>(startIndexProvider())};
	}
	[[nodiscard]] constexpr auto crend() const noexcept -> ReverseIterator {
		return ReverseIterator{static_cast<IndexType>(startIndexProvider())};
	}

	[[nodiscard]] constexpr auto operator[](IndexType index) const noexcept -> IndexType {
		return startIndexProvider() + index;
	}
};

}   // namespace ContainerManip::internal