#include <cstddef>

#include <array>
#include <forward_list>
#include <iterator>
#include <tuple>
#include <type_traits>
#include <utility>

#include <range.hpp>

#include <catch2/catch_template_test_macros.hpp>
#include <catch2/catch_test_macros.hpp>

#include "OperandPresence.hpp"

template <typename ContainerType, typename T, size_t length>
constexpr auto arrayEqual(const ContainerType& rhs, const std::array<T, length>& lhs) noexcept {
	auto it1   = std::begin(rhs);
	auto it2   = std::begin(lhs);
	auto endIt = std::end(lhs);
	while (it2 != endIt) {
		if (*it1++ != *it2++) {
			return false;
		}
	}
	return true;
}

template <typename ContainerType, typename IntegralType, IntegralType... sequence>
constexpr auto
createContainerWithSequence(std::integer_sequence<IntegralType, sequence...> /*sequence*/)
	-> ContainerType {
	return ContainerType{sequence...};
};

TEST_CASE("Basic functionality", "[range]") {
	static constexpr auto testSequenceLength = 6U;

	constexpr auto arr = createContainerWithSequence<std::array<size_t, testSequenceLength>>(
		std::make_index_sequence<testSequenceLength>{});

	constexpr auto rangeContainer = ContainerManip::range(arr);

	STATIC_REQUIRE(std::end(rangeContainer) - std::begin(rangeContainer) == arr.size());
	STATIC_CHECK(arrayEqual(rangeContainer, arr));
}

TEST_CASE("Basic functionality 2", "[range]") {
	static constexpr auto testSequenceLength = 6U;

	auto list = createContainerWithSequence<std::forward_list<size_t>>(
		std::make_index_sequence<testSequenceLength>{});
	auto rangeContainer = ContainerManip::range(list);

	auto index         = 0U;
	auto rangeIterator = begin(rangeContainer);
	for (auto it = begin(list); it != end(list); it++) {
		CHECK(*rangeIterator++ == index++);
	}
	CHECK(rangeIterator == end(rangeContainer));
}

TEST_CASE("Container with size iterator post increment", "[range]") {
	constexpr auto rangeContainer = ContainerManip::range(std::array{4, 5, 6, 7});

	auto iterator         = std::begin(rangeContainer);
	auto originalIterator = iterator++;

	CHECK(*originalIterator == 0);
	CHECK(*iterator == 1);
}

TEST_CASE("Container with size iterator pre increment", "[range]") {
	constexpr auto rangeContainer = ContainerManip::range(std::array{4, 5, 6, 7});

	auto iterator         = std::begin(rangeContainer);
	auto originalIterator = ++iterator;

	CHECK(*originalIterator == 1);
	CHECK(*iterator == 1);
}

TEST_CASE("Container with size iterator post increment reverse", "[range]") {
	constexpr auto rangeContainer = ContainerManip::range(std::array{4, 5, 6, 7});

	auto iterator         = std::rbegin(rangeContainer);
	auto originalIterator = iterator++;

	CHECK(*originalIterator == 3);
	CHECK(*iterator == 2);
}

TEST_CASE("Container with size iterator pre increment reverse", "[range]") {
	constexpr auto rangeContainer = ContainerManip::range(std::array{4, 5, 6, 7});

	auto iterator         = std::rbegin(rangeContainer);
	auto originalIterator = ++iterator;

	CHECK(*originalIterator == 2);
	CHECK(*iterator == 2);
}

TEST_CASE("Container with size iterator post decrement", "[range]") {
	constexpr auto rangeContainer = ContainerManip::range(std::array{4, 5, 6, 7});

	auto iterator         = std::end(rangeContainer);
	auto originalIterator = iterator--;

	CHECK(originalIterator == std::end(rangeContainer));
	CHECK(*iterator == 3);
}

TEST_CASE("Container with size iterator pre decrement", "[range]") {
	constexpr auto rangeContainer = ContainerManip::range(std::array{4, 5, 6, 7});

	auto iterator         = std::end(rangeContainer);
	auto originalIterator = --iterator;

	CHECK(*originalIterator == 3);
	CHECK(*iterator == 3);
}

TEST_CASE("Container with size iterator post decrement reverse", "[range]") {
	constexpr auto rangeContainer = ContainerManip::range(std::array{4, 5, 6, 7});

	auto iterator         = std::rend(rangeContainer);
	auto originalIterator = iterator--;

	CHECK(originalIterator == std::rend(rangeContainer));
	CHECK(*iterator == 0);
}

TEST_CASE("Container with size iterator pre decrement reverse", "[range]") {
	constexpr auto rangeContainer = ContainerManip::range(std::array{4, 5, 6, 7});

	auto iterator         = std::rend(rangeContainer);
	auto originalIterator = --iterator;

	CHECK(*originalIterator == 0);
	CHECK(*iterator == 0);
}

TEST_CASE("Container with size iterator compare equal", "[range]") {
	static constexpr auto arrayLength    = 2;
	constexpr auto        rangeContainer = ContainerManip::range(std::array<int, arrayLength>{});

	STATIC_CHECK(std::cbegin(rangeContainer) == std::cbegin(rangeContainer));
	STATIC_CHECK(std::cend(rangeContainer) == std::cend(rangeContainer));
	STATIC_CHECK_FALSE(std::cbegin(rangeContainer) == std::cend(rangeContainer));
	STATIC_CHECK_FALSE(std::next(std::cbegin(rangeContainer)) == std::cbegin(rangeContainer));
	STATIC_CHECK(std::next(std::cbegin(rangeContainer)) == std::prev(std::cend(rangeContainer)));
}

TEST_CASE("Container with size iterator compare not equal", "[range]") {
	static constexpr auto arrayLength    = 2;
	constexpr auto        rangeContainer = ContainerManip::range(std::array<int, arrayLength>{});

	STATIC_CHECK_FALSE(std::cbegin(rangeContainer) != std::cbegin(rangeContainer));
	STATIC_CHECK_FALSE(std::cend(rangeContainer) != std::cend(rangeContainer));
	STATIC_CHECK(std::cbegin(rangeContainer) != std::cend(rangeContainer));
	STATIC_CHECK(std::next(std::cbegin(rangeContainer)) != std::cbegin(rangeContainer));
	STATIC_CHECK_FALSE(std::next(std::cbegin(rangeContainer))
					   != std::prev(std::cend(rangeContainer)));
}

TEST_CASE("Container with size iterator compare less than", "[range]") {
	static constexpr auto arrayLength    = 2;
	static constexpr auto rangeContainer = ContainerManip::range(std::array<int, arrayLength>{});

	static constexpr auto iterator = std::next(std::cbegin(rangeContainer));

	STATIC_CHECK_FALSE(iterator < iterator);
	STATIC_CHECK(iterator < std::next(iterator));
	STATIC_CHECK_FALSE(std::next(iterator) < iterator);
	STATIC_CHECK_FALSE(iterator < std::prev(iterator));
	STATIC_CHECK(std::prev(iterator) < iterator);

	STATIC_CHECK(iterator < std::end(rangeContainer));
	STATIC_CHECK_FALSE(std::end(rangeContainer) < iterator);
	STATIC_CHECK_FALSE(std::next(iterator) < std::end(rangeContainer));
	STATIC_CHECK_FALSE(std::end(rangeContainer) < std::next(iterator));
}

TEST_CASE("Container with size iterator compare less than or equal", "[range]") {
	static constexpr auto arrayLength    = 2;
	static constexpr auto rangeContainer = ContainerManip::range(std::array<int, arrayLength>{});

	static constexpr auto iterator = std::next(std::begin(rangeContainer));

	STATIC_CHECK(iterator <= iterator);
	STATIC_CHECK(iterator <= std::next(iterator));
	STATIC_CHECK_FALSE(std::next(iterator) <= iterator);
	STATIC_CHECK_FALSE(iterator <= std::prev(iterator));
	STATIC_CHECK(std::prev(iterator) <= iterator);

	STATIC_CHECK(iterator <= std::end(rangeContainer));
	STATIC_CHECK_FALSE(std::end(rangeContainer) <= iterator);
	STATIC_CHECK(std::next(iterator) <= std::end(rangeContainer));
	STATIC_CHECK(std::end(rangeContainer) <= std::next(iterator));
}

TEST_CASE("Container with size iterator compare greater than", "[range]") {
	static constexpr auto arrayLength    = 2;
	static constexpr auto rangeContainer = ContainerManip::range(std::array<int, arrayLength>{});

	static constexpr auto iterator = std::next(std::begin(rangeContainer));

	STATIC_CHECK_FALSE(iterator > iterator);
	STATIC_CHECK_FALSE(iterator > std::next(iterator));
	STATIC_CHECK(std::next(iterator) > iterator);
	STATIC_CHECK(iterator > std::prev(iterator));
	STATIC_CHECK_FALSE(std::prev(iterator) > iterator);

	STATIC_CHECK_FALSE(iterator > std::end(rangeContainer));
	STATIC_CHECK(std::end(rangeContainer) > iterator);
	STATIC_CHECK_FALSE(std::next(iterator) > std::end(rangeContainer));
	STATIC_CHECK_FALSE(std::end(rangeContainer) > std::next(iterator));
}

TEST_CASE("Container with size iterator compare greater than or equal", "[range]") {
	static constexpr auto arrayLength    = 2;
	static constexpr auto rangeContainer = ContainerManip::range(std::array<int, arrayLength>{});

	static constexpr auto iterator = std::next(std::begin(rangeContainer));

	STATIC_CHECK(iterator >= iterator);
	STATIC_CHECK_FALSE(iterator >= std::next(iterator));
	STATIC_CHECK(std::next(iterator) >= iterator);
	STATIC_CHECK(iterator >= std::prev(iterator));
	STATIC_CHECK_FALSE(std::prev(iterator) >= iterator);

	STATIC_CHECK_FALSE(iterator >= std::end(rangeContainer));
	STATIC_CHECK(std::end(rangeContainer) >= iterator);
	STATIC_CHECK(std::next(iterator) >= std::end(rangeContainer));
	STATIC_CHECK(std::end(rangeContainer) >= std::next(iterator));
}

TEST_CASE("Container with size iterator add", "[range]") {
	constexpr auto arrayLength     = 10;
	auto           zippedContainer = ContainerManip::range(std::array<int, arrayLength>{});

	auto first = std::begin(zippedContainer);
	auto copy  = first;

	CHECK(first + 1 == ++copy);
	CHECK(first + 2 == first + 1 + 1);
}

TEST_CASE("Container with size iterator subtract", "[range]") {
	constexpr auto arrayLength     = 10;
	auto           zippedContainer = ContainerManip::range(std::array<int, arrayLength>{});

	auto last = std::end(zippedContainer);
	auto copy = last;

	CHECK(last - 1 == --copy);
	CHECK(last - 2 == last - 1 - 1);
}

TEST_CASE("Container with size iterator distance", "[range]") {
	constexpr auto arrayLength     = 10;
	auto           zippedContainer = ContainerManip::range(std::array<int, arrayLength>{});

	auto first = std::begin(zippedContainer);
	auto last  = std::end(zippedContainer);

	CHECK((first + 2) - first == 2);
	CHECK(last - first == arrayLength);
}

TEST_CASE("Container with size iterator add assign", "[range]") {
	constexpr auto arrayLength     = 10;
	auto           zippedContainer = ContainerManip::range(std::array<int, arrayLength>{});

	auto iterator = std::begin(zippedContainer);

	{
		auto itCopy = iterator;
		itCopy += 2;
		CHECK(itCopy == iterator + 2);
	}
	{
		auto itCopy = iterator;
		itCopy += arrayLength;
		CHECK(itCopy == std::end(zippedContainer));
	}
}

TEST_CASE("Container with size iterator subtract assign", "[range]") {
	constexpr auto arrayLength     = 10;
	auto           zippedContainer = ContainerManip::range(std::array<int, arrayLength>{});

	auto iterator = std::end(zippedContainer);

	{
		auto itCopy = iterator;
		itCopy -= 2;
		CHECK(itCopy == iterator - 2);
	}
	{
		auto itCopy = iterator;
		itCopy -= arrayLength;
		CHECK(itCopy == std::begin(zippedContainer));
	}
}

TEST_CASE("Container with size iterator index", "[range]") {
	constexpr auto arrayLength     = 10;
	auto           zippedContainer = ContainerManip::range([]() {
        std::array<int, arrayLength> result{};
        auto                         num = 0;
        for (auto& entry : result) {
            entry = num++;
        }
        return result;
    }());

	auto first = std::begin(zippedContainer);

	CHECK(first[0] == *first);
	CHECK(first[arrayLength / 2] == *(first + arrayLength / 2));
	CHECK(first[arrayLength - 1] == *std::prev(std::end(zippedContainer)));
}

/////

template <typename IT>
class RandomAccessIteratorWrapper {
public:
	using difference_type   = typename std::iterator_traits<IT>::difference_type;
	using value_type        = typename std::iterator_traits<IT>::value_type;
	using pointer           = typename std::iterator_traits<IT>::pointer;
	using reference         = typename std::iterator_traits<IT>::reference;
	using iterator_category = std::random_access_iterator_tag;

	constexpr RandomAccessIteratorWrapper() noexcept(std::is_nothrow_constructible_v<IT>)
		: iterator{} {}

	explicit constexpr RandomAccessIteratorWrapper(IT iter) noexcept(
		std::is_nothrow_constructible_v<IT, IT>)
		: iterator{iter} {}

	constexpr auto operator*() noexcept(noexcept(*iterator)) -> decltype(auto) { return *iterator; }
	constexpr auto operator*() const noexcept(noexcept(*iterator)) -> decltype(auto) {
		return *iterator;
	}
	constexpr auto operator->() noexcept(noexcept(iterator.operator->())) -> decltype(auto) {
		return iterator.operator->();
	}
	constexpr auto operator->() const noexcept(noexcept(iterator.operator->())) -> decltype(auto) {
		return iterator.operator->();
	}

	constexpr auto operator++() noexcept(noexcept(++iterator)) -> RandomAccessIteratorWrapper& {
		++iterator;
		return *this;
	}
	constexpr auto operator++(int /*unused*/) noexcept(noexcept(RandomAccessIteratorWrapper{
		iterator++})) -> RandomAccessIteratorWrapper {
		return RandomAccessIteratorWrapper{iterator++};
	}

	constexpr auto operator--() noexcept(noexcept(--iterator)) -> RandomAccessIteratorWrapper& {
		--iterator;
		return *this;
	}
	constexpr auto operator--(int /*unused*/) noexcept(noexcept(RandomAccessIteratorWrapper{
		iterator--})) -> RandomAccessIteratorWrapper {
		return RandomAccessIteratorWrapper{iterator--};
	}

	[[nodiscard]] constexpr auto
	operator==(RandomAccessIteratorWrapper& rhs) noexcept(noexcept(iterator == rhs.iterator))
		-> bool {
		return iterator == rhs.iterator;
	}
	[[nodiscard]] constexpr auto operator==(RandomAccessIteratorWrapper& rhs) const
		noexcept(noexcept(iterator == rhs.iterator)) -> bool {
		return iterator == rhs.iterator;
	}
	[[nodiscard]] constexpr auto
	operator!=(RandomAccessIteratorWrapper& rhs) noexcept(noexcept(iterator != rhs.iterator))
		-> bool {
		return iterator != rhs.iterator;
	}
	[[nodiscard]] constexpr auto operator!=(RandomAccessIteratorWrapper& rhs) const
		noexcept(noexcept(iterator != rhs.iterator)) -> bool {
		return iterator != rhs.iterator;
	}
	[[nodiscard]] constexpr auto
	operator<=(RandomAccessIteratorWrapper& rhs) noexcept(noexcept(iterator <= rhs.iterator))
		-> bool {
		return iterator <= rhs.iterator;
	}
	[[nodiscard]] constexpr auto operator<=(RandomAccessIteratorWrapper& rhs) const
		noexcept(noexcept(iterator <= rhs.iterator)) -> bool {
		return iterator <= rhs.iterator;
	}
	[[nodiscard]] constexpr auto
	operator>=(RandomAccessIteratorWrapper& rhs) noexcept(noexcept(iterator >= rhs.iterator))
		-> bool {
		return iterator >= rhs.iterator;
	}
	[[nodiscard]] constexpr auto operator>=(RandomAccessIteratorWrapper& rhs) const
		noexcept(noexcept(iterator >= rhs.iterator)) -> bool {
		return iterator >= rhs.iterator;
	}
	[[nodiscard]] constexpr auto
	operator<(RandomAccessIteratorWrapper& rhs) noexcept(noexcept(iterator < rhs.iterator))
		-> bool {
		return iterator < rhs.iterator;
	}
	[[nodiscard]] constexpr auto operator<(RandomAccessIteratorWrapper& rhs) const
		noexcept(noexcept(iterator < rhs.iterator)) -> bool {
		return iterator < rhs.iterator;
	}
	[[nodiscard]] constexpr auto
	operator>(RandomAccessIteratorWrapper& rhs) noexcept(noexcept(iterator > rhs.iterator))
		-> bool {
		return iterator > rhs.iterator;
	}
	[[nodiscard]] constexpr auto operator>(RandomAccessIteratorWrapper& rhs) const
		noexcept(noexcept(iterator > rhs.iterator)) -> bool {
		return iterator > rhs.iterator;
	}

	[[nodiscard]] constexpr auto
	operator==(const RandomAccessIteratorWrapper& rhs) noexcept(noexcept(iterator == rhs.iterator))
		-> bool {
		return iterator == rhs.iterator;
	}
	[[nodiscard]] constexpr auto operator==(const RandomAccessIteratorWrapper& rhs) const
		noexcept(noexcept(iterator == rhs.iterator)) -> bool {
		return iterator == rhs.iterator;
	}
	[[nodiscard]] constexpr auto
	operator!=(const RandomAccessIteratorWrapper& rhs) noexcept(noexcept(iterator != rhs.iterator))
		-> bool {
		return iterator != rhs.iterator;
	}
	[[nodiscard]] constexpr auto operator!=(const RandomAccessIteratorWrapper& rhs) const
		noexcept(noexcept(iterator != rhs.iterator)) -> bool {
		return iterator != rhs.iterator;
	}
	[[nodiscard]] constexpr auto
	operator<=(const RandomAccessIteratorWrapper& rhs) noexcept(noexcept(iterator <= rhs.iterator))
		-> bool {
		return iterator <= rhs.iterator;
	}
	[[nodiscard]] constexpr auto operator<=(const RandomAccessIteratorWrapper& rhs) const
		noexcept(noexcept(iterator <= rhs.iterator)) -> bool {
		return iterator <= rhs.iterator;
	}
	[[nodiscard]] constexpr auto
	operator>=(const RandomAccessIteratorWrapper& rhs) noexcept(noexcept(iterator >= rhs.iterator))
		-> bool {
		return iterator >= rhs.iterator;
	}
	[[nodiscard]] constexpr auto operator>=(const RandomAccessIteratorWrapper& rhs) const
		noexcept(noexcept(iterator >= rhs.iterator)) -> bool {
		return iterator >= rhs.iterator;
	}
	[[nodiscard]] constexpr auto
	operator<(const RandomAccessIteratorWrapper& rhs) noexcept(noexcept(iterator < rhs.iterator))
		-> bool {
		return iterator < rhs.iterator;
	}
	[[nodiscard]] constexpr auto operator<(const RandomAccessIteratorWrapper& rhs) const
		noexcept(noexcept(iterator < rhs.iterator)) -> bool {
		return iterator < rhs.iterator;
	}
	[[nodiscard]] constexpr auto
	operator>(const RandomAccessIteratorWrapper& rhs) noexcept(noexcept(iterator > rhs.iterator))
		-> bool {
		return iterator > rhs.iterator;
	}
	[[nodiscard]] constexpr auto operator>(const RandomAccessIteratorWrapper& rhs) const
		noexcept(noexcept(iterator > rhs.iterator)) -> bool {
		return iterator > rhs.iterator;
	}

	constexpr auto operator+(difference_type offset) noexcept(noexcept(RandomAccessIteratorWrapper{
		iterator + offset})) -> RandomAccessIteratorWrapper {
		return RandomAccessIteratorWrapper{iterator + offset};
	}
	constexpr auto operator+(difference_type offset) const
		noexcept(noexcept(RandomAccessIteratorWrapper{iterator + offset}))
			-> RandomAccessIteratorWrapper {
		return RandomAccessIteratorWrapper{iterator + offset};
	}
	constexpr auto operator-(difference_type offset) noexcept(noexcept(RandomAccessIteratorWrapper{
		iterator - offset})) -> RandomAccessIteratorWrapper {
		return RandomAccessIteratorWrapper{iterator - offset};
	}
	constexpr auto operator-(difference_type offset) const
		noexcept(noexcept(RandomAccessIteratorWrapper{iterator - offset}))
			-> RandomAccessIteratorWrapper {
		return RandomAccessIteratorWrapper{iterator - offset};
	}

	constexpr auto operator+=(difference_type offset) noexcept(noexcept(RandomAccessIteratorWrapper{
		iterator += offset})) -> RandomAccessIteratorWrapper {
		return RandomAccessIteratorWrapper{iterator += offset};
	}
	constexpr auto operator-=(difference_type offset) noexcept(noexcept(RandomAccessIteratorWrapper{
		iterator -= offset})) -> RandomAccessIteratorWrapper {
		return RandomAccessIteratorWrapper{iterator -= offset};
	}

	constexpr auto
	operator-(RandomAccessIteratorWrapper& rhs) noexcept(noexcept(iterator - rhs.iterator))
		-> decltype(auto) {
		return iterator - rhs.iterator;
	}
	constexpr auto operator-(RandomAccessIteratorWrapper& rhs) const
		noexcept(noexcept(iterator - rhs.iterator)) -> decltype(auto) {
		return iterator - rhs.iterator;
	}
	constexpr auto
	operator-(const RandomAccessIteratorWrapper& rhs) noexcept(noexcept(iterator - rhs.iterator))
		-> decltype(auto) {
		return iterator - rhs.iterator;
	}
	constexpr auto operator-(const RandomAccessIteratorWrapper& rhs) const
		noexcept(noexcept(iterator - rhs.iterator)) -> decltype(auto) {
		return iterator - rhs.iterator;
	}

	constexpr auto operator[](difference_type offset) noexcept(noexcept(iterator[offset]))
		-> decltype(auto) {
		return iterator[offset];
	}
	constexpr auto operator[](difference_type offset) const noexcept(noexcept(iterator[offset]))
		-> decltype(auto) {
		return iterator[offset];
	}

private:
	IT iterator;
};

template <template <typename> typename IteratorWrapper, typename ContainerType>
class ContainerWrapperWithoutSize {
public:
	explicit constexpr ContainerWrapperWithoutSize(ContainerType orignalContainer)
		: container{orignalContainer} {}

	[[nodiscard]] constexpr auto
	begin() noexcept(noexcept(IteratorWrapper{std::begin(container)})) {
		return IteratorWrapper{std::begin(container)};
	}
	[[nodiscard]] constexpr auto begin() const
		noexcept(noexcept(IteratorWrapper{std::begin(container)})) {
		return IteratorWrapper{std::begin(container)};
	}
	[[nodiscard]] constexpr auto
	cbegin() noexcept(noexcept(IteratorWrapper{std::cbegin(container)})) {
		return IteratorWrapper{std::cbegin(container)};
	}
	[[nodiscard]] constexpr auto
	rbegin() noexcept(noexcept(IteratorWrapper{std::rbegin(container)})) {
		return IteratorWrapper{std::rbegin(container)};
	}
	[[nodiscard]] constexpr auto rbegin() const
		noexcept(noexcept(IteratorWrapper{std::rbegin(container)})) {
		return IteratorWrapper{std::rbegin(container)};
	}
	[[nodiscard]] constexpr auto
	crbegin() noexcept(noexcept(IteratorWrapper{std::crbegin(container)})) {
		return IteratorWrapper{std::crbegin(container)};
	}

	[[nodiscard]] constexpr auto end() noexcept(noexcept(IteratorWrapper{std::end(container)})) {
		return IteratorWrapper{std::end(container)};
	}
	[[nodiscard]] constexpr auto end() const
		noexcept(noexcept(IteratorWrapper{std::end(container)})) {
		return IteratorWrapper{std::end(container)};
	}
	[[nodiscard]] constexpr auto cend() noexcept(noexcept(IteratorWrapper{std::cend(container)})) {
		return IteratorWrapper{std::cend(container)};
	}
	[[nodiscard]] constexpr auto rend() noexcept(noexcept(IteratorWrapper{std::rend(container)})) {
		return IteratorWrapper{std::rend(container)};
	}
	[[nodiscard]] constexpr auto rend() const
		noexcept(noexcept(IteratorWrapper{std::rend(container)})) {
		return IteratorWrapper{std::rend(container)};
	}
	[[nodiscard]] constexpr auto
	crend() noexcept(noexcept(IteratorWrapper{std::crend(container)})) {
		return IteratorWrapper{std::crend(container)};
	}

protected:
	[[nodiscard]] constexpr auto getContainer() noexcept -> ContainerType& { return container; }
	[[nodiscard]] constexpr auto getContainer() const noexcept -> const ContainerType& {
		return container;
	}

private:
	ContainerType container;
};

template <template <typename> typename IteratorWrapper, typename ContainerType>
class ContainerWrapperWithSize
	: public ContainerWrapperWithoutSize<IteratorWrapper, ContainerType> {
	using Base = ContainerWrapperWithoutSize<IteratorWrapper, ContainerType>;

	template <typename T, typename = decltype(std::declval<ContainerType&>().size())>
	[[nodiscard]] constexpr auto
	sizeHelper(T /*unused*/) noexcept(noexcept(std::declval<ContainerType&>().size()))
		-> decltype(auto) {
		return Base::getContainer().size();
	};
	template <typename T, typename = decltype(std::declval<const ContainerType&>().size())>
	[[nodiscard]] constexpr auto sizeHelper(T /*unused*/) const
		noexcept(noexcept(std::declval<const ContainerType&>().size())) -> decltype(auto) {
		return Base::getContainer().size();
	};

	[[nodiscard]] constexpr auto
	sizeHelper(float /*unused*/) noexcept(noexcept(std::size(std::declval<ContainerType&>())))
		-> decltype(auto) {
		return std::size(Base::getContainer());
	};
	[[nodiscard]] constexpr auto sizeHelper(float /*unused*/) const
		noexcept(noexcept(std::size(std::declval<const ContainerType&>()))) -> decltype(auto) {
		return std::size(Base::getContainer());
	};

public:
	using ContainerWrapperWithoutSize<IteratorWrapper, ContainerType>::ContainerWrapperWithoutSize;

	[[nodiscard]] constexpr auto size() noexcept(noexcept(sizeHelper(0))) -> decltype(auto) {
		return sizeHelper(0);
	}
	[[nodiscard]] constexpr auto size() const noexcept(noexcept(sizeHelper(0))) -> decltype(auto) {
		return sizeHelper(0);
	}
};

enum class ContainerHasSizeFunction { with, without };

template <template <typename> typename IteratorWrapper,
		  ContainerHasSizeFunction containerHasSizeFunction>
struct Factory {
private:
	template <typename Category>
	static constexpr auto includesHelper() noexcept -> bool {
		if constexpr (std::is_same_v<iterator_category, std::output_iterator_tag>) {
			return std::is_same_v<Category, std::output_iterator_tag>;
		}

		return std::is_base_of_v<Category, iterator_category>;
	}

public:
	template <typename SubContainerType>
	using ContainerType =
		std::conditional_t<containerHasSizeFunction == ContainerHasSizeFunction::with,
						   ContainerWrapperWithSize<IteratorWrapper, SubContainerType>,
						   ContainerWrapperWithoutSize<IteratorWrapper, SubContainerType>>;

	template <typename SubContainerType>
	constexpr auto operator()(SubContainerType container) const
		noexcept(noexcept(ContainerType<SubContainerType>{container})) {
		return ContainerType<SubContainerType>{container};
	}

	using iterator_category =
		typename std::iterator_traits<IteratorWrapper<int*>>::iterator_category;

	template <typename Category>
	static constexpr bool includes = includesHelper<Category>();

	static constexpr bool hasSizeFunction =
		containerHasSizeFunction == ContainerHasSizeFunction::with;
	static constexpr bool hasReverseIterators = hasSizeFunction;
};

using TTT = std::tuple<Factory<RandomAccessIteratorWrapper, ContainerHasSizeFunction::with>,
					   Factory<RandomAccessIteratorWrapper, ContainerHasSizeFunction::without>>;

TEMPLATE_LIST_TEST_CASE("iterator default construct ", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	STATIC_CHECK(HasOperator<DefaultConstructor>{}(rangeContainer));
	STATIC_CHECK(HasOperator<DefaultConstructor, End>{}(rangeContainer));

	if constexpr (TestType::hasReverseIterators) {
		STATIC_CHECK(HasOperator<DefaultConstructor, RBegin>{}(rangeContainer));
		STATIC_CHECK(HasOperator<DefaultConstructor, REnd>{}(rangeContainer));
	}
}

TEMPLATE_LIST_TEST_CASE("iterator copy assign ", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	STATIC_CHECK(HasOperator<CopyConstructor>{}(rangeContainer));
	STATIC_CHECK(HasOperator<CopyConstructor, End>{}(rangeContainer));

	if constexpr (TestType::hasReverseIterators) {
		STATIC_CHECK(HasOperator<CopyConstructor, RBegin>{}(rangeContainer));
		STATIC_CHECK(HasOperator<CopyConstructor, REnd>{}(rangeContainer));
	}
}

TEMPLATE_LIST_TEST_CASE("iterator move construct ", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	STATIC_CHECK(HasOperator<OperatorCopyAssignment>{}(rangeContainer));
	STATIC_CHECK(HasOperator<OperatorCopyAssignment, End>{}(rangeContainer));

	if constexpr (TestType::hasReverseIterators) {
		STATIC_CHECK(HasOperator<OperatorCopyAssignment, RBegin>{}(rangeContainer));
		STATIC_CHECK(HasOperator<OperatorCopyAssignment, REnd>{}(rangeContainer));
	}
}

TEMPLATE_LIST_TEST_CASE("iterator move assign ", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	STATIC_CHECK(HasOperator<MoveConstructor>{}(rangeContainer));
	STATIC_CHECK(HasOperator<MoveConstructor, End>{}(rangeContainer));

	if constexpr (TestType::hasReverseIterators) {
		STATIC_CHECK(HasOperator<MoveConstructor, RBegin>{}(rangeContainer));
		STATIC_CHECK(HasOperator<MoveConstructor, REnd>{}(rangeContainer));
	}
}

TEMPLATE_LIST_TEST_CASE("iterator copy construct ", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	STATIC_CHECK(HasOperator<OperatorMoveAssignment>{}(rangeContainer));
	STATIC_CHECK(HasOperator<OperatorMoveAssignment, End>{}(rangeContainer));

	if constexpr (TestType::hasReverseIterators) {
		STATIC_CHECK(HasOperator<OperatorMoveAssignment, RBegin>{}(rangeContainer));
		STATIC_CHECK(HasOperator<OperatorMoveAssignment, REnd>{}(rangeContainer));
	}
}

TEMPLATE_LIST_TEST_CASE("iterator post increment", "[range]", TTT) {
	static constexpr TestType factory{};
	static constexpr auto     container = factory(std::array{1, 2, 3, 4});

	auto rangeContainer = ContainerManip::range(container);

	constexpr auto test = [](auto iterator, auto value, auto valueAfterIncrement) {
		CHECK(*iterator == value);

		auto resultIterator = iterator++;

		CHECK(*iterator == valueAfterIncrement);
		CHECK(*resultIterator == value);
	};

	STATIC_CHECK(HasOperator<OperatorPostIncrement>{}(rangeContainer));
	test(std::begin(rangeContainer), 0, 1);
	test(std::cbegin(rangeContainer), 0, 1);

	if constexpr (TestType::hasReverseIterators) {
		STATIC_CHECK(HasOperator<OperatorPostIncrement, RBegin>{}(rangeContainer));
		test(std::rbegin(rangeContainer), 3, 2);
		test(std::crbegin(rangeContainer), 3, 2);
	}
}

TEMPLATE_LIST_TEST_CASE("iterator pre increment", "[range]", TTT) {
	static constexpr TestType factory{};
	static constexpr auto     container = factory(std::array{1, 2, 3, 4});

	auto rangeContainer = ContainerManip::range(container);

	constexpr auto test = [](auto iterator, auto value, auto valueAfterIncrement) {
		CHECK(*iterator == value);

		auto resultIterator = ++iterator;

		CHECK(*iterator == valueAfterIncrement);
		CHECK(*resultIterator == valueAfterIncrement);
	};

	STATIC_CHECK(HasOperator<OperatorPreIncrement>{}(rangeContainer));
	test(std::begin(rangeContainer), 0, 1);
	test(std::cbegin(rangeContainer), 0, 1);

	if constexpr (TestType::hasReverseIterators) {
		STATIC_CHECK(HasOperator<OperatorPreIncrement, RBegin>{}(rangeContainer));
		test(std::rbegin(rangeContainer), 3, 2);
		test(std::crbegin(rangeContainer), 3, 2);
	}
}

TEMPLATE_LIST_TEST_CASE("iterator post decrement", "[range]", TTT) {
	static constexpr TestType factory{};
	static constexpr auto     container = factory(std::array{1, 2, 3, 4});

	auto rangeContainer = ContainerManip::range(container);

	constexpr auto test = [](auto iterator, auto value, auto valueAfterDecrement) {
		CHECK(*iterator == value);

		auto resultIterator = iterator--;

		CHECK(*iterator == valueAfterDecrement);
		CHECK(*resultIterator == value);
	};

	STATIC_CHECK(HasOperator<OperatorPostDecrement>{}(rangeContainer));
	test(++std::begin(rangeContainer), 1, 0);
	test(++std::cbegin(rangeContainer), 1, 0);

	if constexpr (TestType::hasReverseIterators) {
		STATIC_CHECK(HasOperator<OperatorPostDecrement, RBegin>{}(rangeContainer));
		test(++std::rbegin(rangeContainer), 2, 3);
		test(++std::crbegin(rangeContainer), 2, 3);
	}
}

TEMPLATE_LIST_TEST_CASE("iterator pre decrement", "[range]", TTT) {
	static constexpr TestType factory{};
	static constexpr auto     container = factory(std::array{1, 2, 3, 4});

	auto rangeContainer = ContainerManip::range(container);

	constexpr auto test = [](auto iterator, auto value, auto valueAfterDecrement) {
		CHECK(*iterator == value);

		auto resultIterator = --iterator;

		CHECK(*iterator == valueAfterDecrement);
		CHECK(*resultIterator == valueAfterDecrement);
	};

	STATIC_CHECK(HasOperator<OperatorPreDecrement>{}(rangeContainer));
	test(++std::begin(rangeContainer), 1, 0);
	test(++std::cbegin(rangeContainer), 1, 0);

	if constexpr (TestType::hasReverseIterators) {
		STATIC_CHECK(HasOperator<OperatorPreDecrement, RBegin>{}(rangeContainer));
		test(++std::rbegin(rangeContainer), 2, 3);
		test(++std::crbegin(rangeContainer), 2, 3);
	}
}

TEMPLATE_LIST_TEST_CASE("iterator dereference", "[range]", TTT) {
	static constexpr TestType factory{};
	static constexpr auto     testContainerSize = 10U;
	static constexpr auto     container         = factory(std::array<int, testContainerSize>{});

	static constexpr auto rangeContainer = ContainerManip::range(container);

	STATIC_CHECK(HasOperator<OperatorDeref>{}(rangeContainer));
	{
		static constexpr auto iteratorFirstElement  = std::cbegin(rangeContainer);
		static constexpr auto iteratorSecondElement = std::next(iteratorFirstElement);
		static constexpr auto iteratorFirthElement  = std::next(iteratorSecondElement);

		STATIC_CHECK(*iteratorFirstElement == 0);
		STATIC_CHECK(*iteratorSecondElement == 1);
		STATIC_CHECK(*iteratorFirthElement == 2);

		if constexpr (HasOperator<OperatorPreDecrement, End>{}(rangeContainer)) {
			static constexpr auto iteratorEnd               = std::cend(rangeContainer);
			static constexpr auto iteratorLastElement       = std::prev(iteratorEnd);
			static constexpr auto iteratorOneButLastElement = std::prev(iteratorLastElement);

			STATIC_CHECK(*iteratorLastElement == testContainerSize - 1);
			STATIC_CHECK(*iteratorOneButLastElement == testContainerSize - 2);
		}
	}
}

TEMPLATE_LIST_TEST_CASE("reverse iterator dereference", "[range]", TTT) {
	static constexpr TestType factory{};
	static constexpr auto     testContainerSize = 10U;
	static constexpr auto     container         = factory(std::array<int, testContainerSize>{});

	static constexpr auto rangeContainer = ContainerManip::range(container);

	if constexpr (TestType::hasReverseIterators) {
		STATIC_CHECK(HasOperator<OperatorDeref, RBegin>{}(rangeContainer));

		static constexpr auto iteratorFirstElement  = std::crbegin(rangeContainer);
		static constexpr auto iteratorSecondElement = std::next(iteratorFirstElement);
		static constexpr auto iteratorFirthElement  = std::next(iteratorSecondElement);

		STATIC_CHECK(*iteratorFirstElement == testContainerSize - 1);
		STATIC_CHECK(*iteratorSecondElement == testContainerSize - 2);
		STATIC_CHECK(*iteratorFirthElement == testContainerSize - 3);

		if constexpr (HasOperator<OperatorPreDecrement, REnd>{}(rangeContainer)) {
			static constexpr auto iteratorEnd               = std::crend(rangeContainer);
			static constexpr auto iteratorLastElement       = std::prev(iteratorEnd);
			static constexpr auto iteratorOneButLastElement = std::prev(iteratorLastElement);

			STATIC_CHECK(*iteratorLastElement == 0);
			STATIC_CHECK(*iteratorOneButLastElement == 1);
		}
	}
}

TEMPLATE_LIST_TEST_CASE("iterator dereference pointer", "[range]", TTT) {
	static constexpr TestType factory{};
	static constexpr auto     testContainerSize = 10U;
	static constexpr auto     container         = factory(std::array<int, testContainerSize>{});

	static constexpr auto rangeContainer = ContainerManip::range(container);

	STATIC_CHECK(HasOperator<OperatorDerefPtr>{}(rangeContainer));
	{
		static constexpr auto iteratorFirstElement  = std::cbegin(rangeContainer);
		static constexpr auto iteratorSecondElement = std::next(iteratorFirstElement);
		static constexpr auto iteratorFirthElement  = std::next(iteratorSecondElement);

		STATIC_CHECK(iteratorFirstElement.operator->() == 0);
		STATIC_CHECK(iteratorSecondElement.operator->() == 1);
		STATIC_CHECK(iteratorFirthElement.operator->() == 2);

		if constexpr (HasOperator<OperatorPreDecrement, End>{}(rangeContainer)) {
			static constexpr auto iteratorEnd               = std::cend(rangeContainer);
			static constexpr auto iteratorLastElement       = std::prev(iteratorEnd);
			static constexpr auto iteratorOneButLastElement = std::prev(iteratorLastElement);

			STATIC_CHECK(iteratorLastElement.operator->() == testContainerSize - 1);
			STATIC_CHECK(iteratorOneButLastElement.operator->() == testContainerSize - 2);
		}
	}
}

TEMPLATE_LIST_TEST_CASE("reverse iterator dereference pointer", "[range]", TTT) {
	static constexpr TestType factory{};
	static constexpr auto     testContainerSize = 10U;
	static constexpr auto     container         = factory(std::array<int, testContainerSize>{});

	static constexpr auto rangeContainer = ContainerManip::range(container);

	if constexpr (TestType::hasReverseIterators) {
		STATIC_CHECK(HasOperator<OperatorDerefPtr, RBegin>{}(rangeContainer));

		static constexpr auto iteratorFirstElement  = std::crbegin(rangeContainer);
		static constexpr auto iteratorSecondElement = std::next(iteratorFirstElement);
		static constexpr auto iteratorFirthElement  = std::next(iteratorSecondElement);

		STATIC_CHECK(iteratorFirstElement.operator->() == testContainerSize - 1);
		STATIC_CHECK(iteratorSecondElement.operator->() == testContainerSize - 2);
		STATIC_CHECK(iteratorFirthElement.operator->() == testContainerSize - 3);

		if constexpr (HasOperator<OperatorPreDecrement, REnd>{}(rangeContainer)) {
			static constexpr auto iteratorEnd               = std::crend(rangeContainer);
			static constexpr auto iteratorLastElement       = std::prev(iteratorEnd);
			static constexpr auto iteratorOneButLastElement = std::prev(iteratorLastElement);

			STATIC_CHECK(iteratorLastElement.operator->() == 0);
			STATIC_CHECK(iteratorOneButLastElement.operator->() == 1);
		}
	}
}

TEMPLATE_LIST_TEST_CASE("iterator compare equal", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	STATIC_CHECK(HasOperator<OperatorEqual>{}(rangeContainer));

	STATIC_CHECK(std::cbegin(rangeContainer) == std::cbegin(rangeContainer));
	STATIC_CHECK(std::cend(rangeContainer) == std::cend(rangeContainer));
	STATIC_CHECK_FALSE(std::cbegin(rangeContainer) == std::cend(rangeContainer));
	STATIC_CHECK_FALSE(std::next(std::cbegin(rangeContainer)) == std::cbegin(rangeContainer));
	STATIC_CHECK(std::next(std::next(std::cbegin(rangeContainer))) == std::cend(rangeContainer));
}

TEMPLATE_LIST_TEST_CASE("reverse iterator compare equal", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	if constexpr (TestType::hasReverseIterators) {
		STATIC_CHECK(HasOperator<OperatorEqual, RBegin>{}(rangeContainer));

		STATIC_CHECK(std::crbegin(rangeContainer) == std::crbegin(rangeContainer));
		STATIC_CHECK(std::crend(rangeContainer) == std::crend(rangeContainer));
		STATIC_CHECK_FALSE(std::crbegin(rangeContainer) == std::crend(rangeContainer));
		STATIC_CHECK_FALSE(std::next(std::crbegin(rangeContainer)) == std::crbegin(rangeContainer));
		STATIC_CHECK(std::next(std::crbegin(rangeContainer))
					 == std::prev(std::crend(rangeContainer)));
	}
}

TEMPLATE_LIST_TEST_CASE("iterator compare not equal", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	STATIC_CHECK(HasOperator<OperatorNotEqual>{}(rangeContainer));

	STATIC_CHECK_FALSE(std::cbegin(rangeContainer) != std::cbegin(rangeContainer));
	STATIC_CHECK_FALSE(std::cend(rangeContainer) != std::cend(rangeContainer));
	STATIC_CHECK(std::cbegin(rangeContainer) != std::cend(rangeContainer));
	STATIC_CHECK(std::next(std::cbegin(rangeContainer)) != std::cbegin(rangeContainer));
	STATIC_CHECK_FALSE(std::next(std::next(std::cbegin(rangeContainer)))
					   != std::cend(rangeContainer));
}

TEMPLATE_LIST_TEST_CASE("reverse iterator compare not equal", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	STATIC_CHECK(HasOperator<OperatorNotEqual>{}(rangeContainer));

	if constexpr (TestType::hasReverseIterators) {
		STATIC_CHECK(HasOperator<OperatorNotEqual, RBegin>{}(rangeContainer));

		STATIC_CHECK_FALSE(std::crbegin(rangeContainer) != std::crbegin(rangeContainer));
		STATIC_CHECK_FALSE(std::crend(rangeContainer) != std::crend(rangeContainer));
		STATIC_CHECK(std::crbegin(rangeContainer) != std::crend(rangeContainer));
		STATIC_CHECK(std::next(std::crbegin(rangeContainer)) != std::crbegin(rangeContainer));
		STATIC_CHECK_FALSE(std::next(std::next(std::crbegin(rangeContainer)))
						   != std::crend(rangeContainer));
	}
}

TEMPLATE_LIST_TEST_CASE("iterator compare less than", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	STATIC_CHECK(HasOperator<OperatorLessThan>{}(rangeContainer));

	static constexpr auto iterator = std::next(std::cbegin(rangeContainer));
	STATIC_CHECK_FALSE(iterator < iterator);
	STATIC_CHECK(iterator < std::next(iterator));
	STATIC_CHECK_FALSE(std::next(iterator) < iterator);
	STATIC_CHECK_FALSE(iterator < std::prev(iterator));
	STATIC_CHECK(std::prev(iterator) < iterator);

	STATIC_CHECK(iterator < std::cend(rangeContainer));
	STATIC_CHECK_FALSE(std::cend(rangeContainer) < iterator);
	STATIC_CHECK_FALSE(std::next(iterator) < std::cend(rangeContainer));
	STATIC_CHECK_FALSE(std::cend(rangeContainer) < std::next(iterator));
}

TEMPLATE_LIST_TEST_CASE("reverse iterator compare less than", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	if constexpr (TestType::hasReverseIterators) {
		STATIC_CHECK(HasOperator<OperatorLessThan, RBegin>{}(rangeContainer));

		static constexpr auto iterator = std::next(std::crbegin(rangeContainer));
		STATIC_CHECK_FALSE(iterator < iterator);
		STATIC_CHECK(iterator < std::next(iterator));
		STATIC_CHECK_FALSE(std::next(iterator) < iterator);
		STATIC_CHECK_FALSE(iterator < std::prev(iterator));
		STATIC_CHECK(std::prev(iterator) < iterator);

		STATIC_CHECK(iterator < std::crend(rangeContainer));
		STATIC_CHECK_FALSE(std::crend(rangeContainer) < iterator);
		STATIC_CHECK_FALSE(std::next(iterator) < std::crend(rangeContainer));
		STATIC_CHECK_FALSE(std::crend(rangeContainer) < std::next(iterator));
	}
}

TEMPLATE_LIST_TEST_CASE("iterator compare less than or equal", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	STATIC_CHECK(HasOperator<OperatorLessThanEqual>{}(rangeContainer));

	static constexpr auto iterator = std::next(std::cbegin(rangeContainer));
	STATIC_CHECK(iterator <= iterator);
	STATIC_CHECK(iterator <= std::next(iterator));
	STATIC_CHECK_FALSE(std::next(iterator) <= iterator);
	STATIC_CHECK_FALSE(iterator <= std::prev(iterator));
	STATIC_CHECK(std::prev(iterator) <= iterator);

	STATIC_CHECK(iterator <= std::cend(rangeContainer));
	STATIC_CHECK_FALSE(std::cend(rangeContainer) <= iterator);
	STATIC_CHECK(std::next(iterator) <= std::cend(rangeContainer));
	STATIC_CHECK(std::cend(rangeContainer) <= std::next(iterator));
}

TEMPLATE_LIST_TEST_CASE("reverse iterator compare less than or equal", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	if constexpr (TestType::hasReverseIterators) {
		STATIC_CHECK(HasOperator<OperatorLessThanEqual, RBegin>{}(rangeContainer));

		static constexpr auto iterator = std::next(std::crbegin(rangeContainer));
		STATIC_CHECK(iterator <= iterator);
		STATIC_CHECK(iterator <= std::next(iterator));
		STATIC_CHECK_FALSE(std::next(iterator) <= iterator);
		STATIC_CHECK_FALSE(iterator <= std::prev(iterator));
		STATIC_CHECK(std::prev(iterator) <= iterator);

		STATIC_CHECK(iterator <= std::crend(rangeContainer));
		STATIC_CHECK_FALSE(std::crend(rangeContainer) <= iterator);
		STATIC_CHECK(std::next(iterator) <= std::crend(rangeContainer));
		STATIC_CHECK(std::crend(rangeContainer) <= std::next(iterator));
	}
}

TEMPLATE_LIST_TEST_CASE("iterator compare greater than", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	STATIC_CHECK(HasOperator<OperatorGreaterThan>{}(rangeContainer));

	static constexpr auto iterator = std::next(std::cbegin(rangeContainer));
	STATIC_CHECK_FALSE(iterator > iterator);
	STATIC_CHECK_FALSE(iterator > std::next(iterator));
	STATIC_CHECK(std::next(iterator) > iterator);
	STATIC_CHECK(iterator > std::prev(iterator));
	STATIC_CHECK_FALSE(std::prev(iterator) > iterator);

	STATIC_CHECK_FALSE(iterator > std::cend(rangeContainer));
	STATIC_CHECK(std::cend(rangeContainer) > iterator);
	STATIC_CHECK_FALSE(std::next(iterator) > std::cend(rangeContainer));
	STATIC_CHECK_FALSE(std::cend(rangeContainer) > std::next(iterator));
}

TEMPLATE_LIST_TEST_CASE("reverse iterator compare greater than", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	if constexpr (TestType::hasReverseIterators) {
		STATIC_CHECK(HasOperator<OperatorGreaterThan, RBegin>{}(rangeContainer));

		static constexpr auto iterator = std::next(std::crbegin(rangeContainer));
		STATIC_CHECK_FALSE(iterator > iterator);
		STATIC_CHECK_FALSE(iterator > std::next(iterator));
		STATIC_CHECK(std::next(iterator) > iterator);
		STATIC_CHECK(iterator > std::prev(iterator));
		STATIC_CHECK_FALSE(std::prev(iterator) > iterator);

		STATIC_CHECK_FALSE(iterator > std::crend(rangeContainer));
		STATIC_CHECK(std::crend(rangeContainer) > iterator);
		STATIC_CHECK_FALSE(std::next(iterator) > std::crend(rangeContainer));
		STATIC_CHECK_FALSE(std::crend(rangeContainer) > std::next(iterator));
	}
}

TEMPLATE_LIST_TEST_CASE("iterator compare greater than or equal", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	STATIC_CHECK(HasOperator<OperatorGreaterThanEqual>{}(rangeContainer));

	static constexpr auto iterator = std::next(std::cbegin(rangeContainer));
	STATIC_CHECK(iterator >= iterator);
	STATIC_CHECK_FALSE(iterator >= std::next(iterator));
	STATIC_CHECK(std::next(iterator) >= iterator);
	STATIC_CHECK(iterator >= std::prev(iterator));
	STATIC_CHECK_FALSE(std::prev(iterator) >= iterator);

	STATIC_CHECK_FALSE(iterator >= std::cend(rangeContainer));
	STATIC_CHECK(std::cend(rangeContainer) >= iterator);
	STATIC_CHECK(std::next(iterator) >= std::cend(rangeContainer));
	STATIC_CHECK(std::cend(rangeContainer) >= std::next(iterator));
}

TEMPLATE_LIST_TEST_CASE("reverse iterator compare greater than or equal", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	if constexpr (TestType::hasReverseIterators) {
		STATIC_CHECK(HasOperator<OperatorGreaterThanEqual, RBegin>{}(rangeContainer));

		static constexpr auto iterator = std::next(std::crbegin(rangeContainer));
		STATIC_CHECK(iterator >= iterator);
		STATIC_CHECK_FALSE(iterator >= std::next(iterator));
		STATIC_CHECK(std::next(iterator) >= iterator);
		STATIC_CHECK(iterator >= std::prev(iterator));
		STATIC_CHECK_FALSE(std::prev(iterator) >= iterator);

		STATIC_CHECK_FALSE(iterator >= std::crend(rangeContainer));
		STATIC_CHECK(std::crend(rangeContainer) >= iterator);
		STATIC_CHECK(std::next(iterator) >= std::crend(rangeContainer));
		STATIC_CHECK(std::crend(rangeContainer) >= std::next(iterator));
	}
}

TEMPLATE_LIST_TEST_CASE("iterator add", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	STATIC_CHECK(HasOperator<OperatorAdd>{}(rangeContainer));

	static constexpr auto firstElementIterator  = std::cbegin(rangeContainer);
	static constexpr auto secondElementIterator = std::next(firstElementIterator);
	static constexpr auto thirdElementIterator  = std::next(secondElementIterator);

	// NOLINTNEXTLINE(*-redundant-expression)
	STATIC_CHECK(firstElementIterator + 0 == firstElementIterator);
	STATIC_CHECK(firstElementIterator + 1 == secondElementIterator);
	STATIC_CHECK(secondElementIterator + 1 == thirdElementIterator);
	STATIC_CHECK(firstElementIterator + 2 == thirdElementIterator);
	STATIC_CHECK(firstElementIterator + 2 == std::cend(rangeContainer));

	STATIC_CHECK(secondElementIterator + (-1) == firstElementIterator);
	STATIC_CHECK(thirdElementIterator + (-1) == secondElementIterator);
	STATIC_CHECK(thirdElementIterator + (-2) == firstElementIterator);

	if constexpr (HasOperator<OperatorAdd, End>{}(rangeContainer)) {
		STATIC_CHECK(std::cend(rangeContainer) + 0 == std::cend(rangeContainer));
		STATIC_CHECK(std::cend(rangeContainer) + (-1) == secondElementIterator);
		STATIC_CHECK(std::cend(rangeContainer) + (-2) == firstElementIterator);
	}
}

TEMPLATE_LIST_TEST_CASE("reverse iterator add", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	if constexpr (TestType::hasReverseIterators) {
		STATIC_CHECK(HasOperator<OperatorAdd, RBegin>{}(rangeContainer));

		static constexpr auto firstElementIterator  = std::crbegin(rangeContainer);
		static constexpr auto secondElementIterator = std::next(firstElementIterator);
		static constexpr auto thirdElementIterator  = std::next(secondElementIterator);

		// NOLINTNEXTLINE(*-redundant-expression)
		STATIC_CHECK(firstElementIterator + 0 == firstElementIterator);
		STATIC_CHECK(firstElementIterator + 1 == secondElementIterator);
		STATIC_CHECK(secondElementIterator + 1 == thirdElementIterator);
		STATIC_CHECK(firstElementIterator + 2 == thirdElementIterator);
		STATIC_CHECK(firstElementIterator + 2 == std::crend(rangeContainer));

		STATIC_CHECK(secondElementIterator + (-1) == firstElementIterator);
		STATIC_CHECK(thirdElementIterator + (-1) == secondElementIterator);
		STATIC_CHECK(thirdElementIterator + (-2) == firstElementIterator);

		if constexpr (HasOperator<OperatorAdd, REnd>{}(rangeContainer)) {
			STATIC_CHECK(std::crend(rangeContainer) + 0 == std::crend(rangeContainer));
			STATIC_CHECK(std::crend(rangeContainer) + (-1) == secondElementIterator);
			STATIC_CHECK(std::crend(rangeContainer) + (-2) == firstElementIterator);
		}
	}
}

TEMPLATE_LIST_TEST_CASE("iterator add to difference", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	STATIC_CHECK(HasOperator<OperatorAddToDifference>{}(rangeContainer));

	static constexpr auto firstElementIterator  = std::cbegin(rangeContainer);
	static constexpr auto secondElementIterator = std::next(firstElementIterator);
	static constexpr auto thirdElementIterator  = std::next(secondElementIterator);

	// NOLINTNEXTLINE(*-redundant-expression)
	STATIC_CHECK(0 + firstElementIterator == firstElementIterator);
	STATIC_CHECK(1 + firstElementIterator == secondElementIterator);
	STATIC_CHECK(1 + secondElementIterator == thirdElementIterator);
	STATIC_CHECK(2 + firstElementIterator == thirdElementIterator);
	STATIC_CHECK(2 + firstElementIterator == std::cend(rangeContainer));

	STATIC_CHECK((-1) + secondElementIterator == firstElementIterator);
	STATIC_CHECK((-1) + thirdElementIterator == secondElementIterator);
	STATIC_CHECK((-2) + thirdElementIterator == firstElementIterator);

	if constexpr (HasOperator<OperatorAddToDifference, End>{}(rangeContainer)) {
		STATIC_CHECK(0 + std::cend(rangeContainer) == std::cend(rangeContainer));
		STATIC_CHECK((-1) + std::cend(rangeContainer) == secondElementIterator);
		STATIC_CHECK((-2) + std::cend(rangeContainer) == firstElementIterator);
	}
}

TEMPLATE_LIST_TEST_CASE("reverse iterator add to difference", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	if constexpr (TestType::hasReverseIterators) {
		STATIC_CHECK(HasOperator<OperatorAddToDifference, RBegin>{}(rangeContainer));

		static constexpr auto firstElementIterator  = std::crbegin(rangeContainer);
		static constexpr auto secondElementIterator = std::next(firstElementIterator);
		static constexpr auto thirdElementIterator  = std::next(secondElementIterator);

		// NOLINTNEXTLINE(*-redundant-expression)
		STATIC_CHECK(0 + firstElementIterator == firstElementIterator);
		STATIC_CHECK(1 + firstElementIterator == secondElementIterator);
		STATIC_CHECK(1 + secondElementIterator == thirdElementIterator);
		STATIC_CHECK(2 + firstElementIterator == thirdElementIterator);
		STATIC_CHECK(2 + firstElementIterator == std::crend(rangeContainer));

		STATIC_CHECK((-1) + secondElementIterator == firstElementIterator);
		STATIC_CHECK((-1) + thirdElementIterator == secondElementIterator);
		STATIC_CHECK((-2) + thirdElementIterator == firstElementIterator);

		if constexpr (HasOperator<OperatorAddToDifference, REnd>{}(rangeContainer)) {
			STATIC_CHECK(0 + std::crend(rangeContainer) == std::crend(rangeContainer));
			STATIC_CHECK((-1) + std::crend(rangeContainer) == secondElementIterator);
			STATIC_CHECK((-2) + std::crend(rangeContainer) == firstElementIterator);
		}
	}
}

TEMPLATE_LIST_TEST_CASE("iterator subtract", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	STATIC_CHECK(HasOperator<OperatorSubtract>{}(rangeContainer));

	static constexpr auto firstElementIterator  = std::cbegin(rangeContainer);
	static constexpr auto secondElementIterator = std::next(firstElementIterator);
	static constexpr auto thirdElementIterator  = std::next(secondElementIterator);

	// NOLINTNEXTLINE(*-redundant-expression)
	STATIC_CHECK(firstElementIterator - 0 == firstElementIterator);
	STATIC_CHECK(secondElementIterator - 1 == firstElementIterator);
	STATIC_CHECK(thirdElementIterator - 1 == secondElementIterator);
	STATIC_CHECK(thirdElementIterator - 2 == firstElementIterator);
	if constexpr (HasOperator<OperatorSubtract, End>{}(rangeContainer)) {
		STATIC_CHECK(std::cend(rangeContainer) - 0 == std::cend(rangeContainer));
		STATIC_CHECK(std::cend(rangeContainer) - 1 == secondElementIterator);
		STATIC_CHECK(std::cend(rangeContainer) - 2 == firstElementIterator);
	}

	STATIC_CHECK(firstElementIterator - (-1) == secondElementIterator);
	STATIC_CHECK(secondElementIterator - (-1) == thirdElementIterator);
	STATIC_CHECK(firstElementIterator - (-2) == thirdElementIterator);
}

TEMPLATE_LIST_TEST_CASE("reverse iterator subtract", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	if constexpr (TestType::hasReverseIterators) {
		STATIC_CHECK(HasOperator<OperatorSubtract, RBegin>{}(rangeContainer));

		static constexpr auto firstElementIterator  = std::crbegin(rangeContainer);
		static constexpr auto secondElementIterator = std::next(firstElementIterator);
		static constexpr auto thirdElementIterator  = std::next(secondElementIterator);

		// NOLINTNEXTLINE(*-redundant-expression)
		STATIC_CHECK(firstElementIterator - 0 == firstElementIterator);
		STATIC_CHECK(secondElementIterator - 1 == firstElementIterator);
		STATIC_CHECK(thirdElementIterator - 1 == secondElementIterator);
		STATIC_CHECK(thirdElementIterator - 2 == firstElementIterator);
		if constexpr (HasOperator<OperatorSubtract, REnd>{}(rangeContainer)) {
			STATIC_CHECK(std::crend(rangeContainer) - 0 == std::crend(rangeContainer));
			STATIC_CHECK(std::crend(rangeContainer) - 1 == secondElementIterator);
			STATIC_CHECK(std::crend(rangeContainer) - 2 == firstElementIterator);
		}

		STATIC_CHECK(firstElementIterator - (-1) == secondElementIterator);
		STATIC_CHECK(secondElementIterator - (-1) == thirdElementIterator);
		STATIC_CHECK(firstElementIterator - (-2) == thirdElementIterator);
	}
}

TEMPLATE_LIST_TEST_CASE("iterator distance", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	STATIC_CHECK(HasOperator<OperatorDistance>{}(rangeContainer));

	static constexpr auto firstElementIterator  = std::cbegin(rangeContainer);
	static constexpr auto secondElementIterator = std::next(firstElementIterator);
	static constexpr auto thirdElementIterator  = std::next(secondElementIterator);

	STATIC_CHECK(firstElementIterator - firstElementIterator == 0);
	STATIC_CHECK(secondElementIterator - firstElementIterator == 1);
	STATIC_CHECK(thirdElementIterator - secondElementIterator == 1);
	STATIC_CHECK(thirdElementIterator - firstElementIterator == 2);

	if constexpr (HasOperator<OperatorDistance, End>{}(rangeContainer)) {
		STATIC_CHECK(std::cend(rangeContainer) - firstElementIterator == 2);
		STATIC_CHECK(std::cend(rangeContainer) - secondElementIterator == 1);
	}
}

TEMPLATE_LIST_TEST_CASE("reverse iterator distance", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	if constexpr (TestType::hasReverseIterators) {
		STATIC_CHECK(HasOperator<OperatorDistance, RBegin>{}(rangeContainer));

		static constexpr auto firstElementIterator  = std::crbegin(rangeContainer);
		static constexpr auto secondElementIterator = std::next(firstElementIterator);
		static constexpr auto thirdElementIterator  = std::next(secondElementIterator);

		STATIC_CHECK(firstElementIterator - firstElementIterator == 0);
		STATIC_CHECK(secondElementIterator - firstElementIterator == 1);
		STATIC_CHECK(thirdElementIterator - secondElementIterator == 1);
		STATIC_CHECK(thirdElementIterator - firstElementIterator == 2);

		if constexpr (HasOperator<OperatorDistance, REnd>{}(rangeContainer)) {
			STATIC_CHECK(std::crend(rangeContainer) - firstElementIterator == 2);
			STATIC_CHECK(std::crend(rangeContainer) - secondElementIterator == 1);
		}
	}
}

TEMPLATE_LIST_TEST_CASE("iterator add assign", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	static constexpr auto invokeAddAssign = [](auto iterator, auto n) {
		auto operatorResult = iterator += n;
		return std::pair{operatorResult, iterator};
	};

	STATIC_CHECK(HasOperator<OperatorAddAssign>{}(rangeContainer));

	static constexpr auto firstElementIterator  = std::cbegin(rangeContainer);
	static constexpr auto secondElementIterator = std::next(firstElementIterator);
	static constexpr auto thirdElementIterator  = std::next(secondElementIterator);

	STATIC_CHECK(invokeAddAssign(firstElementIterator, 0)
				 == std::pair{firstElementIterator, firstElementIterator});
	STATIC_CHECK(invokeAddAssign(firstElementIterator, 1)
				 == std::pair{secondElementIterator, secondElementIterator});
	STATIC_CHECK(invokeAddAssign(secondElementIterator, 1)
				 == std::pair{thirdElementIterator, thirdElementIterator});
	STATIC_CHECK(invokeAddAssign(firstElementIterator, 2)
				 == std::pair{thirdElementIterator, thirdElementIterator});

	STATIC_CHECK(invokeAddAssign(secondElementIterator, -1)
				 == std::pair{firstElementIterator, firstElementIterator});
	STATIC_CHECK(invokeAddAssign(thirdElementIterator, -1)
				 == std::pair{secondElementIterator, secondElementIterator});
	STATIC_CHECK(invokeAddAssign(thirdElementIterator, -2)
				 == std::pair{firstElementIterator, firstElementIterator});

	if constexpr (HasOperator<OperatorAddAssign, End>{}(rangeContainer)) {
		STATIC_CHECK(invokeAddAssign(std::cend(rangeContainer), 0)
					 == std::pair{std::cend(rangeContainer), std::cend(rangeContainer)});
		STATIC_CHECK(invokeAddAssign(std::cend(rangeContainer), -1)
					 == std::pair{secondElementIterator, secondElementIterator});
		STATIC_CHECK(invokeAddAssign(std::cend(rangeContainer), -2)
					 == std::pair{firstElementIterator, firstElementIterator});
	}
}

TEMPLATE_LIST_TEST_CASE("reverse iterator add assign", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	static constexpr auto invokeAddAssign = [](auto iterator, auto n) {
		auto operatorResult = iterator += n;
		return std::pair{operatorResult, iterator};
	};

	if constexpr (TestType::hasReverseIterators) {
		STATIC_CHECK(HasOperator<OperatorAddAssign, RBegin>{}(rangeContainer));

		static constexpr auto firstElementIterator  = std::crbegin(rangeContainer);
		static constexpr auto secondElementIterator = std::next(firstElementIterator);
		static constexpr auto thirdElementIterator  = std::next(secondElementIterator);

		STATIC_CHECK(invokeAddAssign(firstElementIterator, 0)
					 == std::pair{firstElementIterator, firstElementIterator});
		STATIC_CHECK(invokeAddAssign(firstElementIterator, 1)
					 == std::pair{secondElementIterator, secondElementIterator});
		STATIC_CHECK(invokeAddAssign(secondElementIterator, 1)
					 == std::pair{thirdElementIterator, thirdElementIterator});
		STATIC_CHECK(invokeAddAssign(firstElementIterator, 2)
					 == std::pair{thirdElementIterator, thirdElementIterator});

		STATIC_CHECK(invokeAddAssign(secondElementIterator, -1)
					 == std::pair{firstElementIterator, firstElementIterator});
		STATIC_CHECK(invokeAddAssign(thirdElementIterator, -1)
					 == std::pair{secondElementIterator, secondElementIterator});
		STATIC_CHECK(invokeAddAssign(thirdElementIterator, -2)
					 == std::pair{firstElementIterator, firstElementIterator});

		if constexpr (HasOperator<OperatorAddAssign, REnd>{}(rangeContainer)) {
			STATIC_CHECK(invokeAddAssign(std::crend(rangeContainer), 0)
						 == std::pair{std::crend(rangeContainer), std::crend(rangeContainer)});
			STATIC_CHECK(invokeAddAssign(std::crend(rangeContainer), -1)
						 == std::pair{secondElementIterator, secondElementIterator});
			STATIC_CHECK(invokeAddAssign(std::crend(rangeContainer), -2)
						 == std::pair{firstElementIterator, firstElementIterator});
		}
	}
}

TEMPLATE_LIST_TEST_CASE("iterator subtract assign", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	static constexpr auto invokeSubtractAssign = [](auto iterator, auto n) {
		auto operatorResult = iterator -= n;
		return std::pair{operatorResult, iterator};
	};

	STATIC_CHECK(HasOperator<OperatorSubtractAssign>{}(rangeContainer));

	static constexpr auto firstElementIterator  = std::cbegin(rangeContainer);
	static constexpr auto secondElementIterator = std::next(firstElementIterator);
	static constexpr auto thirdElementIterator  = std::next(secondElementIterator);

	STATIC_CHECK(invokeSubtractAssign(firstElementIterator, 0)
				 == std::pair{firstElementIterator, firstElementIterator});
	STATIC_CHECK(invokeSubtractAssign(secondElementIterator, 1)
				 == std::pair{firstElementIterator, firstElementIterator});
	STATIC_CHECK(invokeSubtractAssign(thirdElementIterator, 1)
				 == std::pair{secondElementIterator, secondElementIterator});
	STATIC_CHECK(invokeSubtractAssign(thirdElementIterator, 2)
				 == std::pair{firstElementIterator, firstElementIterator});

	STATIC_CHECK(invokeSubtractAssign(firstElementIterator, -1)
				 == std::pair{secondElementIterator, secondElementIterator});
	STATIC_CHECK(invokeSubtractAssign(secondElementIterator, -1)
				 == std::pair{thirdElementIterator, thirdElementIterator});
	STATIC_CHECK(invokeSubtractAssign(firstElementIterator, -2)
				 == std::pair{thirdElementIterator, thirdElementIterator});

	if constexpr (HasOperator<OperatorSubtractAssign, End>{}(rangeContainer)) {
		STATIC_CHECK(invokeSubtractAssign(std::cend(rangeContainer), 0)
					 == std::pair{std::cend(rangeContainer), std::cend(rangeContainer)});
		STATIC_CHECK(invokeSubtractAssign(std::cend(rangeContainer), 1)
					 == std::pair{secondElementIterator, secondElementIterator});
		STATIC_CHECK(invokeSubtractAssign(std::cend(rangeContainer), 2)
					 == std::pair{firstElementIterator, firstElementIterator});
	}
}

TEMPLATE_LIST_TEST_CASE("reverse iterator subtract assign", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	static constexpr auto invokeSubtractAssign = [](auto iterator, auto n) {
		auto operatorResult = iterator -= n;
		return std::pair{operatorResult, iterator};
	};

	if constexpr (TestType::hasReverseIterators) {
		STATIC_CHECK(HasOperator<OperatorSubtractAssign, RBegin>{}(rangeContainer));

		static constexpr auto firstElementIterator  = std::crbegin(rangeContainer);
		static constexpr auto secondElementIterator = std::next(firstElementIterator);
		static constexpr auto thirdElementIterator  = std::next(secondElementIterator);

		STATIC_CHECK(invokeSubtractAssign(firstElementIterator, 0)
					 == std::pair{firstElementIterator, firstElementIterator});
		STATIC_CHECK(invokeSubtractAssign(secondElementIterator, 1)
					 == std::pair{firstElementIterator, firstElementIterator});
		STATIC_CHECK(invokeSubtractAssign(thirdElementIterator, 1)
					 == std::pair{secondElementIterator, secondElementIterator});
		STATIC_CHECK(invokeSubtractAssign(thirdElementIterator, 2)
					 == std::pair{firstElementIterator, firstElementIterator});

		STATIC_CHECK(invokeSubtractAssign(firstElementIterator, -1)
					 == std::pair{secondElementIterator, secondElementIterator});
		STATIC_CHECK(invokeSubtractAssign(secondElementIterator, -1)
					 == std::pair{thirdElementIterator, thirdElementIterator});
		STATIC_CHECK(invokeSubtractAssign(firstElementIterator, -2)
					 == std::pair{thirdElementIterator, thirdElementIterator});

		if constexpr (HasOperator<OperatorSubtractAssign, REnd>{}(rangeContainer)) {
			STATIC_CHECK(invokeSubtractAssign(std::crend(rangeContainer), 0)
						 == std::pair{std::crend(rangeContainer), std::crend(rangeContainer)});
			STATIC_CHECK(invokeSubtractAssign(std::crend(rangeContainer), 1)
						 == std::pair{secondElementIterator, secondElementIterator});
			STATIC_CHECK(invokeSubtractAssign(std::crend(rangeContainer), 2)
						 == std::pair{firstElementIterator, firstElementIterator});
		}
	}
}

TEMPLATE_LIST_TEST_CASE("iterator index", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	STATIC_CHECK(HasOperator<OperatorIndex>{}(rangeContainer));

	static constexpr auto firstElementIterator  = std::cbegin(rangeContainer);
	static constexpr auto secondElementIterator = std::next(firstElementIterator);
	static constexpr auto thirdElementIterator  = std::next(secondElementIterator);

	STATIC_CHECK(firstElementIterator[0] == *firstElementIterator);
	STATIC_CHECK(firstElementIterator[1] == *secondElementIterator);
	STATIC_CHECK(secondElementIterator[1] == *thirdElementIterator);
	STATIC_CHECK(firstElementIterator[2] == *thirdElementIterator);

	STATIC_CHECK(secondElementIterator[-1] == *firstElementIterator);
	STATIC_CHECK(thirdElementIterator[-1] == *secondElementIterator);
	STATIC_CHECK(thirdElementIterator[-2] == *firstElementIterator);

	if constexpr (HasOperator<OperatorIndex, End>{}(rangeContainer)) {
		STATIC_CHECK(std::cend(rangeContainer)[-1] == *secondElementIterator);
		STATIC_CHECK(std::cend(rangeContainer)[-2] == *firstElementIterator);
	}
}

TEMPLATE_LIST_TEST_CASE("reverse iterator index", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	if constexpr (TestType::hasReverseIterators) {
		STATIC_CHECK(HasOperator<OperatorIndex, RBegin>{}(rangeContainer));

		static constexpr auto firstElementIterator  = std::crbegin(rangeContainer);
		static constexpr auto secondElementIterator = std::next(firstElementIterator);
		static constexpr auto thirdElementIterator  = std::next(secondElementIterator);

		STATIC_CHECK(firstElementIterator[0] == *firstElementIterator);
		STATIC_CHECK(firstElementIterator[1] == *secondElementIterator);
		STATIC_CHECK(secondElementIterator[1] == *thirdElementIterator);
		STATIC_CHECK(firstElementIterator[2] == *thirdElementIterator);

		STATIC_CHECK(secondElementIterator[-1] == *firstElementIterator);
		STATIC_CHECK(thirdElementIterator[-1] == *secondElementIterator);
		STATIC_CHECK(thirdElementIterator[-2] == *firstElementIterator);

		if constexpr (HasOperator<OperatorIndex, REnd>{}(rangeContainer)) {
			STATIC_CHECK(std::crend(rangeContainer)[-1] == *secondElementIterator);
			STATIC_CHECK(std::crend(rangeContainer)[-2] == *firstElementIterator);
		}
	}
}

TEMPLATE_LIST_TEST_CASE("copy construct range", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	static constexpr auto rangeContainerCopy    = rangeContainer;
	static constexpr auto iteratorFirstElement  = std::cbegin(rangeContainerCopy);
	static constexpr auto iteratorSecondElement = std::next(iteratorFirstElement);

	STATIC_CHECK(*iteratorFirstElement == 0);
	STATIC_CHECK(*iteratorSecondElement == 1);
	STATIC_CHECK(std::next(iteratorSecondElement) == std::cend(rangeContainerCopy));
}

TEMPLATE_LIST_TEST_CASE("move construct range", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto rangeContainer = ContainerManip::range(factory(std::array<int, 2>{}));

	static constexpr auto rangeContainerCopy    = std::move(rangeContainer);
	static constexpr auto iteratorFirstElement  = std::cbegin(rangeContainerCopy);
	static constexpr auto iteratorSecondElement = std::next(iteratorFirstElement);

	STATIC_CHECK(*iteratorFirstElement == 0);
	STATIC_CHECK(*iteratorSecondElement == 1);
	STATIC_CHECK(std::next(iteratorSecondElement) == std::cend(rangeContainerCopy));
}

TEMPLATE_LIST_TEST_CASE("range over lvalue referenced containers", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto container      = factory(std::array<int, 2>{});
	static constexpr auto rangeContainer = ContainerManip::range(container);

	STATIC_CHECK(std::next(std::next(std::cbegin(rangeContainer))) == std::cend(rangeContainer));
}

TEMPLATE_LIST_TEST_CASE("range over rvalue referenced containers", "[range]", TTT) {
	static constexpr TestType factory{};

	static constexpr auto container      = factory(std::array<int, 2>{});
	static constexpr auto rangeContainer = ContainerManip::range(std::move(container));

	STATIC_CHECK(std::next(std::next(std::cbegin(rangeContainer))) == std::cend(rangeContainer));
}
