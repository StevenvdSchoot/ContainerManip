#include <cstddef>

#include <array>
#include <forward_list>
#include <iterator>
#include <list>
#include <string>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <vector>

#include <zip.hpp>

#include <catch2/catch_test_macros.hpp>

#include "OperandPresence.hpp"

template <typename T, size_t length>
constexpr auto arrayEqual(const std::array<T, length>& rhs,
						  const std::array<T, length>& lhs) noexcept {
	auto it1   = begin(rhs);
	auto it2   = begin(lhs);
	auto endIt = end(rhs);
	while (it1 != endIt) {
		if (*it1++ != *it2++) {
			return false;
		}
	}
	return true;
}

TEST_CASE("constexpr iteration over lvalue referenced containers", "[zip]") {
	static constexpr auto testContainerSize = 20U;

	constexpr auto test = []() noexcept {
		std::array<int, testContainerSize>  arr1{};
		std::array<bool, testContainerSize> arr2{};

		int num = 0;
		for (auto& entry : arr1) {
			entry = num++;
		}

		int acc = 0;
		for (auto [i, b] : ContainerManip::zip(arr1, arr2)) {
			if (!b) {
				acc += i;
			}
		}
		return acc == (arr1.size() * (arr1.size() - 1U) / 2U);
	};
	STATIC_CHECK(test());
}

TEST_CASE("constexpr reference iteration over lvalue referenced containers", "[zip]") {
	static constexpr auto arrayLength = 20;

	struct S {
		S()         = default;
		S(const S&) = delete;

		auto operator=(const S&) -> S& = delete;
		S(S&&)                         = delete;

		auto operator=(S&&) -> S& = delete;
		~S()                      = default;
	};

	constexpr auto testArtifacts = []() noexcept {
		std::array<int, arrayLength> arr1{};
		std::array<S, arrayLength>   arr2{};

		int num = 0;
		for (auto& entry : arr1) {
			entry = num++;
		}

		int acc = 0;
		for (auto&& [i, s] : ContainerManip::zip(arr1, arr2)) {
			acc += i;
			i = 0;
		}
		return std::tuple{arr1, acc};
	}();
	constexpr auto arr1 = std::get<0>(testArtifacts);
	constexpr auto acc  = std::get<1>(testArtifacts);

	STATIC_CHECK(acc == (arr1.size() * (arr1.size() - 1U) / 2U));

	constexpr auto nullArray1 = std::array<int, arrayLength>{};
	STATIC_CHECK(arrayEqual(arr1, nullArray1));
}

TEST_CASE("constexpr iteration over rvalue referenced containers", "[zip]") {
	constexpr auto test = []() noexcept {
		auto constexpr arrayLength = 20;

		auto constexpr createArray = []() {
			std::array<int, arrayLength> result{};
			int                          num = 0;
			for (auto& entry : result) {
				entry = num++;
			}
			return result;
		};

		int acc = 0;
		for (auto [i, b] : ContainerManip::zip(createArray(), std::array<bool, arrayLength>{})) {
			if (!b) {
				acc += i;
			}
		}
		return acc == (arrayLength * (arrayLength - 1U) / 2U);
	};
	STATIC_CHECK(test());
}

TEST_CASE("constexpr reference iteration over rvalue referenced containers", "[zip]") {
	struct S {
		S()         = default;
		S(const S&) = delete;

		auto operator=(const S&) -> S& = delete;
		S(S&&)                         = default;

		auto operator=(S&&) -> S& = default;
		~S()                      = default;
	};

	constexpr auto test = []() noexcept {
		auto constexpr arrayLength = 20;

		auto constexpr createArray = []() {
			std::array<int, arrayLength> result{};
			int                          num = 0;
			for (auto& entry : result) {
				entry = num++;
			}
			return result;
		};

		int  acc             = 0;
		auto zippedContainer = ContainerManip::zip(createArray(), std::array<S, arrayLength>{});
		for (auto&& [i, s] : zippedContainer) {
			acc += i;
		}
		return acc == (arrayLength * (arrayLength - 1U) / 2U);
	};

	STATIC_CHECK(test());
}

TEST_CASE("3 containers with different element types", "[zip]") {
	static constexpr auto testContainerSize = 20U;

	std::array<int, testContainerSize>         arr1{};
	std::array<bool, testContainerSize>        arr2{};
	std::array<std::string, testContainerSize> arr3{};

	int num = 0;
	for (auto& entry : arr1) {
		entry = num++;
	}
	num = 0;
	for (auto& entry : arr3) {
		entry = std::to_string(num++);
	}

	num = 0;
	for (auto [i, b, s] : ContainerManip::zip(arr1, arr2, arr3)) {
		CHECK(i == num);
		CHECK(!b);
		CHECK(s == std::to_string(num));
		num++;
	}
}

TEST_CASE("const iterator", "[zip]") {
	auto zippedContainer = ContainerManip::zip(std::array{0, 1, 2, 3, 4});

	int index = 0;
	for (auto it = cbegin(zippedContainer); it != cend(zippedContainer); it++) {
		CHECK(std::get<0>(*it) == index++);
	}
}

TEST_CASE("const reverse iterator", "[zip]") {
	auto zippedContainer = ContainerManip::zip(std::array{0, 1, 2, 3, 4});

	int index = 4;
	for (auto it = crbegin(zippedContainer); it != crend(zippedContainer); it++) {
		CHECK(std::get<0>(*it) == index--);
	}
}

TEST_CASE("Copy constructing zipped container", "[zip]") {
	constexpr auto arrayLength = 5U;

	auto validateCopiedContainer = []() {
		auto zippedContainer = ContainerManip::zip(std::array<int, arrayLength>{0, 1, 2, 3, 4});

		return [zippedContainerCopy = zippedContainer]() {
			auto iterator = begin(zippedContainerCopy);
			for (int index = 0; index < arrayLength; iterator++, index++) {
				REQUIRE(iterator != end(zippedContainerCopy));
				CHECK(std::get<0>(*iterator) == index);
			};
			CHECK(iterator == end(zippedContainerCopy));
		};
	}();

	validateCopiedContainer();
}

TEST_CASE("Move constructing zipped container", "[zip]") {
	constexpr auto arrayLength = 5U;

	auto validateMovedContainer = []() {
		auto zippedContainer = ContainerManip::zip(std::array<int, arrayLength>{0, 1, 2, 3, 4});

		return [zippedContainerCopy = std::move(zippedContainer)]() {
			auto iterator = begin(zippedContainerCopy);
			for (int index = 0; index < arrayLength; iterator++, index++) {
				REQUIRE(iterator != end(zippedContainerCopy));
				CHECK(std::get<0>(*iterator) == index);
			};
			CHECK(iterator == end(zippedContainerCopy));
		};
	}();

	validateMovedContainer();
}

TEST_CASE("Copy assigning zipped container", "[zip]") {
	constexpr auto arrayLength = 5U;

	auto zippedContainer = ContainerManip::zip(std::array<int, arrayLength>{0, 0, 0, 0, 0});

	{
		auto zippedContainerNew = ContainerManip::zip(std::array<int, arrayLength>{0, 1, 2, 3, 4});
		zippedContainer         = zippedContainerNew;
	}

	auto iterator = begin(zippedContainer);
	for (int index = 0; index < arrayLength; iterator++, index++) {
		REQUIRE(iterator != end(zippedContainer));
		CHECK(std::get<0>(*iterator) == index);
	};
	CHECK(iterator == end(zippedContainer));
}

TEST_CASE("Move assigning zipped container", "[zip]") {
	constexpr auto arrayLength = 5U;

	auto zippedContainer = ContainerManip::zip(std::array<int, arrayLength>{0, 0, 0, 0, 0});

	{
		auto zippedContainerNew = ContainerManip::zip(std::array<int, arrayLength>{0, 1, 2, 3, 4});
		zippedContainer         = std::move(zippedContainerNew);
	}

	auto iterator = begin(zippedContainer);
	for (int index = 0; index < arrayLength; iterator++, index++) {
		REQUIRE(iterator != end(zippedContainer));
		CHECK(std::get<0>(*iterator) == index);
	};
	CHECK(iterator == end(zippedContainer));
}

template <class InputIt, class UnaryPredicate>
constexpr auto findIfNot(InputIt first, InputIt last, const UnaryPredicate& unaryPredicate)
	-> InputIt {
	for (; first != last; ++first) {
		if (!unaryPredicate(*first)) {
			return first;
		}
	}

	return last;
}

template <class InputIt, class UnaryPredicate>
constexpr auto allOf(InputIt first, InputIt last, const UnaryPredicate& unaryPredicate) -> bool {
	return findIfNot(first, last, unaryPredicate) == last;
}

TEST_CASE("Zipping zipped container", "[zip]") {
	static constexpr auto arrayLength = 5U;

	static constexpr auto zippedZippedContainer = ContainerManip::zip(ContainerManip::zip(
		std::array<int, arrayLength>{0, 1, 2, 3, 4}, std::array<int, arrayLength>{4, 3, 2, 1, 0}));

	static constexpr auto makeTupleOfTuple = [](auto... elements){
		return std::make_tuple(std::tuple{elements...});
	};

	static constexpr auto iterator = std::cbegin(zippedZippedContainer);
	STATIC_CHECK(*iterator == makeTupleOfTuple(0, 4));
	STATIC_CHECK(*std::next(iterator) == makeTupleOfTuple(1, 3));
	STATIC_CHECK(*std::next(iterator, 2) == makeTupleOfTuple(2, 2));
	STATIC_CHECK(*std::next(iterator, 3) == makeTupleOfTuple(3, 1));
	STATIC_CHECK(*std::next(iterator, 4) == makeTupleOfTuple(4, 0));
	STATIC_CHECK(std::next(iterator, 5) == std::cend(zippedZippedContainer));

	STATIC_CHECK(allOf(begin(zippedZippedContainer), end(zippedZippedContainer),
					   [](const auto& zippedZippedElement) {
						   const auto& zippedElement = std::get<0>(zippedZippedElement);
						   return std::get<0>(zippedElement) + std::get<1>(zippedElement) == 4;
					   }));
}

TEST_CASE("iterator post increment", "[zip]") {
	static auto constexpr arrayLength =4U;
	
	auto zippedContainer       = ContainerManip::internal::zip(std::array<int, arrayLength>{});

	for (auto it = begin(zippedContainer);;) {
		auto thisValue = *it++;
		if (it == end(zippedContainer)) {
			break;
		}
		std::get<0>(thisValue) = 1;
	}

	CHECK(allOf(cbegin(zippedContainer), std::prev(cend(zippedContainer)),
					  [](auto const& element) { return std::get<0>(element) == 1; }));
	CHECK(std::get<0>(*std::prev(cend(zippedContainer))) == 0);
}

TEST_CASE("iterator pre increment", "[zip]") {
	auto constexpr arrayLength = 10;
	auto zippedContainer       = ContainerManip::zip(std::array<int, arrayLength>{});

	for (auto it = begin(zippedContainer);;) {
		auto thisValue = *++it;
		if (it == std::prev(end(zippedContainer))) {
			break;
		}
		std::get<0>(thisValue) = 1;
	}

	CHECK(std::get<0>(*cbegin(zippedContainer)) == 0);
	CHECK(allOf(std::next(cbegin(zippedContainer)), std::prev(cend(zippedContainer)),
					  [](auto const& element) { return std::get<0>(element) == 1; }));
	CHECK(std::get<0>(*std::prev(cend(zippedContainer))) == 0);
}

TEST_CASE("iterator post increment reverse", "[zip]") {
	auto constexpr arrayLength = 10;
	auto zippedContainer       = ContainerManip::zip(std::array<int, arrayLength>{});

	for (auto it = rbegin(zippedContainer); it != rend(zippedContainer);) {
		auto thisValue = *it++;
		if (it == rend(zippedContainer)) {
			break;
		}
		std::get<0>(thisValue) = 1;
	}

	CHECK(std::get<0>(*cbegin(zippedContainer)) == 0);
	CHECK(allOf(std::next(cbegin(zippedContainer)), cend(zippedContainer),
					  [](auto const& element) { return std::get<0>(element) == 1; }));
}

TEST_CASE("iterator pre increment reverse", "[zip]") {
	auto constexpr arrayLength = 10;
	auto zippedContainer       = ContainerManip::zip(std::array<int, arrayLength>{});

	for (auto it = rbegin(zippedContainer);;) {
		auto thisValue = *++it;
		if (it == std::prev(rend(zippedContainer))) {
			break;
		}
		std::get<0>(thisValue) = 1;
	}

	CHECK(std::get<0>(*cbegin(zippedContainer)) == 0);
	CHECK(allOf(std::next(cbegin(zippedContainer)), std::prev(cend(zippedContainer)),
					  [](auto const& element) { return std::get<0>(element) == 1; }));
	CHECK(std::get<0>(*std::prev(cend(zippedContainer))) == 0);
}

TEST_CASE("iterator compare equal", "[zip]") {
	static auto constexpr arrayLength     = 2;
	static auto constexpr zippedContainer = ContainerManip::zip(std::array<int, arrayLength>{});

	STATIC_CHECK(cbegin(zippedContainer) == cbegin(zippedContainer));
	STATIC_CHECK(cend(zippedContainer) == cend(zippedContainer));
	STATIC_CHECK_FALSE(cbegin(zippedContainer) == cend(zippedContainer));
	STATIC_CHECK_FALSE(std::next(cbegin(zippedContainer)) == cbegin(zippedContainer));
	STATIC_CHECK(std::next(cbegin(zippedContainer)) == std::prev(cend(zippedContainer)));
}

TEST_CASE("iterator compare not equal", "[zip]") {
	static auto constexpr arrayLength     = 2;
	static auto constexpr zippedContainer = ContainerManip::zip(std::array<int, arrayLength>{});

	STATIC_CHECK_FALSE(cbegin(zippedContainer) != cbegin(zippedContainer));
	STATIC_CHECK_FALSE(cend(zippedContainer) != cend(zippedContainer));
	STATIC_CHECK(cbegin(zippedContainer) != cend(zippedContainer));
	STATIC_CHECK(std::next(cbegin(zippedContainer)) != cbegin(zippedContainer));
	STATIC_CHECK_FALSE(std::next(cbegin(zippedContainer)) != std::prev(cend(zippedContainer)));
}

TEST_CASE("iterator compare less than", "[zip]") {
	static auto constexpr arrayLength     = 2;
	static auto constexpr zippedContainer = ContainerManip::zip(std::array<int, arrayLength>{});

	static auto constexpr iterator = std::next(cbegin(zippedContainer));

	STATIC_CHECK_FALSE(iterator < iterator);
	STATIC_CHECK(iterator < std::next(iterator));
	STATIC_CHECK_FALSE(std::next(iterator) < iterator);
	STATIC_CHECK_FALSE(iterator < std::prev(iterator));
	STATIC_CHECK(std::prev(iterator) < iterator);

	STATIC_CHECK(iterator < end(zippedContainer));
	STATIC_CHECK_FALSE(end(zippedContainer) < iterator);
	STATIC_CHECK_FALSE(std::next(iterator) < end(zippedContainer));
	STATIC_CHECK_FALSE(end(zippedContainer) < std::next(iterator));
}

TEST_CASE("iterator compare less than or equal", "[zip]") {
	static auto constexpr arrayLength     = 2;
	static auto constexpr zippedContainer = ContainerManip::zip(std::array<int, arrayLength>{});

	static auto constexpr iterator = std::next(begin(zippedContainer));

	STATIC_CHECK(iterator <= iterator);
	STATIC_CHECK(iterator <= std::next(iterator));
	STATIC_CHECK_FALSE(std::next(iterator) <= iterator);
	STATIC_CHECK_FALSE(iterator <= std::prev(iterator));
	STATIC_CHECK(std::prev(iterator) <= iterator);

	STATIC_CHECK(iterator <= end(zippedContainer));
	STATIC_CHECK_FALSE(end(zippedContainer) <= iterator);
	STATIC_CHECK(std::next(iterator) <= end(zippedContainer));
	STATIC_CHECK(end(zippedContainer) <= std::next(iterator));
}

TEST_CASE("iterator compare greater than", "[zip]") {
	static auto constexpr arrayLength     = 2;
	static auto constexpr zippedContainer = ContainerManip::zip(std::array<int, arrayLength>{});

	static auto constexpr iterator = std::next(begin(zippedContainer));

	STATIC_CHECK_FALSE(iterator > iterator);
	STATIC_CHECK_FALSE(iterator > std::next(iterator));
	STATIC_CHECK(std::next(iterator) > iterator);
	STATIC_CHECK(iterator > std::prev(iterator));
	STATIC_CHECK_FALSE(std::prev(iterator) > iterator);

	STATIC_CHECK_FALSE(iterator > end(zippedContainer));
	STATIC_CHECK(end(zippedContainer) > iterator);
	STATIC_CHECK_FALSE(std::next(iterator) > end(zippedContainer));
	STATIC_CHECK_FALSE(end(zippedContainer) > std::next(iterator));
}

TEST_CASE("iterator compare greater than or equal", "[zip]") {
	static auto constexpr arrayLength     = 2;
	static auto constexpr zippedContainer = ContainerManip::zip(std::array<int, arrayLength>{});

	static auto constexpr iterator = std::next(begin(zippedContainer));

	STATIC_CHECK(iterator >= iterator);
	STATIC_CHECK_FALSE(iterator >= std::next(iterator));
	STATIC_CHECK(std::next(iterator) >= iterator);
	STATIC_CHECK(iterator >= std::prev(iterator));
	STATIC_CHECK_FALSE(std::prev(iterator) >= iterator);

	STATIC_CHECK_FALSE(iterator >= end(zippedContainer));
	STATIC_CHECK(end(zippedContainer) >= iterator);
	STATIC_CHECK(std::next(iterator) >= end(zippedContainer));
	STATIC_CHECK(end(zippedContainer) >= std::next(iterator));
}

TEST_CASE("iterator add", "[zip]") {
	auto constexpr arrayLength = 10;
	auto zippedContainer       = ContainerManip::zip(std::array<int, arrayLength>{});

	auto first = begin(zippedContainer);
	auto copy  = first;

	CHECK(first + 1 == ++copy);
	CHECK(first + 2 == first + 1 + 1);
}

TEST_CASE("iterator subtract", "[zip]") {
	auto constexpr arrayLength = 10;
	auto zippedContainer       = ContainerManip::zip(std::array<int, arrayLength>{});

	auto last = end(zippedContainer);
	auto copy = last;

	CHECK(last - 1 == --copy);
	CHECK(last - 2 == last - 1 - 1);
}

TEST_CASE("iterator distance", "[zip]") {
	auto constexpr arrayLength = 10;
	auto zippedContainer       = ContainerManip::zip(std::array<int, arrayLength>{});

	auto first = begin(zippedContainer);
	auto last  = end(zippedContainer);

	CHECK((first + 2) - first == 2);
	CHECK(last - first == arrayLength);
}

TEST_CASE("iterator add assign", "[zip]") {
	auto constexpr arrayLength = 10;
	auto zippedContainer       = ContainerManip::zip(std::array<int, arrayLength>{});

	auto iterator = begin(zippedContainer);

	{
		auto itCopy = iterator;
		itCopy += 2;
		CHECK(itCopy == iterator + 2);
	}
	{
		auto itCopy = iterator;
		itCopy += arrayLength;
		CHECK(itCopy == end(zippedContainer));
	}
}

TEST_CASE("iterator subtract assign", "[zip]") {
	auto constexpr arrayLength = 10;
	auto zippedContainer       = ContainerManip::zip(std::array<int, arrayLength>{});

	auto iterator = end(zippedContainer);

	{
		auto itCopy = iterator;
		itCopy -= 2;
		CHECK(itCopy == iterator - 2);
	}
	{
		auto itCopy = iterator;
		itCopy -= arrayLength;
		CHECK(itCopy == begin(zippedContainer));
	}
}

TEST_CASE("iterator index", "[zip]") {
	auto constexpr arrayLength = 10;
	auto zippedContainer       = ContainerManip::zip([]() {
		std::array<int, arrayLength> result{};
		auto                         num = 0;
		for (auto& entry : result) {
			entry = num++;
		}
		return result;
	}());

	auto first = begin(zippedContainer);

	CHECK(first[0] == *first);
	CHECK(first[arrayLength / 2] == *(first + arrayLength / 2));
	CHECK(first[arrayLength - 1] == *std::prev(end(zippedContainer)));
}

template <typename IteratorCategory, typename... ContainerTypes>
constexpr auto zipYieldsIteratorOfType(ContainerTypes&&... containers) noexcept -> bool {
	auto zippedContainer = ContainerManip::zip(std::forward<ContainerTypes>(containers)...);
	return std::is_same_v<IteratorCategory, typename std::iterator_traits<decltype(begin(
												zippedContainer))>::iterator_category>;
};

TEST_CASE("random access iterator deduction", "[zip]") {
	constexpr auto arrayLength = 20;

	std::array<int, arrayLength> arr1{};
	std::array<int, arrayLength> arr2{};
	std::vector<int>             vec1(arrayLength);
	std::vector<int>             vec2(arrayLength);

	STATIC_CHECK(zipYieldsIteratorOfType<std::random_access_iterator_tag>(vec1));
	STATIC_CHECK(zipYieldsIteratorOfType<std::random_access_iterator_tag>(vec1, vec2));
	STATIC_CHECK(zipYieldsIteratorOfType<std::random_access_iterator_tag>(arr1, vec2));
	STATIC_CHECK(zipYieldsIteratorOfType<std::random_access_iterator_tag>(arr1, vec2, arr2));
	STATIC_CHECK(zipYieldsIteratorOfType<std::random_access_iterator_tag>(vec1, arr1, vec2));
	STATIC_CHECK(zipYieldsIteratorOfType<std::random_access_iterator_tag>(vec1, arr1, vec2, arr2));
}

TEST_CASE("bidirectional iterator deduction", "[zip]") {
	constexpr auto arrayLength = 20;

	std::array<int, arrayLength> arr1{};
	std::array<int, arrayLength> arr2{};
	std::list<int>               list1(arrayLength);
	std::list<int>               list2(arrayLength);

	STATIC_CHECK(zipYieldsIteratorOfType<std::bidirectional_iterator_tag>(list1));
	STATIC_CHECK(zipYieldsIteratorOfType<std::bidirectional_iterator_tag>(list1, list2));
	STATIC_CHECK(zipYieldsIteratorOfType<std::bidirectional_iterator_tag>(arr1, list2));
	STATIC_CHECK(zipYieldsIteratorOfType<std::bidirectional_iterator_tag>(arr1, list2, arr2));
	STATIC_CHECK(zipYieldsIteratorOfType<std::bidirectional_iterator_tag>(list1, arr1, list2));
	STATIC_CHECK(
		zipYieldsIteratorOfType<std::bidirectional_iterator_tag>(list1, arr1, list2, arr2));
}

TEST_CASE("forward iterator deduction", "[zip]") {
	constexpr auto arrayLength = 20;

	std::array<int, arrayLength> arr1{};
	std::array<int, arrayLength> arr2{};
	std::list<int>               list1(arrayLength);
	std::list<int>               list2(arrayLength);
	std::forward_list<int>       flist1(arrayLength);
	std::forward_list<int>       flist2(arrayLength);

	STATIC_CHECK(zipYieldsIteratorOfType<std::forward_iterator_tag>(flist1));
	STATIC_CHECK(zipYieldsIteratorOfType<std::forward_iterator_tag>(flist1, flist2));
	STATIC_CHECK(zipYieldsIteratorOfType<std::forward_iterator_tag>(flist1, list1));
	STATIC_CHECK(zipYieldsIteratorOfType<std::forward_iterator_tag>(arr1, flist2));
	STATIC_CHECK(zipYieldsIteratorOfType<std::forward_iterator_tag>(arr1, flist2, list1));
	STATIC_CHECK(zipYieldsIteratorOfType<std::forward_iterator_tag>(arr1, flist2, arr2));
	STATIC_CHECK(zipYieldsIteratorOfType<std::forward_iterator_tag>(flist1, arr1, flist2));
	STATIC_CHECK(zipYieldsIteratorOfType<std::forward_iterator_tag>(flist1, list1, flist2));
	STATIC_CHECK(zipYieldsIteratorOfType<std::forward_iterator_tag>(flist1, arr1, flist2, arr2));
	STATIC_CHECK(zipYieldsIteratorOfType<std::forward_iterator_tag>(flist1, list1, flist2, list2));
}

TEST_CASE("random access iterator implementation", "[zip]") {
	auto zippedContainer = ContainerManip::zip(std::array<int, 1>{});

	STATIC_CHECK(HasOperator<DefaultConstructor>{}(zippedContainer));
	STATIC_CHECK(HasOperator<DefaultConstructor, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<CopyConstructor>{}(zippedContainer));
	STATIC_CHECK(HasOperator<CopyConstructor, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorCopyAssignment>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorCopyAssignment, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<MoveConstructor>{}(zippedContainer));
	STATIC_CHECK(HasOperator<MoveConstructor, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorMoveAssignment>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorMoveAssignment, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorDeref>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorDeref, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorDeref>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorDeref, RBegin>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorDerefPtr>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorDerefPtr, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorDerefPtr>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorDerefPtr, RBegin>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorPreIncrement>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorPreIncrement, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorPostIncrement>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorPostIncrement, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorEqual>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorEqual, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorEqual>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorEqual, RBegin>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorNotEqual>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorNotEqual, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorNotEqual>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorNotEqual, RBegin>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorPreDecrement>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorPreDecrement, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorPostDecrement>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorPostDecrement, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorLessThan>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorLessThan, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorLessThan>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorLessThan, RBegin>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorLessThanEqual>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorLessThanEqual, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorLessThanEqual>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorLessThanEqual, RBegin>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorGreaterThan>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorGreaterThan, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorGreaterThan>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorGreaterThan, RBegin>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorGreaterThanEqual>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorGreaterThanEqual, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorGreaterThanEqual>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorGreaterThanEqual, RBegin>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorAdd>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorAdd, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorAdd>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorAdd, RBegin>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorAddToDifference>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorAddToDifference, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorAddToDifference>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorAddToDifference, RBegin>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorSubtract>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorSubtract, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorDistance>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorDistance, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorAddAssign>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorAddAssign, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorSubtractAssign>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorSubtractAssign, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorIndex>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorIndex, RBegin>{}(zippedContainer));
}

TEST_CASE("bidirectional iterator implementation", "[zip]") {
	auto zippedContainer = ContainerManip::zip(std::list<int>{});

	STATIC_CHECK(HasOperator<DefaultConstructor>{}(zippedContainer));
	STATIC_CHECK(HasOperator<DefaultConstructor, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<CopyConstructor>{}(zippedContainer));
	STATIC_CHECK(HasOperator<CopyConstructor, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorCopyAssignment>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorCopyAssignment, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<MoveConstructor>{}(zippedContainer));
	STATIC_CHECK(HasOperator<MoveConstructor, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorMoveAssignment>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorMoveAssignment, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorDeref>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorDeref, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorDeref>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorDeref, RBegin>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorDerefPtr>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorDerefPtr, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorDerefPtr>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorDerefPtr, RBegin>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorPreIncrement>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorPreIncrement, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorPostIncrement>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorPostIncrement, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorEqual>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorEqual, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorEqual>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorEqual, RBegin>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorNotEqual>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorNotEqual, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorNotEqual>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorNotEqual, RBegin>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorPreDecrement>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorPreDecrement, RBegin>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorPostDecrement>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorPostDecrement, RBegin>{}(zippedContainer));

	STATIC_CHECK(NotHasOperator<OperatorLessThan>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorLessThan, RBegin>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorLessThanEqual>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorLessThanEqual, RBegin>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorGreaterThan>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorGreaterThan, RBegin>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorGreaterThanEqual>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorGreaterThanEqual, RBegin>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorAdd>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorAdd, RBegin>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorAddToDifference>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorAddToDifference, RBegin>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorSubtract>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorSubtract, RBegin>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorDistance>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorDistance, RBegin>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorAddAssign>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorAddAssign, RBegin>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorSubtractAssign>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorSubtractAssign, RBegin>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorIndex>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorIndex, RBegin>{}(zippedContainer));
}

TEST_CASE("forward iterator implementation", "[zip]") {
	auto zippedContainer = ContainerManip::zip(std::forward_list<int>{});

	STATIC_CHECK(HasOperator<DefaultConstructor>{}(zippedContainer));
	STATIC_CHECK(HasOperator<CopyConstructor>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorCopyAssignment>{}(zippedContainer));
	STATIC_CHECK(HasOperator<MoveConstructor>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorMoveAssignment>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorDeref>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorDeref>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorDerefPtr>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorDerefPtr>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorPreIncrement>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorPostIncrement>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorEqual>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorEqual>{}(std::as_const(zippedContainer)));
	STATIC_CHECK(HasOperator<OperatorNotEqual>{}(zippedContainer));
	STATIC_CHECK(HasOperator<OperatorNotEqual>{}(std::as_const(zippedContainer)));

	STATIC_CHECK(NotHasOperator<OperatorPreDecrement>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorPostDecrement>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorLessThan>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorLessThanEqual>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorGreaterThan>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorGreaterThanEqual>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorAdd>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorAddToDifference>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorSubtract>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorDistance>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorAddAssign>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorSubtractAssign>{}(zippedContainer));
	STATIC_CHECK(NotHasOperator<OperatorIndex>{}(zippedContainer));
}
