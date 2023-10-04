#include <utility>

struct Begin {
	template <typename T>
	constexpr auto operator()(T& container) const noexcept {
		return std::begin(container);
	}
};
struct RBegin {
	template <typename T>
	constexpr auto operator()(T& container) const noexcept {
		return std::rbegin(container);
	}
};

struct End {
	template <typename T>
	constexpr auto operator()(T& container) const noexcept {
		return std::end(container);
	}
};

struct REnd {
	template <typename T>
	constexpr auto operator()(T& container) const noexcept {
		return std::rend(container);
	}
};

template <template <typename T> class Operator, typename ContainerToIterator = Begin>
struct HasOperator {
private:
	template <typename Op>
	[[nodiscard]] constexpr auto testPart2(int /*unused*/) const noexcept
		-> std::conditional_t<std::is_same_v<decltype(Op::value), bool>, bool, bool> {
		return Op::value;
	}
	template <typename Op>
	[[nodiscard]] constexpr auto testPart2(float /*unused*/) const noexcept -> bool {
		return true;
	}

	template <typename ContainerType>
	[[nodiscard]] constexpr auto test(ContainerType& container, int /*unused*/) const noexcept
		-> std::conditional_t<true, bool, Operator<decltype(ContainerToIterator{}(container))>> {
		return testPart2<Operator<decltype(ContainerToIterator{}(container))>>(0);
	}
	template <typename ContainerType>
	[[nodiscard]] constexpr auto test(ContainerType& /*container*/, float /*unused*/) const noexcept
		-> bool {
		return false;
	}

public:
	template <typename ContainerType>
	[[nodiscard]] constexpr auto operator()(ContainerType& container) const noexcept -> bool {
		return test(container, 0);
	}
};

template <template <typename T> class Operator, typename ContainerToIterator = Begin>
struct NotHasOperator {
private:
	template <typename ContainerType>
	[[nodiscard]] constexpr auto test(ContainerType& container, int /*unused*/) const noexcept
		-> std::conditional_t<true, bool, Operator<decltype(ContainerToIterator{}(container))>> {
		return false;
	}
	template <typename ContainerType>
	[[nodiscard]] constexpr auto test(ContainerType& /*container*/, float /*unused*/) const noexcept
		-> bool {
		return true;
	}

public:
	template <typename ContainerType>
	[[nodiscard]] constexpr auto operator()(ContainerType& container) const noexcept -> bool {
		return test(container, 0)
			   && test(static_cast<std::add_const_t<ContainerType>&>(container), 0);
	}
};

template <typename IteratorType>
using DefaultConstructor = decltype(IteratorType{});
template <typename IteratorType>
using CopyConstructor = decltype(IteratorType{*static_cast<const IteratorType*>(nullptr)});
template <typename IteratorType>
using OperatorCopyAssignment = decltype(static_cast<IteratorType*>(nullptr)->operator=(
	*static_cast<const IteratorType*>(nullptr)));
template <typename IteratorType>
using MoveConstructor = decltype(IteratorType{std::declval<IteratorType>()});
template <typename IteratorType>
using OperatorMoveAssignment =
	decltype(static_cast<IteratorType*>(nullptr)->operator=(std::declval<IteratorType>()));
template <typename IteratorType>
using OperatorDeref = std::is_same<decltype(static_cast<IteratorType*>(nullptr)->operator*()),
								   typename IteratorType::reference>;
template <typename IteratorType>
using OperatorDerefPtr = std::is_same<decltype(static_cast<IteratorType*>(nullptr)->operator->()),
									  typename IteratorType::reference>;
template <typename IteratorType>
using OperatorPreIncrement =
	std::is_same<decltype(static_cast<IteratorType*>(nullptr)->operator++()), IteratorType&>;
template <typename IteratorType>
using OperatorPostIncrement =
	std::is_convertible<decltype(static_cast<IteratorType*>(nullptr)->operator++(0)),
						const IteratorType&>;
template <typename IteratorType>
using OperatorEqual = std::is_convertible<decltype(static_cast<IteratorType*>(nullptr)->operator==(
											  *static_cast<IteratorType*>(nullptr))),
										  bool>;
template <typename IteratorType>
using OperatorNotEqual =
	std::is_convertible<decltype(static_cast<IteratorType*>(nullptr)->operator!=(
							*static_cast<IteratorType*>(nullptr))),
						bool>;
template <typename IteratorType>
using OperatorPreDecrement =
	std::is_same<decltype(static_cast<IteratorType*>(nullptr)->operator--()), IteratorType&>;
template <typename IteratorType>
using OperatorPostDecrement =
	std::is_convertible<decltype(static_cast<IteratorType*>(nullptr)->operator--(0)),
						const IteratorType&>;
template <typename IteratorType>
using OperatorLessThan =
	std::is_convertible<decltype(static_cast<IteratorType*>(nullptr)->operator<(
							*static_cast<IteratorType*>(nullptr))),
						bool>;
template <typename IteratorType>
using OperatorLessThanEqual =
	std::is_convertible<decltype(static_cast<IteratorType*>(nullptr)->operator<=(
							*static_cast<IteratorType*>(nullptr))),
						bool>;
template <typename IteratorType>
using OperatorGreaterThan =
	std::is_convertible<decltype(static_cast<IteratorType*>(nullptr)->operator>(
							*static_cast<IteratorType*>(nullptr))),
						bool>;
template <typename IteratorType>
using OperatorGreaterThanEqual =
	std::is_convertible<decltype(static_cast<IteratorType*>(nullptr)->operator>=(
							*static_cast<IteratorType*>(nullptr))),
						bool>;
template <typename IteratorType>
using OperatorAdd =
	std::is_same<decltype(static_cast<IteratorType*>(nullptr)->operator+(0)), IteratorType>;
template <typename IteratorType>
using OperatorAddToDifference =
	std::is_same<decltype(0 + std::declval<IteratorType>()), IteratorType>;
template <typename IteratorType>
using OperatorSubtract =
	std::is_same<decltype(static_cast<IteratorType*>(nullptr)->operator-(0)), IteratorType>;
template <typename IteratorType>
using OperatorDistance = std::is_same<decltype(static_cast<IteratorType*>(nullptr)->operator-(
										  *static_cast<IteratorType*>(nullptr))),
									  typename IteratorType::difference_type>;
template <typename IteratorType>
using OperatorAddAssign =
	std::is_same<decltype(static_cast<IteratorType*>(nullptr)->operator+=(0)), IteratorType&>;
template <typename IteratorType>
using OperatorSubtractAssign =
	std::is_same<decltype(static_cast<IteratorType*>(nullptr)->operator-=(0)), IteratorType&>;
template <typename IteratorType>
using OperatorIndex =
	std::is_convertible<decltype(static_cast<IteratorType*>(nullptr)->operator[](0)),
						typename IteratorType::reference>;