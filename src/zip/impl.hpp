#include <cstddef>
#include <iterator>
#include <tuple>
#include <type_traits>
#include <utility>

namespace ContainerManip::internal {

// NOLINTBEGIN(*-missing-std-forward)

template <typename T>
struct RemoveRvalueRef {
	using Type = T;
};

template <typename T>
struct RemoveRvalueRef<T&&> {
	using Type = T;
};

template <typename T>
using RemoveRvalueRef_t = typename RemoveRvalueRef<T>::Type;

template <typename... TagsTypes>
struct CombinedIteratorTrait;

template <typename TagsType>
struct CombinedIteratorTrait<TagsType> {
	using Type = TagsType;
};

template <typename TagsType2, typename... TagsTypeRemainder>
struct CombinedIteratorTrait<std::input_iterator_tag, TagsType2, TagsTypeRemainder...> {
public:
	using Type = std::input_iterator_tag;
};

template <typename TagsType2, typename... TagsTypeRemainder>
struct CombinedIteratorTrait<std::output_iterator_tag, TagsType2, TagsTypeRemainder...> {
private:
	using RemainderTag = typename CombinedIteratorTrait<TagsType2, TagsTypeRemainder...>::Type;

public:
	using Type = std::conditional_t<std::is_same_v<RemainderTag, std::output_iterator_tag>,
									std::output_iterator_tag, std::input_iterator_tag>;
};

template <typename TagsType2, typename... TagsTypeRemainder>
struct CombinedIteratorTrait<std::forward_iterator_tag, TagsType2, TagsTypeRemainder...> {
private:
	using RemainderTag = typename CombinedIteratorTrait<TagsType2, TagsTypeRemainder...>::Type;
	static constexpr auto remainderIsLowerTag =
		std::is_same_v<RemainderTag, std::input_iterator_tag>
		|| std::is_same_v<RemainderTag, std::output_iterator_tag>;

public:
	using Type = std::conditional_t<remainderIsLowerTag, RemainderTag, std::forward_iterator_tag>;
};

template <typename TagsType2, typename... TagsTypeRemainder>
struct CombinedIteratorTrait<std::bidirectional_iterator_tag, TagsType2, TagsTypeRemainder...> {
private:
	using RemainderTag = typename CombinedIteratorTrait<TagsType2, TagsTypeRemainder...>::Type;
	static constexpr auto remainderIsLowerTag =
		std::is_same_v<RemainderTag, std::input_iterator_tag>
		|| std::is_same_v<RemainderTag, std::output_iterator_tag>
		|| std::is_same_v<RemainderTag, std::forward_iterator_tag>;

public:
	using Type =
		std::conditional_t<remainderIsLowerTag, RemainderTag, std::bidirectional_iterator_tag>;
};

template <typename TagsType2, typename... TagsTypeRemainder>
struct CombinedIteratorTrait<std::random_access_iterator_tag, TagsType2, TagsTypeRemainder...> {
private:
	using RemainderTag = typename CombinedIteratorTrait<TagsType2, TagsTypeRemainder...>::Type;
	static constexpr auto remainderIsLowerTag =
		std::is_same_v<RemainderTag, std::input_iterator_tag>
		|| std::is_same_v<RemainderTag, std::output_iterator_tag>
		|| std::is_same_v<RemainderTag, std::forward_iterator_tag>
		|| std::is_same_v<RemainderTag, std::bidirectional_iterator_tag>;

public:
	using Type =
		std::conditional_t<remainderIsLowerTag, RemainderTag, std::random_access_iterator_tag>;
};

template <typename... TagsTypes>
using CombinedIteratorTrait_t = typename CombinedIteratorTrait<TagsTypes...>::Type;

template <typename T>
constexpr bool alwaysFalse = false;

template <typename T>
static constexpr auto declref() noexcept -> T& {
#if !defined(__clang__) && defined(__GNUC__) && __GNUC__ <= 11
	return *static_cast<T*>(nullptr);
#else
	static_assert(alwaysFalse<T>, "declref not allowed in an evaluated context");
#endif
}

template <typename T>
struct MakeConstIfReference {
	using Type = T;
};

template <typename T>
struct MakeConstIfReference<T&> {
	using Type = std::add_const_t<T>&;
};

template <typename T>
using MakeConstIfReference_t = typename MakeConstIfReference<T>::Type;

template <typename IteratorType>
struct ReverseIteratorHelper {
	static constexpr auto isReverseIterator = false;
	using BaseIterator                      = IteratorType;
};

template <typename IteratorType>
struct ReverseIteratorHelper<std::reverse_iterator<IteratorType>> {
	static constexpr auto isReverseIterator = true;
	using BaseIterator                      = IteratorType;
};

// The c++17 standard requires the operators of std::reverse_iterator to not
// throw (given the underlying functions don't throw), while not being noexcept
// qualified. This workaround allows noexcept qualification even when reverse
// iterators are used, regardless of whether the reverse iterator is actualy
// noexcept qualified

template <typename IteratorType>
static constexpr void postIncrement(IteratorType& iterator) noexcept([]() noexcept {
	if constexpr (ReverseIteratorHelper<IteratorType>::isReverseIterator) {
		return noexcept(declref<typename ReverseIteratorHelper<IteratorType>::BaseIterator>()--);
	} else {
		return noexcept(iterator++);
	}
}());

template <typename IteratorType>
static constexpr void preIncrement(IteratorType& iterator) noexcept([]() noexcept {
	if constexpr (ReverseIteratorHelper<IteratorType>::isReverseIterator) {
		return noexcept(--declref<typename ReverseIteratorHelper<IteratorType>::BaseIterator>());
	} else {
		return noexcept(++iterator);
	}
}());

template <typename IteratorType>
static constexpr void postDecrement(IteratorType& iterator) noexcept([]() noexcept {
	if constexpr (ReverseIteratorHelper<IteratorType>::isReverseIterator) {
		return noexcept(declref<typename ReverseIteratorHelper<IteratorType>::BaseIterator>()++);
	} else {
		return noexcept(iterator--);
	}
}());

template <typename IteratorType>
static constexpr void preDecrement(IteratorType& iterator) noexcept([]() noexcept {
	if constexpr (ReverseIteratorHelper<IteratorType>::isReverseIterator) {
		return noexcept(++declref<typename ReverseIteratorHelper<IteratorType>::BaseIterator>());
	} else {
		return noexcept(--iterator);
	}
}());

template <typename IteratorType, typename T>
static constexpr void add(IteratorType& iterator, T n) noexcept([]() noexcept {
	if constexpr (ReverseIteratorHelper<IteratorType>::isReverseIterator) {
		return noexcept(declref<typename ReverseIteratorHelper<IteratorType>::BaseIterator>() - n);
	} else {
		return noexcept(iterator + n);
	}
}());

template <typename IteratorType, typename T>
static constexpr void addAssign(IteratorType& iterator, T n) noexcept([]() noexcept {
	if constexpr (ReverseIteratorHelper<IteratorType>::isReverseIterator) {
		return noexcept(declref<typename ReverseIteratorHelper<IteratorType>::BaseIterator>() -= n);
	} else {
		return noexcept(iterator += n);
	}
}());

template <typename IteratorType, typename T>
static constexpr void subtract(IteratorType& iterator, T n) noexcept([]() noexcept {
	if constexpr (ReverseIteratorHelper<IteratorType>::isReverseIterator) {
		return noexcept(declref<typename ReverseIteratorHelper<IteratorType>::BaseIterator>() + n);
	} else {
		return noexcept(iterator - n);
	}
}());

template <typename IteratorType, typename T>
static constexpr void subtractAssign(IteratorType& iterator, T n) noexcept([]() noexcept {
	if constexpr (ReverseIteratorHelper<IteratorType>::isReverseIterator) {
		return noexcept(declref<typename ReverseIteratorHelper<IteratorType>::BaseIterator>() += n);
	} else {
		return noexcept(iterator -= n);
	}
}());

template <typename IterType, typename IteratorType1, typename... IteratorTypes>
class IteratorBase {
public:
	using difference_type = typename std::iterator_traits<IteratorType1>::difference_type;
	using value_type      = std::tuple<std::decay_t<decltype(*std::declval<IteratorType1>())>,
                                  std::decay_t<decltype(*std::declval<IteratorTypes>())>...>;
	using pointer         = void;
	using reference       = std::tuple<decltype(*std::declval<IteratorType1>()),
                                 decltype(*std::declval<IteratorTypes>())...>;
	using const_reference =
		std::tuple<MakeConstIfReference_t<decltype(*std::declval<IteratorType1>())>,
				   MakeConstIfReference_t<decltype(*std::declval<IteratorTypes>())>...>;

	explicit constexpr IteratorBase(
		IteratorType1 iterator1,
		IteratorTypes... iteratorRemainders) noexcept((std::
														   is_nothrow_constructible_v<IteratorType1,
																					  IteratorType1>
													   && ...
													   && std::is_nothrow_constructible_v<
														   IteratorTypes, IteratorTypes>))
		: iterators{iterator1, iteratorRemainders...} {}

protected:
	constexpr IteratorBase() noexcept((std::is_nothrow_constructible_v<IteratorType1> && ...
									   && std::is_nothrow_constructible_v<IteratorTypes>)) =
		default;

	[[nodiscard]] constexpr auto getIterators() noexcept -> auto& { return iterators; }
	[[nodiscard]] constexpr auto getIterators() const noexcept -> const auto& { return iterators; }
	template <std::size_t... i>
	[[nodiscard]] constexpr auto dereference(std::index_sequence<i...> /*unused*/) noexcept(
		(noexcept(*std::get<i>(declref<IteratorBase>().iterators)) && ...)) {
		return reference{*std::get<i>(iterators)...};
	}
	template <std::size_t... i>
	[[nodiscard]] constexpr auto dereference(std::index_sequence<i...> /*unused*/) const
		noexcept((noexcept(*std::get<i>(declref<const IteratorBase>().iterators)) && ...)) {
		return const_reference{*std::get<i>(iterators)...};
	}

	template <std::size_t... i, typename F>
	[[nodiscard]] constexpr auto transformCopy(std::index_sequence<i...> /*unused*/, F func) const
		noexcept((noexcept(func(std::get<i>(declref<const IteratorBase>().iterators))) && ...)) {
		return IterType{func(std::get<i>(iterators))...};
	}

	template <std::size_t... i, typename F>
	constexpr void visit(std::index_sequence<i...> /*unused*/, F func) noexcept(
		(noexcept(func(std::get<i>(declref<IteratorBase>().iterators))) && ...)) {
		(func(std::get<i>(iterators)), ...);
	}

private:
	std::tuple<IteratorType1, IteratorTypes...> iterators{};
};

template <typename IteratorCategory, typename IterType, typename... IteratorTypes>
class Iterator;

template <typename IterType, typename IteratorType1, typename... IteratorTypes>
class Iterator<std::input_iterator_tag, IterType, IteratorType1, IteratorTypes...>
	: public IteratorBase<IterType, IteratorType1, IteratorTypes...> {
	using Base = IteratorBase<IterType, IteratorType1, IteratorTypes...>;

	struct PreIncrement {
		template <typename T>
		constexpr auto operator()(T& iterator) const noexcept(noexcept(preIncrement(iterator))) {
			return ++iterator;
		}
	};

	struct PostIncrement {
		template <typename T>
		constexpr auto operator()(T& iterator) const noexcept(noexcept(postIncrement(iterator))) {
			return iterator++;
		}
	};

protected:
	using Base::dereference;
	using Base::getIterators;
	using Base::transformCopy;
	using Base::visit;

public:
	using iterator_category = std::input_iterator_tag;

	using typename Base::const_reference;
	using typename Base::difference_type;
	using typename Base::pointer;
	using typename Base::reference;
	using typename Base::value_type;

	using Base::IteratorBase;

	[[nodiscard]] constexpr auto operator*() noexcept(noexcept(declref<Iterator>().dereference(
		std::make_index_sequence<sizeof...(IteratorTypes) + 1U>{}))) -> reference {
		return dereference(std::make_index_sequence<sizeof...(IteratorTypes) + 1U>{});
	}

	[[nodiscard]] constexpr auto operator*() const
		noexcept(noexcept(declref<const Iterator>().dereference(
			std::make_index_sequence<sizeof...(IteratorTypes) + 1U>{}))) -> const_reference {
		return dereference(std::make_index_sequence<sizeof...(IteratorTypes) + 1U>{});
	}

	[[nodiscard]] constexpr auto operator->() noexcept(noexcept(declref<Iterator>().dereference(
		std::make_index_sequence<sizeof...(IteratorTypes) + 1U>{}))) -> reference {
		return dereference(std::make_index_sequence<sizeof...(IteratorTypes) + 1U>{});
	}

	[[nodiscard]] constexpr auto operator->() const
		noexcept(noexcept(declref<const Iterator>().dereference(
			std::make_index_sequence<sizeof...(IteratorTypes) + 1U>{}))) -> const_reference {
		return dereference(std::make_index_sequence<sizeof...(IteratorTypes) + 1U>{});
	}

	constexpr auto operator++() noexcept(noexcept(declref<Iterator>().visit(
		std::make_index_sequence<sizeof...(IteratorTypes) + 1U>{}, PreIncrement{}))) -> IterType& {
		visit(std::make_index_sequence<sizeof...(IteratorTypes) + 1U>{}, PreIncrement{});
		return static_cast<IterType&>(*this);
	}

	constexpr auto operator++(int) & noexcept(noexcept(declref<Iterator>().visit(
		std::make_index_sequence<sizeof...(IteratorTypes) + 1U>{}, PostIncrement{}))) -> IterType {
		IterType copy = static_cast<IterType&>(*this);
		visit(std::make_index_sequence<sizeof...(IteratorTypes) + 1U>{}, PostIncrement{});
		return copy;
	}

	[[nodiscard]] constexpr auto operator==(Iterator other) const
		noexcept(noexcept(std::get<0>(declref<const Iterator>().getIterators())
						  == std::get<0>(other.getIterators()))) -> bool {
		return std::get<0>(getIterators()) == std::get<0>(other.getIterators());
	}

	[[nodiscard]] constexpr auto operator!=(Iterator other) const
		noexcept(noexcept(std::get<0>(declref<const Iterator>().getIterators())
						  != std::get<0>(other.getIterators()))) -> bool {
		return std::get<0>(getIterators()) != std::get<0>(other.getIterators());
	}
};

template <typename IterType, typename IteratorType1, typename... IteratorTypes>
class Iterator<std::output_iterator_tag, IterType, IteratorType1, IteratorTypes...>
	: public Iterator<std::input_iterator_tag, IterType, IteratorType1, IteratorTypes...> {
	using Base = Iterator<std::input_iterator_tag, IterType, IteratorType1, IteratorTypes...>;

protected:
	using Base::dereference;
	using Base::getIterators;
	using Base::transformCopy;
	using Base::visit;

public:
	using iterator_category = std::output_iterator_tag;

	using typename Base::const_reference;
	using typename Base::difference_type;
	using typename Base::pointer;
	using typename Base::reference;
	using typename Base::value_type;

	using Base::Iterator;
};

template <typename IterType, typename IteratorType1, typename... IteratorTypes>
class Iterator<std::forward_iterator_tag, IterType, IteratorType1, IteratorTypes...>
	: public Iterator<std::output_iterator_tag, IterType, IteratorType1, IteratorTypes...> {
	using Base = Iterator<std::output_iterator_tag, IterType, IteratorType1, IteratorTypes...>;

protected:
	using Base::dereference;
	using Base::getIterators;
	using Base::transformCopy;
	using Base::visit;

public:
	using iterator_category = std::forward_iterator_tag;

	using typename Base::const_reference;
	using typename Base::difference_type;
	using typename Base::pointer;
	using typename Base::reference;
	using typename Base::value_type;

	using Base::Iterator;
};

template <typename IterType, typename IteratorType1, typename... IteratorTypes>
class Iterator<std::bidirectional_iterator_tag, IterType, IteratorType1, IteratorTypes...>
	: public Iterator<std::forward_iterator_tag, IterType, IteratorType1, IteratorTypes...> {
	using Base = Iterator<std::forward_iterator_tag, IterType, IteratorType1, IteratorTypes...>;

	struct PreDecrement {
		template <typename T>
		constexpr auto operator()(T& iterator) const noexcept(noexcept(preDecrement(iterator))) {
			return --iterator;
		}
	};

	struct PostDecrement {
		template <typename T>
		constexpr auto operator()(T& iterator) const noexcept(noexcept(postDecrement(iterator))) {
			return iterator--;
		}
	};

protected:
	using Base::dereference;
	using Base::getIterators;
	using Base::transformCopy;
	using Base::visit;

public:
	using iterator_category = std::bidirectional_iterator_tag;

	using typename Base::const_reference;
	using typename Base::difference_type;
	using typename Base::pointer;
	using typename Base::reference;
	using typename Base::value_type;

	using Base::Iterator;

	constexpr auto operator--() noexcept(noexcept(declref<Iterator>().visit(
		std::make_index_sequence<sizeof...(IteratorTypes) + 1U>{}, PreDecrement{}))) -> IterType& {
		visit(std::make_index_sequence<sizeof...(IteratorTypes) + 1U>{}, PreDecrement{});
		return static_cast<IterType&>(*this);
	}

	constexpr auto operator--(int) & noexcept(noexcept(declref<Iterator>().visit(
		std::make_index_sequence<sizeof...(IteratorTypes) + 1U>{}, PostDecrement{}))) -> IterType {
		IterType copy = static_cast<IterType&>(*this);
		visit(std::make_index_sequence<sizeof...(IteratorTypes) + 1U>{}, PostDecrement{});
		return copy;
	}
};

template <typename IterType, typename IteratorType1, typename... IteratorTypes>
class Iterator<std::random_access_iterator_tag, IterType, IteratorType1, IteratorTypes...>
	: public Iterator<std::bidirectional_iterator_tag, IterType, IteratorType1, IteratorTypes...> {
	using Base =
		Iterator<std::bidirectional_iterator_tag, IterType, IteratorType1, IteratorTypes...>;

	struct AddN {
		typename Base::difference_type n;

		template <typename T>
		constexpr auto operator()(const T& iterator) const noexcept(noexcept(add(iterator, n))) {
			return iterator + n;
		}
	};

	struct AddAssignN {
		typename Base::difference_type n;

		template <typename T>
		constexpr auto operator()(T& iterator) const noexcept(noexcept(addAssign(iterator, n))) {
			return iterator += n;
		}
	};

	struct SubtractN {
		typename Base::difference_type n;

		template <typename T>
		constexpr auto operator()(const T& iterator) const
			noexcept(noexcept(subtract(iterator, n))) {
			return iterator - n;
		}
	};

	struct SubtractAssignN {
		typename Base::difference_type n;

		template <typename T>
		constexpr auto operator()(T& iterator) const
			noexcept(noexcept(subtractAssign(iterator, n))) {
			return iterator -= n;
		}
	};

protected:
	using Base::dereference;
	using Base::getIterators;
	using Base::transformCopy;
	using Base::visit;

public:
	using iterator_category = std::random_access_iterator_tag;

	using typename Base::const_reference;
	using typename Base::difference_type;
	using typename Base::pointer;
	using typename Base::reference;
	using typename Base::value_type;

	using Base::Iterator;

	[[nodiscard]] constexpr auto operator<(Iterator other) const
		noexcept(noexcept(std::get<0>(declref<const Iterator>().getIterators())
						  < std::get<0>(other.getIterators()))) -> bool {
		return std::get<0>(getIterators()) < std::get<0>(other.getIterators());
	}

	[[nodiscard]] constexpr auto operator>(Iterator other) const
		noexcept(noexcept(std::get<0>(declref<const Iterator>().getIterators())
						  > std::get<0>(other.getIterators()))) -> bool {
		return std::get<0>(getIterators()) > std::get<0>(other.getIterators());
	}

	[[nodiscard]] constexpr auto operator<=(Iterator other) const
		noexcept(noexcept(std::get<0>(declref<const Iterator>().getIterators())
						  <= std::get<0>(other.getIterators()))) -> bool {
		return std::get<0>(getIterators()) <= std::get<0>(other.getIterators());
	}

	[[nodiscard]] constexpr auto operator>=(Iterator other) const
		noexcept(noexcept(std::get<0>(declref<const Iterator>().getIterators())
						  >= std::get<0>(other.getIterators()))) -> bool {
		return std::get<0>(getIterators()) >= std::get<0>(other.getIterators());
	}

	[[nodiscard]] constexpr auto operator+(difference_type n) const
		noexcept(noexcept(declref<const Iterator>().transformCopy(
			std::make_index_sequence<sizeof...(IteratorTypes) + 1U>{}, AddN{n}))) -> IterType {
		return transformCopy(std::make_index_sequence<sizeof...(IteratorTypes) + 1U>{}, AddN{n});
	}

	[[nodiscard]] constexpr auto operator-(difference_type n) const
		noexcept(noexcept(declref<const Iterator>().transformCopy(
			std::make_index_sequence<sizeof...(IteratorTypes) + 1U>{}, SubtractN{n}))) -> IterType {
		return transformCopy(std::make_index_sequence<sizeof...(IteratorTypes) + 1U>{},
							 SubtractN{n});
	}

	[[nodiscard]] constexpr auto operator-(Iterator other) const
		noexcept(noexcept(std::get<0>(declref<const Iterator>().getIterators())
						  - std::get<0>(other.getIterators()))) -> difference_type {
		return std::get<0>(getIterators()) - std::get<0>(other.getIterators());
	}

	constexpr auto operator+=(difference_type n) noexcept(noexcept(declref<Iterator>().visit(
		std::make_index_sequence<sizeof...(IteratorTypes) + 1U>{}, AddAssignN{n}))) -> IterType& {
		visit(std::make_index_sequence<sizeof...(IteratorTypes) + 1U>{}, AddAssignN{n});
		return static_cast<IterType&>(*this);
	}

	constexpr auto operator-=(difference_type n) noexcept(noexcept(declref<Iterator>().visit(
		std::make_index_sequence<sizeof...(IteratorTypes) + 1U>{}, SubtractAssignN{n})))
		-> IterType& {
		visit(std::make_index_sequence<sizeof...(IteratorTypes) + 1U>{}, SubtractAssignN{n});
		return static_cast<IterType&>(*this);
	}

	[[nodiscard]] constexpr auto
	operator[](difference_type n) noexcept(noexcept(*this + n) && noexcept(**this)) -> reference {
		return *(*this + n);
	}

	[[nodiscard]] constexpr auto operator[](difference_type n) const
		noexcept(noexcept(*this + n) && noexcept(**this)) -> const_reference {
		return *(*this + n);
	}
};

template <typename... IteratorTypes>
struct InputIterator
	: public Iterator<std::input_iterator_tag, InputIterator<IteratorTypes...>, IteratorTypes...> {
	using Iterator<std::input_iterator_tag, InputIterator<IteratorTypes...>,
				   IteratorTypes...>::Iterator;
};

template <typename... IteratorTypes>
constexpr auto createIterator(IteratorTypes... iterators) noexcept(
	noexcept(InputIterator<IteratorTypes...>{iterators...}))
	-> std::enable_if_t<std::is_same_v<std::input_iterator_tag,
									   CombinedIteratorTrait_t<typename std::iterator_traits<
										   IteratorTypes>::iterator_category...>>,
						InputIterator<IteratorTypes...>> {
	return InputIterator<IteratorTypes...>{iterators...};
}

template <typename... IteratorTypes>
struct OutputIterator
	: public Iterator<std::output_iterator_tag, OutputIterator<IteratorTypes...>,
					  IteratorTypes...> {
	using Iterator<std::output_iterator_tag, OutputIterator<IteratorTypes...>,
				   IteratorTypes...>::Iterator;
};

template <typename... IteratorTypes>
constexpr auto createIterator(IteratorTypes... iterators) noexcept(
	noexcept(OutputIterator<IteratorTypes...>{iterators...}))
	-> std::enable_if_t<std::is_same_v<std::output_iterator_tag,
									   CombinedIteratorTrait_t<typename std::iterator_traits<
										   IteratorTypes>::iterator_category...>>,
						OutputIterator<IteratorTypes...>> {
	return OutputIterator<IteratorTypes...>{iterators...};
}

template <typename... IteratorTypes>
struct ForwardIterator
	: public Iterator<std::forward_iterator_tag, ForwardIterator<IteratorTypes...>,
					  IteratorTypes...> {
	using Iterator<std::forward_iterator_tag, ForwardIterator<IteratorTypes...>,
				   IteratorTypes...>::Iterator;
};

template <typename... IteratorTypes>
constexpr auto createIterator(IteratorTypes... iterators) noexcept(
	noexcept(ForwardIterator<IteratorTypes...>{iterators...}))
	-> std::enable_if_t<std::is_same_v<std::forward_iterator_tag,
									   CombinedIteratorTrait_t<typename std::iterator_traits<
										   IteratorTypes>::iterator_category...>>,
						ForwardIterator<IteratorTypes...>> {
	return ForwardIterator<IteratorTypes...>{iterators...};
}

template <typename... IteratorTypes>
struct BidirectionalIterator
	: public Iterator<std::bidirectional_iterator_tag, BidirectionalIterator<IteratorTypes...>,
					  IteratorTypes...> {
	using Iterator<std::bidirectional_iterator_tag, BidirectionalIterator<IteratorTypes...>,
				   IteratorTypes...>::Iterator;
};

template <typename... IteratorTypes>
constexpr auto createIterator(IteratorTypes... iterators) noexcept(
	noexcept(BidirectionalIterator<IteratorTypes...>{iterators...}))
	-> std::enable_if_t<std::is_same_v<std::bidirectional_iterator_tag,
									   CombinedIteratorTrait_t<typename std::iterator_traits<
										   IteratorTypes>::iterator_category...>>,
						BidirectionalIterator<IteratorTypes...>> {
	return BidirectionalIterator<IteratorTypes...>{iterators...};
}

template <typename... IteratorTypes>
struct RandomAccessIterator
	: public Iterator<std::random_access_iterator_tag, RandomAccessIterator<IteratorTypes...>,
					  IteratorTypes...> {
	using Iterator<std::random_access_iterator_tag, RandomAccessIterator<IteratorTypes...>,
				   IteratorTypes...>::Iterator;
};

template <typename... IteratorTypes>
constexpr auto createIterator(IteratorTypes... iterators) noexcept(
	noexcept(RandomAccessIterator<IteratorTypes...>{iterators...}))
	-> std::enable_if_t<std::is_same_v<std::random_access_iterator_tag,
									   CombinedIteratorTrait_t<typename std::iterator_traits<
										   IteratorTypes>::iterator_category...>>,
						RandomAccessIterator<IteratorTypes...>> {
	return RandomAccessIterator<IteratorTypes...>{iterators...};
}

template <typename T>
static constexpr auto getBegin(T& container) noexcept(noexcept(std::begin(container))) {
	return std::begin(container);
}
template <typename T>
static constexpr auto getEnd(T& container) noexcept(noexcept(std::end(container))) {
	return std::end(container);
}
template <typename T>
static constexpr auto getCBegin(T& container) noexcept(noexcept(std::cbegin(container))) {
	return std::cbegin(container);
}
template <typename T>
static constexpr auto getCEnd(T& container) noexcept(noexcept(std::cend(container))) {
	return std::cend(container);
}
template <typename T>
static constexpr auto getRBegin(T& container) noexcept(noexcept(std::rbegin(container))) {
	return std::rbegin(container);
}
template <typename T>
static constexpr auto getREnd(T& container) noexcept(noexcept(std::rend(container))) {
	return std::rend(container);
}
template <typename T>
static constexpr auto getCRBegin(T& container) noexcept(noexcept(std::crbegin(container))) {
	return std::crbegin(container);
}
template <typename T>
static constexpr auto getCREnd(T& container) noexcept(noexcept(std::crend(container))) {
	return std::crend(container);
}

template <typename ContainerTypeTuple, std::size_t... i>
class ZippedContainer {
public:
	using iterator       = std::decay_t<decltype(createIterator(
        getBegin(std::get<i>(declref<ContainerTypeTuple>()))...))>;
	using const_iterator = std::decay_t<decltype(createIterator(
		getCBegin(std::get<i>(declref<ContainerTypeTuple>()))...))>;

	using value_type      = typename iterator::value_type;
	using reference       = typename iterator::reference;
	using pointer         = typename iterator::pointer;
	using const_reference = typename iterator::const_reference;
	using difference_type = typename iterator::difference_type;

	template <typename... ContainerTypes,
			  typename = std::enable_if_t<(1U <= sizeof...(ContainerTypes))>>
	explicit constexpr ZippedContainer(ContainerTypes&&... containers) noexcept(
		noexcept(ContainerTypeTuple{
			std::forward<typename std::tuple_element<i, ContainerTypeTuple>::type>(containers)...}))
		: containers{std::forward<typename std::tuple_element<i, ContainerTypeTuple>::type>(
			containers)...} {}

	[[nodiscard]] constexpr auto begin() noexcept(
		noexcept(createIterator(getBegin(std::get<i>(declref<ContainerTypeTuple>()))...))) {
		return createIterator(getBegin(std::get<i>(containers))...);
	}
	[[nodiscard]] constexpr auto end() noexcept(
		noexcept(createIterator(getEnd(std::get<i>(declref<ContainerTypeTuple>()))...))) {
		return createIterator(getEnd(std::get<i>(containers))...);
	}
	[[nodiscard]] constexpr auto begin() const noexcept(
		noexcept(createIterator(getBegin(std::get<i>(declref<ContainerTypeTuple>()))...))) {
		return createIterator(getBegin(std::get<i>(containers))...);
	}
	[[nodiscard]] constexpr auto end() const
		noexcept(noexcept(createIterator(getEnd(std::get<i>(declref<ContainerTypeTuple>()))...))) {
		return createIterator(getEnd(std::get<i>(containers))...);
	}
	[[nodiscard]] constexpr auto cbegin() const noexcept(
		noexcept(createIterator(getCBegin(std::get<i>(declref<ContainerTypeTuple>()))...))) {
		return createIterator(getCBegin(std::get<i>(containers))...);
	}
	[[nodiscard]] constexpr auto cend() const
		noexcept(noexcept(createIterator(getCEnd(std::get<i>(declref<ContainerTypeTuple>()))...))) {
		return createIterator(getCEnd(std::get<i>(containers))...);
	}
	[[nodiscard]] constexpr auto rbegin() noexcept(
		noexcept(createIterator(getRBegin(std::get<i>(declref<ContainerTypeTuple>()))...))) {
		return createIterator(getRBegin(std::get<i>(containers))...);
	}
	[[nodiscard]] constexpr auto rend() noexcept(
		noexcept(createIterator(getREnd(std::get<i>(declref<ContainerTypeTuple>()))...))) {
		return createIterator(getREnd(std::get<i>(containers))...);
	}
	[[nodiscard]] constexpr auto rbegin() const noexcept(
		noexcept(createIterator(getRBegin(std::get<i>(declref<ContainerTypeTuple>()))...))) {
		return createIterator(getRBegin(std::get<i>(containers))...);
	}
	[[nodiscard]] constexpr auto rend() const
		noexcept(noexcept(createIterator(getREnd(std::get<i>(declref<ContainerTypeTuple>()))...))) {
		return createIterator(getREnd(std::get<i>(containers))...);
	}
	[[nodiscard]] constexpr auto crbegin() const noexcept(
		noexcept(createIterator(getCRBegin(std::get<i>(declref<ContainerTypeTuple>()))...))) {
		return createIterator(getCRBegin(std::get<i>(containers))...);
	}
	[[nodiscard]] constexpr auto crend() const noexcept(
		noexcept(createIterator(getCREnd(std::get<i>(declref<ContainerTypeTuple>()))...))) {
		return createIterator(getCREnd(std::get<i>(containers))...);
	}

private:
	ContainerTypeTuple containers{};
};

template <typename... ContainerType, std::size_t... i>
static constexpr auto
zipHelper(std::index_sequence<i...> /*unused*/, ContainerType&&... containers) noexcept(noexcept(
	ZippedContainer<
		std::tuple<RemoveRvalueRef_t<decltype(std::forward<ContainerType>(containers))>...>, i...>{
		std::forward<ContainerType>(containers)...})) {
	using TupleType =
		std::tuple<RemoveRvalueRef_t<decltype(std::forward<ContainerType>(containers))>...>;
	return ZippedContainer<TupleType, i...>{std::forward<ContainerType>(containers)...};
}

template <typename... IteratorTypes>
static constexpr auto operator+(
	typename std::iterator_traits<RandomAccessIterator<IteratorTypes...>>::difference_type
										   difference,
	RandomAccessIterator<IteratorTypes...> iterator) noexcept(noexcept(iterator + difference)) {
	return iterator + difference;
}

template <typename ContainerType1, typename... ContainerType>
static constexpr auto zip(ContainerType1&& container1, ContainerType&&... containers) noexcept(
	noexcept(internal::zipHelper(std::make_index_sequence<1U + sizeof...(containers)>{},
								 std::forward<ContainerType1>(container1),
								 std::forward<ContainerType>(containers)...))) {
	return internal::zipHelper(std::make_index_sequence<1U + sizeof...(containers)>{},
							   std::forward<ContainerType1>(container1),
							   std::forward<ContainerType>(containers)...);
}

// NOLINTEND(*-missing-std-forward)

}   // namespace ContainerManip::internal
