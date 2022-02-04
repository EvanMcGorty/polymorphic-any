#include<memory>
#include<concepts>
#include<string>
#include<cassert>
#include<limits>

#include"type_ids.h"

//todo: allow further template parameters of unique_ptr or different pointer types
//equality comparison? std::function
//make everything constexpr
//custom stack based alloc function for unique_ptr, have a byte[] buffer in virt that unique_ptr points to when the target object is small enough
//virt second template argument that takes a size_t wrapper or a byte[] wrapper that automatically calculates the largest size from a list of types
//but be sure to be able to distinguish between a pure variant and one that is also an virt
//second template parameter that includes pointer type should be the same thing that can transform this into a variant
// 
//need to cover:
//virt unconstrained, virt constrained to base class
//		a generic constraint (compile time) can also be given on the classes that may be used with virt
//virt with unique_ptr, virt with shared_ptr, etc
//virt with a minimum list of types that it may contain, virt without this list (same as the list being empty)
//virt with stack based memory buffer of at least N, virt without buffer (same as the buffer being N=0)
//if virt has a buffer:
//		buffer can be set to be at least big enough to acommodate the list of the types which it may contain
//		or not
//if virt has a type list
//		virt can disallow objects whose types are not in this list (in this case the manually set buffer is meaningless)
//		or can disallow objects whose types are not on this list unless they fit in the buffer
//		or can simply allow all objects to at least be allocated on the free store if needed
//can choose which operations are expected of its objects (copy construction
//can choose the protocol when an object cant perform an operation i.e.
//		throw exception or assert error or default to evaluate to nullptr/uninitialized
//		or simply disallow types that dont have certain operations

// 
//note: a variadic list wrapper and a concept for that would be nice for users to be able to use
//maybe one day the memory buffer could be used to store more than one object (maybe of different sizes)

//my assert for logic errors when error checking is on
template<std::predicate t, typename error_string_t = char const*>
constexpr void my_assert(t tar,error_string_t message = "[no error message provided]")
{
#ifndef NDEBUG
	if(!tar()){throw std::logic_error(std::move(message));}
#endif
}

//assert that the dynamic cast doesnt result in a nullptr if rtti is on
template<typename to_t, typename from_t, typename error_string_t = char const*>
	requires std::is_pointer_v<to_t>
constexpr void assert_valid_dynamic_cast(from_t* a, to_t compare, error_string_t&& error = "static_cast fail")
{
#ifndef NDEBUG
	my_assert([&]{return dynamic_cast<to_t>(a)==compare;}, std::forward<error_string_t>(error));
#endif
}

template<typename lhs, typename rhs, typename extra_rhs = rhs>
	requires (std::is_pointer_v<lhs> && std::is_pointer_v<rhs> && std::is_pointer_v<extra_rhs>)
size_t ptr_distance()
{
	lhs l = 0;
	intmax_t li = reinterpret_cast<intmax_t>(l - 1) / 2;
	l = reinterpret_cast<lhs>(li);

	extra_rhs r = static_cast<extra_rhs>(static_cast<rhs>(l));
	intmax_t ri = reinterpret_cast<intmax_t>(r);

	return static_cast<size_t>(li - ri);
}

class safe_void_ptr_base
{
#ifndef NDEBUG
	virtual void foo(){} //just to give this class rtti for debug checks
#endif
};

template<typename t>
struct safe_void_ptr_derived : public safe_void_ptr_base
{
	t* wrapped_ptr;
};


template<typename t>
constexpr bool is_any_or_derived_wrapper_v = false; //will be specialized later

class any;

template<typename to_t, typename error_string_t = char const*>
	requires (std::is_pointer_v<to_t> && !is_any_or_derived_wrapper_v<std::remove_pointer_t<to_t>>)
to_t smart_static_cast(any*, error_string_t&& = "bad static cast from any*");

//a universal base class
class any
{
public:
	virtual ~any() {}

private:

template<typename t>
friend class derived_wrapper;
template<typename t>
	requires (!is_virt_v<t>)
friend class virt;

template<typename to_t, typename error_string_t>
	requires (std::is_pointer_v<to_t> && !is_any_or_derived_wrapper_v<std::remove_pointer_t<to_t>>)
friend to_t smart_static_cast(any*, error_string_t&&);

	virtual size_t ptr_distance_from_any() = 0;

	virtual type get_typeof() = 0;

	virtual bool is_const() = 0;

	//virtual bool is_reference() = 0;

	//generic operations without consistent return types
	virtual std::unique_ptr<any> copy_construct() = 0;
	virtual std::unique_ptr<any> move_construct() = 0;


	//virtual void placement_copy_construct(safe_void_ptr_base*) = 0;
	//virtual void placement_move_construct(safe_void_ptr_base*) = 0;
	//non constexpr


	virtual bool try_copy_assign(any*) = 0;
	virtual bool try_move_assign(any*) = 0;

	virtual bool do_copy_assign(safe_void_ptr_base*) = 0;
	virtual bool do_move_assign(safe_void_ptr_base*) = 0;

	//make reference/pointer
	//make const reference/pointer
	//dereference
	//const cast?
	//make optional?
	//make std::unique_ptr?
	//make arbitrary templated class/call arbitrary templated function given by template parameter in virt
	//		maybe make almost_any<t> derive from any in cases when this template parameter is supplied
	//call arbitrary template<t> function by passing in an id so derived wrapper knows which templated function to call
	//		derived_wrapper will have a list of callables in its template arguments
	//		maybe the id could be a unique type_id* like object of a wrapper of the callable template
	//		(because raw templates can also be given ids by passing a template template to a wrapper class)
	//		maybe put everything, including copy costruct and move construct into this system
	//		this could be part of the base class constraint ie virt<typewhichcan<tostring,index>
	
	//make std::any
	//default constructor generator
	//operations with only 1 generic argument (this*)
	//ostream << operator
	//istream >> operator
	//[size_t] index operator
	//conversion to some_iterator<ptr<any>> (by iterating through underlying type if possible)
	//operator bool()
	//conversions to common types (int, double, string)
	//operations with multiple generic arguments
	//same-type comparison operators


	//virtual size_t sizeof_most_derived() const = 0;
	//typeid of most derived object
	//return std::function<ptr<any>(ptr<any>)>(target)

	//maybe not needed:
	//virtual any* any_this() const = 0;
	//virtual void* derived_wrapper_this() const = 0;

};

template<typename t>
class derived_wrapper;

//a wrapper of t that inherits from t and any to provide any with virtual methods to perform operations on t
template<typename t>
	requires std::is_const_v<t>
class derived_wrapper<t> : public std::remove_const_t<t>, public any //declared final so that sizeof can be determined for the whole object
{
public:


	size_t ptr_distance_from_any() override
	{
		static size_t ret = ptr_distance<any*, derived_wrapper<t>* ,t*>();
		return ret;
	}

	type get_typeof() override
	{
		return typeof<std::remove_const_t<t>>();
	}

	bool is_const() override
	{
		return true;
	}

	~derived_wrapper() override = default;

	template<typename...args>
	derived_wrapper(args&&...vals) : t{ std::forward<args>(vals)... } {}

	std::unique_ptr<any> copy_construct() override
	{
		if constexpr (std::is_copy_constructible_v<std::remove_const_t<t>>)
		{
			t const& tocopy = *this;
			return { std::make_unique<derived_wrapper<std::remove_const_t<t>>>(tocopy) };
		}
		else
		{
			return {nullptr};
		}
	}

	std::unique_ptr<any> move_construct() override
	{
		if constexpr (std::is_move_constructible_v<t>) //ie constructible from const rvalue
		{
			t const& tocopy = *this;
			return { std::make_unique<derived_wrapper<std::remove_const_t<t>>>(std::move(tocopy)) };
		}
		else
		{
			return {nullptr};
		}
	}


	bool try_copy_assign(any* tar) override
	{
		my_assert([]{return false;});//library error
		return false;
	}

	bool try_move_assign(any* tar) override
	{
		my_assert([]{return false;});//library error
		return false;
	}

	bool do_copy_assign(safe_void_ptr_base* tar) override
	{
		my_assert([]{return false;});//library error
		return false;
	}

	bool do_move_assign(safe_void_ptr_base* tar) override
	{
		my_assert([]{return false;});//library error
		return false;
	}

};

template<typename t>
class derived_wrapper : public derived_wrapper<const t>
{
public:
	size_t ptr_distance_from_any() override
	{
		static size_t ret = ptr_distance<any*, derived_wrapper<t>*, t*>();
		return ret;
	}

	type get_typeof() override
	{
		return typeof<t>();
	}

	bool is_const() override
	{
		return false;
	}

	~derived_wrapper() override = default;

	template<typename...args>
	derived_wrapper(args&&...vals) : derived_wrapper<const t>{ std::forward<args>(vals)... } {}


	std::unique_ptr<any> move_construct() override
	{
		if constexpr (std::is_move_constructible_v<t>)
		{
			t& tomove = *this;
			return { std::make_unique<derived_wrapper<t>>(std::move(tomove)) };
		}
		else
		{
			return { nullptr };
		}
	}



	bool try_copy_assign(any* tar) override
	{
		if constexpr (std::is_copy_assignable_v<t>)
		{
			if (get_typeof() == tar->get_typeof())
			{
				t* rhs = smart_static_cast<t*>(tar);
				t* lhs = this;
				*lhs = *rhs;
				return true;
			}
			else
			{
				return false;
			}
		}
		else
		{
			return false;
		}
	}

	bool try_move_assign(any* tar) override
	{
		if constexpr (std::is_move_assignable_v<t>)
		{
			if (get_typeof() == tar->get_typeof())
			{
				t* rhs = smart_static_cast<t*>(tar);
				t* lhs = this;
				my_assert([&]{return !tar->is_const();});
				*lhs = std::move(*rhs);
				return true;
			}
			else
			{
				return false;
			}
		}
		else
		{
			return false;
		}
	}


	bool do_copy_assign(safe_void_ptr_base* tar) override
	{
		if constexpr (std::is_copy_assignable_v<t>)
		{
			t* lhs = this;
			*lhs = *(smart_static_cast<safe_void_ptr_derived<t const>*>(tar)->wrapped_ptr);
			return true;
		}
		else
		{
			return false;
		}
	}

	bool do_move_assign(safe_void_ptr_base* tar) override
	{
		if constexpr (std::is_move_assignable_v<t>)
		{
			t* lhs = this;
			*lhs = std::move(*(smart_static_cast<safe_void_ptr_derived<t>*>(tar)->wrapped_ptr));
			return true;
		}
		else
		{
			return false;
		}
	}
};


template<typename t>
constexpr bool is_any_or_derived_wrapper_v<derived_wrapper<t>> = true;

template<>
constexpr bool is_any_or_derived_wrapper_v<any> = true;

//asserts that the cast is valid with dynamic cast
template<typename to_t, typename from_t, typename error_string_t = char const*>
	requires (std::is_pointer_v<to_t> && !std::is_same_v<to_t, any*> && !std::is_same_v<from_t, any>)
constexpr to_t smart_static_cast(from_t* a, error_string_t&& error = "static_cast fail")
{
	to_t result = static_cast<to_t>(a);
	assert_valid_dynamic_cast<to_t>(a, result, std::forward<error_string_t>(error));
	return result;
}

//overload to cast from any* to t*
template<typename to_t, typename error_string_t> //default error_string_t = char const*
	requires (std::is_pointer_v<to_t> && !is_any_or_derived_wrapper_v<std::remove_pointer_t<to_t>>)
to_t smart_static_cast(any* from, error_string_t&& error) //default error = "bad static cast from any*"
{
	to_t result = std::launder(reinterpret_cast<to_t>(reinterpret_cast<std::byte*>(from) - from->ptr_distance_from_any()));
	assert_valid_dynamic_cast<to_t>(from, result, std::forward<error_string_t>(error));
	return result;
}

//overload to cast from derived_wrapper<t>* to any*
template<std::same_as<any*> to_t, typename from_t, typename error_string_t = char const*>
any* smart_static_cast(derived_wrapper<from_t>* from, error_string_t&& error = "bad static cast to any*")
{
	any* result = static_cast<any*>(from);
	assert_valid_dynamic_cast<any*>(from, result, std::forward<error_string_t>(error));
	return result;
}

//just to cover this case when it gets called without creating a derived_wrapper<any>
template<std::same_as<any*> to_t, typename error_string_t = char const*>
any* smart_static_cast(any* from, error_string_t&& error = "this error should never be seen")
{
	return from;
}


//static cast for smart pointers
template<typename to_t, typename from_t, typename error_string_t = char const* >
std::unique_ptr<to_t> sptr_static_cast(std::unique_ptr<from_t>&& a, error_string_t&& error = "sptr_static_cast fail")
{
	from_t* rawptr = a.release();
	to_t* result = smart_static_cast<to_t*>(rawptr);
	assert_valid_dynamic_cast<to_t*>(rawptr, result, std::forward<error_string_t>(error));
	return std::unique_ptr<to_t>(result);
}

//if t is a virt or a virt_data
template<typename t>
constexpr bool is_virt_v = false;

//even though t doesnt actually derive from any, virt acts like it does for virt<any> and virt<t>, so this is necessary
template<typename derived_t, typename base_t>
concept special_derived_from = (
	std::derived_from<derived_t,base_t> ||
	std::same_as<base_t,any>
	)
		&& (
		std::is_const_v<base_t> ||
		!std::is_const_v<derived_t>
		)
	;
	// && !is_virt_v<std::decay_t<derived_t>>);




//if t is not polymorphic, static_cast cannot be used to convert it to any*, so this provides a wrapper type
template<typename t>
struct underlying_type_wrapper
{
	struct type
	{
	private:
		virtual void filler() {}
		//filler just to make this class polymorhic (so dynamic cast works with it)
		//there might be a more standard way of doing this
		t underlying_wrapped_val;
	public:
		template<typename...args>
		type(args&&...vals) :
			underlying_wrapped_val
		{ std::forward<args>(vals)... } {}

		operator t& () { return underlying_wrapped_val; }
		operator t const& () const { return underlying_wrapped_val; }

	};
};


//if t is already polymorphic then naturally it can be used
template<typename t>
	requires std::is_polymorphic_v<t>
struct underlying_type_wrapper<t>
{
	using type = t;
};

template<typename t>
struct underlying_type_wrapper<const t>
{
	using type = typename underlying_type_wrapper<t>::type const;
};



template<typename raw_t>
	requires (!is_virt_v<raw_t>)
class virt;


//a smart pointer wrapper that only holds a t* that is also some derived_wrapper and therefore can be converted to any*
template<typename raw_t>
	requires (!is_virt_v<raw_t>)
class virt_data
{
private:

	template<typename o_t>
		requires (!is_virt_v<o_t>)
	friend class virt_data;
	template<typename o_t>
		requires (!is_virt_v<o_t>)
	friend class virt;

	template<typename result_t, typename...ts>
	friend virt<result_t> make_virt(ts&&...vals)
		requires std::constructible_from<result_t, ts...>;

	template<typename t>
	friend virt_data<t>& get_data_from_virt(virt<t>& a);

	template<typename t>
	friend virt_data<t> const& get_data_from_virt(virt<t>const& a);

	std::unique_ptr<any> ptr;

	using stored_t = typename underlying_type_wrapper<raw_t>::type; //if raw_t is polymorphic, stored_t is the same, otherwise, stored_t is wrapper of raw_t that makes it polymorphic

	void give_unique_ptr(std::unique_ptr<any>&& a)
	{
		my_assert([&] {return bool{ a }; }, "virt_data should not be given a nullptr");
		ptr = std::move(a);
	}

	virt_data(std::unique_ptr<any>&& a)
	{
		give_unique_ptr(std::move(a));
	}

public:

	virt_data() : ptr{nullptr} {}

	void reset()
	{
		ptr = nullptr;
	}


	virt_data(virt_data&& a) : ptr(std::move(a.ptr)) {}
	//virt_data(virt_data const& a) : ptr(a.ptr) {}
	virt_data& operator=(virt_data&& a) { ptr = std::move(a.ptr); return *this; }
	//virt_data& operator=(virt_data const& a) { ptr = a.ptr; return *this; }

	template<special_derived_from<stored_t> tar_t>
	virt_data(virt_data<tar_t>&& a) : ptr(std::move(a.ptr)) {}
	//template<special_derived_from<stored_t> tar_t>
	//virt_data(virt_data<tar_t> const& a) : ptr(a.ptr) {}
	template<special_derived_from<stored_t> tar_t>
	virt_data& operator=(virt_data<tar_t> && a) { ptr = std::move(a.ptr); return *this; }
	//template<special_derived_from<stored_t> tar_t>
	//virt_data& operator=(virt_data<tar_t> const& a) { ptr = a.ptr; return *this; }


	template<special_derived_from<stored_t> tar_t>
	virt_data(virt<tar_t>&& a) : ptr(std::move(get_data_from_virt(a))) {}
	//template<special_derived_from<stored_t> tar_t>
	//virt_data(virt<tar_t> const& a) : ptr(get_data_from_virt(a)) {}
	template<special_derived_from<stored_t> tar_t>
	virt_data& operator=(virt<tar_t>&& a) { ptr = std::move(get_data_from_virt(a).ptr); return *this; }
	//template<special_derived_from<stored_t> tar_t>
	//virt_data& operator=(virt<tar_t> const& a) { ptr = get_data_from_virt(a).ptr; return *this; }

	virt_data& operator=(std::unique_ptr<derived_wrapper<stored_t>>&& a)
	{
		give_unique_ptr(std::move(a));
		return *this;
	}

	std::unique_ptr<stored_t> get_unique_ptr() &&
	{
		return sptr_static_cast<stored_t>(std::move(ptr));
	}

	template<special_derived_from<stored_t> tar_t>
	virt_data<tar_t> downcast() &&
	{
		my_assert([&] {return bool{ ptr }; }, "cannot downcast a nullptr");
		virt_data<tar_t> result = sptr_dynamic_cast<tar_t>(std::move(ptr));
		assert_valid_dynamic_cast<tar_t*>(&*ptr, &*result, "downcast failed");
		return virt_data<tar_t>{result};
	}

	template<typename tar_t>
		requires special_derived_from<stored_t,tar_t>
	virt_data<tar_t> upcast() &&
	{
		my_assert([&] {return bool{ ptr }; }, "cannot upcast a nullptr");
		return { virt_data<tar_t>{std::unique_ptr<tar_t>{std::move(ptr)}} };
	}

};


template<typename result_t, typename...ts>
virt<result_t> make_virt(ts&&...vals)
	requires std::constructible_from<result_t, ts...>;

//a smart pointer class that allows copy and move semantics like std::any does, particularly useful with virtual types
template<typename raw_t>
	requires (!is_virt_v<raw_t>)
class virt
{
private:

	template<typename o_t>
		requires (!is_virt_v<o_t>)
	friend class virt;

	using stored_t = typename underlying_type_wrapper<raw_t>::type; //if raw_t is polymorphic, stored_t is the same, otherwise, stored_t is wrapper of raw_t that makes it polymorphic


public:
	virt_data<raw_t> data;

	void reset()
	{
		data.reset();
	}

	template<typename result_t, typename...ts>
	friend virt<result_t> make_virt(ts&&...vals)
		requires std::constructible_from<result_t, ts...>;

	template<special_derived_from<stored_t> result_t = raw_t, typename...ts>
		requires std::constructible_from<result_t, ts...>
	result_t& emplace(ts&&...args)
	{
		auto&& newdata = make_virt<result_t>(std::forward<ts>(args)...);
		result_t& ret = *newdata;
		data = std::move(newdata);
		return ret;
	}
	template<special_derived_from<stored_t> result_t = raw_t, typename u, typename...ts>
		requires std::constructible_from<result_t, ts...>
	result_t& emplace(std::initializer_list<u> il, ts&&...args)
	{
		return emplace<result_t>(il,std::forward<ts>(args)...);
	}

	explicit virt() : data{} {} //data{} initializes to nullptr


	template<special_derived_from<stored_t> from_t>
	virt(virt_data<from_t>&& a) : data(std::move(a)) {}

	template<special_derived_from<stored_t> from_t>
	virt(virt_data<from_t> const& a) : data(a) {}

	virt(virt_data<raw_t>&& a) : data(std::move(a)) {}

	virt(virt_data<raw_t> const& a) : data(a) {}


	template<special_derived_from<stored_t> rhs_t>
		requires (!is_virt_v<std::decay_t<rhs_t>> && std::constructible_from<stored_t,rhs_t&&>)
	virt(rhs_t&& a) : data{ std::make_unique<derived_wrapper<typename underlying_type_wrapper<rhs_t>::type>>(std::move(a)) } {}

	template<special_derived_from<stored_t> rhs_t>
		requires (!is_virt_v<std::decay_t<rhs_t>> && std::constructible_from<stored_t, rhs_t const&>)
	virt(rhs_t const& a) : data{ std::make_unique<derived_wrapper<typename underlying_type_wrapper<rhs_t>::type>>(a) } {}

	virt(raw_t&& a) requires std::move_constructible<raw_t>
	: data{ std::make_unique<derived_wrapper<stored_t>>(std::move(a)) } 
	{static_assert(!std::is_same_v<raw_t, int> || !std::is_same_v<stored_t,int>);}

	virt(raw_t const& a) requires std::copy_constructible<raw_t>
	: data{ std::make_unique<derived_wrapper<stored_t>>(a) } {}

	template<typename rhs_t>
		requires special_derived_from<std::remove_const_t<rhs_t>, stored_t>
	virt(virt<rhs_t>&& a)
	{
		if (a)
		{
			any* target = a.data.ptr.get();
			data.ptr = target->move_construct();
		}
		else
		{
			reset();
		}
	}

	virt(virt&& a)
	{
		if (a)
		{
			any* target = a.data.ptr.get();
			data.ptr = target->move_construct();
		}
		else
		{
			reset();
		}
	}


	template<typename rhs_t>
		requires special_derived_from<std::remove_const_t<rhs_t>, stored_t>
	virt(virt<rhs_t> const& a)
	{
		if (a)
		{
			any* target = a.data.ptr.get();
			data.ptr = target->copy_construct();
		}
		else
		{
			reset();
		}
	}

	virt(virt const& a)
	{
		if(a)
		{
			any* target = a.data.ptr.get();
			data.ptr = target->copy_construct();
		}
		else
		{
			reset();
		}
	}


	template<special_derived_from<stored_t> from_t>
	virt& operator=(virt_data<from_t>&& a)
		requires (!std::is_const_v<raw_t>)
	{
		data = std::move(a);
		return *this;
	}

	template<special_derived_from<stored_t> from_t>
	virt& operator=(virt_data<from_t> const& a)
		requires (!std::is_const_v<raw_t>)
	{
		data = a;
		return *this;
	}

	template<special_derived_from<stored_t> rhs_t>
		requires (!is_virt_v<std::decay_t<rhs_t>> && std::assignable_from<stored_t, rhs_t&&>
			&& !std::is_const_v<raw_t> )
	virt& operator=(rhs_t&& a)
	{
		if(get_type() == typeof(a))
		{
			data.ptr->do_move_assign(safe_void_ptr_derived<rhs_t>{&a});
		}
		else
		{
			data.ptr = std::move(virt{ std::move(a) }.data.ptr);
		}
		return *this;
	}

	template<special_derived_from<stored_t> rhs_t>
		requires (!is_virt_v<std::decay_t<rhs_t>> && std::assignable_from <stored_t, rhs_t const&>
			&& !std::is_const_v<raw_t>)
	virt& operator=(rhs_t const& a)
	{
		if (get_type() == typeof(a))
		{
			data.ptr->do_copy_assign(safe_void_ptr_derived<rhs_t const>{&a});
		}
		else
		{
			data.ptr = std::move(virt{ a }.data.ptr);
		}
		return *this;
	}

	template<typename rhs_t>
		requires (special_derived_from<std::remove_const_t<rhs_t>, stored_t>
			&& !std::is_const_v<raw_t>) 
	virt& operator=(virt<rhs_t>&& a)
	{
		if(!has_value() || !a.has_value() || !data.ptr->try_move_assign(a.data.ptr.get()))
		{
			data.ptr = std::move(virt{ std::move(a) }.data.ptr);
		}
		return *this;
	}

	virt& operator=(virt&& a)
		requires (!std::is_const_v<raw_t>)
	{
		if (!has_value() || !a.has_value() || !data.ptr->try_move_assign(a.data.ptr.get()))
		{
			data.ptr = std::move(virt{ std::move(a) }.data.ptr);
		}
		return *this;
	}

	template<typename rhs_t>
		requires (special_derived_from<std::remove_const_t<rhs_t>, stored_t>
			&& !std::is_const_v<raw_t>)
	virt& operator=(virt<rhs_t> const& a)
	{
		if (!has_value() || !a.has_value() || !data.ptr->try_copy_assign(a.data.ptr.get()))
		{
			data.ptr = std::move(virt{ a }.data.ptr);
		}
		return *this;
	}

	virt& operator=(virt const& a)
		requires (!std::is_const_v<raw_t>)
	{
		if (!has_value() || !a.has_value() || !data.ptr->try_copy_assign(a.data.ptr.get()))
		{
			data.ptr = std::move(virt{ a }.data.ptr);
		}
		return *this;
	}


	type get_type()
	{
		return data.ptr->get_typeof();
	}

	raw_t* get() const
	{
		if(!data.ptr)
		{
			return nullptr;
		}
		stored_t* ptr = smart_static_cast<stored_t*>(&*data.ptr);
		raw_t& ret = *ptr;
		return &ret;
	}

	operator raw_t const* () const
	{
		return get();
	}

	operator raw_t*()
	{
		return get();
	}

	raw_t const& operator*() const
	{
		my_assert([&] {return has_value(); }, "cannot access empty virt");
		return *get();
	}

	raw_t& operator*()
	{
		my_assert([&] {return has_value(); }, "cannot access empty virt");
		return *get();
	}

	raw_t const* operator->() const
	{
		my_assert([&] {return has_value(); }, "cannot access empty virt");
		return get();
	}
	raw_t* operator->()
	{
		my_assert([&] {return has_value(); }, "cannot access empty virt");
		return get();
	}

	bool has_value() const
	{
		return get() != nullptr;
	}

	operator bool() const
	{
		return has_value();
	}


	//no rtti, always works
	template<typename tar_t>
		requires special_derived_from<stored_t, tar_t>
	tar_t* upcast()
	{
		using r_tar_t = typename underlying_type_wrapper<tar_t>::type;
		my_assert([&] {return has_value();}, "cannot upcast a nullptr");
		r_tar_t* ret = get();
		return ret;
	}

	//needs rtti
	template<special_derived_from<stored_t> tar_t>
	bool can_downcast() //uses dynamic_cast
	{
		using r_tar_t = typename underlying_type_wrapper<tar_t>::type;
		my_assert([&] {return has_value();}, "cannot check can_downcast for a nullptr");
		return dynamic_cast<r_tar_t*>(get()) != nullptr;
	}

	//no rtti
	//can_downcast_exact() //uses some kind of (hand coded) type_id* to check the exact held type

	//no rtti
	//can_cast_exact //upcasts if possible, otherwies tries can_downcast_exact
	
	//may need rtti
	//can_cast //upcasts if possible, otherwise tries can_cast_exact and then dynamic_cast

	//needs rtti for debug check
	template<special_derived_from<stored_t> tar_t>
	tar_t* downcast()
	{
		using r_tar_t = typename underlying_type_wrapper<tar_t>::type;
		my_assert([&] {return has_value(); }, "cannot downcast a nullptr");
		my_assert([&] {return can_downcast<r_tar_t>();}, "downcast failed");
		tar_t& ret = *smart_static_cast<r_tar_t*>(&*data.ptr);
		return &ret;
	}

	//no rtti
	//downcast_exact

	//no rtti
	//cast_exact
	
	//may need rtti for debug check
	//cast

	//needs rtti
	//try_downcast

	//no rtti
	//try_downcast_exact

	//no rtti
	//try_cast_exact

	//may need rtti
	//try_cast

	//needs rtti
	//do_dynamic_cast

};


template<typename t>
virt_data<t>& get_data_from_virt(virt<t>& a)
{
	return a.data;
}
template<typename t>
virt_data<t> const& get_data_from_virt(virt<t>const& a)
{
	return a.data;
}



template<typename t>
constexpr bool is_virt_v<virt<t>> = true;
template<typename t>
constexpr bool is_virt_v<virt_data<t>> = true;


template<typename result_t, typename...ts>
virt<result_t> make_virt(ts&&...vals)
	requires std::constructible_from<result_t, ts...>
{
	using t = typename underlying_type_wrapper<result_t>::type;
	return virt_data<result_t>{std::make_unique<derived_wrapper<t>>(std::forward<ts>(vals)...)};
}

//allocate_virt

//make/allocate _for_overwrite


namespace std
{
	template<typename t>
	void swap(virt<t>& lhs, virt<t>& rhs)
	{
		std::swap(lhs.data, rhs.data);
	}
}