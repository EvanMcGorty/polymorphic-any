#include<memory>
#include<concepts>
#include<string>
#include<cassert>

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

template<std::predicate t>
void my_assert(t tar,std::string message = "[no error message provided]")
{
	assert(tar());
}

template<typename to_t, typename from_t, typename error_string_t = char const* >
	requires std::is_pointer_v<to_t>
void assert_valid_dynamic_cast(from_t* a, error_string_t&& error = "static_cast fail")
{
	my_assert([&]{return dynamic_cast<to_t>(a)!=nullptr;}, std::forward<error_string_t>(error));
}

class any;

template<typename t>
class derived_wrapper;

template<typename t>
constexpr bool is_any_or_derived_wrapper_v = false;


template<typename t>
constexpr bool is_any_or_derived_wrapper_v<derived_wrapper<t>> = true;

template<>
constexpr bool is_any_or_derived_wrapper_v<any> = true;


template<std::same_as<any*> to_t, typename error_string_t = char const*>
any* smart_static_cast(any* from, error_string_t&& error = "this error should never be seen")
{
	return from;
}

template<typename to_t, typename error_string_t = char const*>
	requires (std::is_pointer_v<to_t> && !is_any_or_derived_wrapper_v<std::remove_pointer_t<to_t>>)
to_t smart_static_cast(any* from, error_string_t&& error = "bad static cast from any*")
{
	to_t result = static_cast<to_t>(static_cast<derived_wrapper<std::remove_pointer_t<to_t>>*>(from));
	assert_valid_dynamic_cast<to_t>(from, std::forward<error_string_t>(error));
	return result;
}

template<std::same_as<any*> to_t, typename from_t, typename error_string_t = char const*>
	requires (!is_any_or_derived_wrapper_v<from_t>)
any* smart_static_cast(from_t* from, error_string_t&& error = "bad static cast to any*")
{
	any* result = static_cast<any*>(static_cast<derived_wrapper<from_t>*>(from));
	//technically unsafe because there is no guarantee that from_t actually is also a derived_wrapper<from_t>,
	//but theoretically this should always work
	assert_valid_dynamic_cast<any*>(from, std::forward<error_string_t>(error));
	return result;
}

template<typename to_t, typename from_t, typename error_string_t = char const*>
	requires (std::is_pointer_v<to_t> && !std::is_same_v<to_t,any*> && !std::is_same_v<from_t,any>)
to_t smart_static_cast(from_t* a, error_string_t&& error = "static_cast fail")
{
	assert_valid_dynamic_cast<to_t>(a, std::forward<error_string_t>(error));
	return static_cast<to_t>(a);
}


//static cast for unique_ptr
template<typename to_t, typename from_t, typename error_string_t = char const* >
std::unique_ptr<to_t> sptr_static_cast(std::unique_ptr<from_t>&& a, error_string_t&& error = "sptr_static_cast fail")
{
	from_t* rawptr = a.release();
	assert_valid_dynamic_cast<to_t*>(rawptr, std::forward<error_string_t>(error));
	return std::unique_ptr<to_t>(smart_static_cast<to_t*>(rawptr));
}


class any
{
public:
	virtual ~any(){}

private:

template<typename t>
friend class derived_wrapper;
template<typename t>
friend class virt;

	//generic operations without consistent return types
	virtual std::unique_ptr<any> copy_construct() const = 0;
	virtual std::unique_ptr<any> move_construct() && = 0;
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
class derived_wrapper final : public t, public any //declared final so that sizeof can be determined for the whole object
{
public:

	/*derived_wrapper(t&& a) : t(std::move(a)) {}
	derived_wrapper(t const& a) : t(a) {}*/

	template<typename...args>
	derived_wrapper(args&&...vals) : t{ std::forward<args>(vals)... } {}

	std::unique_ptr<any> copy_construct() const override final
	{
		if constexpr (std::is_copy_constructible_v<t>)
		{
			return { std::make_unique<derived_wrapper>(*smart_static_cast<t const*>(this)) };
		}
		else
		{
			return {nullptr};
		}
	}

	std::unique_ptr<any> move_construct() && override final
	{
		if constexpr (std::is_move_constructible_v<t>)
		{
			return { std::make_unique<derived_wrapper>(std::move(*smart_static_cast<t*>(this))) };
		}
		else
		{
			return { nullptr };
		}
	}

};



template<typename derived_t, typename base_t>
concept special_derived_from = std::derived_from<derived_t,base_t> || std::same_as<base_t,any>;

template<typename t>
class virt_data
{
private:

	template<typename o_t>
	friend class virt;

	std::unique_ptr<t> ptr;
public:

	virt_data() : ptr{nullptr} {}

	virt_data(std::unique_ptr<t>&& a)
	{
		give_unique_ptr(std::move(a));
	}

	virt_data(std::unique_ptr<derived_wrapper<t>>&& a)
	{
		give_unique_ptr(sptr_static_cast<any>(std::move(a)));
	}

	virt_data(virt_data&& a) = default;
	virt_data(virt_data const& a) = delete;
	virt_data& operator=(virt_data&& a) = default;
	virt_data& operator=(virt_data const& a) = delete;


	virt_data& operator=(std::unique_ptr<t>&& a)
	{
		give_unique_ptr(std::move(a));
		return *this;
	}
	virt_data& operator=(std::unique_ptr<derived_wrapper<t>>&& a)
	{
		give_unique_ptr(std::move(a));
		return *this;
	}

	std::unique_ptr<t> get_unique_ptr() &&
	{
		return std::move(ptr);
	}

	void give_unique_ptr(std::unique_ptr<t>&& a)
		requires !std::same_as<t,any>
	{
		my_assert([&] {return bool{ a }; }, "virt_data should not be given a nullptr");
		assert_valid_dynamic_cast<any*>(&*a, "virt_data must be given a pointer to type derived_wrapper<t>");
		//cast to any has an assert in it
		ptr = std::move(a);
	}

	void give_unique_ptr(std::unique_ptr<any>&& a)
	{
		my_assert([&] {return bool{ a }; }, "virt_data should not be given a nullptr");
		ptr = sptr_static_cast<t>(std::move(a));
	}

	template<std::derived_from<t> tar_t>
	bool can_downcast()
	{
		my_assert([&] {return bool{ ptr }; }, "cannot check can_downcast for a nullptr");
		return assert_valid_dynamic_cast<tar_t*>(&*ptr) != nullptr;
	}

	template<special_derived_from<t> tar_t>
	virt_data<tar_t> downcast() &&
	{
		my_assert([&] {return bool{ ptr }; }, "cannot downcast a nullptr");
		assert_valid_dynamic_cast<tar_t*>(&*ptr, "downcast failed");
		
		return virt_data<tar_t>{sptr_dynamic_cast<tar_t>(std::move(ptr))};
	}

	template<typename tar_t>
		requires std::derived_from<t,tar_t>
	virt_data<tar_t> upcast() &&
	{
		my_assert([&] {return bool{ ptr }; }, "cannot upcast a nullptr");
		return { virt_data<tar_t>{std::unique_ptr<tar_t>{std::move(ptr)}} };
	}

};


template<typename t>
struct underlying_type_wrapper
{
	struct type
	//	: public t
	{
	public:
		virtual void filler() {}
		//filler just to make this class polymorhic (so dynamic cast works with it)
		//there might be a more standard way of doing this

		template<typename...args>
		type(args&&...vals) :
		val
		//t
		{std::forward<args>(vals)...} {}



		t val; operator t& () { return val; }

	};
};

//template<typename t>
//	requires std::is_fundamental_v<t>
//struct underlying_type_wrapper<t>
//{
//	using type = t;
//};

template<typename t>
	requires std::is_polymorphic_v<t>
struct underlying_type_wrapper<t>
{
	using type = t;
};

//nullptr state can be achieved by default initialization, or by move constructing from ::data().
//This is an invalid state for any operation other than dereferencing
template<typename raw_t>
class virt
{
private:

	using stored_t = typename underlying_type_wrapper<raw_t>::type; //if raw_t is polymorphic, stored_t is the same, otherwise, stored_t is wrapper of raw_t that makes it polymorphic

	virt(virt_data<stored_t>&& a) : data(std::move(a)) {}

public:

	virt_data<stored_t> data;

	template<typename result_t, typename...ts>
	friend virt<result_t> make_virt(ts&&...vals)
		requires std::constructible_from<result_t, ts...>;

	explicit virt() : data{} {} //data{} initializes to nullptr

	//template<special_derived_from<raw_t> rhs_t>
	//virt(virt_data<rhs_t>&& a)
	//{
	//	my_assert([&] {return bool{ a.ptr }; }, "virt may not be assigned an empty (nullptr) virt_data");
	//	data = std::move(a);
	//}


	////template<special_derived_from<raw_t> rhs_t>
	//template<typename rhs_t>
	//virt& operator=(virt_data<rhs_t>&& a)
	//{
	//	my_assert([&] {return bool{ a.ptr }; }, "virt may not be assigned an empty (nullptr) virt_data");
	//	data = std::move(a);
	//	return *this;
	//}

	virt(raw_t&& a) : data{std::make_unique<derived_wrapper<stored_t>>(std::move(a))} {}
	virt(raw_t const& a) : data{std::make_unique<derived_wrapper<stored_t>>(a)} {}

	template<special_derived_from<raw_t> rhs_t>
	virt(virt<rhs_t>&& a)
	{
		if (a.data.ptr != nullptr)
		{
			any* target = smart_static_cast<any*>(&*a.data.ptr);
			data.ptr = sptr_static_cast<stored_t>(std::move(*target).move_construct());
		}
		else
		{
			data.ptr = nullptr;
		}
	}

	virt(virt&& a)
	{
		if (a.data.ptr != nullptr)
		{
			any* target = smart_static_cast<any*>(&*a.data.ptr);
			data.ptr = sptr_static_cast<stored_t>(std::move(*target).move_construct());
		}
		else
		{
			data.ptr = nullptr;
		}
	}


	template<special_derived_from<raw_t> rhs_t>
	virt(virt<rhs_t> const& a)
	{
		if (a.data.ptr != nullptr)
		{
			any* target = smart_static_cast<any*>(&*a.data.ptr);
			data.ptr = sptr_static_cast<stored_t>(target->copy_construct());
		}
		else
		{
			data.ptr = nullptr;
		}
	}

	virt(virt const& a)
	{
		if(a.data.ptr != nullptr)
		{
			any* target = smart_static_cast<any*>(&*a.data.ptr);
			data.ptr = sptr_static_cast<stored_t>(target->copy_construct());
		}
		else
		{
			data.ptr = nullptr;
		}
	}

	template<special_derived_from<stored_t> rhs_t>
	virt& operator=(virt<rhs_t>&& a)
	{
		data.ptr = std::move(virt{ std::move(a) }.data.ptr);
		return *this;
	}

	virt& operator=(virt&& a)
	{
		data.ptr = std::move(virt{ std::move(a) }.data.ptr);
		return *this;
	}

	template<special_derived_from<stored_t> rhs_t>
	virt& operator=(virt<rhs_t> const& a)
	{
		data.ptr = std::move(virt{ a }.data.ptr);
		return *this;
	}

	virt& operator=(virt const& a)
	{
		data.ptr = std::move(virt{ a }.data.ptr);
		return *this;
	}

private:
	raw_t* get_ptr() const
	{
		if constexpr(std::is_polymorphic_v<raw_t>)
		{
			return smart_static_cast<raw_t*>(&*data.ptr);
		}
		else
		{
			return &data.ptr->val;
		}

	}
public:

	operator raw_t const* () const
	{
		return get_ptr();
	}

	operator raw_t*()
	{
		return get_ptr();
	}

	raw_t const& operator*() const
	{
		return *get_ptr();
	}

	raw_t& operator*()
	{
		return *get_ptr();
	}

	raw_t const* operator->() const
	{
		return get_ptr();
	}
	raw_t* operator->()
	{
		return get_ptr();
	}
	
	bool isempty() const
	{
		return data.ptr == nullptr;
	}

	operator bool() const
	{
		return data.ptr != nullptr;
	}


	//no rtti, always works
	template<typename tar_t>
		requires std::derived_from<raw_t, tar_t>
	tar_t* upcast()
	{
		my_assert([&] {return bool{ data.ptr }; }, "cannot upcast a nullptr");
		return smart_static_cast<tar_t*>(&*data.ptr);
	}

	//needs rtti
	template<special_derived_from<stored_t> tar_t>
	bool can_downcast() //uses dynamic_cast
	{
		my_assert([&] {return bool{ data.ptr }; }, "cannot check can_downcast for a nullptr");
		return dynamic_cast<tar_t*>(&*data.ptr) != nullptr;
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
		my_assert([&] {return bool{ data.ptr }; }, "cannot downcast a nullptr");
		my_assert([&] {return can_downcast<tar_t>();}, "downcast failed");
		return smart_static_cast<tar_t*>(&*data.ptr);
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


template<typename result_t, typename...ts>
virt<result_t> make_virt(ts&&...vals)
	requires std::constructible_from<result_t, ts...>
{
	using t = typename underlying_type_wrapper<result_t>::type;
	return {virt_data<t>{std::make_unique<derived_wrapper<t>>(std::forward<ts>(vals)... )}};
}


