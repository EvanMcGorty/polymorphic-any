#include<memory>
#include<concepts>
#include<string>
#include<cassert>

//todo: allow further template parameters of unique_ptr or different pointer types
//equality comparison? std::function
//make everything constexpr
//custom stack based alloc function for unique_ptr, have a byte[] buffer in virt that unique_ptr points to when the target object is small enough
//virt second template argument that takes a size_t wrapper or a byte[] wrapper that automatically calculates the largest size from a list of types
//but be sure to be able to distinguish between a pure variant and one that is also an any
//second template parameter that includes pointer type should be the same thing that can transform this into a variant
// 
//need to cover:
//any unconstrained, any constrained to base class
//		a generic constraint (compile time) can also be given on the classes that may be used with any
//any with unique_ptr, any with shared_ptr, etc
//any with a minimum list of types that it may contain, any without this list (same as the list being empty)
//any with stack based memory buffer of at least N, any without buffer (same as the buffer being N=0)
//if any has a buffer:
//		buffer can be set to be at least big enough to acommodate the list of the types which it may contain
//		or not
//if any has a type list
//		any can disallow objects whose types are not in this list (in this case the manually set buffer is meaningless)
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

template<typename to_t, typename from_t>
	requires std::is_pointer_v<to_t>
to_t my_dynamic_cast(from_t* a)
{
	return dynamic_cast<to_t>(a);
}

template<typename to_t, typename from_t>
	requires std::is_pointer_v<to_t>
to_t my_static_cast(from_t* a)
{
	my_assert([&]{return my_dynamic_cast<to_t>(a)!=nullptr;},"static_cast fail");
	return static_cast<to_t>(a);
}

//static cast for unique_ptr
template<typename to_t, typename from_t>
std::unique_ptr<to_t> uptrcast(std::unique_ptr<from_t>&& a)
{
	from_t* rawptr = a.release();
	my_assert([&] {return my_dynamic_cast<to_t*>(rawptr) != nullptr; }, "bad static cast");
	return std::unique_ptr<to_t>{my_dynamic_cast<to_t*>(rawptr)};
}


class universal_base
{
private:

template<typename t>
friend class derived_wrapper;
template<typename t>
friend class virt;

	//generic operations without consistent return types
	virtual std::unique_ptr<universal_base> copy_construct() const = 0;
	virtual std::unique_ptr<universal_base> move_construct() && = 0;
	//make reference/pointer
	//make const reference/pointer
	//dereference
	//const cast?
	//make optional?
	//make std::unique_ptr?
	//make arbitrary templated class/call arbitrary templated function given by template parameter in virt
	//		maybe make almost_universal_base<t> derive from universal base in cases when this template parameter is supplied
	//call arbitrary template<t> function by passing in an id so derived wrapper knows which templated function to call
	//		derived_wrapper will have a list of callables in its template arguments
	//		maybe the id could be a unique type_id* like object of a wrapper of the callable template
	//		(because raw templates can also be given ids by passing a template template to a wrapper class)
	//		maybe put everything, including copy costruct and move construct into this system
	
	//make std::any
	//default constructor generator
	//operations with only 1 generic argument (this*)
	//ostream << operator
	//istream >> operator
	//[size_t] index operator
	//conversion to some_iterator<ptr<universal_base>> (by iterating through underlying type if possible)
	//operator bool()
	//conversions to common types (int, double, string)
	//operations with multiple generic arguments
	//same-type comparison operators


	//virtual size_t sizeof_most_derived() const = 0;
	//typeid of most derived object
	//return std::function<ptr<universal_base>(ptr<universal_base>)>(target)

	//maybe not needed:
	//virtual universal_base* universal_base_this() const = 0;
	//virtual void* derived_wrapper_this() const = 0;

};


template<typename t>
class derived_wrapper final : public t, public universal_base //declared final so that sizeof can be determined for the whole object
{
public:

	/*derived_wrapper(t&& a) : t(std::move(a)) {}
	derived_wrapper(t const& a) : t(a) {}*/

	template<typename...args>
	derived_wrapper(args&&...vals) : t{ std::forward<args>(vals)... } {}

	std::unique_ptr<universal_base> copy_construct() const override final
	{
		return { std::make_unique<derived_wrapper>(*my_static_cast<t const*>(this)) };
	}

	std::unique_ptr<universal_base> move_construct() && override final
	{
		return { std::make_unique<derived_wrapper>(std::move(*my_static_cast<t*>(this))) };
	}
};

template<typename from_t, typename error_string_t = char const*>
universal_base* cast_to_universal_base(from_t* from, error_string_t&& error = "bad universal_base cast")
{
	universal_base* result = static_cast<universal_base*>(static_cast<derived_wrapper<from_t>*>(from));
	//technically unsafe because there is no guarantee that from_t actually is also a derived_wrapper<from_t>,
	//but theoretically this should always work
	my_assert([&] {return my_dynamic_cast<universal_base*>(from) == result; }, std::forward<error_string_t>(error));
	return result;
}


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
		give_unique_ptr(std::move(a));
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
	{
		my_assert([&] {return bool{ a }; }, "virt_data should not be given a nullptr");
		cast_to_universal_base(&*a, "virt_data must be given a pointer to type derived_wrapper<t>");
		//cast to universal base has an assert in it
		ptr = std::move(a);
	}

	void give_unique_ptr(std::unique_ptr<derived_wrapper<t>>&& a)
	{
		my_assert([&] {return bool{ a }; }, "virt_data should not be given a nullptr");
		ptr = std::move(a);
	}

	template<std::derived_from<t> tar_t>
	bool can_downcast()
	{
		my_assert([&] {return bool{ ptr }; }, "cannot check can_downcast for a nullptr");
		return my_dynamic_cast<tar_t*>(&*ptr) != nullptr;
	}

	template<std::derived_from<t> tar_t>
	virt_data<tar_t> downcast() &&
	{
		my_assert([&] {return bool{ ptr }; }, "cannot downcast a nullptr");
		my_assert([&] {return my_dynamic_cast<tar_t*>(&*ptr)!=nullptr;}, "downcast failed");
		
		return virt_data<tar_t>{uptrcast<tar_t>(std::move(ptr))};
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
	class type : public t 
	{ 
	private:
		virtual void filler() {}
	//filler just to make this class polymorhic
	public:
		template<typename...args>
		type(args&&...vals) : t{std::forward<args>(vals)...} {}
	};
};

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

public:

	virt_data<stored_t> data;

	//template<typename result_t, typename...ts>
	//friend virt<result_t> make_virt(ts&&...vals);

	explicit virt() : data{} {}

	//template<std::derived_from<raw_t> rhs_t>
	//virt(virt_data<rhs_t>&& a)
	//{
	//	my_assert([&] {return bool{ a.ptr }; }, "virt may not be assigned an empty (nullptr) virt_data");
	//	data = std::move(a);
	//}


	////template<std::derived_from<raw_t> rhs_t>
	//template<typename rhs_t>
	//virt& operator=(virt_data<rhs_t>&& a)
	//{
	//	my_assert([&] {return bool{ a.ptr }; }, "virt may not be assigned an empty (nullptr) virt_data");
	//	data = std::move(a);
	//	return *this;
	//}


	template<std::derived_from<raw_t> rhs_t>
	virt(virt<rhs_t>&& a)
	{
		my_assert([&] {return a.data.ptr != nullptr; }, "virt may not be constructed from an empty (nullptr) virt");
		//my_assert([&] {return my_dynamic_cast<universal_base*>(&*a.data.ptr) != nullptr; },
		//	"virt pointer must point to type universal_base");
		universal_base* target = cast_to_universal_base(&*a.data.ptr);
		data.ptr = uptrcast<stored_t>(std::move(*target).move_construct());
	}

	virt(virt&& a)
	{
		my_assert([&] {return a.data.ptr != nullptr; }, "virt may not be constructed from an empty (nullptr) virt");
		//my_assert([&] {return my_dynamic_cast<universal_base*>(&*a.data.ptr) != nullptr; },
		//	"virt pointer must point to type universal_base");
		universal_base* target = cast_to_universal_base(&*a.data.ptr);
		data.ptr = uptrcast<stored_t>(std::move(*target).move_construct());
	}


	template<std::derived_from<raw_t> rhs_t>
	virt(virt<rhs_t> const& a)
	{
		my_assert([&] {return a.data.ptr != nullptr; }, "virt may not be constructed from an empty (nullptr) virt");
		//my_assert([&] {return my_dynamic_cast<universal_base*>(&*a.data.ptr) != nullptr; },
		//	"virt pointer must point to type universal_base");
		universal_base* target = cast_to_universal_base(&*a.data.ptr);
		data.ptr = uptrcast<stored_t>(target->copy_construct());
	}

	virt(virt const& a)
	{
		my_assert([&] {return a.data.ptr != nullptr; }, "virt may not be constructed from an empty (nullptr) virt");
		//my_assert([&] {return my_dynamic_cast<universal_base*>(&*a.data.ptr) != nullptr; },
		//	"virt pointer must point to type universal_base");
		universal_base* target = cast_to_universal_base(&*a.data.ptr);
		data.ptr = uptrcast<stored_t>(target->copy_construct());
	}

	template<std::derived_from<stored_t> rhs_t>
	virt& operator=(virt<rhs_t>&& a)
	{
		my_assert([&] {return a.data.ptr != nullptr; }, "virt may not be asigned an empty (nullptr) virt");
		data.ptr = virt{ std::move(a) }.data.ptr;
		return *this;
	}

	virt& operator=(virt&& a)
	{
		my_assert([&] {return a.data.ptr != nullptr; }, "virt may not be asigned an empty (nullptr) virt");
		data.ptr = virt{ std::move(a) }.data.ptr;
		return *this;
	}

	template<std::derived_from<stored_t> rhs_t>
	virt& operator=(virt<rhs_t> const& a)
	{
		my_assert([&] {return a.data.ptr != nullptr; }, "virt may not be asigned an empty (nullptr) virt");
		data.ptr = virt{ a }.data.ptr;
		return *this;
	}

	virt& operator=(virt const& a)
	{
		my_assert([&] {return a.data.ptr != nullptr; }, "virt may not be asigned an empty (nullptr) virt");
		data.ptr = virt{ a }.data.ptr;
		return *this;
	}

private:
	raw_t* get_ptr() const
	{
		return my_static_cast<raw_t*>(&*data.ptr);
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

	template<std::derived_from<raw_t> tar_t>
	bool can_downcast()
	{
		my_assert([&] {return bool{ data.ptr }; }, "cannot check can_downcast for a nullptr");
		return my_dynamic_cast<tar_t*>(&*data.ptr) != nullptr;
	}

	template<std::derived_from<raw_t> tar_t>
	tar_t* downcast()
	{
		my_assert([&] {return bool{ data.ptr }; }, "cannot downcast a nullptr");
		my_assert([&] {return can_downcast<tar_t>();}, "downcast failed");
		return my_static_cast<tar_t*>(&*data.ptr);
	}

	template<typename tar_t>
		requires std::derived_from<raw_t, tar_t>
	tar_t* upcast()
	{
		my_assert([&] {return bool{ data.ptr }; }, "cannot upcast a nullptr");
		return my_static_cast<tar_t*>(&*data.ptr);
	}
};


template<typename result_t, typename...ts>
virt<result_t> make_virt(ts&&...vals)
	requires std::constructible_from<result_t, ts...>
{
	using t = typename underlying_type_wrapper<result_t>::type;
	virt<t> ret;
	ret.data = virt_data<t>{std::make_unique<derived_wrapper<t>>(std::forward<ts>(vals)... )};
	return std::move(ret);
}