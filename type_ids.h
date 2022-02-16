

struct some_type_id
{

};

template<typename t>
struct type_id_of : some_type_id
{

};

template<typename t>
constexpr type_id_of<t> global_type_id_for = {};

class type
{
private:
	some_type_id const* id;
	type(some_type_id const* a) : id(a) {}
	template<typename t>
	friend type typeof();
	template<typename t>
	friend type typeof(t&);
public:
	auto operator<=>(type const&) const = default;
};

template<typename t>
type typeof()
{
	return type{&global_type_id_for<t>};
}

template<typename t>
type typeof(t&)
{
	return type{&global_type_id_for<t>};
}