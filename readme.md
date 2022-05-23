# polymorphic-any (virt\<t>)

Note: This project, as well as this readme, is far from complete as I lost interest in it a while ago. It has basic functionality but lacks many of the advanced features i had planned for it. Namely: constexpr support, adts, integration with standard library types (optional/any/variant), closed variants, constraints on derived instances, better reflection, stronger casting system, operations for additional concepts (like iterators, ranges, pointers, various numbers), stack based allocation, custom allocators, reusable dynamically allocated memory, support for t$, t const, and t const& (all with corresponding casting functionality), and a few other things I can't quite remember right now.

virt is a cross between std::any and std::unique_ptr, as well as std::variant and std::optional to some degree.
virt\<t> is able to hold t, any derived class of t, or nothing
t does not need to be virtual, and the dummy class "any" may be used to not constrain the held type.
virt has the pointer like semantics of a smart pointer, and the copy semantics of std::any.
virt is constexpr friendly for operations that do not require rtti (such as smart downcasting via dynamic_cast).
virt also can also use an arbitrarily large static buffer to avoid free store allocations, and reuse allocations


The main philosophy behind virt is that virt\<t1> and virt\<t2> should behave exactly like t1 and t2 (other than assignment between different types)

Unlike std::any, virt also uses move semantics of the underlying types where possible:
```c++
    foo my_foo = foo{};
    foo* foo_ptr = &my_foo;
    foo foo_copy = my_foo; //foo_ptr still points to my_foo
    foo foo_move = std::move(my_foo); //foo_ptr still points to my_foo

    std::any my_any = foo{};
    foo* any_ptr = &std::get<foo>(any1);
    std::any any_copy = my_any; //any_ptr still points to the value of my_any
    std::any any_move = std::move(my_any); //any_ptr no longer points to the value of my_any :(
    
    virt<base_of_foo> my_virt = foo{};
    base_of_foo* virt_ptr = my_virt;
    virt<base_of_foo> virt_copy = my_virt; //virt_ptr still points to the value of my_virt
    virt<base_of_foo> virt_move = std::move(my_virt); //virt_ptr still points to the value of my_virt
```
this of course requires an allocation. To avoid this and use traditional std::any/unique_ptr move semantics, the following can be done:
```c++
    virt_move = virt_data<base_of_foo>{std::move(my_virt)}; //option #1
    virt_move = virt_data{std::move(my_virt)}; //option #1 with template deduction
    virt_move.data = std::move(my_virt); //option #2 (after default initializing virt_move)
```

virt will always call the correct destructor, whether or not your types have virtual destructors, similar to a shared_ptr created via make_shared