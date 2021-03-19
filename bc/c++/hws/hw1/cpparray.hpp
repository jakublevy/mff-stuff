//
// Created by Jakub Levý on 31.10.18.
//

#ifndef _CPPARRAY_HPP_
#define _CPPARRAY_HPP_

#include <memory>

template <typename T, typename Allocator = std::allocator<T>> class Array {
public:

    //STL naming conventions
    using iterator = T *;
    using const_iterator = T const *;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;
    using size_type = std::size_t;
    using value_type = T;
    using difference_type = std::ptrdiff_t;
    using reference = value_type &;
    using const_reference = value_type const &;
    using pointer = typename std::allocator_traits<Allocator>::pointer;
    using const_pointer = typename std::allocator_traits<Allocator>::const_pointer;


    Array() : data(nullptr), avail(nullptr), limit(nullptr) { }
   ~Array() { uncreate(); }
    explicit Array(size_type n, const_reference val = T()) { create(n, val); }
    Array(Array const &other) { create(other.cbegin(), other.cend()); }

    template <typename InputIterator>
    explicit Array(InputIterator begin, InputIterator end) { create(begin, end); }

    //implemented by copy and swap idiom, provides strong exception safety
    Array &operator=(Array other);

    //copy and swap idiom
    Array(Array &&other) : Array() { swap(other); }

    size_type size() const noexcept { return avail - data; }
    size_type capacity() const noexcept { return limit - data; }

    reference operator[](size_type i) { return data[i]; }
    const_reference operator[](size_type i) const { return data[i]; }
    reference at(size_type i);
    const_reference at(size_type i) const;

    iterator begin() noexcept { return data; }
    const_iterator begin() const noexcept { return data; }
    const_iterator cbegin() const noexcept { return data; }
    reverse_iterator rbegin() noexcept { return std::make_reverse_iterator(avail); }
    const_reverse_iterator rbegin() const noexcept { return std::make_reverse_iterator(avail); }
    const_reverse_iterator crbegin() const noexcept { return std::make_reverse_iterator(avail); }

    iterator end() noexcept { return avail; }
    const_iterator end() const noexcept { return avail; }
    const_iterator cend() const noexcept { return avail; }
    reverse_iterator rend() noexcept { return std::make_reverse_iterator(data); }
    const_reverse_iterator rend() const noexcept { return std::make_reverse_iterator(data); }
    const_reverse_iterator crend() const noexcept { return std::make_reverse_iterator(data); }

    //two push back functions, first for l-values and the second for r-values
    //just as other STL containers have
    void push_back(const_reference val);
    void push_back(value_type &&val);

    void pop_back() { destroyLast(); }

    template<typename ...Args>
    void emplace_back(Args &&...args);

    bool empty() const noexcept { return data == avail; }

    const_reference front() const { return *data; }
    reference front() { return *data; }

    const_reference back() const { return *(avail - 1); }
    reference back() { return *(avail - 1); }

    //swap function for two Arrays provided,
    //guarantees not to emit any exceptions
    //used for copy and swap idiom
    void swap(Array &other) noexcept;

    void clear() noexcept { resizeImpl(0, T()); }

    //if count > size()
    //   reallocation is made (then size() == count)
    //   new memory is occupied by default constructed elements or supplied value
    //if count < size()
    //   elements that doesn't fit are destroyed, memory is not deallocated though
    void resize(size_type count) { resizeImpl(count, T()); }
    void resize(size_type count, const_reference value) { resizeImpl(count, value); }

    //preallocates desired memory and keeps it uninitialized
    void reserve(size_type n) { return ensureCapacity(n); }

    //appends one Array at the end the second
    //uses std::uninitialized_copy for l-values and C++17 std::uninitialized_move
    //for r-values
    void append(Array const &a);
    void append(Array &&a);

    //sets capacity() == size()
    void shrink_to_fit() { grow(avail - data); }

private:
    iterator data; //first element in the Array
    iterator avail; //one past last constructed element in the Array
    iterator limit; //one past last allocated element in the Array

    //for low-level managing of memory it's best to use an allocator
    //we want to distinguish between memory that's only allocated
    //but not occupied by an constructed object and memory where fully
    //constructed object is placed
    Allocator alloc;

    //called by constructors, standard STL behaviour
    void create(size_type n, const_reference val);

    template <typename InputIterator>
    void create(InputIterator begin, InputIterator end);

    //those functions are called by destructor (that is always noexcept)
    //they destroy objects and return allocated memory
    //they never throw
    void uncreate() noexcept;
    void destroyLast() noexcept;
    void destroyAll() noexcept;
    void deallocateAll() noexcept;

    //when we run out of space we allocated new space that is twice bigger
    //for resize function purposes we might need to increase space
    //to a certain amount
    void grow() { grow(std::max(2 * capacity(), size_type(1))); }
    void grow(size_type n);

    //capacity() == size()
    bool full() const noexcept { return avail == limit; }

    //allocates space for n value_type objects, sets the iterators
    //accordingly, memory stays uninitialized
    void uninitializedAlloc(size_type n);

    //constructs an object represented by template parameter pack
    //at avail iterator location
    template <typename ...U>
    void uncheckedAppend(U &&...v);

    //logic of STL resize function, called by public resize functions
    template <typename U>
    void resizeImpl(size_type count, U &&val);

    //after calling this function, we are guaranteed that capacity() >= n
    void ensureCapacity(size_type n);

    //doubles the amount of memory if we are already full
    void growIfFull();
};

template <typename T, typename A>
Array<T, A> &Array<T, A>::operator=(Array other) {
    swap(other);
    return *this;
}

template<typename T, typename A>
typename Array<T, A>::reference
Array<T, A>::at(Array::size_type i) {
    if(i < size()) {
        return data[i];
    }
    throw std::out_of_range("Accessed position is out of range.");
}

template<typename T, typename A>
typename Array<T, A>::const_reference
Array<T, A>::at(Array::size_type i) const {
    if(i < size()) {
        return data[i];
    }
    throw std::out_of_range("Accessed position is out of range.");
}

template<typename T, typename A>
void Array<T, A>::push_back(const_reference val) {
    growIfFull();
    uncheckedAppend(val);

}

template<typename T, typename A>
void Array<T, A>::push_back(T &&val) {
    growIfFull();
    uncheckedAppend(std::move(val));
}

template<typename T, typename A>
template<typename ...Args>
void Array<T, A>::emplace_back(Args &&...args) {
    growIfFull();
    uncheckedAppend(std::forward<Args>(args)...);
}

template<typename T, typename A>
void Array<T, A>::swap(Array &other) noexcept {
    std::swap(data, other.data);
    std::swap(avail, other.avail);
    std::swap(limit, other.limit);
}

template<typename T, typename A>
void Array<T, A>::append(Array const &a) {
    ensureCapacity(size() + a.size());
    avail = std::uninitialized_copy(a.data, a.avail, avail);

}

template<typename T, typename A>
void Array<T, A>::append(Array &&a) {
    ensureCapacity(size() + a.size());
    avail = std::uninitialized_move(a.data, a.avail, avail);
}

template<typename T, typename A>
void Array<T, A>::create(Array::size_type n, const_reference val) {
    uninitializedAlloc(n);
    std::uninitialized_fill(data, limit, val);
    avail = data + n;
}

template<typename T, typename A>
template <typename InputIterator>
void Array<T, A>::create(InputIterator begin, InputIterator end) {
    uninitializedAlloc(end - begin);
    limit = avail = std::uninitialized_copy(begin, end, data);
}

template<typename T, typename A>
void Array<T, A>::uncreate() noexcept {
    if(data) {
        destroyAll();
        deallocateAll();
    }
}

template<typename T, typename A>
void Array<T, A>::destroyLast() noexcept {
    std::allocator_traits<A>::destroy(alloc, --avail);
}

template<typename T, typename A>
void Array<T, A>::destroyAll() noexcept {
    while(avail != data) {
        destroyLast();
    }
}

template<typename T, typename A>
void Array<T, A>::deallocateAll() noexcept {
    std::allocator_traits<A>::deallocate(alloc, data, limit - data);
    data = avail = limit = nullptr;
}

template<typename T, typename A>
void Array<T, A>::grow(Array::size_type n) {
    iterator newData = std::allocator_traits<A>::allocate(alloc, n);
    iterator newAvail = std::uninitialized_copy(data, avail, newData);

    uncreate();

    data = newData;
    avail = newAvail;
    limit = data + n;
}

template<typename T, typename A>
void Array<T, A>::uninitializedAlloc(Array::size_type n) {
    data = avail = std::allocator_traits<A>::allocate(alloc, n);
    limit = data + n;
}

template<typename T, typename A>
template<typename ...U>
void Array<T, A>::uncheckedAppend(U &&...v) {
    std::allocator_traits<A>::construct(alloc, avail++, std::forward<U>(v)...);
}

template<typename T, typename A>
template<typename U>
void Array<T, A>::resizeImpl(Array::size_type count, U &&val) {
    while(size() > count) {
        destroyLast();
    }
    while(size() < count) {
        push_back(std::forward<U>(val));
    }
}

template<typename T, typename A>
void Array<T, A>::ensureCapacity(Array::size_type n) {
    if(capacity() < n) {
        grow(n);
    }
}

template<typename T, typename A>
void Array<T, A>::growIfFull() {
    if(full()) {
        grow();
    }

}

#endif //_CPPARRAY_HPP_
