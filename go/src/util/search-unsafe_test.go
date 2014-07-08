/**
* User: dave
*/
package util

import (
	"fmt"
	"strconv"
	"unsafe"
	"testing"
)

func binarySearch (target *interface{}, objects *[]*interface{}, comp func (first *interface{}, second *interface{}) int) int {
	if target == nil {
		return -1;
	}
	len := len(*objects)
	if (len == 0) {
		return -1;
	}
	var idx int
	lower, upper := 0, len - 1
	for (lower < upper) {
		fmt.Printf("lower: %v, upper %v\n", lower, upper)
		idx = (upper + lower) / 2
		vidx := (*objects)[idx]
		fmt.Printf("about to cmp: %v and %v", target, vidx)
		cmp := comp(target, vidx)
		fmt.Printf(":: %v\n", cmp)
		switch {
		case cmp < 0:
			upper = idx
		case cmp > 0:
			lower = idx
		case cmp == 0:
			return idx
		}
	}
	return -1
}

// Converts a pointer to a slice of *Test to a pointer to a slice of *interface{} (unsafely)
func convertSlice (t *[]*Test) *[]*interface{} {
	p := (unsafe.Pointer(t))
	return (*[]*interface{})(p)
}

func downCast (i *interface{}) *Test {
	p := (unsafe.Pointer(i))
	return (*Test)(p)
}

func upCast (t *Test) *interface{} {
	p := (unsafe.Pointer(t))
	return (*interface{})(p)
}

func CompareTo (first *interface{}, second *interface{}) int {
	// you can't do this
	// var t1 *Test = (*first).(*Test)
	// reason (probably) is that *interface{} tries to deref the pointer to interface{}
	// but of course it isn't an interface{} at all and it dies a horrible death (process hangs)
	// so we end up simply tunneling unsafe pointers and pretending they are *interfaces{}
	// really even worse than void* programming in C, because at least there you know that you're
	// working with something really opaque
	var t1 *Test = downCast(first)//(*first).(*Test)
	//fmt.Printf("In CompareTo, t1: %v, second: %v\n", t1.Name, second)
	var t2 *Test = downCast(second)//(*second).(*Test)
	//fmt.Printf("In CompareTo\n")
	fmt.Printf("cmp: %v and %v\n", t1.Name, t2.Name)
	return Strcmp(t1.Name, t2.Name)
}

func TestSearchUnsafe (t *testing.T) {
	fmt.Printf("hello, world\n")

	var array [10]*Test
	for i:=0; i < 10; i	++ {
		array[i] = &Test{"Foo" + strconv.Itoa(i)}
		fmt.Printf("array[%v]: '%v', %p\n", i, *array[i], array[i])
	}

	arraySlice := array[:]

	var target *interface{}
	target = upCast(array[3])

	var slice *[]*interface{}
	slice = convertSlice(&arraySlice)
	val := binarySearch(target, slice, CompareTo)
	fmt.Printf("idx: %v \n",strconv.Itoa(val))
}

