package util

import (
	"fmt"
	"strconv"
	"unsafe"
	"testing"
	//"reflect"
)

type Nameable interface {
	getName() string
	setName(name string)
}

type TestIntf1 struct {
	Name string
}

type TestIntf2 struct {
	Name string
}

func (t *TestIntf1) getName() string {
	return t.Name
}

func (t *TestIntf1) setName(name string) {
	t.Name = name
}

func deferTest (num int) int {
	defer func() { num = 0 } ()
	return num
}

func TestAssignment (t *testing.T) {
	fmt.Printf("in testAssignment()\n")
	test := &TestIntf1{"Foo"}
	fmt.Printf("test -- %T: %p\n", test, test)
	var intf interface{}
	intf = test
	fmt.Printf("intf -- %T: %p\n", intf, intf)

	var test2 *TestIntf1
	test2 = intf.(*TestIntf1)
	fmt.Printf("test2 -- %T: %p %#v\n", test2, test2, test2)
	fmt.Printf("passed type assertion: " + test2.Name + "\n")
	fmt.Printf("done with testAssignment()\n")
}

// Converts a pointer to a slice of *TestIntf1 to a pointer to a slice of *interface{} (unsafely)
func castSlice (t *[]*TestIntf1) *[]*interface{} {
	p := (unsafe.Pointer(t))
	return (*[]*interface{})(p)
}

func cast (i *interface{}) *TestIntf1 {
	p := (unsafe.Pointer(i))
	return (*TestIntf1)(p)
}

func TestCastSlice (t *testing.T) {

	var array [10]*TestIntf1
	for i:=0; i < 10; i	++ {
		array[i] = &TestIntf1{"Foo" + strconv.Itoa(i)}
		fmt.Printf("%p, '%v' sizeof(): %v\n", array[i], array[i], unsafe.Sizeof(array[i]))
	}
	slice := array[:]
	fmt.Printf("slice: %T %p\n", &slice, &slice)

	intfs := castSlice(&slice)
	fmt.Printf("intfs: %T %p\n", intfs, intfs)

	for i, elem := range *intfs {
		if (i > 30) { break }
		fmt.Printf("elem: %v, sizeof(): %v\n", elem, unsafe.Sizeof(elem))
		var test *TestIntf1
		test = cast(elem)
		fmt.Printf("test: %v\n", test)
	}
}

func TestNameable (t *testing.T) {
	test := TestIntf1{"Foo"}
	var nameable Nameable

	nameable = &test
	fmt.Printf("name: %v\n", nameable)
}


func TestPanicAndRecover (t *testing.T) {
	test := &TestIntf1{"Foo"}
	var intf interface{}
	intf = test

	defer func() {
		if r := recover(); r != nil {
			fmt.Printf("recovering ... ", r)
			fmt.Printf("\n")
		}
	} ()

	var test2 TestIntf2
	test2 = intf.(TestIntf2)
	fmt.Printf("passed type assertion: " + test2.Name + "\n")
}

func TestInterfaces (t *testing.T) {
	fmt.Printf("hello, world\n")
	fmt.Printf("deferTest(): " + strconv.Itoa(deferTest(1)) + "\n")

}




