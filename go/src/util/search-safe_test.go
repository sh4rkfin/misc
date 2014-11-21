/**
*/
package util

import (
	"fmt"
	"strconv"
	"os"
	"io"
	"math/rand"
	"testing"
)

type TestSafe struct {
	Name string
}

var debugWriter io.Writer

// Is a slice of pointers to TestSafe instances
type TestArray []*TestSafe

func (list TestArray) Get(idx int) interface{} {
	return list[idx]
}

func (list TestArray) Len () int {
	return len(list)
}

func (list TestArray) Add (element interface{}) bool {
	return false
}

func (list TestArray) Insert (idx int, element interface{}) bool {
	return false
}

func compareTo (first interface{}, second interface{}) int {
	var t1 *TestSafe = first.(*TestSafe)
	var t2 *TestSafe = second.(*TestSafe)
	if debugWriter != nil {
		fmt.Printf("cmp: %p and %p, %v and %v\n", t1, t2, t1.Name, t2.Name)
	}
	return Strcmp(t1.Name, t2.Name)
}

func testIn (arraySlice []*TestSafe) {

	fmt.Printf("        arraySlice: %v, %p, %T\n", arraySlice, arraySlice, arraySlice)
	var list TestArray = arraySlice
	fmt.Printf("    testarray list: %v, %p, %T\n", list, list, list)

	for i, elem := range arraySlice {
		target := elem
		val := BinarySearch(target, list, compareTo, nil)
		fmt.Printf("idx: %v (should be: %v)\n",strconv.Itoa(val), i)
	}
}

func testNotIn (arraySlice []*TestSafe) {

	var list TestArray = arraySlice

	for i, elem := range arraySlice {
		var target interface{} = &TestSafe{elem.Name + ".5"}
		val := BinarySearch(target, list, compareTo, nil)
		insertionPoint := i + 1
		fmt.Printf("idx: %v (should be: %v)\n",strconv.Itoa(val), -insertionPoint - 1)
	}
}

func Compare (first interface{}, second interface{}) int {
	s1 := first.(TestSafe)
	s2 := second.(TestSafe)
	//fmt.Printf("cmp: %v, %v, result %v\n", first, second, Strcmp(s1.Name, s2.Name))
	return Strcmp(s1.Name, s2.Name)
}

func TestBinarySearchInsert (t *testing.T) {
	list := &List{}
	for i:=0; i < 12; i++ {
		n := int32(50)
		elem := TestSafe{strconv.Itoa(int(rand.Int31n(n)))}
		idx := BinarySearch(elem, list, Compare, nil)
		fmt.Printf("val: %v", idx)
		if idx < 0 {
			idx = -idx - 1
		}
		fmt.Printf(", insertion point: %v\n", idx)
		list.Insert(idx, elem)
		fmt.Printf("list %p, %v\n", list, list)
	}
}

func TestSearchSafeMain (t *testing.T) {
	var array [10]*TestSafe
	for i:=0; i < 10; i	++ {
		array[i] = &TestSafe{"Foo" + strconv.Itoa(i)}
		fmt.Printf("array[%v]: '%v', %p\n", i, *array[i], array[i])
	}
	fmt.Printf("array: %v, %p\n", array, array)
	testIn(array[:])

	var list TestArray = array[:]
	var target interface{} = &TestSafe{"Foo9.5"}
	val := BinarySearch(target, list, compareTo, os.Stdout)
	fmt.Printf("idx: %v \n",strconv.Itoa(val))

	testNotIn(array[:])

}

