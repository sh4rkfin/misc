package util

import (
	"fmt"
	"strconv"
	"testing"
)


func (*Test) CompareTo (that *Comparable) int {
	return 0
}

func TestSearch (t *testing.T) {
	fmt.Printf("hello, world\n")

	var array [10]*Test
	for i:=0; i < 10; i	++ {
		array[i] = &Test{"Foo" + strconv.Itoa(i)}
		fmt.Printf("'" + array[i].Name + "'\n")
	}

	var target Comparable
	target = array[4]

	var slice []*Comparable
	// this next line doesn't compile - this is the wrong way to do it!
	//slice = array[:]
	val := binarySearchComparable(&target, &slice)
	fmt.Printf("idx: " + strconv.Itoa(val))
}

