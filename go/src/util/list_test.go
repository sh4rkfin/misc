package util

import (
	"fmt"
	"testing"
	"strconv"
)

func TestList (t *testing.T) {
	list := &List{}
	for i:=0; i < 12; i	++ {
		elem := TestSafe{"Foo" + strconv.Itoa(i)}
		list.Add(elem)
		if list.Len() != i+1 {
			fmt.Printf("list %p, %v\n", list, list)
			t.Errorf("Len(%v) should be %v but is actually %v",
			         list, i+1, list.Len())
		}
	}
	list.Insert(9, TestSafe{"Bar"})
	fmt.Printf("list %p, %v\n", list, list)
}

