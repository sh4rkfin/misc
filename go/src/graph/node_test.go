package graph

import (
	"fmt"
	"strconv"
	"testing"
)

func TestBasic(t *testing.T) {

	a := NewNode("a")
	b := NewNode("b", a)

	t.Logf("child count: %v -> %v", b, b.ChildCount())

	c := NewNode("c", b)

	fmt.Printf("%v", c)
	t.Logf("%v", c)

	t.Logf("depth: %v -> %d", c, c.Depth())
}

func TestLotsOfNodes(t *testing.T) {
	var array [40]*Node

	count := len(array)

	for i := 0; i < count; i++ {
		array[i] = NewNode("Foo"+strconv.Itoa(i), array[0:i]...)
	}

	last := array[count-1]
	t.Logf("depth: %s -> %d", last.Name(), last.Depth())
}
