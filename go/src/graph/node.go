package graph

import (
	"bytes"
)

type Node struct {
	name     string
	children []*Node
}

func (this *Node) Name() string {
	return this.name
}

func (this *Node) Children() []*Node {
	return this.children
}

func (this *Node) ChildCount() int {
	return len(this.children)
}

func NewNode(name string, children ...*Node) *Node {
	result := new(Node)
	result.name = name
	result.children = make([]*Node, len(children))
	copy(result.children, children)
	//fmt.Printf("children: %v, %v\n", result.children, children)
	return result
}

func (this *Node) Add(child *Node) {
	this.children = append(this.children, child)
}

func (this *Node) depth(depths map[*Node]int) int {
	if depth, exists := depths[this]; exists {
		return depth
	}
	result := -1
	for _, c := range this.children {
		d := c.depth(depths)
		if result < d {
			result = d
		}
	}
	result++
	depths[this] = result
	return result
}

func (this *Node) Depth() int {
	depths := make(map[*Node]int)
	return this.depth(depths)
}

func (this *Node) String() string {
	size := len(this.children)
	var buffer bytes.Buffer
	buffer.WriteString(this.name)
	buffer.WriteString(" [")
	for idx := 0; idx < size; idx++ {
		buffer.WriteString(this.children[idx].name)
		buffer.WriteString(" ")
	}
	buffer.WriteString("]")
	return buffer.String()
}
