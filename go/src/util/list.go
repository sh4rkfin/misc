package util

// Represents a collection, the elements of which are O(1) accessible by index.
type IndexAccessible interface {
	Len () int
	Get (idx int) interface{}
	Add (element interface{}) bool
	Insert (idx int, element interface{}) bool
}

// An struct implementing IndexAccessible
type List struct {
	count int
	elements []interface{}
}

// Returns the idx-th element of list
func (list *List) Get (idx int) interface{} {
	return list.elements[idx]
}

func (list *List) Len () int {
	return list.count
}

func (list *List) resize (elementsToCopy int) []interface{} {
	newsize := max(len(list.elements) * 2, 10)
	result := make([]interface{}, newsize)
	if elementsToCopy > 0 {
		copy(result, list.elements[0:elementsToCopy])
	}
	return result
}

func (list *List) Add (element interface{}) bool {
	//fmt.Printf("list %v, len: %v, count: %v\n", list, len(list.elements), list.count)
	if list.count > len(list.elements) - 1 {
		list.elements = list.resize(len(list.elements))
	}
	list.elements[list.count] = element
	list.count++
	return true
}

func (list *List) Insert (idx int, element interface{}) bool {
	//fmt.Printf("list %v, len: %v, count: %v, idx: %v\n", list, len(list.elements), list.count, idx)
	dest := list.elements
	if list.count > len(list.elements) - 1 {
		dest = list.resize(idx)
	}
	copy(dest[idx + 1:], list.elements[idx:])
	dest[idx] = element
	list.elements = dest
	list.count++
	return true
}

