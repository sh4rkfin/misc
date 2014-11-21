package util

import (
	"fmt"
	"io"
)

func min (first int, second int) int {
	if first < second {
		return first
	}
	return second
}

func max (first int, second int) int {
	if first > second {
		return first
	}
	return second
}

type Comparable interface {
	CompareTo (that *Comparable) int
}

func Strcmp(a, b string) int {
	lenb := len(b)
	lena := len(a)
	min := lenb
	if lena < lenb {
		min = lena
	}
	diff := 0
	for i := 0; i < min && diff == 0; i++ {
		diff = int(a[i]) - int(b[i])
	}
	if diff == 0 {
		diff = lena - lenb
	}
	return diff
}

func BinarySearch (target interface{},
				   list IndexAccessible,
				   comp func (first interface{}, second interface{}) int,
                   debug io.Writer) int {
	if target == nil {
		return -1;
	}
	if debug != nil {
		fmt.Fprintf(debug, "random access list: %v, %p, %T\n", list, list, list)
	}
	len := list.Len()
	if (len == 0) {
		return -1;
	}
	var idx int
	lower, upper := 0, len
	for (lower < upper) {
		idx = (upper + lower) / 2
		if debug != nil {
			fmt.Fprintf(debug, "lower: %v, idx: %v, upper %v\n", lower, idx, upper)
		}
		vidx := list.Get(idx)
		if debug != nil {
			fmt.Fprintf(debug, "about to cmp: %v and %v\n", target, vidx)
		}
		cmp := comp(target, vidx)
		if debug != nil {
			fmt.Fprintf(debug, ":: %v\n", cmp)
		}
		switch {
		case cmp < 0:
			upper = idx
		case cmp > 0:
			lower = idx + 1
		case cmp == 0:
			return idx
		}
	}
	return -lower - 1
}




