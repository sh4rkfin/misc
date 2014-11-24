package util

import (
	"reflect"
	"unsafe"
)

func vtoi(c []*Comparable) (i []*interface{}) {
	if len(c) > 0 {
		h := (*reflect.SliceHeader)(unsafe.Pointer(&i))
		h.Data = uintptr(unsafe.Pointer(&c[0]))
		h.Len = len(c)
		h.Cap = cap(c)
	}
	return
}

func binarySearchComparable(target *Comparable, objects *[]*Comparable) int {
	if target == nil {
		return -1
	}
	len := len(*objects)
	if len == 0 {
		return -1
	}
	var idx int
	lower, upper := 0, len
	for lower < upper {
		idx = (upper - lower) / 2
		vidx := (*objects)[idx]
		cmp := (*target).CompareTo(vidx)
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
