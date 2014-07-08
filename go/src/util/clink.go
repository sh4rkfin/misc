package util

/*
#include <stdlib.h>
*/
import "C"

func Random() int {
	var r C.long = C.random()
	return int(r)
}

func Seed(i int) {
	C.srandom(C.uint(i))
}
