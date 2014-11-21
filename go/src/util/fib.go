package util

// fibonacci is a function that returns
// a function that returns an int.
func Fibonacci() func() int {
	var last, current = 0, 1
	return func() int {
		var result = current
		current, last = result + last, result
		return result
	}
}

