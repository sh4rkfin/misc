package util

import (
	"fmt"
	"testing"
)

func TestFib(t *testing.T) {
	f := Fibonacci()
	for i := 0; i < 10; i++ {
		fmt.Println(f())
	}
}
