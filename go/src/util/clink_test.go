package util

import (
	"fmt"
	"testing"
)

func TestRandom(t *testing.T) {
	for i := 0; i < 10; i++ {
		rand := Random()
		fmt.Printf("rand: %v\n", rand)
	}
}
