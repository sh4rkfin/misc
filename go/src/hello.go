/**
 * User: dave
 */
package main

import (
       "fmt"
)

func main() {
	sum := 0
	for i := 0; i < 10; i++ {
		sum += i
	}
	// here's a quick change
    fmt.Printf("hello, world: %v\n", sum)
}
