// This program just exists to import some libraries and demonstrate
// that the build works, it doesn't do anything useful.
package main

import (
	"fmt"
	"somelib"
	"someproto"
)

func main() {
	p := someproto.Person{
		Name: somelib.Name(),
	}
	fmt.Println(somelib.Greet(fmt.Sprintf("%v", p)))
}
