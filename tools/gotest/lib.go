package somelib

import "fmt"

func Name() string {
	return "edef"
}

func Greet(s string) string {
	return fmt.Sprintf("Hello %s", s)
}
