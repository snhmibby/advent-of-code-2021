package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func croak(e error, msg string) {
	if e != nil {
		fmt.Fprintf(os.Stderr, "%s (%v).", msg, e)
		os.Exit(1)
	}
}

func getInput() []int {
	file, err := os.Open("input")
	croak(err, "Cannot open file 'input'")
	scanner := bufio.NewScanner(file)
	var numbers []int
	for scanner.Scan() {
		n, err := strconv.Atoi(scanner.Text())
		if err == nil {
			numbers = append(numbers, n)
		}
	}
	return numbers
}

func sum(a []int) int {
	s := 0
	for i := 0; i < len(a); i++ {
		s += a[i]
	}
	return s
}

const windowSize = 3

func window(in []int, i int) int {
	return sum(in[i : i+windowSize])
}

func main() {
	in := getInput()
	cnt := 0
	for i := 0; i < len(in)-windowSize; i++ {
		if window(in, i) < window(in, i+1) {
			cnt++
		}
	}
	fmt.Println(cnt)
}
