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

func main() {
	in := getInput()
	cnt := 0
	for i := 0; i < len(in)-1; i++ {
		if in[i] < in[i+1] {
			cnt++
		}
	}
	fmt.Println(cnt)
}
