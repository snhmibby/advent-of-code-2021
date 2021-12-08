package main

import (
	"fmt"
	"io"
	"os"
	"strings"
)

type Crabs map[int]int

func croak(e error, msg string) {
	if e != nil {
		fmt.Fprintf(os.Stderr, "%s (%v).", msg, e)
		os.Exit(1)
	}
}

func getInput() Crabs {
	crabs := make(Crabs)
	file, err := os.Open("input")
	croak(err, "Cannot open file 'input'")
	in, _ := io.ReadAll(file)
	for _, s := range strings.Split(string(in), ",") {
		var pos int
		fmt.Sscanf(s, "%d", &pos)
		crabs[pos] = crabs[pos] + 1
	}
	return crabs
}

func maxPos(c Crabs) int {
	m := 0
	for k := range c {
		if k > m {
			m = k
		}
	}
	return m
}

func abs(x int) int {
	if x < 0 {
		return -x
	} else {
		return x
	}
}

func fuel(x int) int {
	return (x * (x + 1)) / 2
}

//bruteforce cost
func cost(crabs Crabs, pos int) int {
	sum := 0
	for p, c := range crabs {
		sum += fuel(abs(pos-p)) * c
	}
	return sum
}

func main() {
	crabs := getInput()
	minCost := 99999999
	for i := 0; i < maxPos(crabs); i++ {
		c := cost(crabs, i)
		if c < minCost {
			minCost = c
		}
	}
	fmt.Println(minCost)
}
