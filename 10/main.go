package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
)

func assert(guard bool, msg string, args ...interface{}) {
	if !guard {
		log.Fatalf(msg, args...)
	}
}

func croak(e error, msg string) {
	if e != nil {
		fmt.Fprintf(os.Stderr, "%s (%v).", msg, e)
		os.Exit(1)
	}
}

func getInput() []string {
	var in []string
	file, err := os.Open("input")
	croak(err, "Cannot open file 'input'")
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		in = append(in, scanner.Text())
	}
	return in
}

type Stk []rune

func (stk Stk) empty() bool {
	return len(stk) == 0
}

func (stk *Stk) push(r rune) {
	*stk = append(*stk, r)
}

func (stk *Stk) pop() rune {
	if stk.empty() {
		panic("pop on empty stack")
	}
	last := len(*stk) - 1
	top := (*stk)[last]
	*stk = (*stk)[:last]
	return top
}

func (stk *Stk) peek() rune {
	if stk.empty() {
		panic("peek on empty stack")
	}
	return (*stk)[len(*stk)-1]
}

var pair = map[rune]rune{
	'(': ')',
	'{': '}',
	'<': '>',
	'[': ']',
}

func open(r rune) bool {
	_, ok := pair[r]
	return ok
}

var score2 = map[rune]int{
	')': 1,
	']': 2,
	'}': 3,
	'>': 4,
}

var score = map[rune]int{
	')': 3,
	']': 57,
	'}': 1197,
	'>': 25137,
}

func scoreLine(in string) int {
	var s Stk
	for _, c := range in {
		if open(c) {
			s.push(c)
			continue
		}
		//eat matching closing brace
		match := s.pop()
		if c != pair[match] {
			return score[c]
		}
	}

	// process incomplete line
	part2_score := 0
	for !s.empty() {
		match := pair[s.pop()]
		part2_score *= 5 //right.
		part2_score += score2[match]
	}
	return -part2_score
}

func main() {
	sum1 := 0
	s2 := []int{}
	for _, l := range getInput() {
		s := scoreLine(l)
		if s < 0 {
			s2 = append(s2, -s)
		} else {
			sum1 += s
		}
	}
	sort.Ints(s2)
	fmt.Println(sum1)
	fmt.Println(s2[len(s2)/2])
}
