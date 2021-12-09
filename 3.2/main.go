package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func mkInt(s string) int {
	var x int
	for i := 0; i < Nbits; i++ {
		if s[i] == '1' {
			x |= 1 << (Nbits - 1 - i)
		}
	}
	return x
}

func croak(e error, msg string) {
	if e != nil {
		fmt.Fprintf(os.Stderr, "%s (%v).", msg, e)
		os.Exit(1)
	}
}

const Nbits = 12

func getInput() []string {
	file, err := os.Open("input")
	croak(err, "Cannot open file 'input'")
	scanner := bufio.NewScanner(file)
	var in []string

	for scanner.Scan() {
		s := scanner.Text()
		in = append(in, s)
	}
	return in
}

func lcb(in []string, b int) byte {
	ones, zeros := count(in, b)
	//lean towards 0 for problem description
	if zeros <= ones {
		return '0'
	} else {
		return '1'
	}
}

func mcb(in []string, b int) byte {
	ones, zeros := count(in, b)
	//lean towards 1 for problem description
	if ones >= zeros {
		return '1'
	} else {
		return '0'
	}
}

func count(in []string, b int) (ones, zeros int) {
	ones = 0
	zeros = 0
	for i := 0; i < len(in); i++ {
		if in[i][b] == '1' {
			ones++
		} else {
			zeros++
		}
	}
	return ones, zeros
}

type testfn func(string) bool
type gentest func(in []string, b int) testfn

func filter(in []string, test testfn) (out []string) {
	out = []string{}
	for _, v := range in {
		if test(v) {
			out = append(out, v)
		}
	}
	return out
}

func loop(in []string, gen gentest) string {
	cpy := make([]string, len(in))
	copy(cpy, in)
	b := 0
	for len(cpy) > 1 {
		if b == Nbits {
			log.Fatalln("Couldn't find single value in loop")
		}
		test := gen(cpy, b)
		cpy = filter(cpy, test)
		b++
	}
	return cpy[0]
}

func oxygen(in []string) string {
	return loop(in, func(x []string, b int) testfn {
		m := mcb(x, b)
		return func(s string) bool {
			return s[b] == m
		}
	})
}

func scrubber(in []string) string {
	return loop(in, func(x []string, b int) testfn {
		m := lcb(x, b)
		return func(s string) bool {
			return s[b] == m
		}
	})
}

func main() {
	in := getInput()
	oxygen := mkInt(oxygen(in))
	scrubber := mkInt(scrubber(in))
	fmt.Println(oxygen * scrubber)
}
