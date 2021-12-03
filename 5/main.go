package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

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

func countBits(in []string) (ones, zeros []int) {
	ones = make([]int, len(in))
	zeros = make([]int, len(in))
	for _, s := range in {
		if len(s) < Nbits {
			continue
		}

		for i := 0; i < Nbits; i++ {
			if s[i] == '0' {
				zeros[i]++
			} else {
				ones[i]++
			}
		}
	}
	return ones, zeros
}

func getCommonBit(ones, zeros []int, i int) (mcb, lcb int) {
	if ones[i] == zeros[i] {
		log.Fatalf("ones == zeros?")
	} else if ones[i] > zeros[i] {
		mcb = 1
		lcb = 0
	} else {
		mcb = 0
		lcb = 1
	}
	return
}

func main() {
	var gamma, epsilon int

	in := getInput()
	ones, zeros := countBits(in)
	for i := 0; i < Nbits; i++ {
		mcb, lcb := getCommonBit(ones, zeros, i)
		gamma |= mcb << (Nbits - 1 - i)
		epsilon |= lcb << (Nbits - 1 - i)
	}
	fmt.Print(gamma * epsilon)
}
