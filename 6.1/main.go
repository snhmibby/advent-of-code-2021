package main

import (
	"fmt"
	"io"
	"os"
	"strings"
)

//cluster generations
const NewGen = 8
const OldGen = 6

type Fishies [9]int64

func croak(e error, msg string) {
	if e != nil {
		fmt.Fprintf(os.Stderr, "%s (%v).", msg, e)
		os.Exit(1)
	}
}

func getInput() Fishies {
	var fishies Fishies
	file, err := os.Open("input")
	croak(err, "Cannot open file 'input'")
	in, _ := io.ReadAll(file)
	for _, s := range strings.Split(string(in), ",") {
		var generation int64
		fmt.Sscanf(s, "%d", &generation)
		if 0 <= generation && generation <= NewGen {
			fishies[generation]++
		}
	}
	return fishies
}

func Day(old Fishies) Fishies {
	var f Fishies
	f[NewGen] = old[0]
	f[OldGen] = old[0]
	for i := 1; i < 9; i++ {
		f[i-1] += old[i]
	}
	return f
}

func sum(a Fishies) int64 {
	s := int64(0)
	for _, v := range a {
		s += v
	}
	return s
}

func main() {
	const days = 256
	fishies := getInput()
	for day := 0; day < days; day++ {
		fishies = Day(fishies)
	}
	fmt.Println(sum(fishies))
}
