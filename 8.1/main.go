package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
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

const Segments = 7

//bitmap (set) of segments
type Wire uint

func mkWire(s string) Wire {
	w := Wire(0)
	for _, c := range s {
		if _, ok := normalDisplay[c]; !ok {
			panic("bad wire")
		}
		w |= normalDisplay[c]
	}
	return w
}

func (w Wire) nSegments() int {
	n := 0
	for i := 0; i < Segments; i++ {
		if w&(1<<i) != 0 {
			n++
		}
	}
	return n
}

func mapWire(s_ []string) []Wire {
	w := []Wire{}
	for _, s := range s_ {
		w = append(w, mkWire(s))
	}
	return w
}

var normalDisplay = map[rune]Wire{
	'a': 1 << 0, // #(0,2,3,5,6,7,8,9)   = 8
	'b': 1 << 1, // #(0,4,5,6,8,9)       = 6
	'c': 1 << 2, // #(0,1,2,3,4,7,8,9)   = 8
	'd': 1 << 3, // #(2,3,4,5,6,8,9)     = 7
	'e': 1 << 4, // #(0,2,6,8)           = 4
	'f': 1 << 5, // #(0,1,3,4,5,6,7,8,9) = 9
	'g': 1 << 6, // #(0,2,3,5,6,8,9)     = 7
}

var normalNumbers = []Wire{
	mkWire("abcefg"),  //0, len = 6
	mkWire("cf"),      //1, len = 2
	mkWire("acdeg"),   //2, len = 5
	mkWire("acdfg"),   //3, len = 5
	mkWire("bcdf"),    //4, len = 4
	mkWire("abdfg"),   //5, len = 5
	mkWire("abdefg"),  //6, len = 6
	mkWire("acf"),     //7, len = 3
	mkWire("abcdefg"), //8, len = 7
	mkWire("abcdfg"),  //9, len = 6
}

type Display struct {
	combinations, output []Wire //input
}

//solve the combination wires into numbers
func (d *Display) Solve() map[Wire]int {
	var peek [10]Wire
	solved := make(map[Wire]int)
	//part 1, the easy ones
	for _, w := range d.combinations {
		switch w.nSegments() {
		case 2:
			solved[w] = 1
			peek[1] = w
		case 3:
			solved[w] = 7
			peek[7] = w
		case 4:
			solved[w] = 4
			peek[4] = w
		case 7:
			solved[w] = 8
			peek[8] = w
		}
	}
	//part 2, the hard ones
	// only assume 1,4,7 and 8 segments' are known
	for _, w := range d.combinations {
		switch w.nSegments() {
		case 5:
			//either 2, 3 or 5
			segment_BD := peek[4] &^ peek[1]
			if w&peek[1] == peek[1] {
				solved[w] = 3
			} else if (w & segment_BD) == segment_BD {
				solved[w] = 5
			} else {
				solved[w] = 2
			}
		case 6:
			//either 0, 6 or 9
			if w&peek[4] == peek[4] {
				solved[w] = 9
			} else if w&peek[1] == peek[1] {
				solved[w] = 0
			} else {
				solved[w] = 6
			}
		}
	}
	return solved
}

func (d *Display) Print() {
	numbers := d.Solve()
	for _, digit := range d.output {
		fmt.Print(numbers[digit])
	}
}

func parseDisplay(s string) Display {
	sp := strings.Split(s, " | ")
	assert(len(sp) == 2, "Couldn't parse input")
	var d Display
	d.combinations = mapWire(strings.Split(sp[0], " "))
	d.output = mapWire(strings.Split(sp[1], " "))
	assert(len(d.combinations) == 10, "bad # combinations (%d:%v)", len(d.combinations), d.combinations)
	assert(len(d.output) == 4, "bad # output (%s)", sp[1])
	return d
}

func getInput() []Display {
	file, err := os.Open("input")
	croak(err, "Cannot open file 'input'")
	scanner := bufio.NewScanner(file)
	var input []Display
	for scanner.Scan() {
		d := parseDisplay(scanner.Text())
		input = append(input, d)
	}
	return input
}

func main() {
	part1 := 0
	part2 := 0
	for _, d := range getInput() {
		number := d.Solve()
		x := 0
		for _, n := range d.output {
			x = x*10 + number[n]
			switch number[n] {
			case 1, 4, 7, 8:
				part1++
			}
		}
		part2 += x
	}
	fmt.Println(part1)
	fmt.Println(part2)
}
