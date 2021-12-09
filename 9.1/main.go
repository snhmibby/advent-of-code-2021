package main

import (
	"bufio"
	"fmt"
	"image"
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

type Map [][]int
type Basin map[image.Point]bool

func getInput() Map {
	var m Map
	file, err := os.Open("input")
	croak(err, "Cannot open file 'input'")
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		row := make([]int, len(scanner.Text()))
		for i, c := range scanner.Text() {
			row[i] = int(c) - '0'
			assert(0 <= row[i] && row[i] <= 9, "value out of range (%c)\n", c)
		}
		m = append(m, row)
	}

	//check all rows are the same length
	for _, row := range m {
		assert(len(row) == len(m[0]), "rows not all the same length!\n")
	}
	return m
}

func height(m Map, pt image.Point) int {
	return m[pt.X][pt.Y]
}

func validPt(m Map, pt image.Point) bool {
	goodX := 0 <= pt.X && pt.X < len(m)
	goodY := 0 <= pt.Y && pt.Y < len(m[0])
	return goodX && goodY
}

var offsets = [4]image.Point{
	{-1, 0}, {1, 0}, {0, -1}, {0, 1},
}

func neighbours(m Map, pt image.Point) (res []image.Point) {
	for _, n_ := range offsets {
		n := pt.Add(n_)
		if validPt(m, n) {
			res = append(res, n)
		}
	}
	return
}

func lowPoints(m Map) (res []image.Point) {
	for r, row := range m {
		for c, h := range row {
			low := true
			pt := image.Pt(r, c)
			for _, p := range neighbours(m, pt) {
				low = low && h < height(m, p)
			}
			if low {
				res = append(res, pt)
			}
		}
	}
	return
}

func risk(m Map, pt image.Point) int {
	return 1 + height(m, pt)
}

func sum_risk(m Map, pts []image.Point) (sum int) {
	for _, pt := range pts {
		sum += risk(m, pt)
	}
	return
}

func calcBasin(m Map, b Basin, pt image.Point) {
	if b[pt] || height(m, pt) == 9 {
		return
	}
	b[pt] = true
	for _, n := range neighbours(m, pt) {
		//only search 'up'
		if height(m, pt) <= height(m, n) {
			calcBasin(m, b, n)
		}
	}
}

//calculate the basin for a low point
func basin(m Map, pt image.Point) Basin {
	b := make(Basin)
	calcBasin(m, b, pt)
	return b
}

func basins(m Map) (b []int) {
	for _, lp := range lowPoints(m) {
		b = append(b, len(basin(m, lp)))
	}
	return b
}

func part2(m Map) int {
	p2 := basins(m)
	sort.Ints(p2)
	l := len(p2)
	return p2[l-1] * p2[l-2] * p2[l-3]
}

func main() {
	m := getInput()
	fmt.Println("part 1: ", sum_risk(m, lowPoints(m)))
	fmt.Println("part 2: ", part2(m))
}
