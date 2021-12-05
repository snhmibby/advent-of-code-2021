package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

func croak(e error, msg string) {
	if e != nil {
		fmt.Fprintf(os.Stderr, "%s (%v).", msg, e)
		os.Exit(1)
	}
}

type Location struct {
	x, y int
}

type Line struct {
	from, to Location
}

func max(a, b int) int {
	if a > b {
		return a
	} else {
		return b
	}
}

func min(a, b int) int {
	if a < b {
		return a
	} else {
		return b
	}
}

func (l *Line) Covers() []Location {
	covers := []Location{}
	//only consider orthogonal lines for now
	if l.from.x == l.to.x {
		start := min(l.to.y, l.from.y)
		end := max(l.to.y, l.from.y)
		for i := start; i <= end; i++ {
			covers = append(covers, Location{x: l.from.x, y: i})
		}
	}
	if l.from.y == l.to.y {
		start := min(l.to.x, l.from.x)
		end := max(l.to.x, l.from.x)
		for i := start; i <= end; i++ {
			covers = append(covers, Location{x: i, y: l.from.y})
		}
	}
	return covers
}

func parseLocation(s string) Location {
	var l Location
	n, _ := fmt.Sscanf(s, "%d,%d", &l.x, &l.y)
	if n != 2 {
		log.Fatalf("Could'nt parse location from (%s)\n", s)
	}
	return l
}

func parseLine(s string) Line {
	var l Line
	sp := strings.Split(s, "->")
	if len(sp) != 2 {
		log.Fatalf("Couldn't parse line from (%s)\n", s)
	}
	l.from = parseLocation(sp[0])
	l.to = parseLocation(sp[1])
	return l
}

func orthogonal(l Line) bool {
	return l.from.x == l.to.x || l.from.y == l.to.y
}

func getInput() []Line {
	file, err := os.Open("input")
	croak(err, "Cannot open file 'input'")
	scanner := bufio.NewScanner(file)
	var lines []Line
	for scanner.Scan() {
		l := parseLine(scanner.Text())
		if orthogonal(l) {
			lines = append(lines, l)
		}
	}
	return lines
}

const N = 1000

func main() {
	var field [N][N]int

	vents := getInput()
	for _, l := range vents {
		for _, f := range l.Covers() {
			field[f.x][f.y]++
		}
	}

	//number of points where 2 or more lines overlap
	points := 0
	for i := 0; i < N; i++ {
		for j := 0; j < N; j++ {
			if field[i][j] > 1 {
				points++
			}
		}
	}
	fmt.Println(points)
}
