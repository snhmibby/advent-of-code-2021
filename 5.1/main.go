package main

import (
	"bufio"
	"fmt"
	"image"
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

type Location image.Point

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
	if l.from.X == l.to.X {
		start := min(l.to.Y, l.from.Y)
		end := max(l.to.Y, l.from.Y)
		for i := start; i <= end; i++ {
			covers = append(covers, Location{X: l.from.X, Y: i})
		}
	}
	if l.from.Y == l.to.Y {
		start := min(l.to.X, l.from.X)
		end := max(l.to.X, l.from.X)
		for i := start; i <= end; i++ {
			covers = append(covers, Location{X: i, Y: l.from.Y})
		}
	}
	return covers
}

func parseLocation(s string) Location {
	var l Location
	n, _ := fmt.Sscanf(s, "%d,%d", &l.X, &l.Y)
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
	return l.from.X == l.to.X || l.from.Y == l.to.Y
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
			field[f.X][f.Y]++
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
