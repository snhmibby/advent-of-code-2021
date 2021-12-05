package main

import (
	"bufio"
	"fmt"
	"image"
	"log"
	"math"
	"os"
	"strings"
)

func croak(e error, msg string) {
	if e != nil {
		fmt.Fprintf(os.Stderr, "%s (%v).", msg, e)
		os.Exit(1)
	}
}

type Line struct {
	from, to image.Point
}

func (l *Line) Direction() image.Point {
	v := l.to.Sub(l.from)
	x := float64(v.X)
	y := float64(v.Y)
	dist := math.Sqrt(x*x + y*y)
	dir := image.Pt(int(math.Round(x/dist)), int(math.Round(y/dist)))
	return dir
}

func (l *Line) Covers() []image.Point {
	covers := []image.Point{}
	step := l.Direction()
	p := l.from
	for {
		covers = append(covers, p)
		if p == l.to {
			break
		}
		p = p.Add(step)
	}
	return covers
}

func parseLocation(s string) image.Point {
	var l image.Point
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

func getInput() []Line {
	file, err := os.Open("input")
	croak(err, "Cannot open file 'input'")
	scanner := bufio.NewScanner(file)
	var lines []Line
	for scanner.Scan() {
		l := parseLine(scanner.Text())
		lines = append(lines, l)
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
