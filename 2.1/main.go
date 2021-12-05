package main

import (
	"bufio"
	"fmt"
	"os"
)

type Cmd struct {
	cmd string
	val int
}

func croak(e error, msg string) {
	if e != nil {
		fmt.Fprintf(os.Stderr, "%s (%v).", msg, e)
		os.Exit(1)
	}
}

func getInput() []Cmd {
	file, err := os.Open("input")
	croak(err, "Cannot open file 'input'")
	scanner := bufio.NewScanner(file)
	var cmd []Cmd
	for scanner.Scan() {
		var c Cmd
		n, _ := fmt.Sscanf(scanner.Text(), "%s %d", &c.cmd, &c.val)
		if 2 == n {
			cmd = append(cmd, c)
		}
	}
	return cmd
}

func main() {
	in := getInput()
	var pos, depth int
	for _, c := range in {
		switch c.cmd {
		case "forward":
			pos += c.val
		case "down":
			depth += c.val
		case "up":
			depth -= c.val
		}
	}
	fmt.Print(pos * depth)
}
