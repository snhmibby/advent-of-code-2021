package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type Card [5][5]int

func croak(e error, msg string) {
	if e != nil {
		log.Fatalf("%s: %v\n", msg, e)
	}
}

func (c *Card) Score(marked []bool, last int) int {
	sum := 0
	for i := 0; i < 5; i++ {
		for j := 0; j < 5; j++ {
			if !marked[c[i][j]] {
				sum += c[i][j]
			}
		}
	}
	return sum * last
}

func (c *Card) Bingo(marked []bool) bool {
	//check rows
	for i := 0; i < 5; i++ {
		bingo := true
		for j := 0; j < 5; j++ {
			bingo = bingo && marked[c[i][j]]
		}
		if bingo {
			return true
		}
	}

	//check columns
	for i := 0; i < 5; i++ {
		bingo := true
		for j := 0; j < 5; j++ {
			bingo = bingo && marked[c[j][i]]
		}
		if bingo {
			return true
		}
	}
	return false
}

func (c *Card) Dump() {
	for i := 0; i < 5; i++ {
		fmt.Println(c[i])
	}
}

// return:
// - winning sum of bingo
// - #round of bingo (lower is better)
func (c *Card) Check(numbers []int) (int, int) {
	marked := make([]bool, 100)
	for i, v := range numbers {
		marked[v] = true
		if c.Bingo(marked) {
			return c.Score(marked, v), i
		}
	}
	panic("no bingo")
}

func map_atoi(a []string) []int {
	result := make([]int, len(a))
	for i, v := range a {
		result[i], _ = strconv.Atoi(v)
	}
	return result
}

func getNumbers(s string) []int {
	return map_atoi(strings.Split(s, ","))
}

func scanLine(a *[5]int, s string) bool {
	n, _ := fmt.Sscanf(s, "%d%d%d%d%d", &a[0], &a[1], &a[2], &a[3], &a[4])
	return n == 5
}

func main() {
	file, err := os.Open("input")
	croak(err, "Cannot open file 'input'")
	scanner := bufio.NewScanner(file)

	/* get numbers from the first line */
	scanner.Scan()
	numbers := getNumbers(scanner.Text())

	card := new(Card)
	row := 0
	bestnum := 100
	bestscore := 0
	for scanner.Scan() {
		if scanLine(&card[row], scanner.Text()) {
			row++
			if row == 5 {
				score, num := card.Check(numbers)
				if num < bestnum {
					bestnum = num
					bestscore = score
				}
				row = 0
			}
		}
	}
	fmt.Println(bestscore)
}
