package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

const N = 5

type Card [N][N]int

func croak(e error, msg string) {
	if e != nil {
		log.Fatalf("%s: %v\n", msg, e)
	}
}

func (c *Card) Score(marked []bool, last int) int {
	sum := 0
	for i := 0; i < N; i++ {
		for j := 0; j < N; j++ {
			if !marked[c[i][j]] {
				sum += c[i][j]
			}
		}
	}
	return sum * last
}

func (c *Card) Bingo(marked []bool) bool {
	//check rows
	for i := 0; i < N; i++ {
		bingo := true
		for j := 0; j < N; j++ {
			bingo = bingo && marked[c[i][j]]
		}
		if bingo {
			return true
		}
	}

	//check columns
	for i := 0; i < N; i++ {
		bingo := true
		for j := 0; j < N; j++ {
			bingo = bingo && marked[c[j][i]]
		}
		if bingo {
			return true
		}
	}
	return false
}

func (c *Card) Dump() {
	for i := 0; i < N; i++ {
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

//it doesn't work without the pointers...
//Apparently arrays are also call by value??
func scanLine(a *[N]int, s string) bool {
	n, _ := fmt.Sscanf(s, "%d %d %d %d %d", &a[0], &a[1], &a[2], &a[3], &a[4])
	return n == N
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
			if row == N {
				score, num := card.Check(numbers)
				//fmt.Println(num, score)
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
