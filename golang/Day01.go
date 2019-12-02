package main

/*  Advent of Code 2019 - Day 1 - https://adventofcode.com/2019/day/1

    This is my first Go program!  It's overkill for the problem at hand but it gives me an
    opportunity to learn Go's channels and start getting used to having multiple
    concurrent tasks running.

    The original file I started with was someone else's Go solution to today's problem:
    https://github.com/davidaayers/advent-of-code-2019/blob/master/day01/day01.go

    It is now overhauled to use channels and is completely different from the original
    file, but I owe thanks to David Ayers for giving me a starting point.

*/

import (
  "fmt"
  "io/ioutil"
  "strconv"
  "strings"
  "time"
)


func main() {

  // set up our channels
  masses1 := make (chan int, 100)  // 100 because it seems cool
  masses2 := make (chan int, 100)
  part1   := make (chan int)
  part2   := make (chan int)

  // kick off our goroutines that all run in parallel
  go parse("../inputs/1.input", masses1, masses2)
  go process(1, masses1, part1)
  go process(2, masses2, part2)

  // print the results as soon as they are ready
  fmt.Println("Part 1", <-part1)
  fmt.Println("Part 2", <-part2)
}


func process(part int, mass chan int, result chan int) {

  // this is local to this invocation of process, so two separate totals will be
  // calculated as numbers are read from the file
  var total int

  go calculate(&total, part, mass)
  go monitor  (&total, result)
}


// the main calculator listens on a mass channel, calculating the fuel required every
// time a number comes in, adding it to a total, and optionally causing the new fuel
// amount to need fuel too
func calculate(total *int, part int, mass chan int) {

  for {

    // wait for a number on the mass channel
    m := <-mass

    fuelNeeded := (m / 3) - 2

    if fuelNeeded > 0 {
      *total += fuelNeeded

      // in part 2 we have to account for this fuel's mass too, so--and here's the whole
      // point of doing this in go--we send the new amount back into the mass channel to
      // be processed at some point later
      if part == 2 {
        mass <- fuelNeeded
      }
    }
  }
}


// check a running total every so often.  if it hasn't changed since the last check, we
// consider the calculation done and send the answer over the result channel
func monitor(total *int, result chan int) {

  last := -1
  wait := time.Millisecond * 500    // twice a second

  for {
    if *total == last {
      result <- *total
      return
    } else {
      last = *total
      time.Sleep(wait)
    }
  }
}


// read the file's lines one at a time. convert each to an integer and send it over
// the two mass channels
func parse(filename string, masses1 chan int, masses2 chan int) {

  bytes, _ := ioutil.ReadFile(filename)

  for _, s := range strings.Split(string(bytes), "\n") {

    // convert to int
    mass, _ := strconv.Atoi(s)

    if mass > 0 {
      masses1 <- mass
      masses2 <- mass
    }
  }
}


/*
  jason@ubuntu18d:~/aoc2019/golang$ go run Day01.go
  Part 1 3335787
  Part 2 5000812
*/

