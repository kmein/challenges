package main

import (
  "bufio"
  "fmt"
  "os"
)

type Position struct { row, column int }
type Direction struct { dRow, dColumn int }

type Grid struct {
  Obstructions map[Position]struct{}
  Guard struct {
    Position Position
    Direction Direction
  }
  Bounds struct {
    Rows, Columns int
  }
}

var directions = []Direction{
  {dRow: 0, dColumn: 1},  // Right
  {dRow: +1, dColumn: 0},  // Down
  {dRow: 0, dColumn: -1}, // Left
  {dRow: -1, dColumn: 0}, // Up
}

func readGrid() (Grid, error) {
  var grid Grid
  var foundGuard bool
  var filename string

  if os.Getenv("AOC_TEST") != "" {
    filename = "06.txt.test"
  } else {
    filename = "06.txt"
  }

  file, err := os.Open(filename)
  if err != nil {
    return grid, err
  }
  defer file.Close()

  scanner := bufio.NewScanner(file)

  grid.Obstructions = make(map[Position]struct{})

  for row := 0; scanner.Scan(); row++ {
    line := scanner.Text()
    grid.Bounds.Rows++

    fmt.Printf("%d %s\n", row, line);

    for column, char := range line {
      position := Position{row: row, column: column}

      if char == '#' {
        fmt.Printf("obstruction registered: %+v\n", position)
        grid.Obstructions[position] = struct{}{}
      } else if char == '^' {
        grid.Guard.Position = position
        grid.Guard.Direction = Direction{dColumn: 0, dRow: -1} // upwards
        foundGuard = true
      }
    }

    if len(line) > grid.Bounds.Columns {
      grid.Bounds.Columns = len(line)
    }
  }

  if err := scanner.Err(); err != nil {
    return grid, err
  }

  if foundGuard {
    return grid, nil
  }
  return grid, fmt.Errorf("guard ('^') not found in the grid")
}

func moveGuard(grid *Grid, visited map[Position]struct{}) bool {
  currentPos := grid.Guard.Position
  visited[currentPos] = struct{}{}

  nextPos := Position{
    row: currentPos.row + grid.Guard.Direction.dRow,
    column: currentPos.column + grid.Guard.Direction.dColumn,
  }

  if nextPos.row < 0 || nextPos.row >= grid.Bounds.Rows || nextPos.column < 0 || nextPos.column >= grid.Bounds.Columns {
    return false
  }

  if _, ok := grid.Obstructions[nextPos]; ok {
    fmt.Printf("%d,%d was obstructed\n", nextPos.row, nextPos.column)
    grid.Guard.Direction = directions[(getDirectionIndex(grid.Guard.Direction) + 1) % 4]
  } else {
    grid.Guard.Position = nextPos
    visited[nextPos] = struct{}{}
  }
  return true
}

func getDirectionIndex(direction Direction) int {
  for i, d := range directions {
    if d == direction {
      return i
    }
  }
  return -1
}

func printGridVisited(grid Grid, visited map[Position]struct{}) {
  for row := 0; row < grid.Bounds.Rows; row++ {
    fmt.Printf("%3d ", row)
    for column := 0; column < grid.Bounds.Columns; column++ {
      position := Position{column: column, row: row}
      if _, ok := visited[position]; ok {
        fmt.Printf("X")
      } else if _, ok := grid.Obstructions[position]; ok {
        fmt.Printf("#")
      } else {
        fmt.Printf(".")
      }
    }
    fmt.Printf("\n")
  }
}

func main() {
  grid, err := readGrid()
  if err != nil {
    fmt.Println("Error:", err)
    return
  }

  visited := make(map[Position]struct{})
  for {
    if !moveGuard(&grid, visited) {
      break
    }
  }

  printGridVisited(grid, visited)

  fmt.Printf("Unique positions visited: %d\n", len(visited))
}
