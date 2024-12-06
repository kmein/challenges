package main

import (
  "bufio"
  "fmt"
  "os"
)

type Position struct { row, column int }
type Direction struct { dRow, dColumn int }

type Grid struct {
  Obstructions [][]bool
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
  for row := 0; scanner.Scan(); row++ {
    line := scanner.Text()
    grid.Bounds.Rows++

    fmt.Printf("%d %s\n", row, line);

    if len(grid.Obstructions) < grid.Bounds.Rows {
      grid.Obstructions = append(grid.Obstructions, make([]bool, len(line)))
    }

    for column, char := range line {
      if char == '#' {
        grid.Obstructions[row][column] = true
      } else if char == '^' {
        grid.Guard.Position = Position{row, column}
        grid.Guard.Direction = Direction{dColumn: 0, dRow: -1}
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
  printGridVisited(*grid, visited)
  fmt.Println()

  fmt.Printf("Direction: %+v\n", grid.Guard.Direction)

  currentPos := grid.Guard.Position
  nextPos := Position{
    row: currentPos.row + grid.Guard.Direction.dRow,
    column: currentPos.column + grid.Guard.Direction.dColumn,
  }

  if nextPos.row < 0 || nextPos.row >= grid.Bounds.Rows || nextPos.column < 0 || nextPos.column >= grid.Bounds.Columns {
    visited[currentPos] = struct{}{}
    return false
  }

  if grid.Obstructions[nextPos.row][nextPos.column] {
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
      if _, ok := visited[Position{column, row}]; ok {
        fmt.Printf("X")
      } else if grid.Obstructions[row][column] {
        fmt.Printf("#")
      } else {
        fmt.Printf(".")
      }
    }
    fmt.Printf("\n")
  }
}

func printGridInfo(grid Grid) {
  fmt.Println("Positions of obstructions ('#'):")
  for row := 0; row < grid.Bounds.Rows; row++ {
    for col := 0; col < grid.Bounds.Columns; col++ {
      if grid.Obstructions[row][col] {
        fmt.Printf("Row: %d, Col: %d\n", row, col)
      }
    }
  }

  fmt.Printf("\nPosition of guard ('^'): Row: %d, Col: %d\n", grid.Guard.Position.row, grid.Guard.Position.column)
  fmt.Printf("\nGrid Bounds: %d rows, %d columns\n", grid.Bounds.Rows, grid.Bounds.Columns)
}

func main() {
  grid, err := readGrid()
  if err != nil {
    fmt.Println("Error:", err)
    return
  }

  printGridInfo(grid)

  visited := make(map[Position]struct{})
  for {
    if !moveGuard(&grid, visited) {
      break
    }
  }

  printGridVisited(grid, visited)

  fmt.Printf("Unique positions visited: %d\n", len(visited))

}
