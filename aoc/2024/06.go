package main

import (
  "bufio"
  "fmt"
  "os"
)

type Position struct { row, column int }
type Direction struct { dRow, dColumn int }
type Guard struct {
  Position Position
  StartingPosition Position
  Direction Direction
}

type Grid struct {
  Obstructions map[Position]struct{}
  Guard Guard
  Bounds struct {
    Rows, Columns int
  }
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

    for column, char := range line {
      position := Position{row: row, column: column}

      if char == '#' {
        grid.Obstructions[position] = struct{}{}
      } else if char == '^' {
        grid.Guard.Position = position
        grid.Guard.StartingPosition = position
        grid.Guard.Direction = Up
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

var (
  Left Direction = Direction{dRow: 0, dColumn: -1}
  Right = Direction{dRow: 0, dColumn: 1}
  Down = Direction{dRow: +1, dColumn: 0}
  Up = Direction{dRow: -1, dColumn: 0}
)

func turnRight(direction Direction) Direction {
  if direction == Left {
    return Up
  } else if direction == Up {
    return Right
  } else if direction == Right {
    return Down
  } else if direction == Down {
    return Left
  }
  return Up
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
    grid.Guard.Direction = turnRight(grid.Guard.Direction)
  } else {
    grid.Guard.Position = nextPos
    visited[nextPos] = struct{}{}
  }
  return true
}

func printGridVisited(grid Grid, visited map[Position]struct{}) {
  for row := 0; row < grid.Bounds.Rows; row++ {
    fmt.Printf("%3d ", row)
    for column := 0; column < grid.Bounds.Columns; column++ {
      position := Position{column: column, row: row}
      if _, ok := visited[position]; ok {
        fmt.Printf("X")
      } else if _, ok := grid.Obstructions[position]; ok  {
        fmt.Printf("#")
      } else {
        fmt.Printf(".")
      }
    }
    fmt.Println()
  }
  fmt.Println()
}

func runGrid(grid Grid) (map[Position]struct{}, bool) {
  visited := make(map[Position]struct{})
  for i := 0; i <= grid.Bounds.Rows * grid.Bounds.Columns; i++ {
    if !moveGuard(&grid, visited) {
      return visited, false
    }
  }
  return visited, true
}

func main() {
  grid, err := readGrid()
  if err != nil {
    fmt.Println("Error:", err)
    return
  }

  visited, _ := runGrid(grid)

  fmt.Printf("Unique positions visited: %d\n", len(visited))

  obstructionPositions := 0

  for pathPosition := range visited {
    if pathPosition != grid.Guard.StartingPosition {
      newGrid := Grid {
        Obstructions: make(map[Position]struct{}),
        Guard: Guard{
          StartingPosition: grid.Guard.StartingPosition,
          Position: grid.Guard.StartingPosition,
          Direction: Up,
        },
        Bounds: grid.Bounds,
      }

      for oldObstruction := range grid.Obstructions {
        newGrid.Obstructions[oldObstruction] = struct{}{}
      }
      newGrid.Obstructions[pathPosition] = struct{}{}

      _, loop := runGrid(newGrid)
      if loop {
        obstructionPositions++
      }
    }
  }

  fmt.Printf("Possible loop-causing obstructions: %d\n", obstructionPositions)
}
