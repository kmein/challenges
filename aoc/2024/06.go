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

type move int

const (
    Ok move = iota
    OutOfBounds
    Loop
)

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

    // fmt.Printf("%d %s\n", row, line);

    for column, char := range line {
      position := Position{row: row, column: column}

      if char == '#' {
        // fmt.Printf("obstruction registered: %+v\n", position)
        grid.Obstructions[position] = struct{}{}
      } else if char == '^' {
        grid.Guard.Position = position
        grid.Guard.StartingPosition = position
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

func moveGuard(grid *Grid, visited map[Position]Direction) move {
  currentPos := grid.Guard.Position
  visited[currentPos] = grid.Guard.Direction

  nextPos := Position{
    row: currentPos.row + grid.Guard.Direction.dRow,
    column: currentPos.column + grid.Guard.Direction.dColumn,
  }

  if nextPos.row < 0 || nextPos.row >= grid.Bounds.Rows || nextPos.column < 0 || nextPos.column >= grid.Bounds.Columns {
    return OutOfBounds
  }

  if _, ok := grid.Obstructions[nextPos]; ok {
    grid.Guard.Direction = directions[(getDirectionIndex(grid.Guard.Direction) + 1) % 4]
  } else if dir, ok := visited[nextPos]; ok && dir == grid.Guard.Direction {
    return Loop
  } else {
    grid.Guard.Position = nextPos
    visited[nextPos] = grid.Guard.Direction
  }
  return Ok
}

func getDirectionIndex(direction Direction) int {
  for i, d := range directions {
    if d == direction {
      return i
    }
  }
  return -1
}

func printGridVisited(grid Grid, visited map[Position]Direction) {
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

func main() {
  grid, err := readGrid()
  if err != nil {
    fmt.Println("Error:", err)
    return
  }

  visited := make(map[Position]Direction)
  for {
    if x := moveGuard(&grid, visited); x != Ok {
      fmt.Printf("%+v\n", x)
      break
    }
  }

  printGridVisited(grid, visited)

  fmt.Printf("Unique positions visited: %d\n", len(visited))

  obstructionPositions := 0

  for pathPosition := range visited {
    if pathPosition != grid.Guard.StartingPosition {
      newGrid := Grid {
        Obstructions: make(map[Position]struct{}),
        Guard: Guard{
          StartingPosition: grid.Guard.StartingPosition,
          Position: grid.Guard.StartingPosition,
          Direction: Direction{dRow: -1, dColumn: 0}, // Up
        },
        Bounds: grid.Bounds,
      }

      // insert new obstruction
      for oldObstruction := range grid.Obstructions {
        newGrid.Obstructions[oldObstruction] = struct{}{}
      }
      newGrid.Obstructions[pathPosition] = struct{}{}
      fmt.Printf("inserted obstruction at %+v\n", pathPosition)

      newVisited := make(map[Position]Direction)

      for {
        if x := moveGuard(&newGrid, newVisited); x != Ok {
          if x == Loop {
            fmt.Printf("inserting obstruction at %+v lead to loop of %d\n", pathPosition, len(newVisited))
            printGridVisited(newGrid, newVisited)
            obstructionPositions++
          }
          break
        }
      }
    }
  }

  fmt.Printf("Obstructions could be placed at %d positions", obstructionPositions)
}
