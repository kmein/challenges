import os, sequtils
import std/algorithm

type
  Grid[T] = seq[seq[T]]

proc getGrid(): Grid[char] =
  let fileName =
    if getEnv("AOC_TEST") != "":
      "04.txt.test"
    else:
      "04.txt"
  var result: seq[seq[char]] = @[]
  try:
    for line in lines(fileName):
      result.add(line.toSeq())
  except OSError as e:
    echo "Error reading file: ", e.msg
  return result

# part 1

proc transpose[T](grid: Grid[T]): Grid[T] =
  var transposed: Grid[T] = @[]
  for i in 0 ..< grid[0].len:
    transposed.add(@[])
  for i in 0 ..< grid.len:
    for j in 0 ..< grid[i].len:
      transposed[j].add(grid[i][j])
  return transposed

proc getDiagonals[T](grid: Grid[T]): seq[seq[T]] =
  var diagonals: seq[seq[T]] = @[]

  let rows = grid.len
  let cols = if rows > 0: grid[0].len else: 0

  # from top-left to bottom-right
  for startCol in 0 ..< cols:
    var diagonal: seq[T] = @[]
    var x = 0
    var y = startCol
    while x < rows and y < cols:
      diagonal.add(grid[x][y])
      x += 1
      y += 1
    diagonals.add(diagonal)

  for startRow in 1 ..< rows:
    var diagonal: seq[T] = @[]
    var x = startRow
    var y = 0
    while x < rows and y < cols:
      diagonal.add(grid[x][y])
      x += 1
      y += 1
    diagonals.add(diagonal)

  # top-right to bottom-left
  for startCol in 0 ..< cols:
    var diagonal: seq[T] = @[]
    var x = 0
    var y = startCol
    while x < rows and y >= 0:
      diagonal.add(grid[x][y])
      x += 1
      y -= 1
    diagonals.add(diagonal)

  for startRow in 1 ..< rows:
    var diagonal: seq[T] = @[]
    var x = startRow
    var y = cols - 1
    while x < rows and y >= 0:
      diagonal.add(grid[x][y])
      x += 1
      y -= 1
    diagonals.add(diagonal)

  return diagonals

proc getSearchSpace[T](grid: Grid[T]): seq[seq[T]] =
  var searchSpace: seq[seq[T]] = @[]
  for row in grid:
    searchSpace.add(row)
    searchSpace.add(row.reversed)
  for row in grid.transpose:
    searchSpace.add(row)
    searchSpace.add(row.reversed)
  for row in grid.getDiagonals:
    searchSpace.add(row)
    searchSpace.add(row.reversed)
  return searchSpace

proc countSubsequence[T](needle: seq[T], haystack: seq[T]): int =
  if needle.len == 0 or haystack.len < needle.len:
    return 0
  var count = 0
  for i in 0 ..< (haystack.len - needle.len + 1):
    if haystack[i ..< i + needle.len] == needle:
      count += 1
  return count

let grid = getGrid()
let searchSpace = getSearchSpace(grid)
var count = 0
for row in searchSpace:
  count += countSubsequence(@['X', 'M', 'A', 'S'], row)
echo count

# part 2

proc get3x3Subgrids[T](grid: Grid[T]): seq[Grid[T]] =
  var subgrids: seq[Grid[T]] = @[]
  for i in 0 ..< (grid.len - 3 + 1):
    for j in 0 ..< (grid[i].len - 3 + 1):
      var subgrid: Grid[T] = @[]
      for di in 0 ..< 3:
        subgrid.add(@[])
        for dj in 0 ..< 3:
          subgrid[di].add(grid[i + di][j + dj])
      subgrids.add(subgrid)
  return subgrids

proc isCrossedMAS(subgrid3x3: Grid[char]): bool =
  var isXMAS = true
  let needle = @['M', 'A', 'S']
  for diagonal in subgrid3x3.getDiagonals:
    if diagonal.len == 3:
      isXMAS = isXMAS and ((diagonal == needle or diagonal.reversed == needle))
  return isXMAS

var count2 = 0
for subgrid in get3x3Subgrids(grid):
  if isCrossedMAS(subgrid):
    count2 += 1
echo count2
