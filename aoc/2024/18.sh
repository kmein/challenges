#!/bin/sh

if test "$AOC_TEST"; then
  inputfile=18.txt.test
  gridsize=7
  read_bytes=12
else
  inputfile=18.txt
  gridsize=71

  # for part 2, play with this number until the script fails.
  # then subtract 1 and do do `sed -n NUMBERp 18.txt`
  read_bytes=1024
fi

head -n "$read_bytes" "$inputfile" \
  | awk -v gridsize="$gridsize" '
      BEGIN {
        FS=","
      }
      {
        grid[$1][$2] = 1
      }
      END {
        for (x = 0; x < gridsize; x++) {
          for (y = 0; y < gridsize; y++) {
            if (grid[x][y]) {
              printf "#"
            } else {
              printf "."
            }
          }
          printf "\n"
        }
      }' \
  | python3 -c '
import networkx as nx
import sys
grid = sys.stdin.read()
G = nx.Graph()
for y, row in enumerate(grid.splitlines()):
    for x, cell in enumerate(row):
        if cell != "#":
            G.add_node((x, y))
            if x > 0 and row[x-1] != "#":
                G.add_edge((x-1, y), (x, y))
            if x < len(row) - 1 and row[x+1] != "#":
                G.add_edge((x+1, y), (x, y))
            if y > 0 and grid.splitlines()[y-1][x] != "#":
                G.add_edge((x, y-1), (x, y))
            if y < len(grid.splitlines()) - 1 and grid.splitlines()[y+1][x] != "#":
                G.add_edge((x, y+1), (x, y))
start = (0, 0)
end = (len(row) - 1, len(grid.splitlines()) - 1)
shortest_path_length = nx.shortest_path_length(G, source=start, target=end)

print(f"The shortest path length is: {shortest_path_length}")'
