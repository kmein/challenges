import java.awt.Point;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class Aoc16 {
    static class State {
        Point position;
        int direction, score;

        State(Point position, int direction, int score) {
            this.position = position;
            this.direction = direction;
            this.score = score;
        }
    }

    private static final Point[] DIRECTIONS = {
        new Point(0, 1),   // East
        new Point(1, 0),   // South
        new Point(0, -1),  // West
        new Point(-1, 0)   // North
    };

    public static void main(String[] args) {
        String[] mazeInput = readMazeInput();
        if (mazeInput == null) {
            System.err.println("Failed to read maze input.");
            return;
        }

        Set<Point> bestPathTiles = new HashSet<>();
        int minScore = dijkstra(mazeInput, bestPathTiles);

        System.out.println("The lowest score a Reindeer could possibly get is: " + minScore);
        System.out.println("Number of tiles part of at least one of the best paths: " + bestPathTiles.size());
    }

    private static String[] readMazeInput() {
        String fileName = System.getenv("AOC_TEST") != null ? "16.txt.test" : "16.txt";
        try {
            List<String> lines = Files.readAllLines(Paths.get(fileName));
            return lines.toArray(new String[0]);
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }

    private static boolean isValidMove(String[] maze, Point p) {
        return p.x >= 0 && p.x < maze.length && p.y >= 0 && p.y < maze[0].length() && maze[p.x].charAt(p.y) != '#';
    }

    private static int dijkstra(String[] maze, Set<Point> bestPathTiles) {
        int rows = maze.length;
        int cols = maze[0].length();
        State start = null;
        Point end = null;

        // Parse the maze to find start (S) and end (E) positions
        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                if (maze[r].charAt(c) == 'S') {
                    start = new State(new Point(r, c), 0, 0); // Starting facing East (direction 0)
                } else if (maze[r].charAt(c) == 'E') {
                    end = new Point(r, c);
                }
            }
        }

        // Priority queue for Dijkstra's algorithm
        PriorityQueue<State> pq = new PriorityQueue<>(Comparator.comparingInt(s -> s.score));
        pq.add(start);
        boolean[][][] visited = new boolean[rows][cols][4]; // Visited states

        while (!pq.isEmpty()) {
            State current = pq.poll();

            // If we reached the end, return the score
            if (current.position.equals(end)) {
                return current.score;
            }

            // If already visited this state, skip it
            if (visited[current.position.x][current.position.y][current.direction]) {
                continue;
            }
            visited[current.position.x][current.position.y][current.direction] = true;

            bestPathTiles.add(current.position);

            // Move forward in the current direction
            Point move = DIRECTIONS[current.direction];
            Point newPosition = new Point(current.position.x + move.x, current.position.y + move.y);
            if (isValidMove(maze, newPosition)) {
                pq.add(new State(newPosition, current.direction, current.score + 1));
            }

            // Rotate left
            int newDirectionLeft = (current.direction + 3) % 4; // Rotate left
            pq.add(new State(current.position, newDirectionLeft, current.score + 1000));

            // Rotate right
            int newDirectionRight = (current.direction + 1) % 4; // Rotate right
            pq.add(new State(current.position, newDirectionRight, current.score + 1000));

        }
        return -1; // If no path is found
    }

}
