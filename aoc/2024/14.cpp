#include <cstdlib> // For std::getenv
#include <fstream>
#include <iostream>
#include <set>
#include <sstream>
#include <string>
#include <vector>

struct Robot {
  int p[2]; // Position (x, y)
  int v[2]; // Velocity (vx, vy)
};

std::vector<Robot> get_robots(std::string filename) {
  std::ifstream file(filename);
  std::string line;
  std::vector<Robot> robots;

  if (!file.is_open()) {
    std::cerr << "Error opening file: " << filename << std::endl;
    return robots;
  }

  while (std::getline(file, line)) {
    Robot robot;
    if (sscanf(line.c_str(), "p=%d,%d v=%d,%d", &robot.p[0], &robot.p[1], &robot.v[0], &robot.v[1]) == 4) {
      robots.push_back(robot);
    } else {
    }
  }

  file.close();

  return robots;
}


int rem(int a, int b) {
  int remainder = a % b;
  if (remainder != 0 && (a < 0) != (b < 0)) {
    remainder += b;
  }
  return remainder;
}


int wrap_around(int x, unsigned int max_x) {
  int modulus = rem(x, max_x);
  if (modulus < 0) return max_x + modulus;
  else return modulus;
}

Robot step(long seconds, Robot robot, unsigned int max_x, unsigned int max_y) {
  return {
    {
      wrap_around(robot.p[0] + robot.v[0] * seconds, max_x),
      wrap_around(robot.p[1] + robot.v[1] * seconds, max_y)
    }, {
      robot.v[0],
      robot.v[1]
    }
  };
}

unsigned int safety_factor(std::vector<Robot> robots, unsigned int max_x, unsigned int max_y) {
  unsigned int q1 = 0, q2 = 0, q3 = 0, q4 = 0;
  unsigned int mid_x = max_x / 2,
               mid_y = max_y / 2;

  for (const Robot &robot : robots) {
    if (robot.p[0] < mid_x && robot.p[1] < mid_y) q1++;
    else if (robot.p[0] > mid_x && robot.p[1] < mid_y) q2++;
    else if (robot.p[0] > mid_x && robot.p[1] > mid_y) q3++;
    else if (robot.p[0] < mid_x && robot.p[1] > mid_y) q4++;
    else {
      // Robot on the middle is ignored
    }
  }

  return q1 * q2 * q3 * q4;
}

void drawGrid(const std::vector<Robot>& robots, int max_x, int max_y) {
  std::vector<std::vector<int>> grid(max_y, std::vector<int>(max_x, 0));
  for (const auto& robot : robots) {
    int x = robot.p[0];
    int y = robot.p[1];
    if (x >= 0 && x < max_x && y >= 0 && y < max_y) {
      grid[y][x]++;
    }
  }

  for (int y = 0; y < max_y; ++y) {
    for (int x = 0; x < max_x; ++x) {
      if (grid[y][x] > 0) std::cout << grid[y][x];
      else std::cout << ".";
    }
    std::cout << std::endl;
  }
}

int main() {
  bool test_env = std::getenv("AOC_TEST") != nullptr;
  std::string filename = test_env ? "14.txt.test" : "14.txt";

  unsigned int max_x = test_env ? 11 : 101;
  unsigned int max_y = test_env ? 7 : 103;

  std::vector<Robot> robots = get_robots(filename);

  std::vector<Robot> new_robots;
  for (const auto &robot : robots) {
    Robot updated = step(100, robot, max_x, max_y);
    new_robots.push_back(updated);
  }

  std::cout << safety_factor(new_robots, max_x, max_y) << std::endl;

  if (!test_env) {
    for (int i = 0;; i++) {
      std::vector<Robot> new_robots;
      std::set<std::pair<int, int>> unique_positions;

      for (const auto &robot : robots) {
        Robot updated = step(i, robot, max_x, max_y);
        new_robots.push_back(updated);
        unique_positions.insert({updated.p[0], updated.p[1]});
      }

      if (new_robots.size() == unique_positions.size()) {
        drawGrid(new_robots, max_x, max_y);
        std::cout << i << std::endl;
        break;
      }
    }
  }

  return 0;
}
