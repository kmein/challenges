use std::collections::HashMap;
use std::collections::HashSet;
use std::env;
use std::fs::read_to_string;

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn add(&self, other: &Point) -> Point {
        Point {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
    fn subtract(&self, other: &Point) -> Point {
        Point {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }

    fn in_bounds(&self, other: &Point) -> bool {
        self.x >= 0 && self.y >= 0 && self.x < other.x && self.y < other.y
    }
}

struct Field {
    points: HashMap<Point, char>,
    bounds: Point,
}

impl Field {
    fn read_input() -> Self {
        let file_name = if env::var("AOC_TEST").is_ok() {
            "08.txt.test"
        } else {
            "08.txt"
        };
        let lines: Vec<String> = read_to_string(file_name)
            .unwrap()
            .lines()
            .map(String::from)
            .collect();

        let points = lines
            .iter()
            .enumerate()
            .flat_map(|(y, line)| {
                line.chars().enumerate().filter_map(move |(x, c)| {
                    if c != '.' {
                        Some((
                            Point {
                                x: x as i32,
                                y: y as i32,
                            },
                            c,
                        ))
                    } else {
                        None
                    }
                })
            })
            .collect();

        let bounds = Point {
            y: lines.len() as i32,
            x: lines.get(0).map_or(0, |line| line.len()) as i32,
        };
        Self { points, bounds }
    }

    fn antinodes(&self, extended: bool) -> HashSet<Point> {
        let mut result = HashSet::new();
        let points = &self.points;
        for (point_a, label_a) in points.into_iter() {
            for (point_b, label_b) in points.into_iter() {
                if point_a != point_b && label_a == label_b {
                    let distance = point_a.subtract(&point_b);
                    let mut plus = point_a.add(&distance);
                    let mut minus = point_a.subtract(&distance);

                    if extended {
                        while plus.in_bounds(&self.bounds) {
                            result.insert(plus);
                            plus = plus.add(&distance);
                        }
                        while minus.in_bounds(&self.bounds) {
                            result.insert(minus);
                            minus = minus.subtract(&distance);
                        }
                    } else {
                        if plus.in_bounds(&self.bounds) && self.points.get(&plus) != Some(label_a) {
                            result.insert(plus);
                        }
                        if minus.in_bounds(&self.bounds) && self.points.get(&minus) != Some(label_a)
                        {
                            result.insert(minus);
                        }
                    }
                }
            }
        }
        result
    }
}

fn print_field_antinodes(field: &Field, antinodes: &HashSet<Point>) {
    for y in 0..field.bounds.y {
        for x in 0..field.bounds.x {
            let point = Point { x, y };
            if let Some(c) = field.points.get(&point) {
                print!("{}", c);
            } else if antinodes.contains(&point) {
                print!("#");
            } else {
                print!(".");
            }
        }
        println!();
    }
}

fn main() {
    let field = Field::read_input();

    let antinodes_set = field.antinodes(false);
    print_field_antinodes(&field, &antinodes_set);
    println!("{} antinodes constructed", antinodes_set.len());

    let antinodes_set = field.antinodes(true);
    print_field_antinodes(&field, &antinodes_set);
    println!("{} antinodes constructed", antinodes_set.len());
}
