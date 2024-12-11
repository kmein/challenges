<?php
// stolen from https://www.reddit.com/r/adventofcode/comments/1hbm0al/comment/m1idchh/
function blink($blinks, $numbers) {
  $hash = array_count_values($numbers);
  for ($i = 1; $i <= $blinks; $i++) {
    $new = [];
    foreach ($hash as $key => $value) {
      if ($key == 0) {
        $new[1] = ($new[1] ?? 0) + $value;
      } elseif (strlen($key) % 2 == 0) {
        $len = strlen($key) / 2;
        $left = (int)substr($key, 0, $len);
        $right = (int)substr($key, $len);
        $new[$left] = ($new[$left] ?? 0) + $value;
        $new[$right] = ($new[$right] ?? 0) + $value;
      } else {
        $new[$key * 2024] = ($new[$key * 2024] ?? 0) + $value;
      }
    }
    $hash = $new;
  }
  return array_sum($hash);
}

$filename = getenv('AOC_TEST') ? '11.txt.test' : '11.txt';
$content = file_get_contents($filename);
$numbers = array_map('intval', explode(' ', $content));

$result = blink(25, $numbers);
echo "$result\n";
$result = blink(75, $numbers);
echo "$result\n";
?>
