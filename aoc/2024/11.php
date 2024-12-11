<?php
function splitNumber($number) {
  $numberStr = (string)$number;
  $midpoint = (int)(strlen($numberStr) / 2);
  $leftHalf = substr($numberStr, 0, $midpoint);
  $rightHalf = substr($numberStr, $midpoint);
  return [intval($leftHalf), intval($rightHalf)];
}

function blink(array $numbers) {
  $return = array();
  foreach ($numbers as $number) {
    if ($number == 0) {
      array_push($return, 1);
    } elseif (strlen((string)$number) % 2 == 0) {
      $halves = splitNumber($number);
      array_push($return, ...$halves);
    } else {
      array_push($return, $number * 2024);
    }
  }
  return $return;
}

function blinkN(array $numbers, int $n) {
  $return = $numbers;
  for ($i = 0; $i < $n; $i++) {
    echo "blink " . $i . PHP_EOL;
    $return = blink($return);
  }
  return $return;
}

ini_set('memory_limit', '-1');

$filename = getenv('AOC_TEST') ? '11.txt.test' : '11.txt';

if (!file_exists($filename)) {
  die("File not found: $filename");
}

$content = file_get_contents($filename);

$numbers = array_map('intval', explode(' ', $content));

echo count(blinkN($numbers, 25)) . PHP_EOL;
echo count(blinkN($numbers, 75)) . PHP_EOL;
?>
