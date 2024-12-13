import 'dart:io';
import 'dart:core';

final buttonRegex = RegExp(r'Button A: X\+(\d+), Y\+(\d+)\nButton B: X\+(\d+), Y\+(\d+)\nPrize: X=(\d+), Y=(\d+)');

void main() async {
  final filePath = (Platform.environment['AOC_TEST'] != null) ? '13.txt.test' : '13.txt';

  try {
    final contents = await File(filePath).readAsString();
    final blocks = contents.split('\n\n');
    int totalCost1 = 0;
    int totalCost2 = 0;

    for (int i = 0; i < blocks.length; i++) {
      final block = blocks[i].trim();
      if (block.isNotEmpty) {
        totalCost1 += processBlock(block, i + 1, 0);
        totalCost2 += processBlock(block, i + 1, 10000000000000);
      }
    }

    print("Total cost (part 1): $totalCost1");
    print("Total cost (part 2): $totalCost2");
  } catch (e) {
    print('Error reading file: $e');
  }
}

int processBlock(String block, int setIndex, int coordinateOffset) {
  final match = buttonRegex.firstMatch(block);

  if (match != null) {
    final buttonAX = int.parse(match.group(1)!);
    final buttonAY = int.parse(match.group(2)!);
    final buttonBX = int.parse(match.group(3)!);
    final buttonBY = int.parse(match.group(4)!);
    final prizeX = int.parse(match.group(5)!) + coordinateOffset;
    final prizeY = int.parse(match.group(6)!) + coordinateOffset;

    return calculateCost(buttonAX, buttonAY, buttonBX, buttonBY, prizeX, prizeY);
  } else {
    print('No match found for block $setIndex');
    return 0;
  }
}

int calculateCost(int buttonAX, int buttonAY, int buttonBX, int buttonBY, int prizeX, int prizeY) {
  int determinant = buttonAX * buttonBY - buttonAY * buttonBX;

  if (determinant == 0) {
    print('The system has no unique solution (determinant is zero).');
    return 0;
  }

  final det1 = prizeX * buttonBY - prizeY * buttonBX;
  final det2 = buttonAX * prizeY - buttonAY * prizeX;

  if (det1 % determinant == 0 && det2 % determinant == 0) {
    final aPresses = det1 ~/ determinant;
    final bPresses = det2 ~/ determinant;
    final cost = 3 * aPresses + bPresses;
    print("Need to press A $aPresses times and B $bPresses times. Cost $cost");
    return cost;
  } else {
    print("The system has no integer solution.");
    return 0;
  }
}
