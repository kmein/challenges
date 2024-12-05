import toposort from "npm:toposort";

type Rule = [number, number];

type Update = number[];

type Input = {
  rules: Rule[];
  updates: Update[];
};

function getInput(): Input {
  const text = Deno.readTextFileSync(
    Deno.env.get("AOC_TEST") ? "05.txt.test" : "05.txt",
  );
  const [block1, block2] = text.split("\n\n");
  return {
    rules: block1.split("\n").map((line) => {
      const [prior, posterior] = line.split("|").map((x) => parseInt(x));
      return [prior, posterior];
    }),
    updates: block2.split("\n").filter(Boolean).map((line) =>
      line.split(",").map((x) => parseInt(x))
    ),
  };
}

function sortUpdate(rules: Rule[], update: Update): Update {
  return toposort(
    rules.filter((r) => update.includes(r[0]) && update.includes(r[1])),
  );
}

function sortedCorrectly(rules: Rule[], update: Update): boolean {
  const sortOrder = sortUpdate(rules, update);
  return JSON.stringify(sortOrder) == JSON.stringify(update);
}

function middleElement(update: Update): number {
  return update[Math.floor(update.length / 2)];
}

const input = getInput();

console.log(
  input.updates.filter((u) => sortedCorrectly(input.rules, u)).map(
    middleElement,
  ).reduce((x, y) => x + y),
);

console.log(
  input.updates.filter((u) => !sortedCorrectly(input.rules, u)).map((u) =>
    sortUpdate(input.rules, u)
  ).map(
    middleElement,
  ).reduce((x, y) => x + y),
);
