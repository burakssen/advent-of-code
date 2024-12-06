import { readFileSync, existsSync } from "fs";

interface OrderingRule {
  before: number;
  after: number;
}

const isOrdered = (update: number[], rules: OrderingRule[]): boolean =>
  rules
    .filter(
      (rule) => update.includes(rule.before) && update.includes(rule.after)
    )
    .every((rule) => update.indexOf(rule.before) < update.indexOf(rule.after));

const orderUpdate = (update: number[], rules: OrderingRule[]): number[] => {
  const orderedUpdate = [...update];

  while (true) {
    const violatingRuleIndex = rules.findIndex(
      (rule) =>
        orderedUpdate.includes(rule.before) &&
        orderedUpdate.includes(rule.after) &&
        orderedUpdate.indexOf(rule.before) > orderedUpdate.indexOf(rule.after)
    );

    if (violatingRuleIndex === -1) break;

    const rule = rules[violatingRuleIndex];
    const beforeIndex = orderedUpdate.indexOf(rule.before);
    const afterIndex = orderedUpdate.indexOf(rule.after);

    [orderedUpdate[beforeIndex], orderedUpdate[afterIndex]] = [
      orderedUpdate[afterIndex],
      orderedUpdate[beforeIndex],
    ];
  }

  return orderedUpdate;
};

const main = (): void => {
  const [filename] = process.argv.slice(2);

  if (!filename || !existsSync(filename)) {
    console.error(
      filename
        ? `File not found: ${filename}`
        : "Usage: node main.js <input_file>"
    );
    process.exit(1);
  }

  const content = readFileSync(filename, "utf-8").trim().split("\n");
  const splitIndex = content.findIndex((line) => line.trim() === "");

  const rules: OrderingRule[] = content.slice(0, splitIndex).map((line) => {
    const [before, after] = line.split("|").map(Number);
    return { before, after };
  });

  const updates: number[][] = content
    .slice(splitIndex + 1)
    .map((line) => line.split(",").map(Number));

  const middlePageSum = updates
    .filter((update) => isOrdered(update, rules))
    .reduce((sum, update) => sum + update[Math.floor(update.length / 2)], 0);

  const incorrectOrderMiddlePageSum = updates
    .filter((update) => !isOrdered(update, rules))
    .reduce(
      (sum, update) =>
        sum + orderUpdate(update, rules)[Math.floor(update.length / 2)],
      0
    );

  console.log("Part 1:", middlePageSum);
  console.log("Part 2:", incorrectOrderMiddlePageSum);
};

main();
