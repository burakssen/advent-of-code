<?php

function main() {
    $args = $_SERVER['argv'];

    if (count($args) < 2) {
        echo "Usage: php main.php <input_file>\n";
        exit(1);
    }

    $input_file = $args[1];

    if (!file_exists($input_file)) {
        echo "File not found: $input_file\n";
        exit(1);
    }

    $input = file_get_contents($input_file);

    // split input by lines
    $lines = explode("\n", $input);
    $paper_size = 0;
    $ribbon_length = 0;
    foreach ($lines as $line) {
        // split line by x
        $parts = explode("x", $line);
        if (count($parts) != 3) {
            echo "Invalid input: $line\n";
            continue;
        }

        $l = intval($parts[0]);
        $w = intval($parts[1]);
        $h = intval($parts[2]);

        $sides = [$l, $w, $h];
        sort($sides);

        $ribbon_length += 2 * $sides[0] + 2 * $sides[1] + $l * $w * $h;


        $area = 2 * $l * $w + 2 * $w * $h + 2 * $h * $l;
        $extra = min($l * $w, $w * $h, $h * $l);

        $paper_size += $area + $extra;
    }
    printf("Part 1: %d\n", $paper_size);
    printf("Part 2: %d\n", $ribbon_length);
}

main();