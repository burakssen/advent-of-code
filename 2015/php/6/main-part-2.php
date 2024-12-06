<?php

function get_action($line){
    $line = explode(" ", $line);
    if ($line[0] == "turn"){
        $action = $line[1];
        $start = explode(",", $line[2]);
        $end = explode(",", $line[4]);
        return array($action, $start, $end);
    } else {
        $action = $line[0];
        $start = explode(",", $line[1]);
        $end = explode(",", $line[3]);
        return array($action, $start, $end);
    }
}

function main() {
    $argv = $_SERVER['argv'];
    $argc = $_SERVER['argc'];

    if ($argc < 2) {
        echo "Usage: php main-part1.php <input.txt>\n";
        exit(1);
    }

    $inputFile = $argv[1];
    $input = file_get_contents($inputFile);
    if ($input === false) {
        echo "Failed to read input file: $inputFile\n";
        exit(1);
    }

    $lines = explode("\n", $input);

    $grid = array_fill(0, 1000, array_fill(0, 1000, 0));

    foreach ($lines as $line) {
        if (empty($line)) {
            continue;
        }

        list($action, $start, $end) = get_action($line);
    

        for ($i = $start[0]; $i <= $end[0]; $i++) {
            for ($j = $start[1]; $j <= $end[1]; $j++) {
                if ($action == "on") {
                    $grid[$i][$j] += 1;
                } elseif ($action == "off") {
                    $grid[$i][$j] = max(0, $grid[$i][$j] - 1);
                } elseif ($action == "toggle") {
                    $grid[$i][$j] += 2;
                }
            }
        }        
    }

    $count = 0;
    foreach ($grid as $row) {
        $count += array_sum($row);
    }
    echo "Part 2: $count\n";
}

main();