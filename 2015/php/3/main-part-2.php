<?php

class Position {
    public $x;
    public $y;

    public function __construct($x, $y) {
        $this->x = $x;
        $this->y = $y;
    }

    public function move($direction) {
        switch($direction) {
            case '^':
                $this->y--;
                break;
            case 'v':
                $this->y++;
                break;
            case '<':
                $this->x--;
                break;
            case '>':
                $this->x++;
                break;
        }
    }
}

function main() {
    // get command line arguments
    $args = $_SERVER['argv'];
    if(count($args) < 2) {
        echo "Usage: php main.php <input_file>\n";
        return;
    }

    $filename = $args[1];

    // read input file
    $file = fopen($filename, "r");
    if($file === false) {
        echo "Error: Unable to open file $filename\n";
        return;
    }

    $input = [];
    while(!feof($file)) {
        $line = trim(fgets($file));
        if($line !== "") {
            $input[] = $line;
        }
    }

    fclose($file);

    // create a hashmap to store the number of houses visited
    $houses = [];
    $santa = new Position(0, 0);
    $robo_santa = new Position(0, 0);
    $houses[$santa->x][$santa->y] = 1;
    $houses[$robo_santa->x][$robo_santa->y] += 1;

    $santa_turn = true;
    // loop each line of input
    foreach($input as $line) {
        for($i = 0; $i < strlen($line); $i++) {
            $direction = $line[$i];
            if($santa_turn) {
                $santa->move($direction);
                $houses[$santa->x][$santa->y] = 1;
            } else {
                $robo_santa->move($direction);
                $houses[$robo_santa->x][$robo_santa->y] = 1;
            }
            $santa_turn = !$santa_turn;
        }
    }

    // count the number of houses visited
    $count = 0;
    foreach($houses as $x => $row) {
        $count += count($row);
    }

    echo "Part 2: $count\n";
}

main();