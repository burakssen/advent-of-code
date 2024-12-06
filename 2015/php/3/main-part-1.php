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
    $pos = new Position(0, 0);
    $houses[$pos->x][$pos->y] = 1;

    // loop each line of input
    foreach($input as $line) {
        for($i = 0; $i < strlen($line); $i++) {
            $pos->move($line[$i]);
            if(!isset($houses[$pos->x])) {
                $houses[$pos->x] = [];
            }
            if(!isset($houses[$pos->x][$pos->y])) {
                $houses[$pos->x][$pos->y] = 1;
            } else {
                $houses[$pos->x][$pos->y]++;
            }
        }
    }

    // count the number of houses visited
    $count = 0;
    foreach($houses as $x => $row) {
        $count += count($row);
    }

    echo "Part 1: $count\n";
}

main();