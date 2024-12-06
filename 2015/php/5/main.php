<?php

function check_good_string_part1($string) {
    $vowels = 0;
    $double_letter = false;
    $bad_string = false;

    for($i = 0; $i < strlen($string); $i++) {
        if ($string[$i] == 'a' || $string[$i] == 'e' || $string[$i] == 'i' || $string[$i] == 'o' || $string[$i] == 'u') {
            $vowels++;
        }

        if ($i > 0) {
            if ($string[$i] == $string[$i - 1]) {
                $double_letter = true;
            }
            if ($string[$i] == 'b' && $string[$i - 1] == 'a') {
                $bad_string = true;
            }
            if ($string[$i] == 'd' && $string[$i - 1] == 'c') {
                $bad_string = true;
            }
            if ($string[$i] == 'q' && $string[$i - 1] == 'p') {
                $bad_string = true;
            }
            if ($string[$i] == 'y' && $string[$i - 1] == 'x') {
                $bad_string = true;
            }
        }
    }
    
    return $vowels >= 3 && $double_letter && !$bad_string;
}

function check_good_string_part2($string){
    $pair = false;
    $repeat = false;

    for($i = 0; $i < strlen($string); $i++) {
        if ($i > 0) {
            $pair = $pair || strpos($string, substr($string, $i - 1, 2), $i + 1) !== false;
            $repeat = $repeat || $string[$i] == $string[$i - 2];
        }
    }

    return $pair && $repeat;

}

function main(){
    $args = $_SERVER['argv'];

    if (count($args) < 2) {
        echo "Usage: php main.php <input.txt>\n";
        exit(1);
    }

    $filename = $args[1];

    $file = fopen($filename, "r") or die("Unable to open file!");
    
    if ($file == false) {
        echo "Error: Unable to open file.\n";
        exit(1);
    }

    $all_lines = array();
    while (!feof($file)) {
        $line = fgets($file);
        $all_lines[] = $line;
    }

    fclose($file);

    $part1_count = 0;
    $part2_count = 0;

    for ($i = 0; $i < count($all_lines); $i++) {
        $line = $all_lines[$i];
        
        if (check_good_string_part1($line)) {
            $part1_count++;
        }

        if (check_good_string_part2($line)) {
            $part2_count++;
        }
    }

    echo "Part 1: $part1_count\n";
    echo "Part 2: $part2_count\n";

}

main();