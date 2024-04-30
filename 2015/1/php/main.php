<?php

function main() : void {
    // get command line arguments
    $args = $_SERVER['argv'];
    // remove the first argument which is the script name
    array_shift($args);
    // get the first argument
    if(count($args) < 1) {
        echo "Usage: php main.php <input_file>\n";
        return;
    }

    $input_file = $args[0];

    // read the input file
    $input = file_get_contents($input_file);

    // loop each line
    $lines = explode("\n", $input);
    // loop each character
    $count = 0;
    foreach($lines as $line) {
        $chars = str_split($line);
        foreach($chars as $char) {
            if($char == '(') {
                $count++;
            }
            elseif($char == ')') {
                $count--;
            }
        }
    }

    echo $count . "\n";

}

main();