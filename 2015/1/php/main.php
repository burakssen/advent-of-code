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

main();