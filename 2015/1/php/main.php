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
}main();