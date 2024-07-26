<?php

function main(){
    $args = $_SERVER['argv'];
    if (count($args) < 2){
        echo "Usage: php main.php <input.txt>\n";
        return;
    }

    $filename = $args[1];
    $file = fopen($filename, "r");
    if ($file == false){
        echo "Error: Cannot open file $filename\n";
        return;
    }

    $line = fgets($file);

    $i = 0;
    $leading_5_found = false;
    while(true){
        $input = $line . $i;
        $hash = md5($input);

        if (substr($hash, 0, 6) === "000000"){
            echo "Part 2: $i\n";
            break;
        }

        if (substr($hash, 0, 5) === "00000" && !$leading_5_found){
            echo "Part 1: $i\n";
            $leading_5_found = true;
        }

        $i++;
    }
}

main();