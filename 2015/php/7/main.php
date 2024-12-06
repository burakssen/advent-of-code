<?php

enum InstructionType {
    case AND;
    case OR;
    case NOT;
    case LSHIFT;
    case RSHIFT;
    case ASSIGN;
}

class Instruction {
    public $operation;
    public $input1;
    public $input2;
    public $output;

    public function __construct($operation, $output, $input1, $input2 = "") {
        $this->operation = $operation;
        $this->input1 = $input1;
        $this->input2 = $input2;
        $this->output = $output;
    }

    public function __toString() {
        return "Instruction: $this->input1 $this->input2 -> $this->output";
    }
}

$instructions = array();
$hashmap = array();

function evaluate($target) {
    global $hashmap;
    global $instructions;
    if (array_key_exists($target, $hashmap)) {
        return $hashmap[$target];
    }

    if (is_numeric($target)) {
        return intval($target);
    }

    $instruction = NULL;
    foreach ($instructions as $i) {
        if ($i->output == $target) {
            $instruction = $i;
            break;
        }
    }

    if ($instruction == NULL) {
        return $target;
    }

    $op1 = is_numeric($instruction->input1) ? intval($instruction->input1) : evaluate($instruction->input1, $hashmap, $instructions);
    $op2 = 0;
    if ($instruction->input2 != "") {
        $op2 = is_numeric($instruction->input2) ? intval($instruction->input2) : evaluate($instruction->input2, $hashmap, $instructions);
    }

    $result = 0;
    switch ($instruction->operation) {
        case InstructionType::AND:
            $result = $op1 & $op2;
            break;
        case InstructionType::OR:
            $result = $op1 | $op2;
            break;
        case InstructionType::NOT:
            $result = ~$op1;
            break;
        case InstructionType::LSHIFT:
            $result = $op1 << $op2;
            break;
        case InstructionType::RSHIFT:
            $result = $op1 >> $op2;
            break;
        case InstructionType::ASSIGN:
            $result = $op1;
            break;
    }

    $hashmap[$target] = $result;
    return $result;
}

function main(){
    global $instructions;
    // get command line arguments
    $args = $_SERVER['argv'];
    if (count($args) < 2) {
        echo "Usage: php main.php <input.txt>\n";
        exit(1);
    }

    $filename = $args[1];

    if (!file_exists($filename)) {
        echo "File not found: $filename\n";
        exit(1);
    }

    $file = fopen($filename, "r");

    if ($file === false) {
        echo "Error opening file: $filename\n";
        exit(1);
    }

    $content = fread($file, filesize($filename));
    fclose($file);

    $lines = explode("\n", $content);

    
    
    foreach ($lines as $line) {
        $words = explode(" ", $line);
        
        if (count($words) == 3){
            $instructions[] = new Instruction(InstructionType::ASSIGN, $words[2], $words[0], "");
        }

        if (count($words) == 4){
            $instructions[] = new Instruction(InstructionType::NOT, $words[3], $words[1], "");
        }

        if (count($words) == 5){
            if ($words[1] == "AND"){
                $instructions[] = new Instruction(InstructionType::AND, $words[4], $words[0], $words[2]);
            }
            if ($words[1] == "OR"){
                $instructions[] = new Instruction(InstructionType::OR, $words[4], $words[0], $words[2]);
            }
            if ($words[1] == "LSHIFT"){
                $instructions[] = new Instruction(InstructionType::LSHIFT, $words[4], $words[0], $words[2]);
            }
            if ($words[1] == "RSHIFT"){
                $instructions[] = new Instruction(InstructionType::RSHIFT, $words[4], $words[0], $words[2]);
            }
        }
    }

    $a = evaluate("a");
    echo "Part 1: $a\n";
    global $hashmap;    
    $hashmap = array("b" => $a);
    $result = evaluate("a");
    echo "Part 2: $result\n";
}

main();
?>
