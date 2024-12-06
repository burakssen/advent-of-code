<?php

function countChars($s, &$counts) {
    $counts['codeChars'] += strlen($s);

    for ($i = 1; $i < strlen($s) - 1; $i++) { // Skip first and last quote
        if ($s[$i] === '\\') {
            if ($i + 1 < strlen($s) - 1) {
                switch ($s[$i + 1]) {
                    case '\\':
                    case '"':
                        $counts['stringChars']++;
                        $i++;
                        break;
                    case 'x':
                        if ($i + 3 < strlen($s) - 1) {
                            $counts['stringChars']++;
                            $i += 3;
                        }
                        break;
                    default:
                        $counts['stringChars']++;
                }
            }
        } else {
            $counts['stringChars']++;
        }
    }

    return $counts;
}

function encodeString($s) {
    $encoded = '"';
    for ($i = 0; $i < strlen($s); $i++) {
        $ch = $s[$i];
        if ($ch === '"' || $ch === '\\') {
            $encoded .= '\\';
        }
        $encoded .= $ch;
    }
    $encoded .= '"';
    return array('encoded' => $encoded, 'length' => strlen($encoded));
}

function main() {
    global $argv;

    // get command line arguments
    if (count($argv) < 2) {
        echo "Usage: php main.php <input.txt>\n";
        return;
    }

    $filename = $argv[1];

    if (!file_exists($filename)) {
        echo "File not found: " . $filename . "\n";
        return;
    }

    $data = file_get_contents($filename);

    if (!$data) {
        echo "File is empty: " . $filename . "\n";
        return;
    }

    $lines = array_filter(explode("\n", $data), function($line) {
        return trim($line) !== '';
    });

    $counts = array('codeChars' => 0, 'stringChars' => 0);
    $encodedLength = 0;

    foreach ($lines as $line) {
        $counts = countChars($line, $counts);
        $encoded = encodeString($line);
        $encodedLength += $encoded['length'];
    }

    echo "Part 1: " . ($counts['codeChars'] - $counts['stringChars']) . "\n";
    echo "Part 2: " . ($encodedLength - $counts['codeChars']) . "\n";
}

main();
?>
