const fs = require('fs');

function check_good_string_part1(str){
    // check if string has at least 3 vowels
    let vowels = 0;
    let double_letter = false;
    let bad_string = false;

    for(let i = 0; i < str.length; i++){
        if(str[i] === 'a' || str[i] === 'e' || str[i] === 'i' || str[i] === 'o' || str[i] === 'u'){
            vowels++;
        }

        if(i > 0 && str[i] === str[i-1]){
            double_letter = true;
        }

        if((str[i] === 'a' && str[i+1] === 'b') || (str[i] === 'c' && str[i+1] === 'd') || (str[i] === 'p' && str[i+1] === 'q') || (str[i] === 'x' && str[i+1] === 'y')){
            bad_string = true;
        }
    }

    return vowels >= 3 && double_letter && !bad_string;
}

function check_good_string_part2(str){
    let pair = false;
    let repeat = false;

    for(let i = 0; i < str.length - 1; i++){
        let pair_str = str[i] + str[i+1];
        if(str.indexOf(pair_str, i+2) !== -1){
            pair = true;
        }

        if(str[i] === str[i+2]){
            repeat = true;
        }
    }

    return pair && repeat;
}

function main(){
    // get command line arguments
    const args = process.argv.slice(1);

    if (args.length < 2) {
        console.log('Usage: node main.js <input.txt>');
        return;
    }

    // get file name
    const fileName = args[1];
    
    // read file
    if(!fs.existsSync(fileName)){
        console.log(`File not found: ${fileName}`);
        return;
    }

    const file = fs.readFileSync(fileName, 'utf8');
    const lines = file.split('\n');

    let part1_count = 0;
    let part2_count = 0;
    for (let i = 0; i < lines.length; i++) {
        if (check_good_string_part1(lines[i])) {
            part1_count++;
        }

        if (check_good_string_part2(lines[i])) {
            part2_count++;
        }
    }

    console.log("Part 1: " + part1_count);
    console.log("Part 2: " + part2_count);
}

main();