const fs = require('fs');
const crypto = require('crypto');


function main(){
    // get command line arguments
    const args = process.argv.slice(1);
    if (args.length < 2){
        console.log('Usage: node main.js <input.txt>');
        return;
    }

    // get input file
    const filename = args[1];
    if(!fs.existsSync(filename)){
        console.log('File not found:', filename);
        return;
    }

    // read file
    const prefix = fs.readFileSync(filename, 'utf8');

    var i = 0;
    var leading_5_found = false;
    while(true){
        const input = prefix + i;
        const hash = crypto.createHash('md5').update(input).digest('hex');
        
        if (hash.startsWith('00000') && !leading_5_found){
            console.log("Part 1: " + i);
            leading_5_found = true;
        }

        if (hash.startsWith('000000')){
            console.log("Part 2: " + i);
            break;
        }


        i++;
    }

}

main();