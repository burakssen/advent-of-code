
function main(){
    // Get command line arguments
    var args = process.argv.slice(1);

    if(args.length < 1) {
        console.log("Usage: node main.js <name>");
        process.exit(1);
    }

    var filename = args[1];

    var fs = require('fs');
    // check if file exists
    if(!fs.existsSync(filename)){
        console.log("File does not exist");
        process.exit(1);
    }

    // open the file
    var file = fs.readFileSync(filename, 'utf8');

    // check if the file is empty
    if(file.length == 0){
        console.log("File is empty");
        process.exit(1);
    }

    // split the file by lines
    var lines = file.split('\n');

    // loop each character in the file
    var characters = {};
    var count = 0;
    var basement = 0;
    for(var i = 0; i < lines.length; i++){
        var line = lines[i];
        for(var j = 0; j < line.length; j++){
            var character = line[j];
            if(character in characters){
                characters[character]++;
            } else {
                characters[character] = 1;
            }

            count++;
            if(characters['('] - characters[')'] == -1 && basement == 0){
                basement = count;
            }
        }
    }

    // output the result
    console.log("Part 1: " + (characters['('] - characters[')']));
    console.log("Part 2: " + basement);
}

main();