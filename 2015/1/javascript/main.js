
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

main();