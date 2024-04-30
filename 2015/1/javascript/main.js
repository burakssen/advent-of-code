function main(){
    // Get command line arguments
    var args = process.argv.slice(1);

    if(args.length < 1) {
        console.log("Usage: node main.js <name>");
        process.exit(1);
    }

}main();