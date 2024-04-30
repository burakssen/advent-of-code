function main(){
    // get command line arguments
    const args = process.argv.slice(1);

    if(args.length < 2){
        console.log("Usage: node main.js <input file>");
        return;
    }

    const inputFile = args[1];
    const fs = require('fs');
    const data = fs.readFileSync(inputFile, 'utf8');
    const lines = data.split('\n');

    let total = 0;
    for(let i = 0; i < lines.length; i++){
        // split the line into words
        const words = lines[i].split('x');
        const l = parseInt(words[0]);
        const w = parseInt(words[1]);
        const h = parseInt(words[2]);

        // calculate the surface area
        const lw = l * w;
        const wh = w * h;
        const hl = h * l;

        // calculate the smallest side
        const minSide = Math.min(lw, wh, hl);

        // calculate the total surface area
        const area = 2 * lw + 2 * wh + 2 * hl + minSide;
        total += area;
    }

    console.log(total);
}

main();