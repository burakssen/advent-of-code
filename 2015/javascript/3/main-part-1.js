class Position {
    constructor(x, y){
        this.x = x;
        this.y = y;
    }

    move(direction){
        switch(direction){
            case '^':
                this.y++;
                break;
            case 'v':
                this.y--;
                break;
            case '>':
                this.x++;
                break;
            case '<':
                this.x--;
                break;
        }
    }
}

function main(){
    // get command line arguments
    const args = process.argv.slice(1);

    if(args.length < 1){
        console.log("Usage: node main.js <input_file>");
        return;
    }

    // get input file
    const filename = args[1];
    const fs = require('fs');
    if(!fs.existsSync(filename)){
        console.log(`File ${filename} not found`);
        return;
    }

    // read input file
    const data = fs.readFileSync(filename, 'utf8');
    const lines = data.split('\n');

    let position = new Position(0, 0);

    // create 2d hashmap to store visited positions
    let grid = {};
    grid[position.x] = {};
    grid[position.x][position.y] = 1;

    for(let i = 0; i < lines.length; i++){
        const line = lines[i];
        for(let j = 0; j < line.length; j++){
            const char = line[j];
            position.move(char);
            if (grid[position.x] === undefined){
                grid[position.x] = {};
                grid[position.x][position.y] = 1;
            }
            else if (grid[position.x][position.y] === undefined){
                grid[position.x][position.y] = 1;
            }
            else {
                grid[position.x][position.y]++;
            }
        }
    }

    let count = 0;
    for (let x in grid){
        for (let _ in grid[x]){
            count++;
        }
    }


    console.log("Part 1: " + count);
}

main();