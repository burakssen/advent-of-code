const fs = require('fs');


function get_action(line) {
    const words = line.split(' ');

    if (words[0] === 'turn') {
        return {
            "action": words[1],
            "start": words[2].split(',').map(x => parseInt(x)),
            "end": words[4].split(',').map(x => parseInt(x))
        }
    }
    else {
        return {
            "action": words[0],
            "start": words[1].split(',').map(x => parseInt(x)),
            "end": words[3].split(',').map(x => parseInt(x))
        }
    }
}

function main(){
    // get command line arguments
    var args = process.argv.slice(1);

    if (args.length < 2) {
        console.log("Usage: node main-part1.js <input.txt>");
        return;
    }

    var filename = args[1];

    // read the file
    if (!fs.existsSync(filename)) {
        console.log("File not found: " + filename);
        return;
    }

    var data = fs.readFileSync(filename, 'utf8');

    // split the data into lines
    var lines = data.split('\n');

    var grid = [];
    for (var i = 0; i < 1000; i++) {
        grid.push([]);
        for (var j = 0; j < 1000; j++) {
            grid[i].push(false);
        }
    }

    for (var i = 0; i < lines.length; i++) {
        var line = lines[i].trim();
        
        if (line.length == 0) {
            continue;
        }

        var action = get_action(line);
        
        for (var x = action.start[0]; x <= action.end[0]; x++) {
            for (var y = action.start[1]; y <= action.end[1]; y++) {
                if (action.action === 'on') {
                    grid[x][y] = true;
                }
                else if (action.action === 'off') {
                    grid[x][y] = false;
                }
                else {
                    grid[x][y] = !grid[x][y];
                }
            }
        }
    }

    var count = 0;
    for (var i = 0; i < 1000; i++) {
        for (var j = 0; j < 1000; j++) {
            if (grid[i][j]) {
                count++;
            }
        }
    }

    console.log("Part 1: " + count);

}

main();