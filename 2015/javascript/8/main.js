const fs = require('fs');

function countChars(s, counts) {
    counts.codeChars += s.length;

    for (let i = 1; i < s.length - 1; i++) { // Skip first and last quote
        if (s[i] === '\\') {
            if (i + 1 < s.length - 1) {
                switch (s[i + 1]) {
                    case '\\':
                    case '"':
                        counts.stringChars++;
                        i++;
                        break;
                    case 'x':
                        if (i + 3 < s.length - 1) {
                            counts.stringChars++;
                            i += 3;
                        }
                        break;
                    default:
                        counts.stringChars++;
                }
            }
        } else {
            counts.stringChars++;
        }
    }

    return counts;
}

function encodeString(s) {
    let encoded = '"';
    for (let ch of s) {
        if (ch === '"' || ch === '\\') {
            encoded += '\\';
        }
        encoded += ch;
    }
    encoded += '"';
    return { encoded, length: encoded.length };
}

function main(){
    // get command line arguments
    const args = process.argv.slice(1);
    if (args.length < 2){
        console.log("Usage: node main.js <input.txt>");
        return;
    }

    const filename = args[1];

    if (!fs.existsSync(filename)){
        console.log("File not found: " + filename);
        return;
    }

    const data = fs.readFileSync(filename, 'utf8');

    if (!data){
        console.log("File is empty: " + filename);
        return;
    }

    const lines = data.split('\n').filter(line => line.trim().length > 0);

    let counts = { codeChars: 0, stringChars: 0 };
    let encodedLength = 0;
    
    for (let line of lines) {
        counts = countChars(line, counts);
        const { encoded, length } = encodeString(line);
        encodedLength += length;
    }

    console.log("Part 1:", counts.codeChars - counts.stringChars);

    console.log("Part 2:", encodedLength - counts.codeChars);
}


main();