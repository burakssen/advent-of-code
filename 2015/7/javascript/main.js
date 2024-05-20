const fs = require('fs');

class Instruction {
    constructor(type, input1, input2 = "", output){
        this.type = type;
        this.input1 = input1;
        this.input2 = input2;
        this.output = output;
    }

    getInputs(){
        return [this.input1, this.input2];
    }

    getOutput(){
        return this.output;
    }   
}

let instructions = [];
let hashTable = {};

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

    const lines = data.split("\n");


    for(let i = 0; i < lines.length; i++){
        const line = lines[i].trim();
        if (line.length == 0){
            continue;
        }

        const parts = line.split(" ");

        if (parts.length == 3){
            const instruction = new Instruction("ASSIGN", parts[0], "", parts[2]);
            instructions.push(instruction);
        }

        if (parts.length == 4){
            // NOT
            const instruction = new Instruction("NOT", parts[1], "", parts[3]);
            instructions.push(instruction);
        }

        if (parts.length == 5){
            const instruction = new Instruction(parts[1], parts[0], parts[2], parts[4]);
            instructions.push(instruction);
        }
    }

    let a = eval("a");
    console.log("Part 1: " + a);
    hashTable = {};
    hashTable["b"] = a;
    a = eval("a");
    console.log("Part 2: " + a);
}


function eval(target){
    if (hashTable[target] !== undefined){
        return hashTable[target];
    }

    if (!isNaN(target)){
        return parseInt(target);
    }
    let instruction = instructions.find(x => x.output == target);
    
    let op1 = !isNaN(instruction.getInputs()[0]) ? parseInt(instruction.getInputs()[0]) : eval(instruction.getInputs()[0]);
    let op2 = 0;
    if (instruction.getInputs().length > 1){
        op2 = !isNaN(instruction.getInputs()[1]) ? parseInt(instruction.getInputs()[1]) : eval(instruction.getInputs()[1]);
    }

    let result = 0;
    switch(instruction.type){
        case "AND":
            result = op1 & op2;
            break;
        case "OR":
            result = op1 | op2;
            break;
        case "LSHIFT":
            result = op1 << op2;
            break;
        case "RSHIFT":
            result = op1 >> op2;
            break;
        case "NOT":
            result = ~op1;
            break;
        case "ASSIGN":
            result = op1;
            break;
    }

    hashTable[target] = result;
    return result;
}


main();