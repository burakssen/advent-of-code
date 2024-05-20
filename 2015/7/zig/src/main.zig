const std = @import("std");
const ArenaAllocator = std.heap.ArenaAllocator;
const assert = std.debug.assert;
const eql = std.mem.eql;
const string = @import("string");
const String = string.String;

const InstructionType = enum(u8) {
    AND = 0,
    OR,
    LSHIFT,
    RSHIFT,
    NOT,
    ASSIGN,
};

const Instruction = struct {
    type: InstructionType,
    lhs: []const u8,
    rhs: ?[]const u8,
    result: []const u8,
};

var instructions: std.ArrayList(Instruction) = undefined;
var hash_map: std.StringHashMap(u16) = undefined;

fn get_instruction(line: []const u8, line_buffer: *String) Instruction {
    var instr = Instruction{
        .type = InstructionType.ASSIGN,
        .lhs = undefined,
        .rhs = undefined,
        .result = undefined,
    };

    line_buffer.concat(line) catch {
        return instr;
    };

    if (line_buffer.find("AND") != null) {
        instr.type = InstructionType.AND;
        instr.lhs = line_buffer.split(" ", 0).?;
        instr.rhs = line_buffer.split(" ", 2).?;
        instr.result = line_buffer.split(" ", 4).?;
        return instr;
    }

    if (line_buffer.find("OR") != null) {
        instr.type = InstructionType.OR;
        instr.lhs = line_buffer.split(" ", 0).?;
        instr.rhs = line_buffer.split(" ", 2).?;
        instr.result = line_buffer.split(" ", 4).?;
        return instr;
    }

    if (line_buffer.find("LSHIFT") != null) {
        instr.type = InstructionType.LSHIFT;
        instr.lhs = line_buffer.split(" ", 0).?;
        instr.rhs = line_buffer.split(" ", 2).?;
        instr.result = line_buffer.split(" ", 4).?;
        return instr;
    }

    if (line_buffer.find("RSHIFT") != null) {
        instr.type = InstructionType.RSHIFT;
        instr.lhs = line_buffer.split(" ", 0).?;
        instr.rhs = line_buffer.split(" ", 2).?;
        instr.result = line_buffer.split(" ", 4).?;
        return instr;
    }

    if (line_buffer.find("NOT") != null) {
        instr.type = InstructionType.NOT;
        instr.lhs = line_buffer.split(" ", 1).?;
        instr.result = line_buffer.split(" ", 3).?;
        return instr;
    }

    if (line_buffer.find("->") != null) {
        instr.type = InstructionType.ASSIGN;
        instr.lhs = line_buffer.split(" ", 0).?;
        instr.result = line_buffer.split(" ", 2).?;
        return instr;
    }

    return instr;
}

fn get_target(target: []const u8) ?Instruction {
    for (instructions.items) |instr| {
        if (eql(u8, instr.result, target)) {
            return instr;
        }
    }

    return null;
}

fn evalOperand(operand: []const u8) u16 {
    // First, try to parse the operand as an integer literal
    const intResult = std.fmt.parseInt(u16, operand, 10);
    if (intResult) |value| {
        return value;
    } else |err| {
        switch (err) {
            std.fmt.ParseIntError.InvalidCharacter, std.fmt.ParseIntError.Overflow => {
                // If the operand is not an integer literal, try to evaluate it as a target
                return eval(operand);
            },
        }
    }
}

fn eval(target: []const u8) u16 {
    if (hash_map.contains(target)) {
        return hash_map.get(target).?;
    }

    const instr = get_target(target) orelse null;

    if (instr == null) {
        std.debug.print("Target: {s} not found\n", .{target});
        return 0;
    }

    var result: u16 = 0;

    const lhs = evalOperand(instr.?.lhs);
    var rhs: u16 = 0;
    if (instr.?.rhs) |rhs_operand| {
        rhs = evalOperand(rhs_operand);
    }

    switch (instr.?.type) {
        InstructionType.ASSIGN => {
            result = lhs;
        },
        InstructionType.AND => {
            result = lhs & rhs;
        },
        InstructionType.OR => {
            result = lhs | rhs;
        },
        InstructionType.LSHIFT => {
            result = lhs << @as(u4, @intCast(rhs));
        },
        InstructionType.RSHIFT => {
            result = lhs >> @as(u4, @intCast(rhs));
        },
        InstructionType.NOT => {
            result = ~lhs;
        },
    }

    hash_map.put(instr.?.result, result) catch {
        std.debug.print("Failed to insert key: {s} value: {d}\n", .{ instr.?.result, result });
    };
    return result;
}
pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer std.process.argsFree(std.heap.page_allocator, args);

    if (args.len < 2) {
        try stdout.print("Usage: zig run main.zig <input_file>\n", .{});
        return;
    }

    const file_path = args[1];
    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var line_buf: [64]u8 = undefined;

    instructions = std.ArrayList(Instruction).init(std.heap.page_allocator);
    defer instructions.deinit();
    hash_map = std.StringHashMap(u16).init(std.heap.page_allocator);
    defer hash_map.deinit();

    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var buffers: std.ArrayList(String) = std.ArrayList(String).init(std.heap.page_allocator);
    defer buffers.deinit();

    while (true) {
        var line_buffer = String.init(arena.allocator());
        const line_bytes = try in_stream.readUntilDelimiterOrEof(&line_buf, '\n');
        if (line_bytes == null) {
            break;
        }

        if (line_bytes) |line| {
            // Remove the newline character
            const instr = get_instruction(line, &line_buffer);
            try instructions.append(instr);
        }

        try buffers.append(line_buffer);
    }

    var a = eval("a");
    try stdout.print("Part 1: {d}\n", .{a});
    hash_map.clearRetainingCapacity();
    hash_map.put("b", a) catch {
        std.debug.print("Failed to insert key: {s} value: {d}\n", .{ "b", a });
    };
    a = eval("a");
    try stdout.print("Part 2: {d}\n", .{a});

    for (0..buffers.items.len) |i| {
        buffers.items[i].deinit();
    }
}
