const std = @import("std");

const ProgramResult = struct {
    accumulator: i64,
    terminated: bool,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: {s} [filename]\n", .{args[0]});
        return;
    }

    const file_contents = try std.fs.cwd().readFileAlloc(allocator, args[1], 1_000_000);
    defer allocator.free(file_contents);

    var lines = std.ArrayList([]const u8).init(allocator);
    defer lines.deinit();

    var start_index: usize = 0;
    for (file_contents, 0..) |byte, i| {
        if (byte == '\n') {
            try lines.append(file_contents[start_index..i]);
            start_index = i + 1;
        }
    }

    if (start_index < file_contents.len) {
        try lines.append(file_contents[start_index..]);
    }

    // Part 1
    const part1_result = try runProgram(lines, allocator, null);
    std.debug.print("Part 1: {}\n", .{part1_result.accumulator});

    // Part 2
    var i: usize = 0;
    while (i < lines.items.len) : (i += 1) {
        const line = lines.items[i];
        const split_index = std.mem.indexOf(u8, line, " ") orelse continue;
        const operation = line[0..split_index];

        if (std.mem.eql(u8, operation, "acc")) {
            continue;
        }

        const result = try runProgram(lines, allocator, i);
        if (result.terminated) {
            std.debug.print("Part 2: {}\n", .{result.accumulator});
            break;
        }
    }
}

fn runProgram(lines: std.ArrayList([]const u8), allocator: std.mem.Allocator, swap_index: ?usize) !ProgramResult {
    var visited = try std.ArrayList(bool).initCapacity(allocator, lines.items.len);
    defer visited.deinit();
    try visited.resize(lines.items.len);

    var pointer: usize = 0;
    var accumulator: i64 = 0;

    while (pointer < lines.items.len) {
        if (visited.items[pointer]) {
            return ProgramResult{ .accumulator = accumulator, .terminated = false };
        }

        visited.items[pointer] = true;
        const split_index = std.mem.indexOf(u8, lines.items[pointer], " ") orelse continue;
        const operation = lines.items[pointer][0..split_index];
        const sign = lines.items[pointer][split_index + 1];
        const number_str = lines.items[pointer][(split_index + 2)..];
        const number = try std.fmt.parseInt(i64, number_str, 10);

        if (std.mem.eql(u8, operation, "acc")) {
            accumulator += if (sign == '+') number else -number;
            pointer += 1;
        } else if (std.mem.eql(u8, operation, "jmp")) {
            if (swap_index != null and swap_index.? == pointer) {
                // Treat as nop
                pointer += 1;
            } else {
                const jump = if (sign == '+') number else -number;
                pointer = @intCast(@as(i64, @intCast(pointer)) + jump);
            }
        } else if (std.mem.eql(u8, operation, "nop")) {
            if (swap_index != null and swap_index.? == pointer) {
                // Treat as jmp
                const jump = if (sign == '+') number else -number;
                pointer = @intCast(@as(i64, @intCast(pointer)) + jump);
            } else {
                pointer += 1;
            }
        }
    }

    return ProgramResult{ .accumulator = accumulator, .terminated = true };
}
