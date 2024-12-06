// File: main.zig

const std = @import("std");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer std.process.argsFree(std.heap.page_allocator, args);

    if (args.len < 2) {
        try stdout.print("Error: Please provide a file path as an argument.\n", .{});
        return;
    }

    const file_path = args[1];
    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    var buffered_reader = std.io.bufferedReader(file.reader());
    var reader = buffered_reader.reader();
    var buffer: [10000]u8 = undefined;

    var floor: i64 = 0;
    var count: i64 = 0;
    var basement: i64 = 0;

    while (true) {
        const line = reader.readUntilDelimiterOrEof(buffer[0..], '\n') catch |err| {
            try stdout.print("Error reading file: {}\n", .{err});
            return;
        };

        if (line == null) break;

        // loop each character in the line
        var i: usize = 0;
        while (i < line.?.len) {
            const c = line.?[i];

            if (c == '(') {
                floor += 1;
            } else if (c == ')') {
                floor -= 1;
            }

            i += 1;
            count += 1;
            if (floor == -1 and basement == 0) {
                basement = count;
            }
        }
    }

    try stdout.print("Part 1: {}\n", .{floor});
    try stdout.print("Part 2: {}\n", .{basement});
}
