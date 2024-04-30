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
}
