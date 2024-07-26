const std = @import("std");

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

    const prefix = try file.reader().readAllAlloc(std.heap.page_allocator, 9);
    defer std.heap.page_allocator.free(prefix);

    const prefix_bytes: []u8 = std.mem.sliceAsBytes(prefix);

    var i: i32 = 0;
    var leading_5_found = false;
    while (true) {
        var input_buffer = std.ArrayList(u8).init(std.heap.page_allocator);
        defer input_buffer.deinit();

        try input_buffer.appendSlice(prefix_bytes);
        const i_str = try std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{i});
        defer std.heap.page_allocator.free(i_str);
        try input_buffer.appendSlice(i_str);

        var hash: [std.crypto.hash.Md5.digest_length]u8 = undefined;
        std.crypto.hash.Md5.hash(input_buffer.items, &hash, .{});

        if (hash[0] == 0 and hash[1] == 0 and hash[2] == 0) {
            try stdout.print("Part 2: {d}\n", .{i});
            break;
        }

        if (hash[0] == 0 and hash[1] == 0 and (hash[2] & 0xF0) == 0 and !leading_5_found) {
            try stdout.print("Part 1: {d}\n", .{i});
            leading_5_found = true;
        }

        i += 1;
    }
}
