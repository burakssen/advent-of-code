const std = @import("std");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len < 2) return error.InvalidArgCount;

    const file = try std.fs.cwd().openFile(args[1], .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var lines = std.mem.splitScalar(u8, content, '\n');
    const timestamp = try std.fmt.parseInt(u32, lines.next().?, 10);
    const ids_line = lines.next().?;

    // Part 1
    var min_wait: u32 = std.math.maxInt(u32);
    var min_id: u32 = undefined;

    // Part 2
    var offset: usize = 0;
    var time: u64 = 0;
    var step: u64 = 1;

    var ids_split = std.mem.splitScalar(u8, ids_line, ',');
    while (ids_split.next()) |id_str| {
        if (std.mem.eql(u8, id_str, "x")) {
            offset += 1;
            continue;
        }
        const id = try std.fmt.parseInt(u32, id_str, 10);

        // Part 1 calculation
        const wait = id - (timestamp % id);
        if (wait < min_wait) {
            min_wait = wait;
            min_id = id;
        }

        // Part 2 calculation
        while ((time + offset) % id != 0) time += step;
        step *= id;
        offset += 1;
    }

    try stdout.print("Part 1: {d}\n", .{min_id * min_wait});
    try stdout.print("Part 2: {d}\n", .{time});
}
