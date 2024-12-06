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

    var numbers = std.ArrayList(u64).init(allocator);
    try numbers.append(0);

    var buf: [16]u8 = undefined;
    while (try file.reader().readUntilDelimiterOrEof(&buf, '\n')) |line|
        try numbers.append(try std.fmt.parseInt(u64, line, 10));

    std.mem.sort(u64, numbers.items, {}, comptime std.sort.asc(u64));
    try numbers.append(numbers.items[numbers.items.len - 1] + 3);

    var diffs = [_]u64{0} ** 4;
    for (numbers.items[1..], 0..) |jolt, i|
        diffs[jolt - numbers.items[i]] += 1;

    try stdout.print("Part 1: {}\n", .{diffs[1] * diffs[3]});

    var paths = try allocator.alloc(u64, numbers.items.len);
    defer allocator.free(paths);
    @memset(paths, 0);
    paths[0] = 1;

    for (numbers.items, 0..) |_, i| {
        var j: usize = i + 1;
        while (j < paths.len and numbers.items[j] - numbers.items[i] <= 3) : (j += 1)
            paths[j] += paths[i];
    }

    try stdout.print("Part 2: {}\n", .{paths[paths.len - 1]});
}
