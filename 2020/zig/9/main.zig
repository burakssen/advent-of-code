const std = @import("std");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const args = try std.process.argsAlloc(arena.allocator());
    defer std.process.argsFree(arena.allocator(), args);

    if (args.len < 2) return error.InvalidArgCount;

    const file = try std.fs.cwd().openFile(args[1], .{});
    defer file.close();

    var numbers = std.ArrayList(u64).init(arena.allocator());
    var buf: [16]u8 = undefined;
    var reader = file.reader();

    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line|
        try numbers.append(try std.fmt.parseInt(u64, line, 10));

    var i: usize = 25;
    var part1_number: u64 = 0;
    var part2_number: u64 = 0;
    while (i < numbers.items.len) : (i += 1) {
        if (!isValidNumber(numbers.items[i - 25 .. i + 1])) {
            part1_number = numbers.items[i];
            if (findContiguousRange(numbers.items, part1_number)) |range| {
                var min: u64 = range[0];
                var max: u64 = range[0];
                for (range) |num| {
                    min = @min(min, num);
                    max = @max(max, num);
                }
                part2_number = min + max;
            }
            break;
        }
    }

    std.debug.print("Part 1: {}\n", .{part1_number});
    std.debug.print("Part 2: {}\n", .{part2_number});
}

fn isValidNumber(window: []const u64) bool {
    const target = window[window.len - 1];
    var seen = std.AutoHashMap(u64, void).init(std.heap.page_allocator);
    defer seen.deinit();

    for (window[0 .. window.len - 1]) |num| {
        const complement = if (target > num) target - num else continue;
        if (seen.contains(complement)) return true;
        seen.put(num, {}) catch return false;
    }
    return false;
}

fn findContiguousRange(numbers: []const u64, target: u64) ?[]const u64 {
    var start: usize = 0;
    var end: usize = 0;
    var sum: u64 = 0;

    while (end < numbers.len) : (end += 1) {
        sum += numbers[end];
        while (sum > target) : (start += 1) sum -= numbers[start];
        if (sum == target and end - start > 0) return numbers[start .. end + 1];
    }
    return null;
}
