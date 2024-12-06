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

    const content = try file.readToEndAlloc(allocator, 1024);
    defer allocator.free(content);

    var numbers: [10]u32 = undefined;
    var count: usize = 0;

    var it = std.mem.splitScalar(u8, std.mem.trimRight(u8, content, "\n"), ',');
    while (it.next()) |number| : (count += 1) {
        numbers[count] = try std.fmt.parseInt(u32, number, 10);
    }

    try stdout.print("Part 1: {}\n", .{try playGame(&numbers, count, 2020)});
    try stdout.print("Part 2: {}\n", .{try playGame(&numbers, count, 30000000)});
}

fn playGame(numbers: []u32, count: usize, turns: u32) !u32 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Allocate memory for the game
    var last_seen = try allocator.alloc(u32, turns);
    defer allocator.free(last_seen);

    // Initialize all positions to 0
    @memset(last_seen, 0);

    // Initialize with starting numbers except the last one
    for (numbers[0 .. count - 1], 0..) |num, i| {
        last_seen[num] = @intCast(i + 1);
    }

    var last_number = numbers[count - 1];
    var turn: u32 = @intCast(count);

    // Main game loop
    while (turn < turns) : (turn += 1) {
        const last_seen_turn = last_seen[last_number];
        last_seen[last_number] = turn;
        last_number = if (last_seen_turn > 0) turn - last_seen_turn else 0;
    }

    return last_number;
}
