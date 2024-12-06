const std = @import("std");

pub fn main() !void {
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

    try std.io.getStdOut().writer().print("Part 1: {d}\nPart 2: {d}\n", .{
        try navigateShip(content),
        try navigateWithWaypoint(content),
    });
}

fn navigateShip(content: []const u8) !i32 {
    var x: i32 = 0;
    var y: i32 = 0;
    var direction: i32 = 90; // Start facing east (90 degrees)

    var lines = std.mem.splitScalar(u8, content, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        const amount = try std.fmt.parseInt(i32, line[1..], 10);
        switch (line[0]) {
            'N' => y += amount,
            'S' => y -= amount,
            'E' => x += amount,
            'W' => x -= amount,
            'L' => direction = @mod(direction - amount, 360),
            'R' => direction = @mod(direction + amount, 360),
            'F' => {
                switch (direction) {
                    0 => y += amount, // North
                    90 => x += amount, // East
                    180 => y -= amount, // South
                    270 => x -= amount, // West
                    else => unreachable,
                }
            },
            else => unreachable,
        }
    }

    const abs_x: i32 = if (x < 0) -x else x;
    const abs_y: i32 = if (y < 0) -y else y;
    return abs_x + abs_y;
}

fn navigateWithWaypoint(content: []const u8) !i32 {
    var ship = [_]i32{ 0, 0 };
    var waypoint = [_]i32{ 10, 1 }; // Relative to ship: 10 units east, 1 unit north

    var lines = std.mem.splitScalar(u8, content, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        const amount = try std.fmt.parseInt(i32, line[1..], 10);
        switch (line[0]) {
            'F' => {
                ship[0] += waypoint[0] * amount;
                ship[1] += waypoint[1] * amount;
            },
            'N' => waypoint[1] += amount,
            'S' => waypoint[1] -= amount,
            'E' => waypoint[0] += amount,
            'W' => waypoint[0] -= amount,
            'L' => {
                const rotations = @divFloor(amount, 90);
                for (0..@intCast(rotations)) |_| {
                    const temp = waypoint[0];
                    waypoint[0] = -waypoint[1];
                    waypoint[1] = temp;
                }
            },
            'R' => {
                const rotations = @divFloor(amount, 90);
                for (0..@intCast(rotations)) |_| {
                    const temp = waypoint[0];
                    waypoint[0] = waypoint[1];
                    waypoint[1] = -temp;
                }
            },
            else => unreachable,
        }
    }

    const abs_x: i32 = if (ship[0] < 0) -ship[0] else ship[0];
    const abs_y: i32 = if (ship[1] < 0) -ship[1] else ship[1];
    return abs_x + abs_y;
}
