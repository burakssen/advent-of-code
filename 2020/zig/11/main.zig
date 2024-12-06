const std = @import("std");

const Grid = std.ArrayList(std.ArrayList(u8));
const directions = [_][2]i32{
    .{ -1, -1 }, .{ -1, 0 }, .{ -1, 1 },
    .{ 0, -1 },  .{ 0, 1 },  .{ 1, -1 },
    .{ 1, 0 },   .{ 1, 1 },
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len < 2) return error.InvalidArgCount;

    const file = try std.fs.cwd().openFile(args[1], .{});
    defer file.close();

    var grid = Grid.init(allocator);
    defer {
        for (grid.items) |row| row.deinit();
        grid.deinit();
    }

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var lines = std.mem.splitScalar(u8, content, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        var row = std.ArrayList(u8).init(allocator);
        try row.appendSlice(line);
        try grid.append(row);
    }

    try std.io.getStdOut().writer().print("Part 1: {d}\nPart 2: {d}\n", .{
        try simulateSeats(&grid, countAdjacent, 4),
        try simulateSeats(&grid, countVisible, 5),
    });
}

fn simulateSeats(grid: *Grid, countFn: fn (*Grid, usize, usize) u32, tolerance: u32) !u32 {
    var current = try copyGrid(grid);
    defer {
        for (current.items) |row| row.deinit();
        current.deinit();
    }

    while (true) {
        var next = try copyGrid(&current);
        defer {
            for (next.items) |row| row.deinit();
            next.deinit();
        }
        var changed = false;

        for (current.items, 0..) |row, i| {
            for (row.items, 0..) |seat, j| {
                const occupied = countFn(&current, i, j);
                switch (seat) {
                    'L' => if (occupied == 0) {
                        next.items[i].items[j] = '#';
                        changed = true;
                    },
                    '#' => if (occupied >= tolerance) {
                        next.items[i].items[j] = 'L';
                        changed = true;
                    },
                    else => {},
                }
            }
        }

        if (!changed) break;
        for (current.items, 0..) |_, i| {
            @memcpy(current.items[i].items, next.items[i].items);
        }
    }

    var count: u32 = 0;
    for (current.items) |row| {
        for (row.items) |seat| count += @intFromBool(seat == '#');
    }
    return count;
}

fn copyGrid(grid: *const Grid) !Grid {
    var new_grid = Grid.init(grid.allocator);
    errdefer new_grid.deinit();

    for (grid.items) |row| {
        var new_row = std.ArrayList(u8).init(grid.allocator);
        errdefer new_row.deinit();
        try new_row.appendSlice(row.items);
        try new_grid.append(new_row);
    }
    return new_grid;
}

fn countAdjacent(grid: *Grid, row: usize, col: usize) u32 {
    var count: u32 = 0;
    for (directions) |dir| {
        const new_row = @as(i32, @intCast(row)) + dir[0];
        const new_col = @as(i32, @intCast(col)) + dir[1];
        if (new_row >= 0 and new_row < grid.items.len and
            new_col >= 0 and new_col < grid.items[0].items.len and
            grid.items[@intCast(new_row)].items[@intCast(new_col)] == '#')
        {
            count += 1;
        }
    }
    return count;
}

fn countVisible(grid: *Grid, row: usize, col: usize) u32 {
    var count: u32 = 0;
    const rows = @as(i32, @intCast(grid.items.len));
    const cols = @as(i32, @intCast(grid.items[0].items.len));

    for (directions) |dir| {
        var curr_row = @as(i32, @intCast(row)) + dir[0];
        var curr_col = @as(i32, @intCast(col)) + dir[1];

        while (curr_row >= 0 and curr_row < rows and curr_col >= 0 and curr_col < cols) {
            const seat = grid.items[@intCast(curr_row)].items[@intCast(curr_col)];
            if (seat == '#' or seat == 'L') {
                count += @intFromBool(seat == '#');
                break;
            }
            curr_row += dir[0];
            curr_col += dir[1];
        }
    }
    return count;
}
