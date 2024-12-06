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

    // Part 1
    var grid = try Grid.parse(allocator, content, false);
    defer grid.deinit();

    var i: usize = 0;
    while (i < 6) : (i += 1) {
        try grid.step();
    }

    try stdout.print("Part 1: {}\n", .{grid.countActive()});

    // Part 2
    var grid4D = try Grid.parse(allocator, content, true);
    defer grid4D.deinit();

    i = 0;
    while (i < 6) : (i += 1) {
        try grid4D.step();
    }

    try stdout.print("Part 2: {}\n", .{grid4D.countActive()});
}

const Point = struct {
    x: i32,
    y: i32,
    z: i32,
    w: i32 = 0,

    pub fn init(x: i32, y: i32, z: i32) Point {
        return Point{ .x = x, .y = y, .z = z };
    }

    pub fn init4D(x: i32, y: i32, z: i32, w: i32) Point {
        return Point{ .x = x, .y = y, .z = z, .w = w };
    }
};

const Grid = struct {
    active: std.AutoHashMap(Point, void),
    allocator: std.mem.Allocator,
    is4D: bool,

    pub fn init(allocator: std.mem.Allocator, is4D: bool) Grid {
        return Grid{
            .active = std.AutoHashMap(Point, void).init(allocator),
            .allocator = allocator,
            .is4D = is4D,
        };
    }

    pub fn deinit(self: *Grid) void {
        self.active.deinit();
    }

    pub fn parse(allocator: std.mem.Allocator, input: []const u8, is4D: bool) !Grid {
        var grid = Grid.init(allocator, is4D);
        errdefer grid.deinit();

        var lines = std.mem.tokenizeScalar(u8, input, '\n');
        var y: i32 = 0;
        while (lines.next()) |line| {
            for (line, 0..) |c, x| {
                if (c == '#') {
                    try grid.active.put(Point.init(@intCast(x), y, 0), {});
                }
            }
            y += 1;
        }
        return grid;
    }

    fn countActiveNeighbors(self: *const Grid, point: Point) usize {
        var count: usize = 0;
        const ranges = [_]i32{ -1, 0, 1 };

        for (ranges) |dx| {
            for (ranges) |dy| {
                for (ranges) |dz| {
                    if (self.is4D) {
                        for (ranges) |dw| {
                            if (dx == 0 and dy == 0 and dz == 0 and dw == 0) continue;
                            const neighbor = Point.init4D(
                                point.x + dx,
                                point.y + dy,
                                point.z + dz,
                                point.w + dw,
                            );
                            if (self.active.contains(neighbor)) count += 1;
                        }
                    } else {
                        if (dx == 0 and dy == 0 and dz == 0) continue;
                        const neighbor = Point.init(
                            point.x + dx,
                            point.y + dy,
                            point.z + dz,
                        );
                        if (self.active.contains(neighbor)) count += 1;
                    }
                }
            }
        }
        return count;
    }

    pub fn step(self: *Grid) !void {
        var new_active = std.AutoHashMap(Point, void).init(self.allocator);
        errdefer new_active.deinit();

        var min_x: i32 = std.math.maxInt(i32);
        var max_x: i32 = std.math.minInt(i32);
        var min_y: i32 = std.math.maxInt(i32);
        var max_y: i32 = std.math.minInt(i32);
        var min_z: i32 = std.math.maxInt(i32);
        var max_z: i32 = std.math.minInt(i32);
        var min_w: i32 = std.math.maxInt(i32);
        var max_w: i32 = std.math.minInt(i32);

        var it = self.active.keyIterator();
        while (it.next()) |point| {
            min_x = @min(min_x, point.x);
            max_x = @max(max_x, point.x);
            min_y = @min(min_y, point.y);
            max_y = @max(max_y, point.y);
            min_z = @min(min_z, point.z);
            max_z = @max(max_z, point.z);
            if (self.is4D) {
                min_w = @min(min_w, point.w);
                max_w = @max(max_w, point.w);
            }
        }

        var x = min_x - 1;
        while (x <= max_x + 1) : (x += 1) {
            var y = min_y - 1;
            while (y <= max_y + 1) : (y += 1) {
                var z = min_z - 1;
                while (z <= max_z + 1) : (z += 1) {
                    if (self.is4D) {
                        var w = min_w - 1;
                        while (w <= max_w + 1) : (w += 1) {
                            const point = Point.init4D(x, y, z, w);
                            const neighbors = self.countActiveNeighbors(point);
                            const is_active = self.active.contains(point);

                            if (is_active and (neighbors == 2 or neighbors == 3)) {
                                try new_active.put(point, {});
                            } else if (!is_active and neighbors == 3) {
                                try new_active.put(point, {});
                            }
                        }
                    } else {
                        const point = Point.init(x, y, z);
                        const neighbors = self.countActiveNeighbors(point);
                        const is_active = self.active.contains(point);

                        if (is_active and (neighbors == 2 or neighbors == 3)) {
                            try new_active.put(point, {});
                        } else if (!is_active and neighbors == 3) {
                            try new_active.put(point, {});
                        }
                    }
                }
            }
        }

        var old = self.active;
        self.active = new_active;
        old.deinit();
    }

    pub fn countActive(self: *const Grid) usize {
        return self.active.count();
    }
};
