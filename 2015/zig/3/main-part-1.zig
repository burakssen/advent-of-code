const std = @import("std");

const Position = struct {
    x: i32,
    y: i32,

    pub fn hash(self: Position) u64 {
        return std.mem.hash(self);
    }

    pub fn eq(self: @This(), other: @This()) bool {
        return self.x == other.x and self.y == other.y;
    }

    pub fn move(self: Position, c: u8) Position {
        switch (c) {
            'v' => return Position{ .x = self.x, .y = self.y - 1 },
            '^' => return Position{ .x = self.x, .y = self.y + 1 },
            '<' => return Position{ .x = self.x - 1, .y = self.y },
            '>' => return Position{ .x = self.x + 1, .y = self.y },
            else => return self,
        }
    }
};

pub fn hashPosition(items: []Position, value: Position) bool {
    for (items) |item| {
        if (item.eq(value)) {
            return true;
        }
    }
    return false;
}

pub fn main() !void {
    // get command line arguments
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

    const file_contents = try file.reader().readAllAlloc(std.heap.page_allocator, 10000);

    var position: Position = Position{ .x = 0, .y = 0 };

    var visited = std.ArrayList(Position).init(std.heap.page_allocator);
    defer visited.deinit();

    try visited.append(position);

    for (file_contents) |c| {
        position = position.move(c);
        if (hashPosition(visited.items, position)) {
            continue;
        }
        try visited.append(position);
    }

    try stdout.print("Part 1: {}\n", .{visited.items.len});
}
