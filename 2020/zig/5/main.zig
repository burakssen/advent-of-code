const std = @import("std");

const ReadByLineIterator = struct {
    file: std.fs.File,
    buf_reader: std.io.BufferedReader(4096, std.fs.File.Reader),
    buf: [64]u8,

    pub fn next(self: *@This()) !?[]u8 {
        return self.buf_reader.reader().readUntilDelimiterOrEof(&self.buf, '\n');
    }

    pub fn deinit(self: *@This()) void {
        self.file.close();
    }
};

fn readByLine(filename: []const u8) !ReadByLineIterator {
    const file = try std.fs.cwd().openFile(filename, .{});
    return ReadByLineIterator{
        .file = file,
        .buf_reader = std.io.bufferedReader(file.reader()),
        .buf = undefined,
    };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: {s} [filename]\n", .{args[0]});
        return;
    }

    var lines = try readByLine(args[1]);
    defer lines.deinit();

    var seat_ids = std.ArrayList(i32).init(allocator);
    defer seat_ids.deinit();

    while (try lines.next()) |line| {
        var row_min: i32 = 0;
        var row_max: i32 = 127;
        var col_min: i32 = 0;
        var col_max: i32 = 7;

        for (line) |c| {
            if (c == 'F' or c == 'B') {
                const row_mid = row_min + @divTrunc((row_max - row_min), 2);
                if (c == 'F') {
                    row_max = row_mid;
                } else {
                    row_min = row_mid + 1;
                }
            }

            if (c == 'L' or c == 'R') {
                const col_mid = col_min + @divTrunc((col_max - col_min), 2);
                if (c == 'L') {
                    col_max = col_mid;
                } else {
                    col_min = col_mid + 1;
                }
            }
        }

        const seat_id: i32 = row_min * 8 + col_min;
        try seat_ids.append(seat_id);
    }

    std.mem.sort(i32, seat_ids.items, {}, comptime std.sort.asc(i32));
    const largest_seat_id = seat_ids.items[seat_ids.items.len - 1];

    var prev_seat_id: i32 = seat_ids.items[0];
    var my_seat_id: i32 = 0;

    for (seat_ids.items[1..]) |seat_id| {
        if (seat_id - prev_seat_id == 2) {
            my_seat_id = prev_seat_id + 1;
            break;
        }
        prev_seat_id = seat_id;
    }

    std.debug.print("Part 1: {d}\n", .{largest_seat_id});
    std.debug.print("Part 2: {d}\n", .{my_seat_id});
}
