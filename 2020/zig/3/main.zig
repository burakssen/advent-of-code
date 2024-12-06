const std = @import("std");

const ReaderType = std.fs.File.Reader;
const BufReaderType = std.io.BufferedReader(4096, ReaderType);
const BufReaderReaderType = BufReaderType.Reader;

pub const ReadByLineIterator = struct {
    file: std.fs.File,
    reader: ReaderType,
    buf_reader: BufReaderType,
    stream: ?BufReaderReaderType,
    buf: [64]u8,

    pub fn next(self: *@This()) !?[]u8 {
        if (self.stream == null) {
            self.stream = self.buf_reader.reader();
        }
        if (self.stream) |stream| {
            return stream.readUntilDelimiterOrEof(&self.buf, '\n');
        }
        unreachable;
    }

    pub fn deinit(self: *@This()) void {
        self.file.close();
    }
};

pub fn readByLine(filename: []const u8) !ReadByLineIterator {
    var file = try std.fs.cwd().openFile(filename, .{});
    const reader = file.reader();
    const buf_reader = std.io.bufferedReader(reader);
    return ReadByLineIterator{
        .file = file,
        .reader = reader,
        .buf_reader = buf_reader,
        .stream = null,
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

    const filename = args[1];
    var lines = try readByLine(filename);

    var line_count: usize = 0;
    var max_line_length: usize = 0;

    while (try lines.next()) |line| {
        line_count += 1;
        max_line_length = @max(max_line_length, line.len);
    }

    lines = try readByLine(filename);
    defer lines.deinit();

    var grid = try allocator.alloc([]u8, line_count);
    defer {
        for (grid) |row| {
            allocator.free(row);
        }
        allocator.free(grid);
    }

    var row: usize = 0;
    while (try lines.next()) |line| : (row += 1) {
        grid[row] = try allocator.alloc(u8, max_line_length);
        @memset(grid[row], ' '); // Pad with spaces
        @memcpy(grid[row][0..line.len], line);
    }

    const slopes = [_]Slope{
        Slope{ .right = 1, .down = 1 },
        Slope{ .right = 3, .down = 1 },
        Slope{ .right = 5, .down = 1 },
        Slope{ .right = 7, .down = 1 },
        Slope{ .right = 1, .down = 2 },
    };

    var part1_result: u64 = 0;
    var part2_result: u64 = 1;

    for (slopes) |slope| {
        var x: usize = 0;
        var y: usize = 0;
        var trees: u64 = 0;

        while (y < line_count) : (y += slope.down) {
            if (grid[y][x] == '#') {
                trees += 1;
            }

            x = (x + slope.right) % max_line_length;
        }

        if (slope.right == 3 and slope.down == 1) {
            part1_result = trees;
        }

        part2_result *= trees;
    }

    std.debug.print("Part 1: {}\n", .{part1_result});
    std.debug.print("Part 2: {}\n", .{part2_result});
}
const Slope = struct {
    right: usize,
    down: usize,
};
