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
    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer std.process.argsFree(std.heap.page_allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: {s} [filename]\n", .{args[0]});
        return;
    }

    const filename = args[1];
    var lines = try readByLine(filename);
    defer lines.deinit();

    var part1_count: usize = 0;
    var part2_count: usize = 0;
    while (try lines.next()) |line| {
        const l = try Line.parse(line);
        if (l.isValidPart1()) {
            part1_count += 1;
        }

        if (l.isValidPart2()) {
            part2_count += 1;
        }
    }

    std.debug.print("Part 1: {}\n", .{part1_count});
    std.debug.print("Part 2: {}\n", .{part2_count});
}

pub const Line = struct {
    min: u32,
    max: u32,
    letter: u8,
    password: []const u8,

    pub fn parse(input: []const u8) !Line {
        var parts = std.mem.tokenize(u8, input, " :-");
        const min = try std.fmt.parseInt(u32, parts.next() orelse return error.InvalidInput, 10);
        const max = try std.fmt.parseInt(u32, parts.next() orelse return error.InvalidInput, 10);
        const letter_str = parts.next() orelse return error.InvalidInput;
        const letter = letter_str[0];
        const password = parts.next() orelse return error.InvalidInput;

        return Line{ .min = min, .max = max, .letter = letter, .password = password };
    }

    pub fn isValidPart1(self: Line) bool {
        const count = std.mem.count(u8, self.password, &[_]u8{self.letter});
        return count >= self.min and count <= self.max;
    }

    pub fn isValidPart2(self: Line) bool {
        const a = self.password[self.min - 1] == self.letter;
        const b = self.password[self.max - 1] == self.letter;
        return a != b;
    }
};
