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

    // Create a list of integers from the lines
    const allocator = std.heap.page_allocator;
    var numbers = std.ArrayList(i32).init(allocator);
    defer numbers.deinit();

    while (try lines.next()) |line| {
        const num = try std.fmt.parseInt(i32, line, 10);
        try numbers.append(num);
    }

    std.mem.sort(i32, numbers.items, {}, comptime std.sort.asc(i32));

    if (findTwoNumbersWithSum(numbers.items, 2020)) |result| {
        std.debug.print("Part 1: {d}\n", .{result.a * result.b});
    }

    if (findThreeNumbersWithSum(numbers.items, 2020)) |result| {
        std.debug.print("Part 2: {d}\n", .{result.a * result.b * result.c});
    }
}

fn findTwoNumbersWithSum(numbers: []const i32, target_sum: i32) ?(struct { a: i32, b: i32 }) {
    var left: usize = 0;
    var right: usize = numbers.len - 1;

    while (left < right) {
        const sum = numbers[left] + numbers[right];
        if (sum == target_sum) return .{ .a = numbers[left], .b = numbers[right] };
        if (sum < target_sum) left += 1 else right -= 1;
    }

    return null;
}

fn findThreeNumbersWithSum(numbers: []const i32, target_sum: i32) ?(struct { a: i32, b: i32, c: i32 }) {
    for (numbers, 0..) |first, i| {
        var left = i + 1;
        var right = numbers.len - 1;

        while (left < right) {
            const sum = first + numbers[left] + numbers[right];
            if (sum == target_sum) return .{ .a = first, .b = numbers[left], .c = numbers[right] };
            if (sum < target_sum) left += 1 else right -= 1;
        }
    }

    return null;
}
