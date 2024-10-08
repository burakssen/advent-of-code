const std = @import("std");

fn is_equal(window: []u8) bool {
    return window[0] == window[1];
}

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const allocator = std.heap.page_allocator;
    const arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        try stdout.print("Error: Please provide a file path as an argument.\n", .{});
        return;
    }

    const file_path = args[1];
    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    // Buffer to read the file contents
    var buffer: [10000]u8 = undefined;

    const bytes_read = try file.readAll(buffer[0..]);
    const digit_sequence = buffer[0..bytes_read];

    var sum_part1: i32 = 0;
    var sum_part2: i32 = 0;
    const len = digit_sequence.len;

    // Part 1: Check each digit against the next digit, considering the circular nature
    for (0..len) |i| {
        const current_digit = digit_sequence[i];
        const next_digit = digit_sequence[(i + 1) % len]; // Wrap around using modulo
        if (current_digit == next_digit) {
            // Convert the character to an integer ('0' -> 0, '1' -> 1, ..., '9' -> 9)
            sum_part1 += current_digit - '0';
        }
    }

    // Part 2: Check each digit against the digit halfway around the circular list
    const half_len = len / 2; // Since the problem states that the list has an even number of elements
    for (0..len) |i| {
        const current_digit = digit_sequence[i];
        const halfway_digit = digit_sequence[(i + half_len) % len]; // Wrap around using modulo
        if (current_digit == halfway_digit) {
            sum_part2 += current_digit - '0';
        }
    }

    try stdout.print("Part 1: {}\n", .{sum_part1});
    try stdout.print("Part 2: {}\n", .{sum_part2});
}
