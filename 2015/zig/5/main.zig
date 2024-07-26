const std = @import("std");

pub fn main() !void {
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

    // read the file
    var buffer: [17]u8 = undefined;

    var part1_count: i32 = 0;
    var part2_count: i32 = 0;

    while (true) {
        const n = try file.read(buffer[0..]);

        if (n == 0) {
            break;
        }

        if (check_good_string_part1(buffer[0..n])) {
            part1_count += 1;
        }

        if (check_good_string_part2(buffer[0..n])) {
            part2_count += 1;
        }
    }

    try stdout.print("Part 1: {}\n", .{part1_count});
    try stdout.print("Part 2: {}\n", .{part2_count});
}

pub fn check_good_string_part1(s: []const u8) bool {
    var vowels: i64 = 0;
    var double_letter: bool = false;
    var bad_string: bool = false;

    for (0..s.len) |i| {
        if (s[i] == 'a' or s[i] == 'e' or s[i] == 'i' or s[i] == 'o' or s[i] == 'u') {
            vowels += 1;
        }

        if (i > 0) {
            if (s[i] == s[i - 1]) {
                double_letter = true;
            }

            if ((s[i] == 'b' and s[i - 1] == 'a') or
                (s[i] == 'd' and s[i - 1] == 'c') or
                (s[i] == 'q' and s[i - 1] == 'p') or
                (s[i] == 'y' and s[i - 1] == 'x'))
            {
                bad_string = true;
            }
        }
    }

    return vowels >= 3 and double_letter and !bad_string;
}

pub fn check_good_string_part2(s: []const u8) bool {
    var pair: bool = false;
    var repeat: bool = false;

    for (0..(s.len - 1)) |i| {
        if (i < s.len - 2 and s[i] == s[i + 2]) {
            repeat = true;
        }

        var j = i + 2;
        while (j < s.len - 1) : (j += 1) {
            if (s[i] == s[j] and s[i + 1] == s[j + 1]) {
                pair = true;
                break;
            }
        }
    }

    return pair and repeat;
}
