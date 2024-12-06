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

    // Part 1 variables
    var group_answers_part1 = std.ArrayList(u8).init(allocator);
    defer group_answers_part1.deinit();
    var part1_sum: usize = 0;

    // Part 2 variables
    var group_answers_part2 = std.ArrayList(u8).init(allocator);
    defer group_answers_part2.deinit();
    var part2_sum: usize = 0;
    var is_new_group = true;

    while (try lines.next()) |line| {
        if (line.len == 0) {
            // Process end of group for both parts
            part1_sum += group_answers_part1.items.len;
            part2_sum += group_answers_part2.items.len;

            // Reset for next group
            group_answers_part1.clearRetainingCapacity();
            group_answers_part2.clearRetainingCapacity();
            is_new_group = true;
            continue;
        }

        // Part 1: Union of answers
        for (line) |c| {
            if (std.mem.indexOfScalar(u8, group_answers_part1.items, c) == null) {
                try group_answers_part1.append(c);
            }
        }

        // Part 2: Intersection of answers
        if (is_new_group) {
            // Initialize with the first person's answers
            try group_answers_part2.appendSlice(line);
            is_new_group = false;
        } else {
            // Retain only the common answers
            var i: usize = 0;
            while (i < group_answers_part2.items.len) {
                if (std.mem.indexOfScalar(u8, line, group_answers_part2.items[i]) == null) {
                    _ = group_answers_part2.swapRemove(i);
                } else {
                    i += 1;
                }
            }
        }
    }

    // Final group processing
    part1_sum += group_answers_part1.items.len;
    part2_sum += group_answers_part2.items.len;

    std.debug.print("Part 1: {}\n", .{part1_sum});
    std.debug.print("Part 2: {}\n", .{part2_sum});
}
