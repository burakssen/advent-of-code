const std = @import("std");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer std.process.argsFree(std.heap.page_allocator, args);

    if (args.len < 2) {
        try stdout.print("Error: Please provide a file path as an argument.\n", .{});
        return;
    }

    const file_path = args[1];
    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    var buffered_reader = std.io.bufferedReader(file.reader());
    var reader = buffered_reader.reader();
    var buffer: [32]u8 = undefined;

    var paper_size: i32 = 0;
    var ribbon_length: i32 = 0;
    while (true) {
        const line_opt = reader.readUntilDelimiterOrEof(buffer[0..], '\n') catch |err| {
            try stdout.print("Error reading file: {}\n", .{err});
            return;
        };

        if (line_opt) |line| {
            const delim = 'x';
            var iter = std.mem.split(u8, line, &[_]u8{delim});

            var dimensions: [3]i32 = undefined;
            var dimensi: usize = 0;

            while (iter.next()) |part| {
                const val = std.fmt.parseInt(i32, part, 10) catch |err| {
                    try stdout.print("Error parsing integer: {}\n", .{err});
                    return;
                };

                dimensions[dimensi] = val;
                dimensi += 1;
            }

            if (dimensi != 3) {
                try stdout.print("Error: Expected 3 dimensions, got {}\n", .{dimensi});
                return;
            }

            const l = dimensions[0];
            const w = dimensions[1];
            const h = dimensions[2];

            var sides = [_]i32{ l, w, h };

            std.mem.sort(i32, &sides, {}, std.sort.asc(i32));

            ribbon_length += 2 * sides[0] + 2 * sides[1] + l * w * h;

            const lw = l * w;
            const wh = w * h;
            const hl = h * l;

            const area = 2 * l * w + 2 * w * h + 2 * h * l;
            var slack = lw;
            if (wh < slack) {
                slack = wh;
            }
            if (hl < slack) {
                slack = hl;
            }

            paper_size += area + slack;
        } else {
            break;
        }
    }

    try stdout.print("Part 1: {}\n", .{paper_size});
    try stdout.print("Part 2: {}\n", .{ribbon_length});
}
