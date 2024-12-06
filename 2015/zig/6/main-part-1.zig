const std = @import("std");

const Pos = struct {
    x: usize,
    y: usize,
};

const Action = struct {
    action_type: []const u8,
    start: Pos,
    end: Pos,
};

const SplitResult = struct {
    parts: [5][]const u8,
    num_parts: usize,
};

fn split_string(input: []const u8, sep: u8) SplitResult {
    var result = SplitResult{
        .parts = undefined,
        .num_parts = 0,
    };

    var start: usize = 0;
    result.parts = [_][]u8{""} ** 5;
    for (input, 0..) |c, i| {
        if (c == sep) {
            result.parts[result.num_parts] = input[start..i];
            result.num_parts += 1;
            start = i + 1;
        }
    }

    if (start < input.len) {
        result.parts[result.num_parts] = input[start..];
        result.num_parts += 1;
    }

    return result;
}

fn get_action_type(line: []const u8) !Action {
    var action: Action = undefined;

    const result = split_string(line, ' ');

    if (result.num_parts == 4) {
        action.action_type = result.parts[0];
        const start_parts = split_string(result.parts[1], ',');
        action.start.x = try std.fmt.parseInt(usize, start_parts.parts[0], 10);
        action.start.y = try std.fmt.parseInt(usize, start_parts.parts[1], 10);
        const end_parts = split_string(result.parts[3], ',');
        action.end.x = try std.fmt.parseInt(usize, end_parts.parts[0], 10);
        action.end.y = try std.fmt.parseInt(usize, end_parts.parts[1], 10);
    } else if (result.num_parts == 5) {
        action.action_type = result.parts[1];
        const start_parts = split_string(result.parts[2], ',');
        action.start.x = try std.fmt.parseInt(usize, start_parts.parts[0], 10);
        action.start.y = try std.fmt.parseInt(usize, start_parts.parts[1], 10);
        const end_parts = split_string(result.parts[4], ',');
        action.end.x = try std.fmt.parseInt(usize, end_parts.parts[0], 10);
        action.end.y = try std.fmt.parseInt(usize, end_parts.parts[1], 10);
    }

    return action;
}

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

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var line: [33]u8 = undefined;

    var light_grid: [1000][1000]bool = undefined;
    while (true) {
        const line_bytes = try in_stream.readUntilDelimiterOrEof(line[0..], '\n');

        if (line_bytes == null) {
            break;
        }

        if (line_bytes) |line_b| {
            const action = try get_action_type(line_b);

            for (action.start.x..action.end.x + 1) |x| {
                for (action.start.y..action.end.y + 1) |y| {
                    if (std.mem.eql(u8, action.action_type, "on")) {
                        light_grid[x][y] = true;
                    } else if (std.mem.eql(u8, action.action_type, "off")) {
                        light_grid[x][y] = false;
                    } else if (std.mem.eql(u8, action.action_type, "toggle")) {
                        light_grid[x][y] = !light_grid[x][y];
                    }
                }
            }
        }
    }

    var num_lights_on: i32 = 0;
    for (light_grid) |row| {
        for (row) |light| {
            if (light) {
                num_lights_on += 1;
            }
        }
    }

    try stdout.print("Part 1: {}\n", .{num_lights_on});
}
