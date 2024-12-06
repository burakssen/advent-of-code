const std = @import("std");

const ReadByLineIterator = struct {
    file: std.fs.File,
    buf_reader: std.io.BufferedReader(4096, std.fs.File.Reader),
    buf: [1024]u8,

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
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: {s} [filename]\n", .{args[0]});
        return;
    }

    var lines = try readByLine(args[1]);
    defer lines.deinit();

    var passport_lines = std.ArrayList([]const u8).init(allocator);
    defer {
        for (passport_lines.items) |line| allocator.free(line);
        passport_lines.deinit();
    }

    var passports = std.ArrayList([]const u8).init(allocator);
    defer {
        for (passports.items) |passport| allocator.free(passport);
        passports.deinit();
    }

    while (try lines.next()) |line| {
        if (line.len == 0) {
            const passport_str = try std.mem.join(allocator, " ", passport_lines.items);
            try passports.append(passport_str);
            for (passport_lines.items) |pl| allocator.free(pl);
            passport_lines.clearRetainingCapacity();
        } else {
            try passport_lines.append(try allocator.dupe(u8, line));
        }
    }

    if (passport_lines.items.len > 0) {
        const passport_str = try std.mem.join(allocator, " ", passport_lines.items);
        try passports.append(passport_str);
    }

    var part1_valid_count: usize = 0;
    var part2_valid_count: usize = 0;

    for (passports.items) |passport| {
        part1_valid_count += @intFromBool(try isPassportValid(passport, false));
        part2_valid_count += @intFromBool(try isPassportValid(passport, true));
    }

    std.debug.print("Part 1: {}\n", .{part1_valid_count});
    std.debug.print("Part 2: {}\n", .{part2_valid_count});
}

fn isPassportValid(passport: []const u8, part2: bool) !bool {
    const required_fields = [_][]const u8{ "byr:", "iyr:", "eyr:", "hgt:", "hcl:", "ecl:", "pid:" };

    // Check presence of required fields
    for (required_fields) |field| {
        if (std.mem.indexOf(u8, passport, field) == null) return false;
    }

    if (!part2) return true;

    const validations = .{
        .byr = struct {
            fn validate(value: []const u8) !bool {
                const year = try std.fmt.parseInt(u32, value, 10);
                return year >= 1920 and year <= 2002;
            }
        }.validate,
        .iyr = struct {
            fn validate(value: []const u8) !bool {
                const year = try std.fmt.parseInt(u32, value, 10);
                return year >= 2010 and year <= 2020;
            }
        }.validate,
        .eyr = struct {
            fn validate(value: []const u8) !bool {
                const year = try std.fmt.parseInt(u32, value, 10);
                return year >= 2020 and year <= 2030;
            }
        }.validate,
        .hgt = struct {
            fn validate(value: []const u8) !bool {
                if (value.len < 3) return false;
                const unit = value[value.len - 2 ..];
                const val = value[0 .. value.len - 2];
                const height = try std.fmt.parseInt(u32, val, 10);
                return if (std.mem.eql(u8, unit, "cm"))
                    height >= 150 and height <= 193
                else if (std.mem.eql(u8, unit, "in"))
                    height >= 59 and height <= 76
                else
                    false;
            }
        }.validate,
        .hcl = struct {
            fn validate(value: []const u8) bool {
                if (value.len != 7 or value[0] != '#') return false;
                for (value[1..]) |c| {
                    if (!std.ascii.isHex(c)) return false;
                }
                return true;
            }
        }.validate,
        .ecl = struct {
            fn validate(value: []const u8) bool {
                const valid_colors = [_][]const u8{ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" };
                return for (valid_colors) |color| {
                    if (std.mem.eql(u8, value, color)) break true;
                } else false;
            }
        }.validate,
        .pid = struct {
            fn validate(value: []const u8) bool {
                if (value.len != 9) return false;
                for (value) |c| {
                    if (!std.ascii.isDigit(c)) return false;
                }
                return true;
            }
        }.validate,
    };

    var passport_fields = std.mem.split(u8, passport, " ");
    while (passport_fields.next()) |field| {
        const field_name = field[0..3];
        const field_value = field[4..];

        inline for (std.meta.fields(@TypeOf(validations))) |f| {
            if (std.mem.eql(u8, field_name, f.name)) {
                const validation_fn = @field(validations, f.name);
                const result = switch (@typeInfo(@TypeOf(validation_fn))) {
                    .Fn => |fn_info| blk: {
                        if (fn_info.return_type) |return_type| {
                            if (return_type == bool) {
                                break :blk validation_fn(field_value);
                            } else {
                                break :blk try validation_fn(field_value);
                            }
                        }
                        unreachable;
                    },
                    else => unreachable,
                };

                if (!result) return false;
            }
        }
    }

    return true;
}
