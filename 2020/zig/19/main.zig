const std = @import("std");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const allocator = std.heap.page_allocator;

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len < 2) return error.InvalidArgCount;

    const file = try std.fs.cwd().openFile(args[1], .{});
    defer file.close();
    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var rules = std.StringHashMap(Rule).init(allocator);
    var lines = std.mem.split(u8, content, "\n");
    var messages = std.ArrayList([]const u8).init(allocator);

    // Parse rules
    while (lines.next()) |line| {
        if (line.len == 0) break;
        const rule_data = try parseRule(line);
        try rules.put(rule_data.@"0", rule_data.@"1");
    }

    // Collect messages
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        try messages.append(line);
    }

    // Part 1
    var count: usize = 0;
    for (messages.items) |msg| {
        if (rules.get("0")) |rule| {
            if (rule.matches(rules, msg, 0, 0)) |end_pos| {
                if (end_pos == msg.len) count += 1;
            }
        }
    }
    try stdout.print("Part 1: {}\n", .{count});

    // Part 2
    count = 0;
    for (messages.items) |msg| {
        if (Rule.matchPart2(rules, msg, 0)) count += 1;
    }
    try stdout.print("Part 2: {}\n", .{count});
}

const Rule = union(enum) {
    literal: u8,
    subrules: [][]usize,

    fn matches(self: Rule, rules: std.StringHashMap(Rule), msg: []const u8, pos: usize, depth: usize) ?usize {
        if (depth > 15 or pos >= msg.len) return null;

        switch (self) {
            .literal => |c| {
                if (msg[pos] == c) return pos + 1;
                return null;
            },
            .subrules => |sequences| {
                for (sequences) |seq| {
                    var curr_pos = pos;
                    var valid = true;

                    for (seq) |rule_num| {
                        var key_buf: [10]u8 = undefined;
                        const key = std.fmt.bufPrint(&key_buf, "{d}", .{rule_num}) catch return null;
                        const rule = rules.get(key) orelse return null;

                        if (rule.matches(rules, msg, curr_pos, depth + 1)) |new_pos| {
                            curr_pos = new_pos;
                        } else {
                            valid = false;
                            break;
                        }
                    }
                    if (valid) return curr_pos;
                }
                return null;
            },
        }
    }

    fn matchRule42(rules: std.StringHashMap(Rule), msg: []const u8, pos: usize) ?usize {
        const rule = rules.get("42") orelse return null;
        return rule.matches(rules, msg, pos, 0);
    }

    fn matchRule31(rules: std.StringHashMap(Rule), msg: []const u8, pos: usize) ?usize {
        const rule = rules.get("31") orelse return null;
        return rule.matches(rules, msg, pos, 0);
    }

    fn matchPart2(rules: std.StringHashMap(Rule), msg: []const u8, pos: usize) bool {
        var count42: usize = 0;
        var count31: usize = 0;
        var curr_pos = pos;

        // Match one or more rule 42s
        while (curr_pos < msg.len) {
            if (matchRule42(rules, msg, curr_pos)) |new_pos| {
                curr_pos = new_pos;
                count42 += 1;
            } else break;
        }

        // Match rule 31s
        while (curr_pos < msg.len) {
            if (matchRule31(rules, msg, curr_pos)) |new_pos| {
                curr_pos = new_pos;
                count31 += 1;
            } else break;
        }

        // Valid if:
        // 1. We consumed the entire message
        // 2. We have at least one rule 31
        // 3. We have more rule 42s than rule 31s
        // 4. We have at least two rule 42s (one for rule 8 and one for rule 11)
        return curr_pos == msg.len and count31 > 0 and count42 > count31 and count42 >= 2;
    }
};

fn parseRule(line: []const u8) !struct { []const u8, Rule } {
    var parts = std.mem.split(u8, line, ": ");
    const num = parts.next().?;
    const rule = parts.next().?;

    if (rule[0] == '"') return .{ num, .{ .literal = rule[1] } };

    var seqs = std.ArrayList([]usize).init(std.heap.page_allocator);
    var parts_it = std.mem.split(u8, rule, " | ");
    while (parts_it.next()) |part| {
        var nums = std.ArrayList(usize).init(std.heap.page_allocator);
        var nums_it = std.mem.split(u8, part, " ");
        while (nums_it.next()) |n| try nums.append(try std.fmt.parseInt(usize, n, 10));
        try seqs.append(nums.items);
    }
    return .{ num, .{ .subrules = seqs.items } };
}
