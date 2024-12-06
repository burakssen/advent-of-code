const std = @import("std");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len < 2) return error.InvalidArgCount;

    const file = try std.fs.cwd().openFile(args[1], .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    // Parse rules
    var rules = try parseRules(content, allocator);
    defer rules.deinit();

    // Skip to my ticket
    var sections = std.mem.splitSequence(u8, content, "\n\n");
    _ = sections.next().?; // Skip rules section

    // Parse my ticket
    const my_ticket_section = sections.next().?;
    var my_ticket_lines = std.mem.splitScalar(u8, my_ticket_section, '\n');
    _ = my_ticket_lines.next().?; // Skip "your ticket:" line
    var my_ticket = try parseTicket(my_ticket_lines.next().?, allocator);
    defer my_ticket.deinit();

    // Parse nearby tickets
    const nearby_section = sections.next().?;
    var nearby_lines = std.mem.splitScalar(u8, nearby_section, '\n');
    _ = nearby_lines.next().?; // Skip "nearby tickets:" line

    var nearby_tickets = std.ArrayList(Ticket).init(allocator);
    defer {
        for (nearby_tickets.items) |*ticket| {
            ticket.deinit();
        }
        nearby_tickets.deinit();
    }

    // Part 1: Find invalid tickets
    var invalid_sum: u32 = 0;
    while (nearby_lines.next()) |line| {
        if (line.len == 0) continue;
        var ticket = try parseTicket(line, allocator);
        invalid_sum += findInvalidSum(ticket, rules.items);
        if (isValidTicket(ticket, rules.items)) {
            try nearby_tickets.append(ticket);
        } else {
            ticket.deinit();
        }
    }

    try stdout.print("Part 1: {}\n", .{invalid_sum});

    // Part 2: Determine field positions
    var valid_tickets = try allocator.alloc(Ticket, nearby_tickets.items.len + 1);
    defer allocator.free(valid_tickets);
    @memcpy(valid_tickets[0..nearby_tickets.items.len], nearby_tickets.items);
    valid_tickets[nearby_tickets.items.len] = my_ticket;

    const positions = try determineFieldPositions(valid_tickets, rules.items, allocator);
    defer allocator.free(positions);

    // Calculate product of departure fields
    var result: u64 = 1;
    for (rules.items, 0..) |rule, i| {
        if (std.mem.startsWith(u8, rule.name, "departure")) {
            result *= my_ticket.values.items[positions[i]];
        }
    }

    try stdout.print("Part 2: {}\n", .{result});
}

const Rule = struct {
    name: []const u8,
    ranges: [2][2]u32,

    fn isValid(self: Rule, value: u32) bool {
        return (value >= self.ranges[0][0] and value <= self.ranges[0][1]) or
            (value >= self.ranges[1][0] and value <= self.ranges[1][1]);
    }
};

const Ticket = struct {
    values: std.ArrayList(u32),

    fn init(allocator: std.mem.Allocator) Ticket {
        return .{ .values = std.ArrayList(u32).init(allocator) };
    }

    fn deinit(self: *Ticket) void {
        self.values.deinit();
    }
};

fn parseRules(content: []const u8, allocator: std.mem.Allocator) !std.ArrayList(Rule) {
    var rules = std.ArrayList(Rule).init(allocator);
    var lines = std.mem.splitScalar(u8, content, '\n');

    while (lines.next()) |line| {
        if (line.len == 0) break;
        var parts = std.mem.splitSequence(u8, line, ": ");
        const name = parts.next().?;
        const ranges_str = parts.next().?;

        var ranges_parts = std.mem.splitSequence(u8, ranges_str, " or ");
        var rule = Rule{ .name = name, .ranges = undefined };

        inline for (0..2) |i| {
            const range_str = ranges_parts.next().?;
            var range_parts = std.mem.splitSequence(u8, range_str, "-");
            rule.ranges[i][0] = try std.fmt.parseInt(u32, range_parts.next().?, 10);
            rule.ranges[i][1] = try std.fmt.parseInt(u32, range_parts.next().?, 10);
        }

        try rules.append(rule);
    }
    return rules;
}

fn parseTicket(line: []const u8, allocator: std.mem.Allocator) !Ticket {
    var ticket = Ticket.init(allocator);
    var numbers = std.mem.splitScalar(u8, line, ',');
    while (numbers.next()) |num| {
        try ticket.values.append(try std.fmt.parseInt(u32, num, 10));
    }
    return ticket;
}

fn isValidForAnyRule(value: u32, rules: []const Rule) bool {
    for (rules) |rule| {
        if (rule.isValid(value)) return true;
    }
    return false;
}

fn findInvalidSum(ticket: Ticket, rules: []const Rule) u32 {
    var sum: u32 = 0;
    for (ticket.values.items) |value| {
        if (!isValidForAnyRule(value, rules)) {
            sum += value;
        }
    }
    return sum;
}

fn isValidTicket(ticket: Ticket, rules: []const Rule) bool {
    for (ticket.values.items) |value| {
        if (!isValidForAnyRule(value, rules)) {
            return false;
        }
    }
    return true;
}

fn determineFieldPositions(tickets: []const Ticket, rules: []const Rule, allocator: std.mem.Allocator) ![]usize {
    const field_count = rules.len;
    const possible_positions = try allocator.alloc(std.ArrayList(usize), field_count);
    defer {
        for (possible_positions) |*list| {
            list.deinit();
        }
        allocator.free(possible_positions);
    }

    // Initialize possible positions for each rule
    for (possible_positions, 0..) |*positions, i| {
        positions.* = std.ArrayList(usize).init(allocator);
        var pos: usize = 0;
        while (pos < field_count) : (pos += 1) {
            var valid = true;
            for (tickets) |ticket| {
                if (!rules[i].isValid(ticket.values.items[pos])) {
                    valid = false;
                    break;
                }
            }
            if (valid) {
                try positions.append(pos);
            }
        }
    }

    var result = try allocator.alloc(usize, field_count);
    var found = try allocator.alloc(bool, field_count);
    @memset(found, false);
    defer allocator.free(found);

    // Solve positions
    var solved: usize = 0;
    while (solved < field_count) {
        for (possible_positions, 0..) |positions, i| {
            if (positions.items.len == 1 and !found[i]) {
                const pos = positions.items[0];
                result[i] = pos;
                found[i] = true;
                solved += 1;

                // Remove this position from other rules
                for (possible_positions, 0..) |*other_positions, j| {
                    if (i != j) {
                        var k: usize = 0;
                        while (k < other_positions.items.len) {
                            if (other_positions.items[k] == pos) {
                                _ = other_positions.swapRemove(k);
                            } else {
                                k += 1;
                            }
                        }
                    }
                }
            }
        }
    }

    return result;
}
