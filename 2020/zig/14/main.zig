const std = @import("std");

const Mask = struct {
    or_mask: u64 = 0,
    and_mask: u64 = ~@as(u64, 0),
    floating: std.ArrayList(u6),

    fn init(allocator: std.mem.Allocator) Mask {
        return .{
            .floating = std.ArrayList(u6).init(allocator),
        };
    }

    fn deinit(self: *Mask) void {
        self.floating.deinit();
    }

    fn update(self: *Mask, line: []const u8, comptime part2: bool) !void {
        self.or_mask = 0;
        self.and_mask = ~@as(u64, 0);
        self.floating.clearRetainingCapacity();

        const mask = line[7..];
        for (mask, 0..) |c, i| {
            const bit_pos: u6 = @intCast(35 - i);
            switch (c) {
                '1' => {
                    if (part2) {
                        self.or_mask |= @as(u64, 1) << bit_pos;
                    } else {
                        self.or_mask |= @as(u64, 1) << bit_pos;
                        self.and_mask |= @as(u64, 1) << bit_pos;
                    }
                },
                '0' => if (!part2) {
                    self.and_mask &= ~(@as(u64, 1) << bit_pos);
                },
                'X' => if (part2) {
                    try self.floating.append(bit_pos);
                },
                else => unreachable,
            }
        }
    }

    fn applyPart1(self: *const Mask, value: u64) u64 {
        return (value & self.and_mask) | self.or_mask;
    }

    fn applyPart2(self: *const Mask, addr: u64) !std.ArrayList(u64) {
        var addresses = std.ArrayList(u64).init(self.floating.allocator);
        const combinations = @as(u64, 1) << @as(u6, @intCast(self.floating.items.len));

        var i: u64 = 0;
        while (i < combinations) : (i += 1) {
            var final_addr = addr | self.or_mask;
            for (self.floating.items, 0..) |bit_pos, j| {
                if ((i >> @intCast(j)) & 1 == 1) {
                    final_addr |= @as(u64, 1) << bit_pos;
                } else {
                    final_addr &= ~(@as(u64, 1) << bit_pos);
                }
            }
            try addresses.append(final_addr);
        }
        return addresses;
    }
};

fn processMemory(content: []const u8, allocator: std.mem.Allocator, comptime part2: bool) !u64 {
    var lines = std.mem.splitScalar(u8, content, '\n');
    var memory = std.AutoHashMap(u64, u64).init(allocator);
    defer memory.deinit();

    var mask = Mask.init(allocator);
    defer mask.deinit();

    while (lines.next()) |line| {
        if (line.len == 0) continue;
        if (std.mem.startsWith(u8, line, "mask")) {
            try mask.update(line, part2);
        } else {
            var it = std.mem.splitSequence(u8, line, "] = ");
            const addr = try std.fmt.parseInt(u64, it.next().?[4..], 10);
            const value = try std.fmt.parseInt(u64, it.next().?, 10);

            if (part2) {
                var addresses = try mask.applyPart2(addr);
                defer addresses.deinit();
                for (addresses.items) |final_addr| {
                    try memory.put(final_addr, value);
                }
            } else {
                try memory.put(addr, mask.applyPart1(value));
            }
        }
    }

    var sum: u64 = 0;
    var value_it = memory.valueIterator();
    while (value_it.next()) |value| {
        sum += value.*;
    }
    return sum;
}

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

    try stdout.print("Part 1: {d}\n", .{try processMemory(content, allocator, false)});
    try stdout.print("Part 2: {d}\n", .{try processMemory(content, allocator, true)});
}
