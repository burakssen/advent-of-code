const std = @import("std");

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

    const file_contents = try std.fs.cwd().readFileAlloc(allocator, args[1], 1_000_000);
    defer allocator.free(file_contents);

    var lines = std.mem.splitScalar(u8, file_contents, '\n');
    var containers = std.ArrayList(Container).init(allocator);
    defer {
        for (containers.items) |*container| {
            allocator.free(container.name);
            for (container.contains.items) |inner_bag| {
                allocator.free(inner_bag);
            }
            container.deinit();
        }
        containers.deinit();
    }

    while (lines.next()) |line| {
        if (line.len == 0) continue;

        const split_index = std.mem.indexOf(u8, line, " bags contain ") orelse continue;
        const outer_bag = line[0..split_index];
        const inner_bags_str = line[split_index + " bags contain ".len .. line.len];

        var ibs = std.mem.splitSequence(u8, inner_bags_str, ", ");

        var container = Container.init(allocator);
        container.name = try allocator.dupe(u8, outer_bag);

        while (ibs.next()) |ib| {
            var inner_bag = std.mem.trim(u8, ib, " ");

            if (std.mem.eql(u8, inner_bag, "no other bags.")) {
                break;
            }

            if (std.mem.endsWith(u8, inner_bag, ".")) {
                inner_bag = inner_bag[0 .. inner_bag.len - 1];
            }

            if (std.mem.endsWith(u8, inner_bag, " bags")) {
                inner_bag = inner_bag[0 .. inner_bag.len - " bags".len];
            }

            if (std.mem.endsWith(u8, inner_bag, " bag")) {
                inner_bag = inner_bag[0 .. inner_bag.len - " bag".len];
            }

            // Skip the quantity at the start
            const bag_name = inner_bag[2..];
            const quantity = inner_bag[0] - '0';
            try container.quantities.append(quantity);
            try container.contains.append(try allocator.dupe(u8, bag_name));
        }

        container.num_contains = container.contains.items.len;
        try containers.append(container);
    }

    const part1_count: usize = try countShinyGoldBags(allocator, containers.items);
    const part2_count: usize = try countBagsInside(allocator, containers.items, "shiny gold");
    std.debug.print("Part 1: {d}\n", .{part1_count});
    std.debug.print("Part 2: {d}\n", .{part2_count});
}

const Container = struct {
    name: []const u8,
    num_contains: usize,
    contains: std.ArrayList([]const u8),
    quantities: std.ArrayList(usize),

    pub fn init(allocator: std.mem.Allocator) Container {
        return Container{
            .name = undefined,
            .num_contains = 0,
            .contains = std.ArrayList([]const u8).init(allocator),
            .quantities = std.ArrayList(usize).init(allocator),
        };
    }

    pub fn deinit(self: *Container) void {
        self.contains.deinit();
        self.quantities.deinit();
    }
};

fn countShinyGoldBags(allocator: std.mem.Allocator, containers: []Container) !usize {
    var can_contain_shiny_gold = std.ArrayList(bool).init(allocator);
    defer can_contain_shiny_gold.deinit();

    try can_contain_shiny_gold.appendNTimes(false, containers.len);
    var changed = true;
    while (changed) {
        changed = false;
        for (containers, 0..) |*container, i| {
            if (can_contain_shiny_gold.items[i]) continue;

            for (container.contains.items) |inner_bag| {
                if (std.mem.eql(u8, inner_bag, "shiny gold")) {
                    can_contain_shiny_gold.items[i] = true;
                    changed = true;
                    break;
                }

                for (containers, 0..) |*potential_container, j| {
                    if (can_contain_shiny_gold.items[j] and
                        std.mem.eql(u8, potential_container.name, inner_bag))
                    {
                        can_contain_shiny_gold.items[i] = true;
                        changed = true;
                        break;
                    }
                }
                if (changed) break;
            }
        }
    }

    var count: usize = 0;
    for (can_contain_shiny_gold.items) |can_contain| {
        if (can_contain) count += 1;
    }

    return count;
}

fn countBagsInside(allocator: std.mem.Allocator, containers: []Container, container_name: []const u8) !usize {
    var container_index: usize = 0;
    for (containers, 0..) |*container, i| {
        if (std.mem.eql(u8, container.name, container_name)) {
            container_index = i;
            break;
        }
    }

    var count: usize = 0;
    for (containers[container_index].contains.items, 0..) |inner_bag, i| {
        count += containers[container_index].quantities.items[i];
        count += containers[container_index].quantities.items[i] * try countBagsInside(allocator, containers, inner_bag);
    }

    return count;
}
