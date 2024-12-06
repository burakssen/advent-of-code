const std = @import("std");
const print = std.debug.print;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        print("Usage: {s} <input_file>\n", .{args[0]});
        return error.InvalidArgCount;
    }

    const file = try std.fs.cwd().openFile(args[1], .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var tiles: [12][12]Tile = undefined;
    for (&tiles) |*row| {
        for (row) |*tile| {
            tile.* = Tile.init(0, [_][10]u8{[_]u8{0} ** 10} ** 10);
        }
    }

    var index: usize = 0;
    var lines = std.mem.split(u8, content, "\n");

    while (lines.next()) |line| {
        if (line.len == 0) continue;
        if (line[0] == 'T') {
            const id = try std.fmt.parseInt(u32, line[5 .. line.len - 1], 10);
            var data = [_][10]u8{[_]u8{0} ** 10} ** 10;

            for (0..10) |i| {
                const row = lines.next() orelse return error.UnexpectedEof;
                if (row.len != 10) return error.InvalidInput;
                @memcpy(&data[i], row[0..10]);
            }

            const i = index / 12;
            const j = index % 12;
            tiles[i][j] = Tile.init(id, data);
            index += 1;
        }
    }

    const corners = findCornerTiles(&tiles);
    var product: u64 = 1;
    for (corners) |corner| {
        std.debug.print("{d}\n", .{corner.id});
        product *= corner.id;
    }

    print("Part 1: {d}\n", .{product});
}

const Tile = struct {
    id: u64,
    data: [10][10]u8,
    edges: [8][10]u8,
    isCorner: bool = false,

    pub fn init(id: u64, data: [10][10]u8) Tile {
        var edges: [8][10]u8 = undefined;
        edges[0] = data[0];
        edges[1] = data[9];

        for (0..10) |i| {
            edges[2][i] = data[i][0];
            edges[3][i] = data[i][9];
            edges[4][i] = data[0][9 - i];
            edges[5][i] = data[9][9 - i];
            edges[6][i] = data[9 - i][0];
            edges[7][i] = data[9 - i][9];
        }

        return .{ .id = id, .data = data, .edges = edges };
    }
};

fn findCornerTiles(tiles: *[12][12]Tile) [4]Tile {
    var corners: [4]Tile = undefined;
    var cornerIndex: usize = 0;

    outer: for (tiles) |*row| {
        for (row) |*tile| {
            if (tile.id == 0) continue;

            var matches: u32 = 0;
            for (tiles) |row2| {
                for (row2) |other| {
                    if (tile.id == other.id) continue;

                    for (tile.edges) |edge| {
                        for (other.edges) |otherEdge| {
                            if (std.mem.eql(u8, &edge, &otherEdge)) {
                                if (tile.id == 3083) {
                                    std.debug.print("Matched {d} with {d} on edge: {s}\n", .{ tile.id, other.id, edge });
                                }

                                matches += 1;
                            }
                        }
                    }
                }
            }

            if (matches == 4) {
                corners[cornerIndex] = tile.*;
                cornerIndex += 1;
                tile.isCorner = true;
                if (cornerIndex == 4) break :outer;
            }
        }
    }

    return corners;
}
