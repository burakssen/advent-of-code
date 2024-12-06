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

    var lines = std.mem.split(u8, content, "\n");

    var tiles = std.ArrayList(Tile).init(allocator);
    defer tiles.deinit();

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

            var tile = Tile.init(id, data);
            if (id == 3083 or id == 3461 or id == 3433 or id == 2287) {
                tile.isCorner = true;
            }
            try tiles.append(tile);
        }
    }

    try assembleTiles(&tiles);
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

    pub fn rotate(self: *Tile) void {
        var newData = [_][10]u8{[_]u8{0} ** 10} ** 10;
        var newEdges = [_][10]u8{[_]u8{0} ** 10} ** 8;

        for (0..10) |i| {
            for (0..10) |j| {
                newData[j][9 - i] = self.data[i][j];
            }
        }

        for (0..8) |i| {
            for (0..10) |j| {
                newEdges[i][j] = self.edges[(i + 6) % 8][j];
            }
        }

        self.data = newData;
        self.edges = newEdges;
    }

    pub fn flip(self: *Tile) void {
        var newData = [_][10]u8{[_]u8{0} ** 10} ** 10;
        var newEdges = [_][10]u8{[_]u8{0} ** 10} ** 8;

        for (0..10) |i| {
            for (0..10) |j| {
                newData[i][9 - j] = self.data[i][j];
            }
        }

        for (0..8) |i| {
            for (0..10) |j| {
                newEdges[i][j] = self.edges[(i + 4) % 8][j];
            }
        }

        self.data = newData;
        self.edges = newEdges;
    }

    pub fn print(self: *@This()) void {
        std.debug.print("Tile {}\n", .{self.id});
        for (self.data) |row| {
            for (row) |cell| {
                std.debug.print("{c}", .{cell});
            }
            std.debug.print("\n", .{});
        }
    }
};

fn assembleTiles(tiles: *std.ArrayList(Tile)) !void {
    // Get the first corner tile
    var cornerTile: Tile = undefined;
    for (tiles.items) |tile| {
        if (tile.isCorner) {
            cornerTile = tile;
            break;
        }
    }

    cornerTile.flip();

    var assembledTiles: [12][12]Tile = undefined;
    assembledTiles[0][0] = cornerTile;

    // Get right matching tile
    for (tiles.items) |*tile| {
        if (tile.id == cornerTile.id) continue;

        for (0..8) |i| {
            if (std.mem.eql(u8, &cornerTile.edges[1], &tile.edges[i])) {
                tile.rotate();
                assembledTiles[0][1] = tile.*;
                break;
            }
        }
    }
}
