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

    var lines = std.mem.splitSequence(u8, content, "\n");
    var totals = [2]i64{ 0, 0 };

    while (lines.next()) |line| {
        if (line.len == 0) continue;
        inline for (0..2) |part| {
            var lexer = Lexer.init(line);
            var parser = try Parser.init(allocator, &lexer, part == 1);
            defer parser.deinit();
            totals[part] += try (try parser.parse()).evaluate();
        }
    }

    try stdout.print("Part 1: {}\n", .{totals[0]});
    try stdout.print("Part 2: {}\n", .{totals[1]});
}

const Token = enum { Open, Close, Number, Star, Plus };

const TokenInfo = struct {
    token: Token,
    value: ?i64 = null,
};

const Operator = enum { Add, Multiply };

const ParseError = error{
    UnexpectedEndOfInput,
    UnexpectedToken,
    ExpectedClosingParenthesis,
    OutOfMemory,
};

const Expression = union(enum) {
    Number: i64,
    Binary: struct {
        left: *Expression,
        right: *Expression,
        op: Operator,
    },

    pub fn evaluate(self: Expression) !i64 {
        return switch (self) {
            .Number => |n| n,
            .Binary => |b| switch (b.op) {
                .Add => try b.left.evaluate() + try b.right.evaluate(),
                .Multiply => try b.left.evaluate() * try b.right.evaluate(),
            },
        };
    }
};

const Lexer = struct {
    input: []const u8,
    pos: usize = 0,

    pub fn init(input: []const u8) Lexer {
        return .{ .input = input };
    }

    pub fn next(self: *Lexer) ?TokenInfo {
        while (self.pos < self.input.len and self.input[self.pos] == ' ') self.pos += 1;
        if (self.pos >= self.input.len) return null;

        const c = self.input[self.pos];
        self.pos += 1;

        return switch (c) {
            '(' => .{ .token = .Open },
            ')' => .{ .token = .Close },
            '*' => .{ .token = .Star },
            '+' => .{ .token = .Plus },
            '0'...'9' => blk: {
                var num: i64 = c - '0';
                while (self.pos < self.input.len) : (self.pos += 1) {
                    const n = self.input[self.pos];
                    if (n < '0' or n > '9') break;
                    num = num * 10 + (n - '0');
                }
                break :blk .{ .token = .Number, .value = num };
            },
            else => null,
        };
    }
};

const Parser = struct {
    allocator: std.mem.Allocator,
    lexer: *Lexer,
    current: ?TokenInfo,
    precedence: bool,

    pub fn init(allocator: std.mem.Allocator, lexer: *Lexer, precedence: bool) !Parser {
        var parser = Parser{
            .allocator = allocator,
            .lexer = lexer,
            .current = null,
            .precedence = precedence,
        };
        parser.current = parser.lexer.next();
        return parser;
    }

    pub fn deinit(self: *Parser) void {
        _ = self;
    }

    fn advance(self: *Parser) void {
        self.current = self.lexer.next();
    }

    fn createBinary(self: *Parser, left: Expression, right: Expression, op: Operator) !Expression {
        const binary = try self.allocator.create(Expression);
        const left_expr = try self.allocator.create(Expression);
        const right_expr = try self.allocator.create(Expression);
        left_expr.* = left;
        right_expr.* = right;
        binary.* = .{ .Binary = .{ .left = left_expr, .right = right_expr, .op = op } };
        return binary.*;
    }

    pub fn parse(self: *Parser) ParseError!Expression {
        return if (self.precedence) self.parseMultiplication() else self.parseExpression();
    }

    fn parseExpression(self: *Parser) ParseError!Expression {
        var left = try self.parsePrimary();

        while (self.current) |token| {
            switch (token.token) {
                .Plus, .Star => {
                    const op: Operator = if (token.token == .Plus) .Add else .Multiply;
                    self.advance();
                    left = try self.createBinary(left, try self.parsePrimary(), op);
                },
                else => break,
            }
        }
        return left;
    }

    fn parseMultiplication(self: *Parser) ParseError!Expression {
        var left = try self.parseAddition();

        while (self.current) |token| {
            if (token.token == .Star) {
                self.advance();
                left = try self.createBinary(left, try self.parseAddition(), .Multiply);
            } else break;
        }
        return left;
    }

    fn parseAddition(self: *Parser) ParseError!Expression {
        var left = try self.parsePrimary();

        while (self.current) |token| {
            if (token.token == .Plus) {
                self.advance();
                left = try self.createBinary(left, try self.parsePrimary(), .Add);
            } else break;
        }
        return left;
    }

    fn parsePrimary(self: *Parser) ParseError!Expression {
        const token = self.current orelse return ParseError.UnexpectedEndOfInput;
        switch (token.token) {
            .Number => {
                const value = token.value.?;
                self.advance();
                return .{ .Number = value };
            },
            .Open => {
                self.advance();
                const expr = try if (self.precedence) self.parseMultiplication() else self.parseExpression();
                if (self.current) |close| {
                    if (close.token != .Close) return ParseError.ExpectedClosingParenthesis;
                    self.advance();
                    return expr;
                }
                return ParseError.ExpectedClosingParenthesis;
            },
            else => return ParseError.UnexpectedToken,
        }
    }
};
