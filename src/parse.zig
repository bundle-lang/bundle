const std = @import("std");
const log = std.log;
const fmt = std.fmt;

const lex = @import("lex.zig");
const ast = @import("ast.zig");

const Allocator = std.mem.Allocator;

const ParseError = error{
    ParseFail,
    Eof,
    Unrecoverable,
};

pub const Parser = struct {
    allocator: Allocator,
    lexer: lex.Lexer,
    nodes: ast.NodeArray,
    err: ?ParseError,

    fn propagateUnrecoverableError(_: *Parser, err: anyerror) ParseError {
        log.err("unrecoverable internal error, got {}", .{err});
        return ParseError.Unrecoverable;
    }

    fn propagateCustomError(self: *Parser, expected: []const u8, found: lex.Token) ParseError {
        log.err("{s}:{}:{} expected `{s}` found `{s}`", .{ self.lexer.unit, found.line, found.column, expected, found.literal });
        return ParseError.ParseFail;
    }

    fn propagateError(self: *Parser, expected: lex.Token.Kind, found: lex.Token) ParseError {
        log.err("{s}:{}:{} expected `{s}` found `{s}`", .{ self.lexer.unit, found.line, found.column, expected.symbol(), found.literal });
        return ParseError.ParseFail;
    }

    fn expectAndSkip(self: *Parser, expected: lex.Token.Kind) ParseError!void {
        const next = self.lexer.nextToken();
        if (next.kind != expected) {
            return self.propagateError(expected, next);
        }
    }

    fn readIdentifier(self: *Parser) ParseError![]const u8 {
        const next = self.lexer.nextToken();
        return switch (next.kind) {
            .identifier => next.literal,
            else => self.propagateError(.identifier, next),
        };
    }

    fn readType(self: *Parser) ParseError!ast.Type {
        const next = self.lexer.nextToken();
        return switch (next.kind) {
            .type_u32 => .type_u32,
            .type_i32 => .type_i32,
            .type_bool => .type_bool,
            else => self.propagateCustomError("Type", next),
        };
    }

    fn parseBasicExpr(self: *Parser) ParseError!ast.NodeKind {
        const next = self.lexer.nextToken();
        return switch (next.kind) {
            .literal_integer => .{ .basic_expr = .{ .integer = fmt.parseUnsigned(u32, next.literal, 0) catch unreachable } },
            .literal_true => .{ .basic_expr = .{ .boolean = true } },
            .literal_false => .{ .basic_expr = .{ .boolean = false } },
            .identifier => .{ .basic_expr = .{ .identifier = next.literal } },
            else => self.propagateCustomError("Expression", next),
        };
    }

    fn parseExpressionC(self: *Parser) ParseError!*ast.NodeKind {
        var primary = self.allocator.create(ast.NodeKind) catch |err|
            return self.propagateUnrecoverableError(err);
        primary.* = try self.parseBasicExpr();
        return primary;
    }

    fn parseExpressionB(self: *Parser) ParseError!*ast.NodeKind {
        var expression1 = try self.parseExpressionC();

        while (self.lexer.peekTokenIs(.star) or self.lexer.peekTokenIs(.slash)) {
            const operator = switch (self.lexer.nextToken().kind) {
                .star => ast.Operator.star,
                .slash => ast.Operator.slash,
                else => unreachable,
            };
            const expression2 = try self.parseExpressionC();

            var expression3 = self.allocator.create(ast.NodeKind) catch |err|
                return self.propagateUnrecoverableError(err);
            expression3.* = .{ .binary_expr = .{ .left = expression1, .operator = operator, .right = expression2 } };
            expression1 = expression3;
        }

        return expression1;
    }

    fn parseExpressionA(self: *Parser) ParseError!*ast.NodeKind {
        var expression1 = try self.parseExpressionB();

        while (self.lexer.peekTokenIs(.plus) or self.lexer.peekTokenIs(.minus)) {
            const operator = switch (self.lexer.nextToken().kind) {
                .plus => ast.Operator.plus,
                .minus => ast.Operator.minus,
                else => unreachable,
            };
            const expression2 = try self.parseExpressionB();

            var expression3 = self.allocator.create(ast.NodeKind) catch |err|
                return self.propagateUnrecoverableError(err);
            expression3.* = .{ .binary_expr = .{ .left = expression1, .operator = operator, .right = expression2 } };
            expression1 = expression3;
        }

        return expression1;
    }

    fn parseExpression(self: *Parser) ParseError!*ast.NodeKind {
        return try self.parseExpressionA();
    }

    fn parseFn(self: *Parser) ParseError!ast.NodeKind {
        const name = try self.readIdentifier();

        try self.expectAndSkip(.open_paren);
        var args = ast.NodeArray.init(self.allocator);

        while (!self.lexer.peekTokenIs(.close_paren)) {
            const arg_name = try self.readIdentifier();
            const arg_type = try self.readType();

            if (!self.lexer.peekTokenIs(.close_paren)) {
                try self.expectAndSkip(.comma);
            }

            args.append(.{ .arg = .{ .name = arg_name, .arg_type = arg_type } }) catch |err|
                return self.propagateUnrecoverableError(err);
        }

        try self.expectAndSkip(.close_paren);
        const fn_type = try self.readType();

        const body = try self.parseBody();

        return ast.NodeKind{ .fn_decl = .{ .name = name, .args = args, .fn_type = fn_type, .body = body } };
    }

    fn parseLet(self: *Parser) ParseError!ast.NodeKind {
        const name = try self.readIdentifier();

        const let_type = try self.readType();
        try self.expectAndSkip(.equal);

        const value = try self.parseExpression();

        return ast.NodeKind{ .var_decl = .{ .name = name, .let_type = let_type, .value = value } };
    }

    fn parseIf(self: *Parser) ParseError!ast.NodeKind {
        const if_condition = try self.parseExpression();
        const if_body = try self.parseBody();

        var elif_nodes = if (self.lexer.peekTokenIs(.keyword_elif)) ast.NodeArray.init(self.allocator) else null;

        while (self.lexer.peekTokenIs(.keyword_elif)) {
            _ = self.lexer.nextToken();

            const elif_condition = try self.parseExpression();
            const elif_body = try self.parseBody();

            elif_nodes.?.append(ast.NodeKind{ .elif_stmt = .{ .elif_condition = elif_condition, .elif_body = elif_body } }) catch |err|
                return self.propagateUnrecoverableError(err);
        }

        var else_body: ?ast.NodeArray = null;
        if (self.lexer.peekTokenIs(.keyword_else)) {
            _ = self.lexer.nextToken();
            else_body = try self.parseBody();
        }

        return ast.NodeKind{ .if_stmt = .{ .if_condition = if_condition, .if_body = if_body, .elif_nodes = elif_nodes, .else_body = else_body } };
    }

    fn parseReturn(self: *Parser) ParseError!ast.NodeKind {
        const value = try self.parseExpression();
        return ast.NodeKind{ .return_stmt = .{ .value = value } };
    }

    fn parseBody(self: *Parser) ParseError!ast.NodeArray {
        var nodes = ast.NodeArray.init(self.allocator);

        try self.expectAndSkip(.open_brace);

        while (!self.lexer.peekTokenIs(.close_brace)) {
            const next = self.lexer.nextToken();
            nodes.append(try switch (next.kind) {
                .keyword_let => self.parseLet(),
                .keyword_if => self.parseIf(),
                .keyword_return => self.parseReturn(),
                else => self.propagateCustomError("Keyword", next),
            }) catch |err| return self.propagateUnrecoverableError(err);
        }

        try self.expectAndSkip(.close_brace);

        return nodes;
    }

    pub fn nextNode(self: *Parser) ParseError!ast.NodeKind {
        const next = self.lexer.nextToken();
        return switch (next.kind) {
            .keyword_fn => self.parseFn(),
            .keyword_let => self.parseLet(),
            .eof => ParseError.Eof,
            else => self.err orelse self.propagateCustomError("Keyword", next),
        };
    }

    pub fn parse(self: *Parser) ParseError!void {
        while (true) {
            if (self.nextNode()) |node| {
                self.nodes.append(node) catch |err| return self.propagateUnrecoverableError(err);
            } else |err| switch (err) {
                ParseError.ParseFail => self.err = err,
                ParseError.Eof => break,
                else => return err,
            }
        }

        return if (self.err) |err| err;
    }
};

pub fn new(allocator: Allocator, lexer: lex.Lexer) Parser {
    return Parser{
        .allocator = allocator,
        .lexer = lexer,
        .nodes = ast.NodeArray.init(allocator),
        .err = null,
    };
}
