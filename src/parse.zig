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
    id_count: ast.NodeId,
    err: ?ParseError,

    fn propagateUnrecoverableError(_: *Parser, err: anyerror) ParseError {
        log.err("unrecoverable internal error, got {}", .{err});
        return ParseError.Unrecoverable;
    }

    fn propagateCustomError(self: *Parser, expected: []const u8, found: lex.Token) ParseError {
        log.err("{s}:{}:{} expected `{s}` found `{s}`", .{ self.lexer.unit, found.line, found.column, expected, found.kind.symbol() });
        return ParseError.ParseFail;
    }

    fn propagateError(self: *Parser, expected: lex.Token.Kind, found: lex.Token) ParseError {
        return self.propagateCustomError(expected.symbol(), found);
    }

    pub fn nextNodeId(self: *Parser) ast.NodeId {
        defer self.id_count += 1;
        return self.id_count;
    }

    fn expectNewLine(self: *Parser) ParseError!void {
        try self.expectAndSkip(.eol);

        while (self.lexer.peekTokenIs(.eol)) {
            _ = self.lexer.nextToken();
        }
    }

    fn expectAndSkip(self: *Parser, expected: lex.Token.Kind) ParseError!void {
        const next = self.lexer.nextToken();
        if (next.kind != expected and !(expected == .eol and next.kind == .eof)) {
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
            .type_i32 => .type_i32,
            .type_bool => .type_bool,
            else => self.propagateCustomError("Type", next),
        };
    }

    fn readOperator(self: *Parser) ParseError!ast.Operator {
        const next = self.lexer.nextToken();
        return switch (next.kind) {
            .plus => ast.Operator.plus,
            .minus => ast.Operator.minus,
            .star => ast.Operator.star,
            .slash => ast.Operator.slash,
            else => self.propagateCustomError("Operator", next),
        };
    }

    fn readUnaryOperator(self: *Parser) ParseError!ast.Operator {
        const next = self.lexer.nextToken();
        return switch (next.kind) {
            .plus => ast.Operator.plus,
            .minus => ast.Operator.minus,
            else => self.propagateCustomError("Unary Operator", next),
        };
    }

    fn parseReference(self: *Parser) ParseError!ast.NodeKind {
        const id = self.nextNodeId();
        const name = try self.readIdentifier();

        return ast.NodeKind{ .reference = .{ .id = id, .name = name } };
    }

    fn parseGroupingExpr(self: *Parser) ParseError!ast.NodeKind {
        try self.expectAndSkip(.open_paren);
        const expr = try self.parseExpr();
        try self.expectAndSkip(.close_paren);

        return ast.NodeKind{ .grouping_expr = .{ .expr = expr } };
    }

    fn parseLiteralExpr(self: *Parser) ParseError!ast.NodeKind {
        const next = self.lexer.nextToken();
        const literal_expr = switch (next.kind) {
            .literal_integer => ast.NodeLiteralExpr{ .integer = fmt.parseUnsigned(u32, next.literal, 0) catch unreachable },
            .literal_true => ast.NodeLiteralExpr{ .boolean = true },
            .literal_false => ast.NodeLiteralExpr{ .boolean = false },
            else => return self.propagateCustomError("Literal", next),
        };

        return ast.NodeKind{ .literal_expr = literal_expr };
    }

    fn parsePrimaryExpr(self: *Parser) ParseError!*ast.NodeKind {
        var primary = self.allocator.create(ast.NodeKind) catch |err|
            return self.propagateUnrecoverableError(err);

        const next = self.lexer.peekToken();
        primary.* = switch (next.kind) {
            .identifier => try self.parseReference(),
            .open_paren => try self.parseGroupingExpr(),
            else => try self.parseLiteralExpr(),
        };

        if (self.lexer.peekTokenIs(.open_paren)) {
            var call = self.allocator.create(ast.NodeKind) catch |err|
                return self.propagateUnrecoverableError(err);
            call.* = try self.parseCallExpr(primary);

            return call;
        }

        return primary;
    }

    fn parseUnaryExpr(self: *Parser) ParseError!*ast.NodeKind {
        var unary = self.allocator.create(ast.NodeKind) catch |err|
            return self.propagateUnrecoverableError(err);

        const operator = try self.readUnaryOperator();
        const expr = try self.parsePrimaryExpr();
        unary.* = .{ .unary_expr = .{ .operator = operator, .expr = expr } };

        return unary;
    }

    fn parseExprC(self: *Parser) ParseError!*ast.NodeKind {
        if (self.lexer.peekTokenIs(.plus) or self.lexer.peekTokenIs(.minus)) {
            return try self.parseUnaryExpr();
        } else {
            return try self.parsePrimaryExpr();
        }
    }

    fn parseExprB(self: *Parser) ParseError!*ast.NodeKind {
        var expression1 = try self.parseExprC();

        while (self.lexer.peekTokenIs(.star) or self.lexer.peekTokenIs(.slash)) {
            const operator = try self.readOperator();
            const expression2 = try self.parseExprC();

            var expression3 = self.allocator.create(ast.NodeKind) catch |err|
                return self.propagateUnrecoverableError(err);
            expression3.* = .{ .binary_expr = .{ .left = expression1, .operator = operator, .right = expression2 } };
            expression1 = expression3;
        }

        return expression1;
    }

    fn parseExprA(self: *Parser) ParseError!*ast.NodeKind {
        var expression1 = try self.parseExprB();

        while (self.lexer.peekTokenIs(.plus) or self.lexer.peekTokenIs(.minus)) {
            const operator = try self.readOperator();
            const expression2 = try self.parseExprB();

            var expression3 = self.allocator.create(ast.NodeKind) catch |err|
                return self.propagateUnrecoverableError(err);
            expression3.* = .{ .binary_expr = .{ .left = expression1, .operator = operator, .right = expression2 } };
            expression1 = expression3;
        }

        return expression1;
    }

    fn parseExpr(self: *Parser) ParseError!*ast.NodeKind {
        return try self.parseExprA();
    }

    fn parseCallExpr(self: *Parser, left_expr: *ast.NodeKind) ParseError!ast.NodeKind {
        try self.expectAndSkip(.open_paren);
        var arguments = ast.NodeArray.init(self.allocator);

        while (!self.lexer.peekTokenIs(.close_paren)) {
            const arg_expr = try self.parseExpr();

            if (!self.lexer.peekTokenIs(.close_paren)) {
                try self.expectAndSkip(.comma);
            }

            arguments.append(arg_expr.*) catch |err| return self.propagateUnrecoverableError(err);
        }

        try self.expectAndSkip(.close_paren);

        return ast.NodeKind{ .call_expr = .{ .left_expr = left_expr, .arguments = arguments } };
    }

    fn parseFnDecl(self: *Parser) ParseError!ast.NodeKind {
        try self.expectAndSkip(.keyword_fn);

        const name = try self.readIdentifier();

        try self.expectAndSkip(.open_paren);
        var parameters = ast.NodeArray.init(self.allocator);

        while (!self.lexer.peekTokenIs(.close_paren)) {
            const parameter_name = try self.readIdentifier();
            const parameter_type = try self.readType();

            if (!self.lexer.peekTokenIs(.close_paren)) {
                try self.expectAndSkip(.comma);
            }

            const parameter = ast.NodeKind{ .parameter = .{ .name = parameter_name, .parameter_type = parameter_type } };
            parameters.append(parameter) catch |err| return self.propagateUnrecoverableError(err);
        }

        try self.expectAndSkip(.close_paren);
        const fn_type = try self.readType();

        const body = try self.parseBody();
        try self.expectNewLine();

        return ast.NodeKind{ .fn_decl = .{ .name = name, .parameters = parameters, .fn_type = fn_type, .body = body } };
    }

    fn parseLetStmt(self: *Parser) ParseError!ast.NodeKind {
        try self.expectAndSkip(.keyword_let);

        const name = try self.readIdentifier();

        const let_type = try self.readType();
        try self.expectAndSkip(.equal);

        const value = try self.parseExpr();

        try self.expectNewLine();

        return ast.NodeKind{ .let_stmt = .{ .name = name, .let_type = let_type, .value = value } };
    }

    fn parseAssignStmt(self: *Parser, left_expr: *ast.NodeKind) ParseError!ast.NodeKind {
        try self.expectAndSkip(.equal);
        const value = try self.parseExpr();

        try self.expectNewLine();

        return ast.NodeKind{ .assign_stmt = .{ .left_expr = left_expr, .value = value } };
    }

    fn parseIfStmt(self: *Parser) ParseError!ast.NodeKind {
        try self.expectAndSkip(.keyword_if);

        const if_condition = try self.parseExpr();
        const if_body = try self.parseBody();

        var elif_nodes = if (self.lexer.peekTokenIs(.keyword_elif)) ast.NodeArray.init(self.allocator) else null;

        while (self.lexer.peekTokenIs(.keyword_elif)) {
            _ = self.lexer.nextToken();

            const elif_condition = try self.parseExpr();
            const elif_body = try self.parseBody();

            const elif_stmt = ast.NodeKind{ .elif_stmt = .{ .elif_condition = elif_condition, .elif_body = elif_body } };
            elif_nodes.?.append(elif_stmt) catch |err| return self.propagateUnrecoverableError(err);
        }

        var else_body: ?ast.NodeBlockStmt = null;
        if (self.lexer.peekTokenIs(.keyword_else)) {
            _ = self.lexer.nextToken();
            else_body = try self.parseBody();
        }

        try self.expectNewLine();

        return ast.NodeKind{ .if_stmt = .{ .if_condition = if_condition, .if_body = if_body, .elif_nodes = elif_nodes, .else_body = else_body } };
    }

    fn parseReturnStmt(self: *Parser) ParseError!ast.NodeKind {
        try self.expectAndSkip(.keyword_return);
        const value = try self.parseExpr();

        try self.expectNewLine();

        return ast.NodeKind{ .return_stmt = .{ .value = value } };
    }

    fn parseBody(self: *Parser) ParseError!ast.NodeBlockStmt {
        try self.expectAndSkip(.open_brace);

        var nodes = ast.NodeArray.init(self.allocator);
        if (!self.lexer.peekTokenIs(.close_brace)) {
            try self.expectNewLine();
        }

        while (!self.lexer.peekTokenIs(.close_brace)) {
            const next = self.lexer.peekToken();
            const node = try switch (next.kind) {
                .keyword_let => self.parseLetStmt(),
                .keyword_if => self.parseIfStmt(),
                .keyword_return => self.parseReturnStmt(),
                else => expr: {
                    const expr = try self.parseExpr();

                    if (self.lexer.peekTokenIs(.equal)) {
                        break :expr try self.parseAssignStmt(expr);
                    }

                    try self.expectNewLine();
                    break :expr expr.*;
                },
            };

            nodes.append(node) catch |err| return self.propagateUnrecoverableError(err);
        }

        try self.expectAndSkip(.close_brace);

        return ast.NodeBlockStmt{ .list = nodes };
    }

    pub fn nextNode(self: *Parser) ParseError!ast.NodeKind {
        const next = self.lexer.peekToken();
        return switch (next.kind) {
            .keyword_fn => self.parseFnDecl(),
            .eof => ParseError.Eof,
            else => err: {
                defer _ = self.lexer.nextToken();
                break :err self.err orelse self.propagateCustomError("Declaration", next);
            },
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
        .id_count = 0,
        .err = null,
    };
}
