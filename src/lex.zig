const std = @import("std");
const ascii = std.ascii;

pub const Token = struct {
    kind: Kind,
    literal: []const u8,
    line: u32,
    column: u32,

    pub const Kind = enum {
        // internals
        eof,
        unknown,

        // keywords
        identifier,
        keyword_fn,
        keyword_return,
        keyword_if,
        keyword_elif,
        keyword_else,
        keyword_let,

        // literals
        literal_integer,
        literal_true,
        literal_false,

        // types
        type_u32,
        type_i32,
        type_bool,

        // operators
        equal,
        plus,
        minus,
        star,
        slash,

        // symbols
        colon,
        semicolon,
        comma,
        open_paren,
        close_paren,
        open_brace,
        close_brace,

        pub fn symbol(kind: Kind) []const u8 {
            return switch (kind) {
                .eof => "Eof",
                .unknown => "Unknown",

                .identifier => "Identifier",
                .keyword_fn => "fn",
                .keyword_return => "return",
                .keyword_if => "if",
                .keyword_elif => "elif",
                .keyword_else => "else",
                .keyword_let => "let",

                .literal_integer => "Integer",
                .literal_true => "true",
                .literal_false => "false",

                .type_u32 => "u32",
                .type_i32 => "i32",
                .type_bool => "bool",

                .equal => "=",
                .plus => "+",
                .minus => "-",
                .star => "*",
                .slash => "/",

                .colon => ":",
                .semicolon => ";",
                .comma => ",",
                .open_paren => "(",
                .close_paren => ")",
                .open_brace => "{",
                .close_brace => "}",
            };
        }
    };

    const keywords = std.ComptimeStringMap(Kind, .{
        .{ "fn", .keyword_fn },
        .{ "return", .keyword_return },
        .{ "if", .keyword_if },
        .{ "elif", .keyword_elif },
        .{ "else", .keyword_else },
        .{ "let", .keyword_let },

        .{ "true", .literal_true },
        .{ "false", .literal_false },

        .{ "u32", .type_u32 },
        .{ "i32", .type_i32 },
        .{ "bool", .type_bool },
    });

    fn keyword(literal: []const u8) ?Kind {
        return Token.keywords.get(literal);
    }
};

pub const Lexer = struct {
    unit: []const u8,
    src: []const u8,
    pos: u32,
    line: u32,
    column: u32,

    fn nextChar(self: *Lexer) void {
        if (self.src.len == 0) {
            return;
        } else if (self.pos < self.src.len) {
            if (self.curChar() == '\n') {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
        self.pos += 1;
    }

    fn curChar(self: *Lexer) u8 {
        return self.src[self.pos];
    }

    pub fn peekTokenIs(self: *Lexer, kind: Token.Kind) bool {
        return self.peekToken().kind == kind;
    }

    fn peekToken(self: *Lexer) Token {
        var copy = self.*;
        return copy.nextToken();
    }

    pub fn nextToken(self: *Lexer) Token {
        while (self.pos < self.src.len and ascii.isSpace(self.curChar())) {
            self.nextChar();
        }

        var token = Token{ .kind = .eof, .literal = "eof", .line = self.line, .column = self.column };

        if (self.pos < self.src.len) {
            var start = self.pos;

            switch (self.curChar()) {
                'a'...'z', 'A'...'Z', '_' => {
                    while (self.pos < self.src.len and (ascii.isAlNum(self.curChar()) or self.curChar() == '_')) {
                        self.nextChar();
                    }
                    token.kind = Token.keyword(self.src[start..self.pos]) orelse .identifier;
                },
                '0'...'9' => {
                    while (self.pos < self.src.len and ascii.isDigit(self.curChar())) {
                        self.nextChar();
                    }
                    token.kind = .literal_integer;

                    while (self.pos < self.src.len and (ascii.isAlNum(self.curChar()) or self.curChar() == '_')) {
                        self.nextChar();
                        token.kind = .unknown;
                    }
                },
                '=' => {
                    self.nextChar();
                    token.kind = .equal;
                },
                '+' => {
                    self.nextChar();
                    token.kind = .plus;
                },
                '-' => {
                    self.nextChar();
                    token.kind = .minus;
                },
                '*' => {
                    self.nextChar();
                    token.kind = .star;
                },
                '/' => {
                    self.nextChar();
                    token.kind = .slash;
                },
                ':' => {
                    self.nextChar();
                    token.kind = .colon;
                },
                ';' => {
                    self.nextChar();
                    token.kind = .semicolon;
                },
                ',' => {
                    self.nextChar();
                    token.kind = .comma;
                },
                '(' => {
                    self.nextChar();
                    token.kind = .open_paren;
                },
                ')' => {
                    self.nextChar();
                    token.kind = .close_paren;
                },
                '{' => {
                    self.nextChar();
                    token.kind = .open_brace;
                },
                '}' => {
                    self.nextChar();
                    token.kind = .close_brace;
                },
                else => {
                    self.nextChar();
                    token.kind = .unknown;
                },
            }

            token.literal = self.src[start..self.pos];
        }

        return token;
    }
};

pub fn new(unit: []const u8, src: []const u8) Lexer {
    return Lexer{
        .unit = unit,
        .src = src,
        .pos = 0,
        .line = 1,
        .column = 1,
    };
}

test "lexer: proper initial values" {
    var lexer = new("", "");

    try std.testing.expectEqual(@as(u32, 0), lexer.pos);
    try std.testing.expectEqual(@as(u32, 1), lexer.line);
    try std.testing.expectEqual(@as(u32, 1), lexer.column);
}

test "curChar: empty input" {
    var lexer = new("test", "");

    try std.testing.expectEqual(@as(u32, 0), lexer.pos);
    lexer.nextChar();
    try std.testing.expectEqual(@as(u32, 0), lexer.pos);
}

test "peekTokenIs: single token" {
    var lexer = new("test", "a");

    try std.testing.expect(lexer.peekTokenIs(.identifier));
}

test "peekTokenIs: eof" {
    var lexer = new("test", "");

    try std.testing.expect(lexer.peekTokenIs(.eof));
}

test "nextToken: eof on empty input" {
    var lexer = new("test", "");

    try std.testing.expect(lexer.nextToken().kind == .eof);
}

test "nextToken: eof after last token" {
    var lexer = new("test", "a");

    try std.testing.expect(lexer.nextToken().kind == .identifier);
    try std.testing.expect(lexer.nextToken().kind == .eof);
}

test "nextToken: eof after eof" {
    var lexer = new("test", "");

    try std.testing.expect(lexer.nextToken().kind == .eof);
    try std.testing.expect(lexer.nextToken().kind == .eof);
}

test "nextToken: lex identifier" {
    var lexer = new("test", "foo");

    try std.testing.expect(lexer.nextToken().kind == .identifier);
}

test "nextToken: lex symbol" {
    var lexer = new("test", "=");

    try std.testing.expect(lexer.nextToken().kind == .equal);
}

test "nextToken: lex unknown" {
    var lexer = new("test", "?");

    try std.testing.expect(lexer.nextToken().kind == .unknown);
}

test "nextToken: lex packed" {
    var lexer = new("test", "id+true?bool");

    try std.testing.expect(lexer.nextToken().kind == .identifier);
    try std.testing.expect(lexer.nextToken().kind == .plus);
    try std.testing.expect(lexer.nextToken().kind == .literal_true);
    try std.testing.expect(lexer.nextToken().kind == .unknown);
    try std.testing.expect(lexer.nextToken().kind == .type_bool);
    try std.testing.expect(lexer.nextToken().kind == .eof);
}
