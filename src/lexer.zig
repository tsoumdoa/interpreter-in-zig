const std = @import("std");
const testing = std.testing;
const token = @import("token.zig");
const TokenType = token.TokenType;

pub const Lexer = struct {
    input: []const u8,
    pos: ?usize = null,
    readPos: usize = 0,
    char: ?u8 = null,

    pub fn init(input: []const u8) Lexer {
        var l = Lexer{
            .input = input,
        };
        l.readChar();
        return l;
    }

    pub inline fn readChar(l: *Lexer) void {
        if (l.readPos >= l.input.len) {
            l.char = 0;
        } else {
            l.char = l.input[l.readPos];
        }
        l.pos = l.readPos;
        l.readPos += 1;
    }

    pub inline fn nextToken(l: *Lexer) token.Token {
        var tok = token.Token{
            .Type = TokenType.EOF,
            .Literal = "",
        };

        if (l.char) |c| {
            switch (c) {
                '=' => {
                    tok = .{ .Type = TokenType.ASSIGN, .Literal = "=" };
                },
                ';' => {
                    tok = .{ .Type = TokenType.SEMICOLON, .Literal = ";" };
                },
                '(' => {
                    tok = .{ .Type = TokenType.LPAREN, .Literal = "(" };
                },
                ')' => {
                    tok = .{ .Type = TokenType.RPAREN, .Literal = ")" };
                },
                ',' => {
                    tok = .{ .Type = TokenType.COMMA, .Literal = "," };
                },
                '+' => {
                    tok = .{ .Type = TokenType.PLUS, .Literal = "+" };
                },
                '{' => {
                    tok = .{ .Type = TokenType.LBRACE, .Literal = "{" };
                },
                '}' => {
                    tok = .{ .Type = TokenType.RBRACE, .Literal = "}" };
                },
                0 => {
                    tok = .{ .Type = TokenType.EOF, .Literal = "" };
                },
                else => {
                    // std.debug.panic("to be implemented", .{});
                    // @panic("not implemented yet");
                },
            }
        }
        l.readChar();
        return tok;
    }
};

test "init Lexer" {
    const input = "Hello, world!";
    const firstChar = input[0];
    var lexer = Lexer.init(input);
    try testing.expectEqual(lexer.input, input);
    try testing.expectEqual(lexer.pos, 0);
    try testing.expectEqual(lexer.readPos, 1);
    try testing.expectEqual(lexer.char, firstChar);
    const a = lexer.nextToken();
    _ = a;
}

test "next token (=+(){},;)" {
    const input = "=+(){},;";

    const expectedTokens = [_]token.Token{
        .{ .Type = TokenType.ASSIGN, .Literal = "=" },
        .{ .Type = TokenType.PLUS, .Literal = "+" },
        .{ .Type = TokenType.LPAREN, .Literal = "(" },
        .{ .Type = TokenType.RPAREN, .Literal = ")" },
        .{ .Type = TokenType.LBRACE, .Literal = "{" },
        .{ .Type = TokenType.RBRACE, .Literal = "}" },
        .{ .Type = TokenType.COMMA, .Literal = "," },
        .{ .Type = TokenType.SEMICOLON, .Literal = ";" },
    };

    var l = Lexer.init(input);
    var index: usize = 0;

    while (l.char != 0) {
        const tok = l.nextToken();
        try testing.expectEqual(tok.Type, expectedTokens[index].Type);
        try testing.expectEqualStrings(tok.Literal, expectedTokens[index].Literal);
        index += 1;
    }
}
