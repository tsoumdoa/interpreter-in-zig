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

    pub inline fn readIdentifier(l: *Lexer) []const u8 {
        const start = l.readPos;
        while (l.char != 0) {
            if (!isLetter(l.char.?)) {
                break;
            }
            l.readChar();
        }
        return l.input[start..l.readPos];
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
                    if (isLetter(c)) {
                        tok = .{ .Type = token.lookupIdent(tok.Literal), .Literal = l.readIdentifier() };
                    } else tok = .{ .Type = TokenType.ILLEGAL, .Literal = "" };
                },
            }
        }
        l.readChar();
        return tok;
    }
};

fn isLetter(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

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

test "test nextToken (=+(){},;)" {
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

test "test nexttToken" {
    const input =
        \\ let five = 5;
        \\ let ten = 10;
        \\
        \\ let add = fn(x, y) {
        \\   x + y;
        \\ };
        \\
        \\ let result = add(five, ten); 
    ;
    const expectedTokens = [_]token.Token{
        .{ .Type = TokenType.LET, .Literal = "let" },
        .{ .Type = TokenType.IDENT, .Literal = "five" },
        .{ .Type = TokenType.ASSIGN, .Literal = "=" },
        .{ .Type = TokenType.INT, .Literal = "5" },
        .{ .Type = TokenType.SEMICOLON, .Literal = ";" },
        .{ .Type = TokenType.LET, .Literal = "let" },
        .{ .Type = TokenType.IDENT, .Literal = "ten" },
        .{ .Type = TokenType.ASSIGN, .Literal = "=" },
        .{ .Type = TokenType.INT, .Literal = "10" },
        .{ .Type = TokenType.SEMICOLON, .Literal = ";" },
        .{ .Type = TokenType.LET, .Literal = "let" },
        .{ .Type = TokenType.IDENT, .Literal = "add" },
        .{ .Type = TokenType.ASSIGN, .Literal = "=" },
        .{ .Type = TokenType.FUNCTION, .Literal = "fn" },
        .{ .Type = TokenType.LPAREN, .Literal = "(" },
        .{ .Type = TokenType.IDENT, .Literal = "x" },
        .{ .Type = TokenType.COMMA, .Literal = "," },
        .{ .Type = TokenType.IDENT, .Literal = "y" },
        .{ .Type = TokenType.RPAREN, .Literal = ")" },
        .{ .Type = TokenType.LBRACE, .Literal = "{" },
        .{ .Type = TokenType.IDENT, .Literal = "x" },
        .{ .Type = TokenType.PLUS, .Literal = "+" },
        .{ .Type = TokenType.IDENT, .Literal = "y" },
        .{ .Type = TokenType.SEMICOLON, .Literal = ";" },
        .{ .Type = TokenType.RBRACE, .Literal = "}" },
        .{ .Type = TokenType.SEMICOLON, .Literal = ";" },
        .{ .Type = TokenType.LET, .Literal = "let" },
        .{ .Type = TokenType.IDENT, .Literal = "result" },
        .{ .Type = TokenType.ASSIGN, .Literal = "=" },
        .{ .Type = TokenType.IDENT, .Literal = "add" },
        .{ .Type = TokenType.LPAREN, .Literal = "(" },
        .{ .Type = TokenType.IDENT, .Literal = "five" },
        .{ .Type = TokenType.COMMA, .Literal = "," },
        .{ .Type = TokenType.IDENT, .Literal = "ten" },
        .{ .Type = TokenType.RPAREN, .Literal = ")" },
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
