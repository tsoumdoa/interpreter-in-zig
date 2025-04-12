const std = @import("std");
const testing = std.testing;
const token = @import("token.zig");
const TokenType = token.TokenType;

pub const Lexer = struct {
    input: []const u8,
    pos: usize = 0,
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

    pub inline fn readNumber(l: *Lexer) []const u8 {
        const start = l.pos;
        while (isDigit(l.char.?)) : (l.readChar()) {}
        return l.input[start..l.pos];
    }

    pub inline fn readIdentifier(l: *Lexer) []const u8 {
        const start = l.pos;
        while (isLetter(l.char.?)) : (l.readChar()) {}
        return l.input[start..l.pos];
    }

    pub inline fn skipWhitespace(l: *Lexer) void {
        while (l.char) |c| {
            if (c == ' ' or c == '\n' or c == '\t') {
                l.readChar();
            } else {
                break;
            }
        }
    }

    pub inline fn peekChar(l: *Lexer) u8 {
        if (l.readPos >= l.input.len) {
            return 0;
        } else {
            return l.input[l.readPos];
        }
    }

    pub inline fn nextToken(l: *Lexer) token.Token {
        var tok = token.Token{
            .Type = TokenType.EOF,
            .Literal = "",
        };

        l.skipWhitespace();
        if (l.char) |c| {
            switch (c) {
                '=' => {
                    if (l.peekChar() == '=') {
                        l.readChar();
                        tok = .{ .Type = TokenType.EQ, .Literal = "==" };
                    } else {
                        tok = .{ .Type = TokenType.ASSIGN, .Literal = "=" };
                    }
                },
                '-' => {
                    tok = .{ .Type = TokenType.MINUS, .Literal = "-" };
                },
                '!' => {
                    if (l.peekChar() == '=') {
                        l.readChar();
                        tok = .{ .Type = TokenType.NOT_EQ, .Literal = "!=" };
                    } else {
                        tok = .{ .Type = TokenType.BANG, .Literal = "!" };
                    }
                },
                '/' => {
                    tok = .{ .Type = TokenType.SLASH, .Literal = "/" };
                },
                '<' => {
                    tok = .{ .Type = TokenType.LT, .Literal = "<" };
                },
                '>' => {
                    tok = .{ .Type = TokenType.GT, .Literal = ">" };
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
                    tok = .{ .Type = TokenType.SUM, .Literal = "+" };
                },
                '*' => {
                    tok = .{ .Type = TokenType.PRODUCT, .Literal = "*" };
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
                        const i = l.readIdentifier();
                        tok = .{ .Type = token.lookupIdent(i), .Literal = i };
                        return tok;
                    } else if (isDigit(c)) {
                        const i = l.readNumber();
                        tok = .{ .Type = TokenType.INT, .Literal = i };
                        return tok;
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

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
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
        .{ .Type = TokenType.SUM, .Literal = "+" },
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

        try testing.expectEqual(expectedTokens[index].Type, tok.Type);
        try testing.expectEqualStrings(expectedTokens[index].Literal, tok.Literal);
        index += 1;
    }
}

test "test extended token" {
    const input =
        \\ let five = 5;
        \\ let ten = 0;
        \\
        \\ let add = fn(x, y) {
        \\   x + y;
        \\ };
        \\
        \\ let result = add(five, ten); 
        \\ !-/*5;
        \\ 5 < 10 > 5;
        \\ if (5 < 10) {
        \\    return true;
        \\} else {
        \\    return false;
        \\}
        \\10==10;
        \\10!=9;
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
        .{ .Type = TokenType.INT, .Literal = "0" },
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
        .{ .Type = TokenType.SUM, .Literal = "+" },
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
        .{ .Type = TokenType.BANG, .Literal = "!" },
        .{ .Type = TokenType.MINUS, .Literal = "-" },
        .{ .Type = TokenType.SLASH, .Literal = "/" },
        .{ .Type = TokenType.PRODUCT, .Literal = "*" },
        .{ .Type = TokenType.INT, .Literal = "5" },
        .{ .Type = TokenType.SEMICOLON, .Literal = ";" },
        .{ .Type = TokenType.INT, .Literal = "5" },
        .{ .Type = TokenType.LT, .Literal = "<" },
        .{ .Type = TokenType.INT, .Literal = "10" },
        .{ .Type = TokenType.GT, .Literal = ">" },
        .{ .Type = TokenType.INT, .Literal = "5" },
        .{ .Type = TokenType.SEMICOLON, .Literal = ";" },
        .{ .Type = TokenType.IF, .Literal = "if" },
        .{ .Type = TokenType.LPAREN, .Literal = "(" },
        .{ .Type = TokenType.INT, .Literal = "5" },
        .{ .Type = TokenType.LT, .Literal = "<" },
        .{ .Type = TokenType.INT, .Literal = "10" },
        .{ .Type = TokenType.RPAREN, .Literal = ")" },
        .{ .Type = TokenType.LBRACE, .Literal = "{" },
        .{ .Type = TokenType.RETURN, .Literal = "return" },
        .{ .Type = TokenType.TRUE, .Literal = "true" },
        .{ .Type = TokenType.SEMICOLON, .Literal = ";" },
        .{ .Type = TokenType.RBRACE, .Literal = "}" },
        .{ .Type = TokenType.ELSE, .Literal = "else" },
        .{ .Type = TokenType.LBRACE, .Literal = "{" },
        .{ .Type = TokenType.RETURN, .Literal = "return" },
        .{ .Type = TokenType.FALSE, .Literal = "false" },
        .{ .Type = TokenType.SEMICOLON, .Literal = ";" },
        .{ .Type = TokenType.RBRACE, .Literal = "}" },
        .{ .Type = TokenType.INT, .Literal = "10" },
        .{ .Type = TokenType.EQ, .Literal = "==" },
        .{ .Type = TokenType.INT, .Literal = "10" },
        .{ .Type = TokenType.SEMICOLON, .Literal = ";" },
        .{ .Type = TokenType.INT, .Literal = "10" },
        .{ .Type = TokenType.NOT_EQ, .Literal = "!=" },
        .{ .Type = TokenType.INT, .Literal = "9" },
        .{ .Type = TokenType.SEMICOLON, .Literal = ";" },
        .{ .Type = TokenType.EOF, .Literal = "" },
    };
    var l = Lexer.init(input);
    var index: usize = 0;

    while (l.char != 0) {
        const tok = l.nextToken();
        try testing.expectEqual(expectedTokens[index].Type, tok.Type);
        try testing.expectEqualStrings(expectedTokens[index].Literal, tok.Literal);
        index += 1;
    }
}
