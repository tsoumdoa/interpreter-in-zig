const std = @import("std");
const testing = std.testing;

// const TokenType = []const u8;
pub const TokenType = enum {
    ILLEGAL,
    EOF,

    IDENT,
    INT,

    ASSIGN,
    PLUS,

    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,

    LBRACE,
    RBRACE,

    FUNCTION,
    LET,
};
pub const Token = struct {
    Type: TokenType,
    Literal: []const u8,
};

test "can initialize token" {
    // thee is no type safety
    const token = Token{
        .Type = TokenType.IDENT,
        .Literal = "hello",
    };
    try std.testing.expectEqualStrings("hello", token.Literal);
}
