const std = @import("std");
const testing = std.testing;

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

pub const Keywords = [_]KeywordType{
    .{ .keyword = "let", .tokenType = TokenType.LET },
    .{ .keyword = "fn", .tokenType = TokenType.FUNCTION },
};

pub const KeywordType = struct {
    keyword: []const u8,
    tokenType: TokenType,
};

pub fn lookupIdent(keyword: []const u8) TokenType {
    for (Keywords) |kw| {
        if (std.mem.eql(u8, kw.keyword, keyword)) {
            return kw.tokenType;
        }
    }
    return TokenType.IDENT;
}

pub const Token = struct {
    Type: TokenType,
    Literal: []const u8,
};

test "can initialize token" {
    const token = Token{
        .Type = TokenType.IDENT,
        .Literal = "hello",
    };
    try std.testing.expectEqualStrings("hello", token.Literal);
    try std.testing.expectEqual(TokenType.IDENT, lookupIdent(token.Literal));
}
