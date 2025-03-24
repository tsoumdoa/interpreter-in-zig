<<<<<<< ours
const std = @import("std");
const testing = std.testing;

pub const TokenType = enum {
    ILLEGAL,
    EOF,

    IDENT,
    INT,

    ASSIGN,
    PLUS,

    MINUS,
    BANG,
    ASTERISK,
    SLASH,

    LT, // <
    GT, // >

    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,

    LBRACE,
    RBRACE,

    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,

    EQ,
    NOT_EQ,
};

pub const Keywords = [_]KeywordType{
    .{ .keyword = "let", .tokenType = TokenType.LET },
    .{ .keyword = "fn", .tokenType = TokenType.FUNCTION },
    .{ .keyword = "true", .tokenType = TokenType.TRUE },
    .{ .keyword = "false", .tokenType = TokenType.FALSE },
    .{ .keyword = "if", .tokenType = TokenType.IF },
    .{ .keyword = "else", .tokenType = TokenType.ELSE },
    .{ .keyword = "return", .tokenType = TokenType.RETURN },
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

test "lookupIdent" {
    try std.testing.expectEqual(TokenType.LET, lookupIdent("let"));
    try std.testing.expectEqual(TokenType.FUNCTION, lookupIdent("fn"));
}
|||||||
=======
const std = @import("std");
const testing = std.testing;

const TokenType = []const u8;
pub const Token = struct {
    Type: TokenType,
    Literal: []const u8,
};

pub const ILLEGAL = "ILLEGAL";
pub const EOF = "EOF";

pub const IDENT = "IDENT"; //add, foobar, x, y,...
pub const INT = "INT"; //123, 0, 1,...

pub const ASSIGN = "=";
pub const PLUS = "+";

pub const COMMA = ",";
pub const SEMICOLON = ";";

pub const LPAREN = "(";
pub const RPAREN = ")";

pub const LBRACE = "{";
pub const RBRACE = "}";

pub const FUNCTION = "FUNCTION";
pub const LET = "LET";

test "can initialize token" {
    // thee is no type safety
    const token = Token{
        .Type = "INT",
        .Literal = "hello",
    };
    try std.testing.expectEqualStrings("hello", token.Literal);
}
>>>>>>> theirs
