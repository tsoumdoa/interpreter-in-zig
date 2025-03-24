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
