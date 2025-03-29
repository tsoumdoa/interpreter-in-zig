const ast = @import("ast.zig");
const std = @import("std");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const debug = std.debug;
const testing = std.testing;

const Parser = struct {
    lexer: *lexer.Lexer,
    curToken: token.Token = undefined,
    peekToken: token.Token = undefined,
    allocator: std.mem.Allocator,

    pub fn init(l: *lexer.Lexer, allocator: std.mem.Allocator) Parser {
        var p = Parser{ .lexer = l, .allocator = allocator };
        p.nextToken();
        p.nextToken();
        return p;
    }

    pub fn nextToken(p: *Parser) void {
        p.curToken = p.peekToken;
        p.peekToken = p.lexer.nextToken();
    }

    pub inline fn parseProgram(p: *Parser) !ast.Program {
        var program = ast.Program.init(p.allocator);
        while (p.curToken.Type != token.TokenType.EOF) : (p.nextToken()) {
            const stmt = try p.parseStatement();

            switch (stmt) {
                .err => |err| {
                    _ = err;
                    // debug.print("parse error {}\n", .{err});
                },
                else => {
                    try program.addStatement(stmt);
                },
            }
        }
        return program;
    }

    pub inline fn parseStatement(p: *Parser) !ast.Statement {
        switch (p.curToken.Type) {
            token.TokenType.LET => {
                return .{ .letStatement = try p.parseLetStatement() };
            },
            else => {
                return .{ .err = ast.AstParseError.UnexpectedToken };
            },
        }
    }

    pub inline fn parseLetStatement(p: *Parser) !ast.LetStatement {
        var stmt = ast.LetStatement{ .Token = p.curToken, .Value = undefined, .Name = undefined };

        if (!p.expectPeek(token.TokenType.IDENT)) return error.ParseError;

        //this gets destroed in  program.deinit
        //maybe there is a better way to do this...
        const identPtr = try p.allocator.create(ast.Identifier);
        const ident = ast.Identifier{ .Token = p.curToken, .Value = p.curToken.Literal };
        identPtr.* = ident;

        stmt.Name = identPtr;

        if (!p.expectPeek(token.TokenType.ASSIGN)) return error.ParseError;

        while (p.curTokenIs(token.TokenType.SEMICOLON)) : (p.nextToken()) {}

        return stmt;
    }

    pub fn curTokenIs(p: *Parser, tokenType: token.TokenType) bool {
        return p.curToken.Type == tokenType;
    }

    pub fn peekTokenIs(p: *Parser, tokenType: token.TokenType) bool {
        return p.peekToken.Type == tokenType;
    }

    pub fn expectPeek(p: *Parser, tokenType: token.TokenType) bool {
        if (p.peekTokenIs(tokenType)) {
            p.nextToken();
            return true;
        } else {
            return false;
        }
    }
};

test "parse let statement" {
    const testAlloc = testing.allocator;
    const input =
        \\ let x = 5;
        \\ let y = 10;
        \\ let foobar = 939393;
    ;

    const expectedIdentiefers = [_][]const u8{ "x", "y", "foobar" };

    var l = lexer.Lexer.init(input);
    var p = Parser.init(&l, testAlloc);
    var program = try p.parseProgram();
    defer program.deinit();

    if (program.Statements.items.len != 3) {
        debug.panic("program.Statements.items.len != 3, got len: {}", .{program.Statements.items.len});
    }

    for (expectedIdentiefers, 0..) |expectedIdent, i| {
        const ident = program.Statements.items[i].letStatement.Name;
        try testing.expectEqualStrings(expectedIdent, ident.Value);
    }
}
