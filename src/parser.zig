const ast = @import("ast.zig");
const std = @import("std");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const debug = std.debug;

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
                    debug.print("parse error {}\n", .{err});
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
                // return null;
            },
        }
    }

    pub inline fn parseLetStatement(p: *Parser) !ast.LetStatement {
        var stmt = ast.LetStatement{ .Token = p.curToken, .Value = undefined, .Name = undefined };

        if (!p.expectPeek(token.TokenType.IDENT)) return error.ParseError;

        var ident = ast.Identifier{ .Token = p.curToken, .Value = p.curToken.Literal };

        stmt.Name = &ident;

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
    const testAlloc = std.testing.allocator;
    const input =
        \\ let x = 5;
        \\ let y = 10;
        \\ let foobar = 939393;
    ;

    var l = lexer.Lexer.init(input);
    var p = Parser.init(&l, testAlloc);
    var program = try p.parseProgram();
    std.debug.print("program: {any}\n", .{program});
    defer program.deinit();
    // defer p.allocator.destroy(program);

    // _ = program;
    // std.debug.print("token: {any}\n", .{program});
    // if (p.parseProgram().Statements.len != 3) {
    //     debug.panic("parse error");
    // }
    // std.debug.print("token: {s}\n", .{p.lexer.input});

    // _ = p;
}
