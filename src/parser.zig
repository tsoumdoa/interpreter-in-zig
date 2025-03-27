const ast = @import("ast.zig");
const std = @import("std");
const lexer = @import("lexer.zig");
const token = @import("token.zig");

const Parser = struct {
    lexer: *lexer.Lexer,
    curToken: token.Token = undefined,
    peekToken: token.Token = undefined,

    pub fn init(l: *lexer.Lexer) Parser {
        var p = Parser{ .lexer = l };
        p.nextToken();
        p.nextToken();
        return p;
    }

    pub fn nextToken(p: *Parser) void {
        p.curToken = p.peekToken;
        p.peekToken = p.lexer.nextToken();
    }

    pub fn parseProgram(p: *Parser) !*ast.Program {
        var astree = ast.Program.init();
        astree.Statements = p.parseStatement();

        while (p.curToken.Type != token.TokenType.EOF) : (p.nextToken()) {
            const stmt = p.parseStatement();
            if (stmt) |s| try astree.Statements.append(s);
        }

        return &astree;
    }

    pub fn parseStatement(p: *Parser) ast.Statement {
        switch (p.curToken.Type) {
            token.TokenType.LET => {
                return p.parseLetStatement();
            },
            else => {
                return null;
            },
        }
    }

    pub fn parseLetStatement(p: *Parser) ast.LetStatement {
        const stmt = ast.LetStatement{ .Token = p.curToken };

        if (!p.expectPeek(token.TokenType.IDENT)) return null;

        stmt.Name = &ast.Identifier{ .Token = p.curToken, .Value = p.curToken.Literal };

        if (!p.expectPeek(token.TokenType.ASSIGN)) return null;

        while (p.curTokenIs(token.TokenType.SEMICOLON)) : (p.nextToken()) {}

        return stmt;
    }

    pub fn curTokenIs(p: *Parser, tokenType: token.TokenType) bool {
        return p.curToken.Type == tokenType;
    }

    pub fn peekTokenIs(p: *Parser, tokenType: token.TokenType) bool {
        return p.peekToken.Type == tokenType;
    }

    pub fn expectPeek(p: *Parser, tokenType: token.TokenType) true {
        if (p.peekTokenIs(tokenType)) {
            p.nextToken();
            return true;
        } else {
            return false;
        }
    }
};

test "parse let statement" {
    const input =
        \\ let x = 5;
        \\ let y = 10;
        \\ let foobar = 939393;
    ;

    var l = lexer.Lexer.init(input);
    const p = Parser.init(&l);
    std.debug.print("token: {s}\n", .{p.lexer.input});
    // _ = p;
}
