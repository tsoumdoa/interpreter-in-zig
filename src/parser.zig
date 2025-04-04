const ast = @import("ast.zig");
const std = @import("std");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const TokenType = token.TokenType;
const debug = std.debug;
const testing = std.testing;
const assert = debug.assert;
const ArrayList = std.ArrayList;

const TokenPrecedence = enum(u8) {
    LOWEST = 1,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
};

const precedences = std.StaticStringMap(TokenPrecedence, .{
    .{ .key = "==", .value = TokenPrecedence.EQ },
});

const Parser = struct {
    lexer: *lexer.Lexer,
    curToken: token.Token = undefined,
    peekToken: token.Token = undefined,
    allocator: std.mem.Allocator,
    errors: ArrayList([]const u8),

    pub fn init(l: *lexer.Lexer, allocator: std.mem.Allocator) Parser {
        var p = Parser{
            .lexer = l,
            .allocator = allocator,
            .errors = ArrayList([]const u8).init(allocator),
        };

        p.nextToken();
        p.nextToken();
        return p;
    }

    pub fn deinit(p: *Parser) void {
        for (p.errors.items) |err| {
            p.allocator.free(err);
        }
        p.errors.deinit();
    }

    pub fn nextToken(p: *Parser) void {
        p.curToken = p.peekToken;
        p.peekToken = p.lexer.nextToken();
    }

    pub fn peekError(p: *Parser, t: TokenType) !void {
        const msg = try std.fmt.allocPrint(p.allocator, "expected nex to be {}, got {} instead", .{ t, p.peekToken.Type });
        try p.errors.append(msg);
    }

    pub inline fn parseProgram(p: *Parser) !ast.Program {
        var program = ast.Program.init(p.allocator);
        while (p.curToken.Type != TokenType.EOF) : (p.nextToken()) {
            const stmt = try p.parseStatement();
            switch (stmt) {
                .err => |err| {
                    _ = err;
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
            TokenType.LET => {
                return .{ .letStatement = try p.parseLetStatement() };
            },
            TokenType.RETURN => {
                return .{ .returnStatement = try p.parseReturnStatement() };
            },
            else => {
                // return .{ .err = ast.AstParseError.UnexpectedToken }; // TODO: implement error handling
                return .{ .expressionStatement = try p.parseExpressionStatement() };
            },
        }
    }

    pub inline fn parseLetStatement(p: *Parser) !ast.LetStatement {
        var stmt = ast.LetStatement{ .Token = p.curToken, .Name = undefined, .Value = undefined };

        const isIdent = p.expectPeek(TokenType.IDENT) catch {
            return error.ParseError;
        };
        _ = isIdent;

        //this gets destroed in  program.deinit
        //maybe there is a better way to do this...
        const identPtr = try p.allocator.create(ast.Identifier);
        identPtr.* = ast.Identifier{ .Token = p.curToken, .Value = p.curToken.Literal };

        stmt.Name = identPtr;

        const isAssign = p.expectPeek(TokenType.ASSIGN) catch {
            return error.ParseError;
        };
        _ = isAssign;

        while (!p.curTokenIs(TokenType.SEMICOLON)) : (p.nextToken()) {}

        return stmt;
    }

    pub inline fn parseReturnStatement(p: *Parser) !ast.ReturnStatement {
        var stmt = ast.ReturnStatement.init(p.curToken);
        p.nextToken();

        const identPtr = try p.allocator.create(ast.Identifier);
        identPtr.* = ast.Identifier{ .Token = p.curToken, .Value = p.curToken.Literal };
        stmt.ReturnValue = identPtr;

        while (!p.curTokenIs(TokenType.SEMICOLON)) : (p.nextToken()) {}

        return stmt;
    }

    pub inline fn parseExpressionStatement(p: *Parser) !ast.ExpressionStatement {
        var stmt = ast.ExpressionStatement.init(p.curToken);
        const expr = try p.parseExpression(TokenPrecedence.LOWEST);
        stmt.Exp = expr;
        while (!p.curTokenIs(TokenType.SEMICOLON)) : (p.nextToken()) {}
        return stmt;
    }

    pub inline fn parseExpression(p: *Parser, precedence: TokenPrecedence) !ast.Expression {
        while (!p.curTokenIs(TokenType.SEMICOLON) and @intFromEnum(precedence) < @intFromEnum(p.peekPrecedent())) : (p.nextToken()) {}

        switch (p.curToken.Type) {
            TokenType.IDENT => {
                return .{ .ident = p.parseIdentifier() };
            },
            TokenType.INT => {
                return .{ .int = p.parseInteger() };
            },
            else => return .{ .err = ast.AstParseError.UnexpectedToken },
        }
    }

    pub inline fn parseIdentifier(p: *Parser) ast.Identifier {
        return ast.Identifier{ .Token = p.curToken, .Value = p.curToken.Literal };
    }

    pub inline fn parseInteger(p: *Parser) ast.Integer {
        return ast.Integer{ .Token = p.curToken, .Value = p.curToken.Literal };
    }

    pub inline fn curTokenIs(p: *Parser, tokenType: TokenType) bool {
        return p.curToken.Type == tokenType;
    }

    pub inline fn peekTokenIs(p: *Parser, tokenType: TokenType) bool {
        return p.peekToken.Type == tokenType;
    }

    pub inline fn expectPeek(p: *Parser, tokenType: TokenType) !bool {
        if (p.peekTokenIs(tokenType)) {
            p.nextToken();
            return true;
        } else {
            try p.peekError(tokenType);
            return false;
        }
    }

    pub inline fn peekPrecedent(p: *Parser) TokenPrecedence {
        _ = p;

        return TokenPrecedence.LOWEST;
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

test "parse let statement with invalid" {
    const testAlloc = testing.allocator;
    const input =
        \\ let x 5;
        \\ let  = 10;
        \\ let 939393;
    ;

    const expectedErrors = [_][]const u8{
        "expected nex to be token.TokenType.ASSIGN, got token.TokenType.INT instead",
        "expected nex to be token.TokenType.IDENT, got token.TokenType.ASSIGN instead",
        "expected nex to be token.TokenType.IDENT, got token.TokenType.INT instead",
        "expected nex to be token.TokenType.ASSIGN, got token.TokenType.INT instead",
    };

    var l = lexer.Lexer.init(input);
    var p = Parser.init(&l, testAlloc);
    defer p.deinit();
    var program = try p.parseProgram();
    defer program.deinit();

    for (p.errors.items, 0..) |err, i| {
        try testing.expectEqualStrings(expectedErrors[i], err);
    }
}

test "parse return ``return` statement" {
    const testAlloc = testing.allocator;
    const input =
        \\ return 5;
        \\ return 10;
        \\ return 939393;
    ;

    const expectedIdentiefers = [_][]const u8{ "5", "10", "939393" };

    var l = lexer.Lexer.init(input);
    var p = Parser.init(&l, testAlloc);
    defer p.deinit();
    var program = try p.parseProgram();
    defer program.deinit();

    if (program.Statements.items.len != 3) {
        debug.panic("program.Statements.items.len != 3, got len: {}", .{program.Statements.items.len});
    }

    for (expectedIdentiefers, 0..) |expectedIdent, i| {
        const ident = program.Statements.items[i].returnStatement.ReturnValue;
        try testing.expectEqualStrings(expectedIdent, ident.Value);
    }
}

test "token precedence" {
    const lowest = @intFromEnum(TokenPrecedence.LOWEST);
    try testing.expectEqual(1, lowest);
    const highest = @intFromEnum(TokenPrecedence.CALL);
    try testing.expectEqual(7, highest);
}

test "parse itentifer expression" {
    const testAlloc = testing.allocator;
    const input = "foobar;";
    var l = lexer.Lexer.init(input);
    var p = Parser.init(&l, testAlloc);
    defer p.deinit();
    var program = try p.parseProgram();
    defer program.deinit();

    if (program.Statements.items.len != 1) {
        debug.panic("program.Statements.items.len != 1, got len: {}", .{program.Statements.items.len});
    }

    var testArrayBuffer = ArrayList(u8).init(testAlloc);
    defer testArrayBuffer.deinit();
    try testing.expectEqual(program.Statements.items[0].expressionStatement.Token.Type, TokenType.IDENT);
    try program.Statements.items[0].expressionStatement.string(&testArrayBuffer);
    try testing.expectEqualStrings("foobar", testArrayBuffer.items);
}

test "parse integer literal expression" {
    const testAlloc = testing.allocator;
    const input = "5;";
    var l = lexer.Lexer.init(input);
    var p = Parser.init(&l, testAlloc);
    defer p.deinit();
    var program = try p.parseProgram();
    defer program.deinit();

    if (program.Statements.items.len != 1) {
        debug.panic("program.Statements.items.len != 1, got len: {}", .{program.Statements.items.len});
    }
    try testing.expectEqual(program.Statements.items[0].expressionStatement.Token.Type, TokenType.INT);
    try testing.expectEqualStrings("5", program.Statements.items[0].expressionStatement.Exp.int.Value);
}
