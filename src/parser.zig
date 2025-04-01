const ast = @import("ast.zig");
const std = @import("std");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const debug = std.debug;
const testing = std.testing;
const assert = debug.assert;
const ArrayList = std.ArrayList;

pub const PrefixParseFn = fn () anyerror!ast.Expression; // Returns ast.Expression
pub const InfixParseFn = fn (ast.Expression) anyerror!ast.Expression; // Takes and returns ast.Expression

const TokenPrecedence = enum(u8) {
    LOWEST = 1,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
};

const Parser = struct {
    lexer: *lexer.Lexer,
    curToken: token.Token = undefined,
    peekToken: token.Token = undefined,
    allocator: std.mem.Allocator,
    errors: ArrayList([]const u8),

    prexifParseFns: std.StringHashMap(*PrefixParseFn),
    infixParseFns: std.StringHashMap(*InfixParseFn),

    pub fn init(l: *lexer.Lexer, allocator: std.mem.Allocator) Parser {
        var p = Parser{
            .lexer = l,
            .allocator = allocator,
            .errors = ArrayList([]const u8).init(allocator),
            .prexifParseFns = std.StringHashMap(*PrefixParseFn).init(allocator),
            .infixParseFns = std.StringHashMap(*InfixParseFn).init(allocator),
        };

        const ptr = &p.parseIdentifier();
        //todo => fix this...
        p.registerPrefixParseFn(token.TokenType.IDENT, ptr);
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

    pub fn peekError(p: *Parser, t: token.TokenType) !void {
        const msg = try std.fmt.allocPrint(p.allocator, "expected nex to be {}, got {} instead", .{ t, p.peekToken.Type });
        try p.errors.append(msg);
    }

    pub inline fn parseProgram(p: *Parser) !ast.Program {
        var program = ast.Program.init(p.allocator);
        while (p.curToken.Type != token.TokenType.EOF) : (p.nextToken()) {
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
            token.TokenType.LET => {
                return .{ .letStatement = try p.parseLetStatement() };
            },
            token.TokenType.RETURN => {
                return .{ .returnStatement = try p.parseReturnStatement() };
            },
            else => {
                const stmt = try p.parseExpressionStatement();
                _ = stmt;
                return .{ .err = ast.AstParseError.UnexpectedToken };
                // return .{ .expressionStatement = try p.parseExpressionStatement() };
            },
        }
    }

    pub inline fn parseLetStatement(p: *Parser) !ast.LetStatement {
        var stmt = ast.LetStatement{ .Token = p.curToken, .Name = undefined, .Value = undefined };

        const isIdent = p.expectPeek(token.TokenType.IDENT) catch {
            return error.ParseError;
        };
        _ = isIdent;

        //this gets destroed in  program.deinit
        //maybe there is a better way to do this...
        const identPtr = try p.allocator.create(ast.Identifier);
        identPtr.* = ast.Identifier{ .Token = p.curToken, .Value = p.curToken.Literal };

        stmt.Name = identPtr;

        const isAssign = p.expectPeek(token.TokenType.ASSIGN) catch {
            return error.ParseError;
        };
        _ = isAssign;

        while (p.curTokenIs(token.TokenType.SEMICOLON)) : (p.nextToken()) {}

        return stmt;
    }

    pub inline fn parseReturnStatement(p: *Parser) !ast.ReturnStatement {
        var stmt = ast.ReturnStatement.init(p.curToken);

        const isIdent = p.expectPeek(token.TokenType.IDENT) catch {
            return error.ParseError;
        };
        _ = isIdent;

        const isAssign = p.expectPeek(token.TokenType.ASSIGN) catch {
            return error.ParseError;
        };
        _ = isAssign;

        while (p.curTokenIs(token.TokenType.SEMICOLON)) : (p.nextToken()) {}

        const identPtr = try p.allocator.create(ast.Identifier);
        identPtr.* = ast.Identifier{ .Token = p.peekToken, .Value = p.peekToken.Literal };
        stmt.ReturnValue = identPtr;

        return stmt;
    }

    pub inline fn parseExpressionStatement(p: *Parser) !ast.ExpressionStatement {
        // pub inline fn parseExpressionStatement(p: *Parser) !ast.ExpressionStatement {
        const stmt = ast.ExpressionStatement.init(p.curToken);
        const expr = p.parseExpression(TokenPrecedence.LOWEST);
        _ = expr;
        // stmt.Exp = expr;
        if (p.peekTokenIs(token.TokenType.SEMICOLON)) {
            p.nextToken();
        }
        return stmt;
    }

    pub inline fn parseExpression(p: *Parser, precedence: TokenPrecedence) ast.Expression {
        _ = precedence;
        // ast.Expression
        const prefix = p.prexifParseFns.get(p.curToken.Literal);
        if (prefix == null) {
            std.debug.print("prefix: {any}\n", .{prefix});
            // return null;
        }
        const expr = ast.Expression.init();
        return expr;

        // if (prefix) |pfn| {
        //     return pfn();
        // } else {
        //     return null;
        // }
    }

    pub inline fn parseIdentifier(p: *Parser) ast.Identifier {
        return ast.Identifier{
            .Token = p.curToken,
            .Value = p.curToken.Literal,
        };
    }

    pub inline fn registerPrefixParseFn(p: *Parser, tokenType: token.TokenType, fnPtr: *PrefixParseFn) void {
        try p.prexifParseFns.put(@intFromEnum(tokenType), fnPtr);
    }

    pub inline fn registerInfixParseFn(p: *Parser, toknType: token.TokenType, fnPtr: *InfixParseFn) void {
        try p.infixParseFns.put(@intFromEnum(toknType), fnPtr);
    }

    pub fn curTokenIs(p: *Parser, tokenType: token.TokenType) bool {
        return p.curToken.Type == tokenType;
    }

    pub fn peekTokenIs(p: *Parser, tokenType: token.TokenType) bool {
        return p.peekToken.Type == tokenType;
    }

    pub fn expectPeek(p: *Parser, tokenType: token.TokenType) !bool {
        if (p.peekTokenIs(tokenType)) {
            p.nextToken();
            return true;
        } else {
            try p.peekError(tokenType);
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

// test "parse itentifer expression" {
//     const testAlloc = testing.allocator;
//     const input = "foobar;";
//     var l = lexer.Lexer.init(input);
//     var p = Parser.init(&l, testAlloc);
//     defer p.deinit();
//     // try p.checkParseError();
//     var program = try p.parseProgram();
//     defer program.deinit();
//
//     if (program.Statements.items.len != 1) {
//         debug.panic("program.Statements.items.len != 1, got len: {}", .{program.Statements.items.len});
//     }
//
//     // const stmt = program.Statements.items[0].expressionStatement;
//     //
//     // const ident = stmt.Exp.expressionNode();
//     // _ = ident;
// }
