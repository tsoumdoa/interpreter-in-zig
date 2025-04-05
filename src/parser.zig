const ast = @import("ast.zig");
const std = @import("std");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const TokenType = token.TokenType;
const debug = std.debug;
const testing = std.testing;
const assert = debug.assert;
const ArrayList = std.ArrayList;

const parseError = error{
    ParseError,
    UnexpectedToken,
    InvalidSyntax,
    Overflow,
    InvalidCharacter,
    OutOfMemory,
    // ... other errors
};

pub const TokenPrecedence = enum(u8) {
    LOWEST = 1,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
};

const precedences = std.StaticStringMap(u8).initComptime(.{
    .{ @tagName(TokenType.EQ), @intFromEnum(TokenPrecedence.EQUALS) },
    .{ @tagName(TokenType.NOT_EQ), @intFromEnum(TokenPrecedence.EQUALS) },
    .{ @tagName(TokenType.LT), @intFromEnum(TokenPrecedence.LESSGREATER) },
    .{ @tagName(TokenType.GT), @intFromEnum(TokenPrecedence.LESSGREATER) },
    .{ @tagName(TokenType.SUM), @intFromEnum(TokenPrecedence.SUM) },
    .{ @tagName(TokenType.MINUS), @intFromEnum(TokenPrecedence.SUM) },
    .{ @tagName(TokenType.SLASH), @intFromEnum(TokenPrecedence.PRODUCT) },
    .{ @tagName(TokenType.PRODUCT), @intFromEnum(TokenPrecedence.PRODUCT) },
});

pub const Parser = struct {
    lexer: *lexer.Lexer,
    curToken: token.Token = undefined,
    peekToken: token.Token = undefined,
    allocator: std.mem.Allocator,
    errors: ArrayList([]const u8),
    expressions: ArrayList(*ast.Expression),

    pub fn init(l: *lexer.Lexer, allocator: std.mem.Allocator) Parser {
        var p = Parser{
            .lexer = l,
            .allocator = allocator,
            .errors = ArrayList([]const u8).init(allocator),
            .expressions = ArrayList(*ast.Expression).init(allocator),
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
        for (p.expressions.items) |expr| {
            p.allocator.destroy(expr);
        }
        p.expressions.deinit();
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

    pub fn parseExpression(p: *Parser, precedence: TokenPrecedence) parseError!ast.Expression {
        var leftExpression = try p.allocator.create(ast.Expression);
        try p.expressions.append(leftExpression);
        switch (p.curToken.Type) {
            TokenType.IDENT => leftExpression.* = .{ .ident = p.parseIdentifier() },
            TokenType.INT => leftExpression.* = .{ .int = try p.parseInteger() },
            TokenType.BANG => leftExpression.* = .{ .prefix = try p.parsePrefixExpression() },
            TokenType.MINUS => leftExpression.* = .{ .prefix = try p.parsePrefixExpression() },
            TokenType.SUM => {},
            TokenType.SLASH => {},
            TokenType.PRODUCT => {},
            TokenType.EQ => {},
            TokenType.NOT_EQ => {},
            TokenType.LT => {},
            TokenType.GT => {},
            else => {
                try p.noPrefixParaseFnError(p.curToken.Type);
            },
        }

        while (!p.curTokenIs(TokenType.SEMICOLON) and @intFromEnum(precedence) < p.peekPrecedence()) {
            p.nextToken();
            const infixExpression = try p.allocator.create(ast.Expression);
            try p.expressions.append(infixExpression);
            // var infixExpression = ast.Expression{ .err = ast.AstParseError.UnexpectedToken };
            switch (p.curToken.Type) {
                TokenType.IDENT => infixExpression.* = .{ .infix = try p.parseInfixExpression(leftExpression) },
                TokenType.INT => infixExpression.* = .{ .infix = try p.parseInfixExpression(leftExpression) },
                TokenType.SUM => infixExpression.* = .{ .infix = try p.parseInfixExpression(leftExpression) },
                TokenType.MINUS => infixExpression.* = .{ .infix = try p.parseInfixExpression(leftExpression) },
                TokenType.SLASH => infixExpression.* = .{ .infix = try p.parseInfixExpression(leftExpression) },
                TokenType.PRODUCT => infixExpression.* = .{ .infix = try p.parseInfixExpression(leftExpression) },
                TokenType.EQ => infixExpression.* = .{ .infix = try p.parseInfixExpression(leftExpression) },
                TokenType.NOT_EQ => infixExpression.* = .{ .infix = try p.parseInfixExpression(leftExpression) },
                TokenType.LT => infixExpression.* = .{ .infix = try p.parseInfixExpression(leftExpression) },
                TokenType.GT => infixExpression.* = .{ .infix = try p.parseInfixExpression(leftExpression) },
                else => {
                    try p.noPrefixParaseFnError(p.curToken.Type);
                },
            }

            leftExpression = infixExpression;
        }
        return leftExpression.*;
    }

    pub inline fn parsePrefixExpression(p: *Parser) !ast.Prefix {
        var prefix = ast.Prefix{
            .Token = p.curToken,
            .Operator = p.curToken.Literal,
            .Right = undefined,
        };
        p.nextToken();
        const right = try p.allocator.create(ast.Expression);
        right.* = try p.parseExpression(TokenPrecedence.PREFIX);
        try p.expressions.append(right);
        prefix.Right = right;
        return prefix;
    }

    pub inline fn parseInfixExpression(p: *Parser, left: *ast.Expression) !ast.Infix {
        // const leftNew = try p.allocator.create(ast.Expression);
        // leftNew.* = left.*;

        var infix = ast.Infix{
            .Token = p.curToken,
            .Operator = p.curToken.Literal,
            .Left = left,
            .Right = undefined,
        };
        const precedence = p.curPrecedence();
        p.nextToken();
        const right = try p.allocator.create(ast.Expression);
        right.* = try p.parseExpression(@as(TokenPrecedence, @enumFromInt(precedence)));
        try p.expressions.append(right);
        infix.Right = right;
        return infix;
    }

    pub inline fn parseIdentifier(p: *Parser) ast.Identifier {
        return ast.Identifier{ .Token = p.curToken, .Value = p.curToken.Literal };
    }

    pub inline fn parseInteger(p: *Parser) !ast.Integer {
        const parsed = try std.fmt.parseInt(i32, p.curToken.Literal, 10);
        return ast.Integer{ .Token = p.curToken, .Value = parsed };
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

    pub inline fn peekPrecedence(p: *Parser) u8 {
        const peeked = precedences.get(@tagName(p.peekToken.Type));
        if (peeked) |v| {
            return v;
        } else return @intFromEnum(TokenPrecedence.LOWEST);
    }

    pub inline fn curPrecedence(p: *Parser) u8 {
        const peeked = precedences.get(@tagName(p.curToken.Type));
        if (peeked) |v| {
            return v;
        } else return @intFromEnum(TokenPrecedence.LOWEST);
    }

    pub inline fn noPrefixParaseFnError(p: *Parser, t: TokenType) !void {
        const msg = try std.fmt.allocPrint(p.allocator, "no prefix function for {}", .{t});
        try p.errors.append(msg);
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
        const isReturn = program.Statements.items[i].returnStatement.tokenLiteral();
        try testing.expectEqualStrings("return", isReturn);
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
    try testing.expectEqual(5, program.Statements.items[0].expressionStatement.Exp.int.Value);
}

test "parse prefix expression" {
    const testAlloc = testing.allocator;
    const Test = struct {
        input: []const u8,
        prefix: []const u8,
        right: i32,
    };

    const tests = [_]Test{
        .{ .input = "!5;", .prefix = "!", .right = 5 },
        .{ .input = "-15;", .prefix = "-", .right = 15 },
    };

    for (tests) |t| {
        var l = lexer.Lexer.init(t.input);
        var p = Parser.init(&l, testAlloc);
        defer p.deinit();
        var program = try p.parseProgram();
        defer program.deinit();

        if (program.Statements.items.len != 1) {
            debug.panic("program.Statements.items.len != 1, got len: {}", .{program.Statements.items.len});
        }

        if (p.errors.items.len != 0) {
            for (p.errors.items) |err| {
                debug.print("error: {s}\n", .{err});
            }
        }

        const operator = program.Statements.items[0].expressionStatement.Exp.prefix.Operator;
        try testing.expectEqualStrings(t.prefix, operator);
        const res = program.Statements.items[0].expressionStatement.Exp.prefix.Right.int.Value;
        try testing.expectEqual(t.right, res);
    }
}

test "parse inline expression" {
    const testAlloc = testing.allocator;
    const InfixTest = struct {
        input: []const u8,
        leftValue: i32,
        operator: []const u8,
        rightValue: i32,
    };

    const tests = [_]InfixTest{
        .{
            .input = "5 + 5;",
            .leftValue = 5,
            .operator = "+",
            .rightValue = 5,
        },
        .{
            .input = "5 - 5;",
            .leftValue = 5,
            .operator = "-",
            .rightValue = 5,
        },
        .{
            .input = "5 * 5;",
            .leftValue = 5,
            .operator = "*",
            .rightValue = 5,
        },
        .{
            .input = "5 / 5;",
            .leftValue = 5,
            .operator = "/",
            .rightValue = 5,
        },
        .{
            .input = "5 < 5;",
            .leftValue = 5,
            .operator = "<",
            .rightValue = 5,
        },
        .{
            .input = "5 > 5;",
            .leftValue = 5,
            .operator = ">",
            .rightValue = 5,
        },
    };

    for (tests) |t| {
        var l = lexer.Lexer.init(t.input);
        var p = Parser.init(&l, testAlloc);
        defer p.deinit();
        var program = try p.parseProgram();
        defer program.deinit();
        if (program.Statements.items.len != 1) {
            debug.panic("program.Statements.items.len != 1, got len: {}", .{program.Statements.items.len});
        }
        if (p.errors.items.len != 0) {
            for (p.errors.items) |err| {
                debug.print("error: {s}\n", .{err});
            }
            debug.panic("parse error - check syntax", .{});
        }
        for (program.Statements.items) |stmt| {
            const exp = stmt.expressionStatement.Exp;

            switch (exp) {
                .infix => |infix| {
                    const left = infix.Left.*;
                    switch (left) {
                        .int => |int| {
                            try testing.expectEqual(t.leftValue, int.Value);
                        },
                        else => {},
                    }

                    try testing.expectEqualStrings(t.operator, infix.Operator);

                    const right = infix.Right.*;
                    switch (right) {
                        .int => |int| {
                            try testing.expectEqual(t.rightValue, int.Value);
                        },
                        else => {},
                    }
                },
                else => {},
            }
        }
    }
}

test "operator precedence parsing" {
    const testAlloc = testing.allocator;
    const OperatorPreTest = struct {
        input: []const u8,
        expected: []const u8,
    };

    const tests = [_]OperatorPreTest{
        .{
            .input = "-a * b;",
            .expected = "((-a) * b)",
        },
        .{
            .input = "!-a;",
            .expected = "(!(-a))",
        },
        .{
            .input = "a + b + c;",
            .expected = "((a + b) + c)",
        },
        .{
            .input = "a + b - c;",
            .expected = "((a + b) - c)",
        },
        .{
            .input = "a * b * c;",
            .expected = "((a * b) * c)",
        },
        .{
            .input = "a * b / c;",
            .expected = "((a * b) / c)",
        },
        .{
            .input = "a + b / c;",
            .expected = "(a + (b / c))",
        },
        .{
            .input = "a + b * c + d / e -f;",
            .expected = "(((a + (b * c)) + (d / e)) - f)",
        },
        .{
            .input = "3 + 4-5 * 5;",
            .expected = "((3 + 4) - (5 * 5))",
        },
        .{
            .input = "5 > 4 == 3 < 4;",
            .expected = "((5 > 4) == (3 < 4))",
        },
        .{
            .input = "5 < 4 != 3 > 4;",
            .expected = "((5 < 4) != (3 > 4))",
        },
        .{
            .input = "3 + 4 * 5 == 3 * 1 + 4 * 5;",
            .expected = "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        },
    };

    for (tests) |t| {
        var l = lexer.Lexer.init(t.input);
        var p = Parser.init(&l, testAlloc);
        defer p.deinit();
        var program = try p.parseProgram();
        defer program.deinit();
        if (program.Statements.items.len != 1) {
            debug.panic("program.Statements.items.len != 1, got len: {}", .{program.Statements.items.len});
        }
        if (p.errors.items.len != 0) {
            for (p.errors.items) |err| {
                debug.print("error: {s}\n", .{err});
            }
            debug.panic("parse error - check syntax", .{});
        }
        const actual = try program.string();
        defer actual.deinit();
        try testing.expectEqualStrings(t.expected, actual.items);
    }
}
