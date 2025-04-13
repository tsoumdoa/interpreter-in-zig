const ast = @import("ast.zig");
const std = @import("std");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const TokenType = token.TokenType;
const debug = std.debug;
const testing = std.testing;
const assert = debug.assert;
const ArrayList = std.ArrayList;
const ParseError = ast.ParseError;

const parseError = error{
    ParseError,
    UnexpectedToken,
    InvalidSyntax,
    Overflow,
    InvalidCharacter,
    OutOfMemory,
    InvalidIfExpression,
    InvalidFunctionLiteral,
    NoRparen,
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
    .{ @tagName(TokenType.LPAREN), @intFromEnum(TokenPrecedence.CALL) },
});

pub const Parser = struct {
    lexer: *lexer.Lexer,
    curToken: token.Token = undefined,
    peekToken: token.Token = undefined,
    allocator: std.mem.Allocator,
    errors: ArrayList([]const u8),
    expressions: ArrayList(*ast.Expression),
    calls: ArrayList(*ast.CallExpression),
    blocks: ArrayList(*ast.BlockStatement),
    params: ArrayList(*ArrayList(*ast.Identifier)),
    callExpressions: ArrayList(*ArrayList(*ast.Expression)),

    pub fn init(l: *lexer.Lexer, allocator: std.mem.Allocator) Parser {
        var p = Parser{
            .lexer = l,
            .allocator = allocator,
            .errors = ArrayList([]const u8).init(allocator),
            .expressions = ArrayList(*ast.Expression).init(allocator),
            .calls = ArrayList(*ast.CallExpression).init(allocator),
            .blocks = ArrayList(*ast.BlockStatement).init(allocator),
            .callExpressions = ArrayList(*ArrayList(*ast.Expression)).init(allocator),
            .params = ArrayList(*ArrayList(*ast.Identifier)).init(allocator),
        };

        p.nextToken();
        p.nextToken();
        return p;
    }

    pub inline fn deinit(p: *Parser) void {
        for (p.errors.items) |err| {
            p.allocator.free(err);
        }
        p.errors.deinit();

        for (p.callExpressions.items) |args| {
            args.deinit();
        }
        p.callExpressions.deinit();

        for (p.calls.items) |call| {
            call.deinit();
            p.allocator.destroy(call);
        }
        p.calls.deinit();

        for (p.blocks.items) |block| {
            block.deinit();
            p.allocator.destroy(block);
        }
        p.blocks.deinit();

        for (p.params.items) |param| {
            param.deinit();
        }
        p.params.deinit();

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

        while (!p.curTokenIs(TokenType.SEMICOLON) and !p.peekTokenIs(TokenType.EOF)) : (p.nextToken()) {}

        return stmt;
    }

    pub inline fn parseReturnStatement(p: *Parser) !ast.ReturnStatement {
        var stmt = ast.ReturnStatement.init(p.curToken);
        p.nextToken();

        const identPtr = try p.allocator.create(ast.Identifier);
        identPtr.* = ast.Identifier{ .Token = p.curToken, .Value = p.curToken.Literal };
        stmt.ReturnValue = identPtr;

        while (!p.curTokenIs(TokenType.SEMICOLON) and !p.peekTokenIs(TokenType.EOF)) : (p.nextToken()) {}

        return stmt;
    }

    pub inline fn parseExpressionStatement(p: *Parser) !ast.ExpressionStatement {
        var stmt = ast.ExpressionStatement.init(p.curToken);
        const expr = p.parseExpression(TokenPrecedence.LOWEST) catch |err| {
            std.debug.print("\x1b[1;31m", .{});
            std.debug.print("parseExpressionStatement err, reason: {}\n", .{err});
            std.debug.print("\x1b[0m", .{});
            return err;
        };
        stmt.Exp = expr;
        if (p.peekTokenIs(TokenType.SEMICOLON)) p.nextToken();
        return stmt;
    }

    pub fn parseExpression(p: *Parser, precedence: TokenPrecedence) parseError!*ast.Expression {
        var leftExpression = try p.allocator.create(ast.Expression);
        try p.expressions.append(leftExpression);
        switch (p.curToken.Type) {
            TokenType.IDENT => leftExpression.* = .{ .ident = p.parseIdentifier() },
            TokenType.INT => leftExpression.* = .{ .int = try p.parseInteger() },
            TokenType.TRUE => leftExpression.* = .{ .boolean = try p.parseBoolean() },
            TokenType.FALSE => leftExpression.* = .{ .boolean = try p.parseBoolean() },
            TokenType.BANG => leftExpression.* = .{ .prefix = try p.parsePrefixExpression() },
            TokenType.MINUS => leftExpression.* = .{ .prefix = try p.parsePrefixExpression() },
            TokenType.IF => leftExpression.* = .{ .ifelse = try p.parseIfExpression() },
            TokenType.LPAREN => leftExpression = try p.parseGroupExpression(),
            TokenType.FUNCTION => leftExpression.* = .{ .functionLiteral = try p.parseFunctionLiteral() },
            TokenType.EOF => {},
            TokenType.RPAREN => {},
            TokenType.SEMICOLON => {},
            TokenType.ELSE => {},
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

        while (!p.peekTokenIs(TokenType.SEMICOLON) and @intFromEnum(precedence) < p.peekPrecedence()) {
            p.nextToken();
            const infixExpression = try p.allocator.create(ast.Expression);
            try p.expressions.append(infixExpression);
            switch (p.curToken.Type) {
                TokenType.IDENT => infixExpression.* = .{ .infix = try p.parseInfixExpression(leftExpression) },
                TokenType.INT => infixExpression.* = .{ .infix = try p.parseInfixExpression(leftExpression) },
                TokenType.SUM => infixExpression.* = .{ .infix = try p.parseInfixExpression(leftExpression) },
                TokenType.MINUS => infixExpression.* = .{ .infix = try p.parseInfixExpression(leftExpression) },
                TokenType.TRUE => leftExpression.* = .{ .infix = try p.parseInfixExpression(leftExpression) },
                TokenType.FALSE => leftExpression.* = .{ .infix = try p.parseInfixExpression(leftExpression) },

                TokenType.SLASH => infixExpression.* = .{ .infix = try p.parseInfixExpression(leftExpression) },
                TokenType.PRODUCT => infixExpression.* = .{ .infix = try p.parseInfixExpression(leftExpression) },
                TokenType.EQ => infixExpression.* = .{ .infix = try p.parseInfixExpression(leftExpression) },
                TokenType.NOT_EQ => infixExpression.* = .{ .infix = try p.parseInfixExpression(leftExpression) },
                TokenType.LT => infixExpression.* = .{ .infix = try p.parseInfixExpression(leftExpression) },
                TokenType.GT => infixExpression.* = .{ .infix = try p.parseInfixExpression(leftExpression) },
                TokenType.LPAREN => infixExpression.* = .{ .callExpression = try p.parseCallExpression(leftExpression) },
                else => {
                    try p.noPrefixParaseFnError(p.curToken.Type);
                },
            }

            leftExpression = infixExpression;
        }
        return leftExpression;
    }

    pub inline fn parsePrefixExpression(p: *Parser) !ast.Prefix {
        var prefix = ast.Prefix{
            .Token = p.curToken,
            .Operator = p.curToken.Literal,
            .Right = undefined,
        };
        p.nextToken();
        prefix.Right = try p.parseExpression(TokenPrecedence.PREFIX);
        return prefix;
    }

    pub inline fn parseInfixExpression(p: *Parser, left: *ast.Expression) !ast.Infix {
        var infix = ast.Infix{
            .Token = p.curToken,
            .Operator = p.curToken.Literal,
            .Left = left,
            .Right = undefined,
        };
        const precedence = p.curPrecedence();
        p.nextToken();
        infix.Right = try p.parseExpression(@as(TokenPrecedence, @enumFromInt(precedence)));
        return infix;
    }

    pub inline fn parseGroupExpression(p: *Parser) !*ast.Expression {
        p.nextToken();
        const exp = try p.parseExpression(TokenPrecedence.LOWEST);
        if (!try p.expectPeek(TokenType.RPAREN)) exp.* = ast.Expression{ .err = ast.AstParseError.NoRparen };
        return exp;
    }

    pub inline fn parseIfExpression(p: *Parser) !ast.IfElse {
        var ifelse = ast.IfElse{
            .Token = p.curToken,
            .Condition = undefined,
            .Consequence = undefined,
            .Alternative = null,
        };

        const isLparen = try p.expectPeek(TokenType.LPAREN);
        if (!isLparen) return parseError.InvalidIfExpression;

        p.nextToken();
        ifelse.Condition = try p.parseExpression(TokenPrecedence.LOWEST);

        const isRParen = try p.expectPeek(TokenType.RPAREN);
        if (!isRParen) return parseError.InvalidIfExpression;

        var isLbrace = try p.expectPeek(TokenType.LBRACE);
        if (!isLbrace) return parseError.InvalidIfExpression;

        ifelse.Consequence = try p.parseBlockStatement();

        p.nextToken();
        if (p.curTokenIs(TokenType.ELSE)) {
            isLbrace = try p.expectPeek(TokenType.LBRACE);
            if (!isLbrace) return parseError.InvalidIfExpression;

            ifelse.Alternative = try p.parseBlockStatement();
        }

        return ifelse;
    }

    pub inline fn parseBlockStatement(p: *Parser) !*ast.BlockStatement {
        var block = try p.allocator.create(ast.BlockStatement);
        block.* = ast.BlockStatement.init(p.allocator, p.curToken);
        try p.blocks.append(block);

        while (!p.curTokenIs(TokenType.RBRACE) and !p.curTokenIs(TokenType.EOF)) {
            p.nextToken();
            const stmt = try p.allocator.create(ast.Statement);
            defer p.allocator.destroy(stmt);
            stmt.* = try p.parseStatement();
            switch (stmt.*) {
                .err => |err| {
                    _ = err;
                },
                else => {
                    try block.addStatement(stmt);
                },
            }
            p.nextToken();
        }
        return block;
    }

    pub inline fn parseFunctionLiteral(p: *Parser) !ast.FunctionLiteral {
        var functionLiteral = ast.FunctionLiteral{
            .Token = p.curToken,
            .Params = undefined,
            .Body = undefined,
        };

        const isLParen = try p.expectPeek(TokenType.LPAREN);
        if (!isLParen) return parseError.InvalidFunctionLiteral;

        const params = try p.parseFunctionParams();
        functionLiteral.Params = params;

        const isLBrace = try p.expectPeek(TokenType.LBRACE);
        if (!isLBrace) return parseError.InvalidFunctionLiteral;

        functionLiteral.Body = try p.parseBlockStatement();

        return functionLiteral;
    }

    pub inline fn parseFunctionParams(p: *Parser) !*ArrayList(*ast.Identifier) {
        var params = ArrayList(*ast.Identifier).init(p.allocator);
        try p.params.append(&params);

        if (p.peekTokenIs(TokenType.RPAREN)) {
            p.nextToken();
            return &params;
        }

        p.nextToken();
        {
            const identPtr = try p.allocator.create(ast.Identifier);
            identPtr.* = .{ .Token = p.curToken, .Value = p.curToken.Literal };
            defer p.allocator.destroy(identPtr);
            try params.append(identPtr);
        }

        while (p.peekTokenIs(TokenType.COMMA)) {
            p.nextToken();
            p.nextToken();
            {
                const identPtr = try p.allocator.create(ast.Identifier);
                identPtr.* = .{ .Token = p.curToken, .Value = p.curToken.Literal };
                defer p.allocator.destroy(identPtr);
                try params.append(identPtr);
            }
        }

        const isRParen = try p.expectPeek(TokenType.RPAREN);
        if (!isRParen) return parseError.NoRparen;

        return &params;
    }
    pub inline fn parseCallExpression(p: *Parser, function: *ast.Expression) !*ast.CallExpression {
        var exp = try p.allocator.create(ast.CallExpression);
        exp.* = ast.CallExpression.init(p.allocator, p.curToken, function);
        try p.parseCallArguments(&exp.Arguments);
        try p.calls.append(exp);
        return exp;
    }

    pub inline fn parseCallArguments(p: *Parser, args: *ArrayList(*ast.Expression)) !void {
        var isRParen = p.peekTokenIs(TokenType.RPAREN);
        if (isRParen) {
            p.nextToken();
            const nullPtr = try p.allocator.create(ast.Expression);
            nullPtr.* = .{ .null = {} };
            try p.expressions.append(nullPtr);
            try args.append(nullPtr);
            return;
        }

        p.nextToken();
        try args.append(try p.parseExpression(TokenPrecedence.LOWEST));

        while (p.peekTokenIs(TokenType.COMMA)) {
            p.nextToken();
            p.nextToken();
            try args.append(try p.parseExpression(TokenPrecedence.LOWEST));
        }

        isRParen = try p.expectPeek(TokenType.RPAREN);
        if (!isRParen) return parseError.NoRparen;
        return;
    }

    pub inline fn parseIdentifier(p: *Parser) ast.Identifier {
        return ast.Identifier{ .Token = p.curToken, .Value = p.curToken.Literal };
    }

    pub inline fn parseInteger(p: *Parser) !ast.Integer {
        const parsed = try std.fmt.parseInt(i32, p.curToken.Literal, 10);
        return ast.Integer{ .Token = p.curToken, .Value = parsed };
    }

    pub inline fn parseBoolean(p: *Parser) !ast.Boolean {
        const parsed = p.curToken.Literal[0] == 't';
        return ast.Boolean{ .Token = p.curToken, .Value = parsed };
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
    defer p.deinit();
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

test "parse identifer expression" {
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

test "parse boolean expression 1" {
    const testAlloc = testing.allocator;
    const Test = struct {
        input: []const u8,
        type: TokenType,
        value: bool,
    };

    const tests = [_]Test{
        .{ .input = "true;", .type = TokenType.TRUE, .value = true },
        .{ .input = "false;", .type = TokenType.FALSE, .value = false },
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

        try testing.expectEqual(program.Statements.items[0].expressionStatement.Token.Type, t.type);
        try testing.expectEqual(program.Statements.items[0].expressionStatement.Exp.boolean.Value, t.value);
    }
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

            switch (exp.*) {
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

test "parse boolean expression 3" {
    const testAlloc = testing.allocator;
    const InfixTest = struct {
        input: []const u8,
        operator: []const u8,
        value: bool,
    };

    const tests = [_]InfixTest{
        .{
            .input = "!true;",
            .operator = "!",
            .value = true,
        },
        .{
            .input = "!false;",
            .operator = "!",
            .value = false,
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

            switch (exp.*) {
                .prefix => |infix| {
                    try testing.expectEqualStrings(t.operator, infix.Operator);
                    try testing.expectEqual(t.value, infix.Right.boolean.Value);
                },
                else => {},
            }
        }
    }
}

test "parse boolean expression 2" {
    const testAlloc = testing.allocator;
    const InfixTest = struct {
        input: []const u8,
        leftValue: bool,
        operator: []const u8,
        rightValue: bool,
    };

    const tests = [_]InfixTest{
        .{
            .input = "true == true;",
            .leftValue = true,
            .operator = "==",
            .rightValue = true,
        },
        .{
            .input = "true != false;",
            .leftValue = true,
            .operator = "!=",
            .rightValue = false,
        },
        .{
            .input = "false == false;",
            .leftValue = false,
            .operator = "==",
            .rightValue = false,
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

            switch (exp.*) {
                .infix => |infix| {
                    const left = infix.Left.*;
                    switch (left) {
                        .boolean => |b| {
                            try testing.expectEqual(t.leftValue, b.Value);
                        },
                        else => {},
                    }

                    try testing.expectEqualStrings(t.operator, infix.Operator);

                    const right = infix.Right.*;
                    switch (right) {
                        .boolean => |b| {
                            try testing.expectEqual(t.rightValue, b.Value);
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
            .input = "-a * b",
            .expected = "((-a) * b)",
        },
        .{
            .input = "!-a",
            .expected = "(!(-a))",
        },
        .{
            .input = "a + b + c",
            .expected = "((a + b) + c)",
        },
        .{
            .input = "a + b - c",
            .expected = "((a + b) - c)",
        },
        .{
            .input = "a * b * c",
            .expected = "((a * b) * c)",
        },
        .{
            .input = "a * b / c",
            .expected = "((a * b) / c)",
        },
        .{
            .input = "a + b / c",
            .expected = "(a + (b / c))",
        },
        .{
            .input = "a + b * c + d / e -f",
            .expected = "(((a + (b * c)) + (d / e)) - f)",
        },
        .{
            .input = "3 + 4-5 * 5",
            .expected = "((3 + 4) - (5 * 5))",
        },
        .{
            .input = "5 > 4 == 3 < 4",
            .expected = "((5 > 4) == (3 < 4))",
        },
        .{
            .input = "5 < 4 != 3 > 4",
            .expected = "((5 < 4) != (3 > 4))",
        },
        .{
            .input = "3 + 4 * 5 == 3 * 1 + 4 * 5",
            .expected = "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        },
        .{
            .input = "true",
            .expected = "true",
        },
        .{
            .input = "false",
            .expected = "false",
        },
        .{
            .input = "3 < 5 == false",
            .expected = "((3 < 5) == false)",
        },
        .{
            .input = "3 < 5 == true",
            .expected = "((3 < 5) == true)",
        },
        .{
            .input = "1 + (2 + 3) + 4",
            .expected = "((1 + (2 + 3)) + 4)",
        },
        .{
            .input = "(5 + 5) * 2",
            .expected = "((5 + 5) * 2)",
        },
        .{
            .input = "2 / (5 + 5)",
            .expected = "(2 / (5 + 5))",
        },
        .{
            .input = "-(5 + 5)",
            .expected = "(-(5 + 5))",
        },
        .{
            .input = "!(true == true)",
            .expected = "(!(true == true))",
        },
        .{
            .input = "a + add(b * c)",
            .expected = "(a + add((b * c)))",
        },
        .{
            .input = "a + add(b * c) + d",
            .expected = "((a + add((b * c))) + d)",
        },
        .{
            .input = "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            .expected = "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        },
        .{
            .input = "add(a + b + c * d / f + g)",
            .expected = "add((((a + b) + ((c * d) / f)) + g))",
        },
    };

    for (tests) |t| {
        var l = lexer.Lexer.init(t.input);
        var p = Parser.init(&l, testAlloc);
        defer p.deinit();
        var program = try p.parseProgram();
        defer program.deinit();
        if (p.errors.items.len != 0) {
            for (p.errors.items) |err| {
                debug.print("error: {s}\n", .{err});
            }
            debug.panic("parse error - check syntax: {s}", .{p.lexer.input});
        }
        const actual = try program.string();
        defer actual.deinit();

        try testing.expectEqualStrings(t.expected, actual.items);
    }
}

test "test if expression" {
    const testAlloc = testing.allocator;
    const input = "if (x<y) {x;}";

    var l = lexer.Lexer.init(input);
    var p = Parser.init(&l, testAlloc);
    defer p.deinit();
    var program = try p.parseProgram();
    defer program.deinit();

    if (program.Statements.items.len != 1) {
        debug.panic("program.Statements.items.len != 1, got len: {}", .{program.Statements.items.len});
    }

    const stmt = program.Statements.items[0].expressionStatement;
    var testArrayBuffer = ArrayList(u8).init(testAlloc);
    defer testArrayBuffer.deinit();
    try stmt.string(&testArrayBuffer);

    switch (stmt.Exp.*) {
        .ifelse => |ifelse| {
            const ifToken = ifelse.tokenLiteral();
            try testing.expectEqualStrings("if", ifToken);

            const ifCondition = ifelse.Condition.infix;

            const left = ifCondition.Left.*;
            try testing.expectEqualStrings("x", left.ident.Value);

            const operator = ifCondition.Operator;
            try testing.expectEqualStrings("<", operator);

            const right = ifCondition.Right.*;
            try testing.expectEqualStrings("y", right.ident.Value);

            const consequences = ifelse.Consequence.*;
            const c = consequences.Statements.items[0];
            const exp = c.expressionStatement.Exp;
            var buffer = ArrayList(u8).init(testAlloc);
            defer buffer.deinit();
            try exp.string(&buffer);
            try testing.expectEqualStrings("x", buffer.items);

            buffer.clearAndFree();

            try ifelse.string(&buffer);
            try testing.expectEqualStrings("if(x < y) x", buffer.items);
        },
        else => {
            debug.panic("stmt.Exp is not ifelse", .{});
        },
    }
}

test "test if else expression" {
    const testAlloc = testing.allocator;
    const input = "if ( x < y ) { x + t;} else { y };";

    var l = lexer.Lexer.init(input);
    var p = Parser.init(&l, testAlloc);
    defer p.deinit();
    var program = try p.parseProgram();
    defer program.deinit();

    if (program.Statements.items.len != 1) {
        debug.panic("program.Statements.items.len != 1, got len: {}", .{program.Statements.items.len});
    }

    const stmt = program.Statements.items[0].expressionStatement;
    var testArrayBuffer = ArrayList(u8).init(testAlloc);
    defer testArrayBuffer.deinit();
    try stmt.string(&testArrayBuffer);

    switch (stmt.Exp.*) {
        .ifelse => |ifelse| {
            const ifToken = ifelse.tokenLiteral();
            try testing.expectEqualStrings("if", ifToken);

            const ifCondition = ifelse.Condition.infix;

            const left = ifCondition.Left.*;
            try testing.expectEqualStrings("x", left.ident.Value);

            const operator = ifCondition.Operator;
            try testing.expectEqualStrings("<", operator);

            const right = ifCondition.Right.*;
            try testing.expectEqualStrings("y", right.ident.Value);

            const consequences = ifelse.Consequence.*;
            const c = consequences.Statements.items[0];
            const exp = c.expressionStatement.Exp;
            var buffer = ArrayList(u8).init(testAlloc);
            defer buffer.deinit();
            try exp.string(&buffer);
            try testing.expectEqualStrings("(x + t)", buffer.items);

            buffer.clearAndFree();

            if (ifelse.Alternative) |alt| {
                const a = alt.Statements.items[0];
                const altExp = a.expressionStatement.Exp;
                try altExp.string(&buffer);
                try testing.expectEqualStrings("y", buffer.items);
            }

            buffer.clearAndFree();
            try ifelse.string(&buffer);
            try testing.expectEqualStrings("if(x < y) (x + t) else y", buffer.items);
        },
        else => {
            debug.panic("stmt.Exp is not ifelse", .{});
        },
    }
}

test "test function literal" {
    const testAlloc = testing.allocator;
    const input = "fn(x, y) { x + y; }";

    var l = lexer.Lexer.init(input);
    var p = Parser.init(&l, testAlloc);
    defer p.deinit();
    var program = try p.parseProgram();
    defer program.deinit();

    if (program.Statements.items.len != 1) {
        debug.panic("program.Statements.items.len != 1, got len: {}", .{program.Statements.items.len});
    }

    const stmt = program.Statements.items[0].expressionStatement;
    var testArrayBuffer = ArrayList(u8).init(testAlloc);
    defer testArrayBuffer.deinit();

    var buffer = ArrayList(u8).init(testAlloc);
    defer buffer.deinit();

    const actual = try program.string();
    defer actual.deinit();
    try testing.expectEqualStrings("fn(x, y) (x + y)", actual.items);

    switch (stmt.Exp.*) {
        .functionLiteral => |functionLiteral| {
            const functionToken = functionLiteral.tokenLiteral();
            try testing.expectEqualStrings("fn", functionToken);

            const params = functionLiteral.Params;
            try testing.expectEqual(2, params.items.len);

            const param = params.items[0];
            try testing.expectEqualStrings("x", param.Value);

            const param2 = params.items[1];
            try testing.expectEqualStrings("y", param2.Value);

            try functionLiteral.string(&buffer);
            try testing.expectEqualStrings("fn(x, y) (x + y)", buffer.items);

            buffer.clearRetainingCapacity();

            const body = functionLiteral.Body.*;
            const exp = body.Statements.items[0].expressionStatement.Exp;
            try exp.string(&buffer);
            try testing.expectEqualStrings("(x + y)", buffer.items);
        },
        else => {
            debug.panic("stmt.Exp is not ifelse", .{});
        },
    }
}

test "test call expression" {
    const testAlloc = testing.allocator;
    const input = "add(1, 2 * 3, 4 + 5)";

    var l = lexer.Lexer.init(input);
    var p = Parser.init(&l, testAlloc);
    defer p.deinit();
    var program = try p.parseProgram();
    defer program.deinit();

    for (p.errors.items) |err| {
        debug.print("error: {s}\n", .{err});
    }

    if (program.Statements.items.len != 1) {
        debug.panic("program.Statements.items.len != 1, got len: {}", .{program.Statements.items.len});
    }

    const actual = try program.string();
    defer actual.deinit();
    try testing.expectEqualStrings("add(1, (2 * 3), (4 + 5))", actual.items);
}
