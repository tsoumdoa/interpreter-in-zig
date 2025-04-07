const std = @import("std");
const debug = std.debug;
const testing = std.testing;
const ArrayList = std.ArrayList;
const token = @import("token.zig");
const parser = @import("parser.zig");
const String = @import("string").String;

const ExpressionType = enum {
    ident,
    int,
    prefix,
    infix,
    boolean,
    ifelse,
    err,
};

const ParseError = error{
    ParseError,
    ParseStringError,
    UnexpectedToken,
    InvalidSyntax,
    OutOfMemory,
    NoSpaceLeft,
};

pub const Expression = union(ExpressionType) {
    ident: Identifier,
    int: Integer,
    prefix: Prefix,
    infix: Infix,
    boolean: Boolean,
    ifelse: IfElse,
    err: AstParseError,

    // inline is not option due to recursive call from infix expression
    pub fn string(e: *const Expression, buffer: *ArrayList(u8)) ParseError!void {
        switch (e.*) {
            .ident => |ident| {
                try buffer.appendSlice(ident.string());
            },
            .int => |int| {
                try int.string(buffer);
            },
            .prefix => |prefix| {
                try prefix.string(buffer);
            },
            .infix => |infix| {
                try infix.string(buffer);
            },
            .boolean => |boolean| {
                try boolean.string(buffer);
            },
            .ifelse => |ifelse| {
                try ifelse.string(buffer);
            },
            .err => |_| {
                return error.ParseError;
            },
        }
    }
};

const StatementType = enum {
    letStatement,
    returnStatement,
    expressionStatement,
    identStatement,
    err,
};

pub const Statement = union(StatementType) {
    letStatement: LetStatement,
    returnStatement: ReturnStatement,
    expressionStatement: ExpressionStatement,
    identStatement: Identifier,
    err: AstParseError,
};
pub const AstParseError = enum {
    ParseError,
    UnexpectedToken,
    InvalidSyntax,
    NoRparen,
    // ... other errors
};

pub const LetStatement = struct {
    Token: token.Token,
    Name: *Identifier,
    Value: *Identifier,

    pub inline fn tokenLiteral(ls: *const LetStatement) []const u8 {
        return ls.Token.Literal;
    }
    pub inline fn string(ls: *const LetStatement, buffer: *ArrayList(u8)) !void {
        try buffer.appendSlice(ls.tokenLiteral());
        try buffer.appendSlice(" ");
        try buffer.appendSlice(ls.Name.string());
        try buffer.appendSlice(" = ");
        if (ls.Value.string().len > 0) {
            try buffer.appendSlice(ls.Value.string());
        }
        try buffer.appendSlice(";");
    }
};

pub const ReturnStatement = struct {
    Token: token.Token,
    ReturnValue: *Identifier,

    pub inline fn init(t: token.Token) ReturnStatement {
        return ReturnStatement{ .Token = t, .ReturnValue = undefined };
    }
    pub inline fn tokenLiteral(rs: *const ReturnStatement) []const u8 {
        return rs.Token.Literal;
    }
    pub inline fn string(rs: *const ReturnStatement, buffer: *ArrayList(u8)) !void {
        try buffer.appendSlice(rs.tokenLiteral());
        try buffer.appendSlice(" ");
        if (rs.ReturnValue.string().len > 0) {
            try buffer.appendSlice(rs.ReturnValue.string());
        }
        try buffer.appendSlice(";");
    }
};

pub const ExpressionStatement = struct {
    Token: token.Token,
    Exp: Expression,

    pub inline fn init(t: token.Token) ExpressionStatement {
        return ExpressionStatement{ .Token = t, .Exp = undefined };
    }
    pub inline fn tokenLiteral(es: *const ExpressionStatement) []const u8 {
        return es.Token.Literal;
    }

    pub inline fn string(es: *const ExpressionStatement, buffer: *ArrayList(u8)) !void {
        switch (es.Exp) {
            .ident => |ident| {
                try buffer.appendSlice(ident.Value);
            },
            .int => |int| {
                try int.string(buffer);
            },
            .infix => |infix| {
                try infix.string(buffer);
            },
            .prefix => |prefix| {
                // _ = prefix;
                try prefix.string(buffer);
            },
            .boolean => |boolean| {
                try boolean.string(buffer);
            },
            else => {},
        }
    }
};

pub const Identifier = struct {
    Token: token.Token,
    Value: []const u8,

    pub inline fn tokenLiteral(i: *const Identifier) []const u8 {
        return i.Token.Literal;
    }
    pub inline fn string(i: *const Identifier) []const u8 {
        return i.Value;
    }
};

pub const Integer = struct {
    Token: token.Token,
    Value: i32,

    pub inline fn tokenLiteral(i: *const Integer) []const u8 {
        return i.Token.Literal;
    }
    pub inline fn string(i: *const Integer, buffer: *std.ArrayList(u8)) !void {
        var buf: [12]u8 = undefined;
        const bytes = try std.fmt.bufPrint(&buf, "{}", .{i.Value});
        try buffer.appendSlice(bytes);
    }
};

pub const Prefix = struct {
    Token: token.Token,
    Operator: []const u8,
    Right: *const Expression,

    pub inline fn tokenLiteral(p: *const Prefix) []const u8 {
        return p.Token.Literal;
    }
    pub inline fn string(p: *const Prefix, buffer: *ArrayList(u8)) !void {
        try buffer.appendSlice("(");
        try buffer.appendSlice(p.Operator);
        const right = p.Right.*;
        try right.string(buffer);
        try buffer.appendSlice(")");
    }
};

pub const Infix = struct {
    Token: token.Token,
    Left: *const Expression,
    Operator: []const u8,
    Right: *const Expression,

    pub inline fn tokenLiteral(i: *const Infix) []const u8 {
        return i.Token.Literal;
    }
    pub inline fn string(i: *const Infix, buffer: *ArrayList(u8)) !void {
        try buffer.appendSlice("(");
        const left = i.Left.*;
        try left.string(buffer);
        try buffer.appendSlice(" ");
        try buffer.appendSlice(i.Operator);
        try buffer.appendSlice(" ");
        const right = i.Right.*;
        try right.string(buffer);
        try buffer.appendSlice(")");
    }
};

pub const Boolean = struct {
    Token: token.Token,
    Value: bool,

    pub inline fn tokenLiteral(b: *const Boolean) []const u8 {
        return b.Token.Literal;
    }
    pub inline fn string(b: *const Boolean, buffer: *ArrayList(u8)) !void {
        try buffer.appendSlice(b.tokenLiteral());
    }
};

pub const IfElse = struct {
    Token: token.Token,
    Condition: *const Expression,
    Consequence: *BlockStatement,
    Alternative: ?*const BlockStatement,

    pub inline fn tokenLiteral(i: *const IfElse) []const u8 {
        return i.Token.Literal;
    }

    pub inline fn string(i: *const IfElse, buffer: *ArrayList(u8)) !void {
        try buffer.appendSlice("if");
        try i.Condition.string(buffer);
        try buffer.appendSlice(" ");
        try i.Condition.string(buffer);
        if (i.Alternative) |alt| {
            try buffer.appendSlice("else ");
            try alt.string(buffer);
        }
    }
};

pub const BlockStatement = struct {
    Token: token.Token,
    Statements: ArrayList(*Statement),
    allocator: std.mem.Allocator,
    pub fn init(allocator: std.mem.Allocator, tokenValue: token.Token) BlockStatement {
        const list = ArrayList(*Statement).init(allocator);
        return BlockStatement{ .Token = tokenValue, .Statements = list, .allocator = allocator };
    }

    pub fn deinit(b: *BlockStatement) void {
        for (b.Statements.items) |stmt| {
            switch (stmt.*) {
                .err => |_| {},
                .letStatement => |let_stmt| {
                    b.allocator.destroy(let_stmt.Name);
                    b.allocator.destroy(let_stmt.Value);
                },
                .returnStatement => |ret_stmt| {
                    b.allocator.destroy(ret_stmt.ReturnValue);
                },
                .expressionStatement => |exp_stmt| {
                    _ = exp_stmt.Exp;
                },
                .identStatement => |ident_stmt| {
                    _ = ident_stmt.Value;
                },
            }
            b.allocator.destroy(stmt);
        }
        b.Statements.deinit();
    }

    pub inline fn tokenLiteral(b: *const BlockStatement) []const u8 {
        return b.Token.Literal;
    }
    pub inline fn string(b: *const BlockStatement, buffer: *ArrayList(u8)) !void {
        for (b.Statements.items) |stmt| {
            switch (stmt.*) {
                .letStatement => |let_stmt| {
                    try let_stmt.string(buffer);
                },
                .returnStatement => |ret_stmt| {
                    try ret_stmt.string(buffer);
                },
                .expressionStatement => |exp_stmt| {
                    try exp_stmt.string(buffer);
                },
                .identStatement => |ident_stmt| {
                    try buffer.appendSlice(ident_stmt.Value);
                },
                .err => |_| {},
            }
        }
    }
};

pub const Program = struct {
    Statements: ArrayList(*Statement),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Program {
        const list = ArrayList(*Statement).init(allocator);
        return Program{ .Statements = list, .allocator = allocator };
    }

    pub fn deinit(p: *Program) void {
        for (p.Statements.items) |stmt| {
            switch (stmt.*) {
                .err => |_| {},
                .letStatement => |let_stmt| {
                    p.allocator.destroy(let_stmt.Name);
                    p.allocator.destroy(stmt);
                },
                .returnStatement => |ret_stmt| {
                    p.allocator.destroy(ret_stmt.ReturnValue);
                    p.allocator.destroy(stmt);
                },
                .expressionStatement => |exp_stmt| {
                    _ = exp_stmt.Exp;
                    p.allocator.destroy(stmt);
                },
                .identStatement => |ident_stmt| {
                    p.allocator.free(ident_stmt.Value);
                    p.allocator.destroy(stmt);
                },
            }
        }
        p.Statements.deinit();
    }

    pub inline fn addStatement(p: *Program, stmt: Statement) !void {
        const stmtPtr = try p.allocator.create(Statement);
        stmtPtr.* = stmt;
        try p.Statements.append(stmtPtr);
    }

    pub inline fn string(p: *Program) !*ArrayList(u8) {
        var buffer = ArrayList(u8).init(p.allocator);

        for (p.Statements.items) |stmt| {
            switch (stmt.*) {
                .letStatement => |let_stmt| {
                    try let_stmt.string(&buffer);
                },
                .returnStatement => |ret_stmt| {
                    try ret_stmt.string(&buffer);
                },
                .expressionStatement => |exp_stmt| {
                    try exp_stmt.string(&buffer);
                },
                .identStatement => |ident_stmt| {
                    const ident = ident_stmt.Value;
                    try buffer.appendSlice(ident);
                },
                .err => |_| {},
            }
        }
        return &buffer;
    }

    pub fn TokenLiteral(p: Program) Program {
        if (p.Statements.len > 0) {
            return p.Stamtements[0].tokenLiteral();
        }
        return "";
    }
};

test "test return value of string fn" {
    const allocator = testing.allocator;
    var p = Program.init(allocator);
    defer p.Statements.deinit();

    const t = token.Token{ .Type = token.TokenType.LET, .Literal = "let" };
    var nameIden = Identifier{
        .Token = token.Token{ .Type = token.TokenType.IDENT, .Literal = "myVar" },
        .Value = "myVar",
    };
    var valueIden = Identifier{
        .Token = token.Token{ .Type = token.TokenType.IDENT, .Literal = "anotherVar" },
        .Value = "anotherVar",
    };
    var lstm = Statement{ .letStatement = LetStatement{
        .Token = t,
        .Name = &nameIden,
        .Value = &valueIden,
    } };
    try p.Statements.append(&lstm);

    const expected = "let myVar = anotherVar;";
    const b = try p.string();
    defer b.deinit();
    try testing.expectEqualStrings(expected, b.items);
}
