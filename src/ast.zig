const std = @import("std");
const testing = std.testing;
const ArrayList = std.ArrayList;
const token = @import("token.zig");
const parser = @import("parser.zig");

const PrefixExpressionType = enum {
    ident,
    int,
    err,
};

pub const PrefixExpression = union(PrefixExpressionType) {
    ident: Identifier,
    int: Integer,
    err: AstParseError,
};

const ExpressionType = enum {
    ident,
    int,
    prefix,
    err,
};

pub const Expression = union(ExpressionType) {
    ident: Identifier,
    int: Integer,
    prefix: Prefix,
    err: AstParseError,
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
    // ... other errors
};

pub const LetStatement = struct {
    Token: token.Token,
    Name: *Identifier,
    Value: *Identifier,

    pub fn tokenLiteral(ls: *const LetStatement) []const u8 {
        return ls.Token.Literal;
    }
    pub fn string(ls: *const LetStatement, buffer: *ArrayList(u8)) !void {
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

    pub fn init(t: token.Token) ReturnStatement {
        return ReturnStatement{ .Token = t, .ReturnValue = undefined };
    }
    pub fn tokenLiteral(rs: *const ReturnStatement) []const u8 {
        return rs.Token.Literal;
    }
    pub fn string(rs: *const ReturnStatement, buffer: *ArrayList(u8)) !void {
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

    pub fn init(t: token.Token) ExpressionStatement {
        return ExpressionStatement{ .Token = t, .Exp = undefined };
    }
    pub fn tokenLiteral(es: *const ExpressionStatement) []const u8 {
        return es.Token.Literal;
    }

    pub fn string(es: *const ExpressionStatement, buffer: *ArrayList(u8)) !void {
        switch (es.Exp) {
            .ident => |ident| {
                try buffer.appendSlice(ident.Value);
            },
            .int => |int| {
                try buffer.appendSlice(int.Value);
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
    Value: []const u8,

    pub inline fn tokenLiteral(i: *const Integer) []const u8 {
        return i.Token.Literal;
    }
    pub inline fn string(i: *const Integer) []const u8 {
        return i.Value;
    }
};

pub const Prefix = struct {
    Token: token.Token,
    Operator: []const u8,
    Right: PrefixExpression,


    pub inline fn tokenLiteral(i: *const Prefix) []const u8 {
        return i.Token.Literal;
    }
    pub inline fn string(p: *const Prefix, buffer: *ArrayList(u8)) !void {
        try buffer.appendSlice("(");
        try buffer.appendSlice(p.Operator);
        switch (p.Right) {
            else => |r| {
                try buffer.appendSlice(r.Right.string());
            },
        }
        try buffer.appendSlice(")");
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
                .err => |_| {},
                else => {},
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
