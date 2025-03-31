const std = @import("std");
const testing = std.testing;
const ArrayList = std.ArrayList;
const token = @import("token.zig");

// pub const Node = struct {
//     pub fn tokenLiteal() []const u8 {}
// };

pub const Expression = struct {
    // Node: Node,
    pub fn expressionNode() void {}
};

const StatementType = enum {
    letStatement,
    returnStatement,
    expressionStatement,
    err,
};

pub const Statement = union(StatementType) {
    letStatement: LetStatement,
    returnStatement: ReturnStatement,
    expressionStatement: ExpressionStatement,
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

    pub fn statementNode() void {}
    pub fn tokenLiteral(ls: *LetStatement) []const u8 {
        return ls.Token.Literal;
    }
    pub fn string(ls: LetStatement) []const u8 {
        return ls.Value.string();
    }
};

pub const ReturnStatement = struct {
    Token: token.Token,
    ReturnValue: *Identifier,

    pub fn init(t: token.Token) ReturnStatement {
        return ReturnStatement{ .Token = t, .ReturnValue = undefined };
    }
    pub fn statementNode() void {}
    pub fn tokenLiteral(rs: *ReturnStatement) []const u8 {
        return rs.Token.Literal;
    }
    pub fn string(rs: ReturnStatement) []const u8 {
        return rs.ReturnValue.string();
    }
};

pub const ExpressionStatement = struct {
    Token: token.Token,
    Exp: Expression,

    pub fn statementNode() void {}
    pub fn tokenLiteral(es: *ExpressionStatement) []const u8 {
        return es.Token.Literal;
    }
    pub fn string(es: ExpressionStatement) []const u8 {
        _ = es;
        return "";
    }
};

pub const Identifier = struct {
    Token: token.Token,
    Value: []const u8,

    pub fn expressionNode() void {}
    pub fn tokenLiteral(i: *Identifier) []const u8 {
        return i.Token.Literal;
    }
    pub fn string(i: Identifier) []const u8 {
        return i.Value;
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
                    _ = exp_stmt;
                    //todo
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

    pub inline fn string(p: *Program) ![]const u8 {
        var buffer = ArrayList(u8).init(p.allocator);
        for (p.Statements.items) |stmt| {
            switch (stmt.*) {
                .letStatement => |let_stmt| {
                    try buffer.appendSlice(let_stmt.string());
                },
                .returnStatement => |ret_stmt| {
                    try buffer.appendSlice(ret_stmt.string());
                },
                .expressionStatement => |exp_stmt| {
                    try buffer.appendSlice(exp_stmt.string());
                },
                .err => |_| {},
            }
        }
        std.debug.print("buffer.items {s}\n", .{buffer.items});
        defer buffer.deinit();
        return "";
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

    // const expected = "let myVar = anotherVar;";
    const v = try p.string();
    _ = v;
    // try testing.expectEqualStrings(expected, v);
}
