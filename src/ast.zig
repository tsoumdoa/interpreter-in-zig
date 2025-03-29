const std = @import("std");
const ArrayList = std.ArrayList;
const token = @import("token.zig");

pub const Node = struct {
    pub fn tokenLiteal() []const u8 {}
};

const StatementType = enum {
    letStatement,
    err,
};

pub const Statement = union(StatementType) {
    letStatement: LetStatement,
    err: AstParseError,
};
pub const AstParseError = enum {
    ParseError,
    UnexpectedToken,
    InvalidSyntax,
    // ... other errors
};

pub const Expression = struct {
    Node: Node,
    pub fn expressionNode() void {}
};

pub const LetStatement = struct {
    Token: token.Token,
    Name: *Identifier,
    Value: Expression,

    pub fn statementNode(ls: *LetStatement) void {
        _ = ls;
        //todo

    }
    pub fn tokenLiteral(ls: *LetStatement) []const u8 {
        return ls.Token.Literal;
    }
};

pub const Identifier = struct {
    Token: token.Token,
    Value: []const u8,

    pub fn expressionNode() void {}
    pub fn tokenLiteral(i: *Identifier) []const u8 {
        return i.Token.Literal;
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
                else => {
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

    pub fn TokenLiteral(p: Program) Program {
        if (p.Statements.len > 0) {
            return p.Stamtements[0].tokenLiteral();
        }
        return "";
    }
};
