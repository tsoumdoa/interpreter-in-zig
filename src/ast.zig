const std = @import("std");
const ArrayList = std.ArrayList;
const token = @import("token.zig");

// pub const Node = struct {
//     pub fn tokenLiteal() []const u8 {}
// };

// pub const Expression = struct {
//     Node: Node,
//     pub fn expressionNode() void {}
// };

const StatementType = enum {
    letStatement,
    returnStatement,
    err,
};

pub const Statement = union(StatementType) {
    letStatement: LetStatement,
    returnStatement: ReturnStatement,
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
    Value: []const u8,

    pub fn statementNode() void {}
    pub fn tokenLiteral(ls: *LetStatement) []const u8 {
        return ls.Token.Literal;
    }
};

pub const ReturnStatement = struct {
    Token: token.Token,
    ReturnValue: []const u8,

    pub fn init(t: token.Token) ReturnStatement {
        return ReturnStatement{ .Token = t, .ReturnValue = undefined };
    }

    pub fn statementNode() void {}
    pub fn tokenLiteral(rs: *ReturnStatement) []const u8 {
        return rs.Token.Literal;
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
                .letStatement => |let_stmt| {
                    p.allocator.destroy(let_stmt.Name);
                    p.allocator.destroy(stmt);
                },
                .returnStatement => |ret_stmt| {
                    p.allocator.free(ret_stmt.ReturnValue);
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
