const std = @import("std");
const ArrayList = std.ArrayList;
const token = @import("token.zig");

pub const Node = struct {
    pub fn tokenLiteal() []const u8 {}
};

pub const Statement = struct {
    Node: Node,
    pub fn statementNode() void {}
};

pub const Expression = struct {
    Node: Node,
    pub fn expressionNode() void {}
};

pub const LetStatement = struct {
    Token: token.Token,
    Name: *Identifier,
    Value: Expression,

    pub fn StatementNode() void {}
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
    Statements: *ArrayList(Statement),

    pub fn init() Program {
        const gpa = std.heap.GeneralPurposeAllocator(.{}){};
        var list = ArrayList(Statement).init(gpa.allocator());
        return Program{ .Statements = &list };
    }

    pub fn TokenLiteral(p: Program) Program {
        if (p.Statements.len > 0) {
            return p.Stamtements[0].tokenLiteral();
        }
        return "";
    }
};
