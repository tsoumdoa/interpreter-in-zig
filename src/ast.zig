const std = @import("std");
const ArrayList = std.ArrayList;
const token = @import("token.zig");

pub const Node = struct {
    pub fn tokenLiteal() []const u8 {}
};

pub const Statement = union {
    letStatement: LetStatement,
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
        // const gpa = std.heap.GeneralPurposeAllocator(.{}){};
        var gpa_impl: std.heap.GeneralPurposeAllocator(.{}) = .{};
        const gpa = gpa_impl.allocator();
        var list = ArrayList(Statement).init(gpa);
        // defer list.deinit();
        return Program{ .Statements = &list };
    }

    pub fn TokenLiteral(p: Program) Program {
        if (p.Statements.len > 0) {
            return p.Stamtements[0].tokenLiteral();
        }
        return "";
    }
};
