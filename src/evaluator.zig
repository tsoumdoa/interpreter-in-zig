const std = @import("std");
const testing = std.testing;
const ArrayList = std.ArrayList;
const ast = @import("ast.zig");
const object = @import("object.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");

pub inline fn Eval(node: *ast.Expression) !object.Object {
    switch (node.*) {
        .int => |i| {
            return object.Object{ .integer = i.Value };
        },
        .null => |_| {
            return object.Object{ .null = {} };
        },
        else => {
            return object.Object{ .null = {} };
        },
    }
}

test "test eval integer expression" {
    const testAlloc = testing.allocator;
    const Test = struct {
        input: []const u8,
        expected: i32,
    };

    const tests = [_]Test{
        .{
            .input = "5;",
            .expected = 5,
        },
        .{
            .input = "10;",
            .expected = 10,
        },
    };

    for (tests) |t| {
        var l = lexer.Lexer.init(t.input);
        var p = parser.Parser.init(&l, testAlloc);
        defer p.deinit();
        var program = try p.parseProgram();
        defer program.deinit();

        const eval = try Eval(program.Statements.items[0].expressionStatement.Exp);
        try testing.expectEqual(t.expected, eval.integer);
    }
}
