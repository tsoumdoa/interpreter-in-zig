const std = @import("std");
const debug = std.debug;
const testing = std.testing;
const ArrayList = std.ArrayList;
const ast = @import("ast.zig");
const object = @import("object.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");

pub fn Eval(node: *ast.Expression, allocator: std.mem.Allocator) !object.Object {
    switch (node.*) {
        .int => |i| {
            return object.Object{ .integer = i.Value };
        },
        .boolean => |b| {
            return object.Object{ .boolean = b.Value };
        },
        .prefix => |p| {
            const rightPtr = try allocator.create(ast.Expression);
            defer allocator.destroy(rightPtr);
            rightPtr.* = p.Right.*;
            const right = try Eval(rightPtr, allocator);
            return evalPrefixExpression(p.Operator, right);
        },

        .null => |_| {
            return object.Object{ .null = {} };
        },
        else => {
            return object.Object{ .null = {} };
        },
    }
}

inline fn evalPrefixExpression(operator: []const u8, right: object.Object) object.Object {
    if (std.mem.eql(u8, operator, "!")) {
        return evalBangOperatorExpression(right);
    } else if (std.mem.eql(u8, operator, "-")) {
        return object.Object{ .integer = -right.integer };
    } else return .{ .null = {} };
}

inline fn evalBangOperatorExpression(right: object.Object) object.Object {
    switch (right) {
        .boolean => |b| return object.Object{ .boolean = !b },
        .integer => |i| return object.Object{ .boolean = i == 0 },
        else => return .{ .null = {} },
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

        const eval = try Eval(program.Statements.items[0].expressionStatement.Exp, testAlloc);
        try testing.expectEqual(t.expected, eval.integer);
    }
}

test "test eval boolean expression" {
    const testAlloc = testing.allocator;
    const Test = struct {
        input: []const u8,
        expected: bool,
    };

    const tests = [_]Test{
        .{
            .input = "true;",
            .expected = true,
        },
        .{
            .input = "false;",
            .expected = false,
        },
    };

    for (tests) |t| {
        var l = lexer.Lexer.init(t.input);
        var p = parser.Parser.init(&l, testAlloc);
        defer p.deinit();
        var program = try p.parseProgram();
        defer program.deinit();

        const eval = try Eval(program.Statements.items[0].expressionStatement.Exp, testAlloc);

        var buffer = ArrayList(u8).init(testAlloc);
        defer buffer.deinit();
        try object.Object.Inspect(eval, &buffer);

        if (eval.boolean) {
            try testing.expectEqualStrings("true", buffer.items);
        } else {
            try testing.expectEqualStrings("false", buffer.items);
        }
    }
}

test "test bang operator" {
    const testAlloc = testing.allocator;
    const Test = struct {
        input: []const u8,
        expected: bool,
    };

    const tests = [_]Test{
        .{
            .input = "!true;",
            .expected = false,
        },
        .{
            .input = "!false;",
            .expected = true,
        },
        .{
            .input = "!5;",
            .expected = false,
        },
        .{
            .input = "!!true;",
            .expected = true,
        },
        .{
            .input = "!!false;",
            .expected = false,
        },
        .{
            .input = "!!5;",
            .expected = true,
        },
    };

    for (tests) |t| {
        var l = lexer.Lexer.init(t.input);
        var p = parser.Parser.init(&l, testAlloc);
        defer p.deinit();
        var program = try p.parseProgram();
        defer program.deinit();

        const eval = try Eval(program.Statements.items[0].expressionStatement.Exp, testAlloc);

        var buffer = ArrayList(u8).init(testAlloc);
        defer buffer.deinit();
        switch (eval) {
            .boolean => |b| {
                testing.expectEqual(t.expected, b) catch |err| {
                    std.debug.print("actual: {}\n", .{err});
                    std.debug.print("input: {s}\n", .{t.input});
                    std.debug.print("expected: {}\n", .{t.expected});
                };
            },
            else => {
                std.debug.panic("unexpected object type, bool expected", .{});
            },
        }
    }
}

test "test eval integer" {
    const testAlloc = testing.allocator;
    const Test = struct {
        input: []const u8,
        expected: i64,
    };

    const tests = [_]Test{
        .{ .input = "5;", .expected = 5 },
        .{ .input = "10;", .expected = 10 },
        .{ .input = "-5;", .expected = -5 },
        .{ .input = "-10;", .expected = -10 },
    };

    for (tests) |t| {
        var l = lexer.Lexer.init(t.input);
        var p = parser.Parser.init(&l, testAlloc);
        defer p.deinit();
        var program = try p.parseProgram();
        defer program.deinit();

        const eval = try Eval(program.Statements.items[0].expressionStatement.Exp, testAlloc);

        var buffer = ArrayList(u8).init(testAlloc);
        defer buffer.deinit();
        switch (eval) {
            .integer => |i| {
                debug.print("actual: {}\n", .{i});
                try testing.expectEqual(t.expected, i);
            },
            else => {
                std.debug.panic("unexpected object type, bool expected", .{});
            },
        }
    }
}
