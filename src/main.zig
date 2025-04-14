const std = @import("std");
const process = std.process;
const expect = std.testing.expect;
const testing = std.testing;
const token = @import("token.zig");
const lexer = @import("lexer.zig");
const repl = @import("repl.zig");
const ast = @import("ast.zig");
const parser = @import("parser.zig");
const object = @import("object.zig");
const evaluator = @import("evaluator.zig");

pub fn main() !void {
    var gpa_impl: std.heap.GeneralPurposeAllocator(.{}) = .{};
    const gpa = gpa_impl.allocator();
    const user = try process.getEnvVarOwned(gpa, "USER");

    try std.io.getStdOut().writer().print(
        "Hello {s}! This is the Monkey programming language!\n",
        .{user},
    );
    try std.io.getStdOut().writer().print("Feel free to type in commands\n", .{});

    try repl.start();
}

test {
    testing.refAllDecls(token);
    testing.refAllDecls(lexer);
    testing.refAllDecls(repl);
    testing.refAllDecls(ast);
    testing.refAllDecls(parser);
    testing.refAllDecls(object);
    testing.refAllDecls(evaluator); 
}
