const std = @import("std");
const testing = std.testing;
const token = @import("token.zig");
const lexer = @import("lexer.zig");

pub fn main() !void {
    std.debug.print("All your {s} are belong to us.\n", .{"codebase"});

    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("Run `zig build test` to run the tests.\n", .{});
}

test {
    testing.refAllDecls(token);
    testing.refAllDecls(lexer);
}
