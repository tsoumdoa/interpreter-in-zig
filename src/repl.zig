const std = @import("std");
const lexer = @import("lexer.zig");
const Reader = std.io.GenericReader;
const Writer = std.io.GenericWriter;
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();

const PROMPT = ">> ";

pub fn start() !void {
    var buf: [1024]u8 = undefined;

    while (true) {
        try stdout.print(PROMPT, .{});

        const line = try stdin.readUntilDelimiterOrEof(buf[0..], '\n');

        if (line) |l| {
            if (l.len == 0) {
                return;
            }
            var lex = lexer.Lexer.init(l);

            while (true) {
                const tok = lex.nextToken();
                if (tok.Type == .EOF) {
                    break;
                }
                try stdout.print("Type: {any} - Literal:{s}\n", .{ tok.Type, tok.Literal });
            }
        }
    }
}
