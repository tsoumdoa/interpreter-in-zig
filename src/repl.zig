const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const Reader = std.io.GenericReader;
const Writer = std.io.GenericWriter;
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();

const PROMPT = ">> ";

pub fn start() !void {
    var buf: [1024]u8 = undefined;

    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    const alloc = gpa.allocator();

    while (true) {
        try stdout.print(PROMPT, .{});

        const line = try stdin.readUntilDelimiterOrEof(buf[0..], '\n');

        if (line) |l| {
            if (l.len == 0) {
                return;
            }
            var lex = lexer.Lexer.init(l);
            var p = parser.Parser.init(&lex, alloc);
            defer p.deinit();
            var program = p.parseProgram() catch |err| {
                try stdout.print("{s}\n", .{monkey});
                try stdout.print("Woops, we run into some monkey business here!:\n", .{});
                try stdout.print("error: {}\n", .{err});
                continue;
            };
            defer program.deinit();
            if (p.errors.items.len != 0) {
                for (p.errors.items) |err| {
                    try stdout.print("{s}\n", .{monkey});
                    try stdout.print("Woops, we run into some monkey business here!:\n", .{});
                    try stdout.print("error: {s}\n", .{err});
                }
                continue;
            }

            const out = try program.string();
            defer out.deinit();

            try stdout.print("{s}\n", .{out.items});
        }
    }
}

const monkey =
    \\   .--.  .-"     "-.  .--.
    \\  / .. \/  .-. .-.  \/ .. \
    \\ | |  '|  /   Y   \  |'  | |
    \\ | \   \  \ 0 | 0 /  /   / |
    \\  \ '- ,\.-"""""""-./, -' /
    \\   ''-' /_   ^ ^   _\ '-''
    \\       |  \._   _./   |
    \\       \   \ '~' /   /
    \\        '._ '-=-' _.'
    \\           '-----'
;
