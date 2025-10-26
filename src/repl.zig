const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const Reader = std.io.Reader;

// const stdout = std.io.getStdOut().writer();
const evaluator = @import("evaluator.zig");

const PROMPT = ">> ";
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

pub fn start() !void {
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    var buf: [1024]u8 = undefined;
    var stdin_reader = std.fs.File.stdin().reader(&buf);

    const stdin = &stdin_reader.interface;

    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    const alloc = gpa.allocator();

    while (true) {
        try stdout.print(PROMPT, .{});

        const l = try stdin.takeDelimiterInclusive('\n');

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

        var buffer: std.ArrayList(u8) = .empty;
        defer buffer.deinit(alloc);
        const object = try evaluator.Eval(program.Statements.items[0].expressionStatement.Exp, alloc);
        try object.Inspect(alloc, &buffer);
        try stdout.print("{s}\n", .{buffer.items});
        try stdout.flush();
    }
    defer gpa.deinit();
}
