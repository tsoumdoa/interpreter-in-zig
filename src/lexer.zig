pub const Lexer = struct {
    input: []const u8,
    pos: usize,
    readPos: usize,
    ch: u8,
};
