const std = @import("std");
const ArrayList = std.ArrayList;

const ObjectType = enum {
    integer,
    boolean,
    null,
};

pub const Object = union(ObjectType) {
    integer: i32,
    boolean: bool,
    null: void,

    pub fn Type(o: Object) ObjectType {
        switch (o) {
            .integer => return ObjectType.INTEGER,
            .boolean => return ObjectType.BOOLEAN,
            .null => return ObjectType.NULL,
        }
    }

    pub fn Inspect(o: Object, alloc: std.mem.Allocator, buffer: *ArrayList(u8)) !void {
        switch (o) {
            .integer => |i| {
                var buf: [12]u8 = undefined;
                const bytes = try std.fmt.bufPrint(&buf, "{}", .{i});
                try buffer.appendSlice(alloc, bytes);
            },
            .boolean => |b| try buffer.appendSlice(alloc, if (b) "true" else "false"),
            .null => try buffer.appendSlice(alloc, "null"),
        }
    }
};
