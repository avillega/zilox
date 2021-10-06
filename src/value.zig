const std = @import("std");
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;

pub const ValueTag = enum {
    boolean,
    number,
    nil,
};

pub const Value = union(ValueTag) {
    boolean: bool,
    number: f64,
    nil,
};

pub fn printValue(value: Value) void {
    switch (value) {
        .nil => std.debug.print("nil", .{}),
        .boolean => |b| std.debug.print("{any}", .{b}),
        .number => |num| std.debug.print("{d}", .{num}),
    }
}

pub fn valuesEq(a: Value, b: Value) bool {
    const aType = @as(ValueTag, a);
    const bType = @as(ValueTag, b);
    if (aType != bType) return false;

    return switch (a) {
        .number => |numberA| numberA == b.number,
        .boolean => |booleanA| booleanA == b.boolean,
        .nil => true,
    };
}

test "size of a Value" {
    try expect(@sizeOf(Value) == 16);
}

test "use of union with tags" {
    const v = Value{ .boolean = true };
    try expect(v == .boolean);
    try expect(v != .number);
    try expect(v != .nil);
    try expect(v.boolean);
}
