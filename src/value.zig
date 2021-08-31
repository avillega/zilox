const std = @import("std");
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;

const DynamicArray = @import("./dynamic_array.zig").DynamicArray;

pub const Value = f64;

pub fn printValue(value: Value) void {
    std.debug.print("{d}", .{value});
}

test "create a Value" {
    const x: Value = 42.3;
    try expect(@TypeOf(x) == Value);
    try expect(x == 42.3);
}
