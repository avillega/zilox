const std = @import("std");
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;
const Obj = @import("./object.zig").Obj;

pub const Value = union(enum) {
    const Self = @This();
    Nil,
    Bool: bool,
    Number: f64,
    Obj: *Obj,

    pub fn isNil(self: Self) bool {
        return self == .Nil;
    }

    pub fn isBool(self: Self) bool {
        return self == .Bool;
    }

    pub fn isNumber(self: Self) bool {
        return self == .Number;
    }

    pub fn isObj(self: Self) bool {
        return self == .Obj;
    }

    pub fn asBool(self: Self) bool {
        std.debug.assert(self.isBool());
        return self.Bool;
    }

    pub fn asNumber(self: Self) f64 {
        std.debug.assert(self.isNumber());
        return self.Number;
    }

    pub fn asObj(self: Self) *Obj {
        std.debug.assert(self.isObj());
        return self.Obj;
    }

    pub fn nil() Self {
        return Value.Nil;
    }

    pub fn fromBool(val: bool) Self {
        return Value{ .Bool = val };
    }

    pub fn fromNumber(val: f64) Self {
        return Value{ .Number = val };
    }

    pub fn fromObj(val: *Obj) Self {
        return Value{ .Obj = val };
    }

    pub fn equals(valA: Self, valB: Self) bool {
        return switch (valA) {
            .Nil => switch (valB) {
                .Nil => true,
                else => false,
            },
            .Bool => |a| switch (valB) {
                .Bool => |b| a == b,
                else => false,
            },
            .Number => |a| switch (valB) {
                .Number => |b| a == b,
                else => false,
            },
            .Obj => |a| switch (valB) {
                .Obj => |b| a == b,
                else => false,
            },
        };
    }

    pub fn isFalsey(self: Self) bool {
        return switch (self) {
            .Nil => true,
            .Bool => |b| !b,
            .Number => false,
            .Obj => false,
        };
    }

    pub fn format(self: Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        switch (self) {
            .Number => |value| try writer.print("{d}", .{value}),
            .Bool => |value| try writer.print("{}", .{value}),
            .Nil => try writer.print("nil", .{}),
            .Obj => |obj| try printObj(obj, writer),
        }
    }
};

fn printObj(obj: *Obj, writer: anytype) !void {
    switch (obj.objType) {
        .String => try writer.print("{s}", .{obj.asString().bytes}),
        else => unreachable,
    }
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
