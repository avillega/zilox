const std = @import("std");
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;
const Obj = @import("./object.zig").Obj;

pub const Value = union(enum) {
    const Self = @This();
    nil,
    boolean: bool,
    number: f64,
    obj: *Obj,

    pub fn isNil(self: Self) bool {
        return self == .nil;
    }

    pub fn isBool(self: Self) bool {
        return self == .boolean;
    }

    pub fn isNumber(self: Self) bool {
        return self == .number;
    }

    pub fn isObj(self: Self) bool {
        return self == .obj;
    }

    pub fn isString(self: Self) bool {
        return self.isObj() and self.asObj().is(.string);
    }

    pub fn asBool(self: Self) bool {
        std.debug.assert(self.isBool());
        return self.boolean;
    }

    pub fn asNumber(self: Self) f64 {
        std.debug.assert(self.isNumber());
        return self.number;
    }

    pub fn asObj(self: Self) *Obj {
        std.debug.assert(self.isObj());
        return self.obj;
    }

    pub fn fromBool(val: bool) Self {
        return Value{ .boolean = val };
    }

    pub fn fromNumber(val: f64) Self {
        return Value{ .number = val };
    }

    pub fn fromObj(val: *Obj) Self {
        return Value{ .obj = val };
    }

    pub fn equals(valA: Self, valB: Self) bool {
        return switch (valA) {
            .nil => switch (valB) {
                .nil => true,
                else => false,
            },
            .boolean => |a| switch (valB) {
                .boolean => |b| a == b,
                else => false,
            },
            .number => |a| switch (valB) {
                .number => |b| a == b,
                else => false,
            },
            .obj => |a| switch (valB) {
                .obj => |b| {
                    return a == b;
                },
                else => false,
            },
        };
    }

    pub fn isFalsey(self: Self) bool {
        return switch (self) {
            .nil => true,
            .boolean => |b| !b,
            .number => false,
            .obj => false,
        };
    }

    pub fn format(self: Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        switch (self) {
            .number => |value| try writer.print("{d}", .{value}),
            .boolean => |value| try writer.print("{}", .{value}),
            .nil => try writer.print("nil", .{}),
            .obj => |obj| try printObj(obj, writer),
        }
    }
};

fn printObj(obj: *Obj, writer: anytype) !void {
    switch (obj.obj_type) {
        .string => try writer.print("{s}", .{obj.asString().bytes}),
    }
}

test "size of a Value" {
    try expect(@sizeOf(Value) == 16);
}
