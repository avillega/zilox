const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("./value.zig").Value;

const ObjType = enum {
    String,
    Filler,
};

pub const Obj = struct {
    objType: ObjType,

    pub fn create(allocator: *Allocator, comptime T: type, objType: ObjType) *T {
        const ptrT = allocator.create(T) catch @panic("Error creating Obj\n");
        ptrT.obj = Obj{ .objType = objType };
        return ptrT;
    }

    pub fn asString(self: *Obj) *String {
        return @fieldParentPtr(String, "obj", self);
    }

    pub fn is(self: *Obj, objType: ObjType) bool {
        return self.objType == objType;
    }

    pub fn toValue(self: *Obj) Value {
        return Value.fromObj(self);
    }

    pub const String = struct {
    obj: Obj,
    bytes: []const u8,

    fn allocate(allocator: *Allocator, bytes: []const u8) *String {
        const str = Obj.create(allocator, @This(), .String);
        str.bytes = bytes;
        return str;
    }

    pub fn copy(allocator: *Allocator, bytes: []const u8) *String {
        const heapBytes = allocator.alloc(u8, bytes.len) catch @panic("Error copying String\n");
        std.mem.copy(u8, heapBytes, bytes);
        return allocate(allocator, heapBytes);
    }
};
};
