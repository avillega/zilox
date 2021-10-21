const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("./value.zig").Value;
const Vm = @import("./vm.zig").Vm;

const ObjType = enum {
    string,
};

pub const Obj = struct {
    obj_type: ObjType,
    next: ?*Obj,

    pub fn create(vm: *Vm, comptime T: type, obj_type: ObjType) *T {
        const ptr_t = vm.allocator.create(T) catch @panic("Error creating Obj\n");
        ptr_t.obj = Obj{
            .obj_type = obj_type,
            .next = vm.objects,
        };
        vm.objects = &ptr_t.obj;

        return ptr_t;
    }

    pub fn asString(self: *Obj) *String {
        return @fieldParentPtr(String, "obj", self);
    }

    pub fn is(self: *Obj, obj_type: ObjType) bool {
        return self.obj_type == obj_type;
    }

    pub fn toValue(self: *Obj) Value {
        return Value.fromObj(self);
    }

    pub fn destroy(self: *Obj, vm: *Vm) void {
        switch (self.obj_type) {
            .string => self.asString().destroy(vm),
        }
    }

    pub const String = struct {
        obj: Obj,
        bytes: []const u8,
        hash: u32,

        fn allocate(vm: *Vm, bytes: []const u8, hash: u32) *String {
            const str = Obj.create(vm, @This(), .string);
            str.bytes = bytes;
            str.hash = hash;
            _ = vm.strings.set(str, Value.nil);
            return str;
        }

        pub fn copy(vm: *Vm, bytes: []const u8) *String {
            const hash = hashBytes(bytes);
            return vm.strings.findString(bytes, hash) orelse {
                const heapBytes = vm.allocator.alloc(u8, bytes.len) catch @panic("Error copying String\n");
                std.mem.copy(u8, heapBytes, bytes);
                return allocate(vm, heapBytes, hash);
            };
        }

        /// Assumes it can take the bytes instead of copying them
        pub fn take(vm: *Vm, bytes: []const u8) *String {
            const hash = hashBytes(bytes);
            const interned = vm.strings.findString(bytes, hash);
            if (interned != null) {
                vm.allocator.free(bytes);
                return interned.?;
            }

            return allocate(vm, bytes, hash);
        }

        pub fn destroy(self: *String, vm: *Vm) void {
            vm.allocator.free(self.bytes);
            vm.allocator.destroy(self);
        }
    };
};

fn hashBytes(bytes: []const u8) u32 {
    var hash: u32 = 2166136261;
    for (bytes) |byte| {
        hash ^= byte;
        hash *%= 16777619;
    }
    return hash;
}
