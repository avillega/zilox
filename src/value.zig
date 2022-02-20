const std = @import("std");
const Vm = @import("vm.zig").Vm;
pub const ValueType = enum {
    bool,
    nil,
    number,
    string,
};

pub const Value = union(ValueType) {
    nil,
    bool: bool,
    number: f64,
    string: *ObjString,

    pub fn eql(a: Value, b: Value) bool {
        return switch (a) {
            .nil => b == .nil,
            .bool => |bo| b == .bool and bo == b.bool,
            .number => |n| b == .number and n == b.number,
            .string => |s| b == .string and s.eql(b.string),
        };
    }

    pub fn format(
        val: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (val) {
            .nil => try writer.writeAll("nil"),
            .bool => if (val.bool) try writer.writeAll("true") else try writer.writeAll("false"),
            .number => try writer.print("{d}", .{val.number}),
            .string => try writer.print("{}", .{val.string}),
        }
    }
};

pub const Obj = struct {
    const Self = @This();
    const FnType = fn (*Obj, *Vm) void;
    deinitFn: FnType,
    next: ?*Obj = null,

    pub fn deinit(obj: *Obj, vm: *Vm) void {
        obj.deinitFn(obj, vm);
    }

    pub fn allocate(self: *Obj, vm: *Vm) void {
        self.next = vm.objects;
        vm.objects = self;
    }
};

pub const ObjString = struct {
    const Self = @This();
    chars: []const u8,
    obj: Obj,

    pub fn copyString(chars: []const u8, vm: *Vm) *Self {
        const allocator = vm.allocator;
        const interned = vm.strings.get(chars);
        if (interned) |obj_str| {
            return obj_str;
        }

        const heap_chars = allocator.dupe(u8, chars) catch std.debug.panic("Not enough memory, you got bigger problems!", .{});
        return allocateString(heap_chars, vm);
    }

    pub fn takeString(chars: []const u8, vm: *Vm) *Self {
        const allocator = vm.allocator;
        const interned = vm.strings.get(chars);
        if (interned) |obj_str| {
            allocator.free(chars);
            return obj_str;
        }
        return allocateString(chars, vm);
    }

    fn allocateString(chars: []const u8, vm: *Vm) *Self {
        const allocator = vm.allocator;
        const allocated_string = allocator.create(Self) catch std.debug.panic("Not enough memory, you got bigger problems!", .{});
        allocated_string.chars = chars;
        allocated_string.obj = Obj{ .deinitFn = deinit };
        allocated_string.obj.allocate(vm);
        vm.strings.put(chars, allocated_string) catch {};
        return allocated_string;
    }

    pub fn eql(a: *Self, b: *Self) bool {
        return std.mem.eql(u8, a.chars, b.chars);
    }

    pub fn format(
        string: ObjString,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("{s}", .{string.chars});
    }

    pub fn deinit(obj: *Obj, vm: *Vm) void {
        const self = @fieldParentPtr(Self, "obj", obj);
        vm.allocator.free(self.chars);
        vm.allocator.destroy(self);
    }
};

test "Obj size" {
    @compileLog("size of Obj: ", @sizeOf(Obj));
    @compileLog("size of *Obj: ", @sizeOf(*Obj));
    @compileLog("size of Val: ", @sizeOf(Value));
}
