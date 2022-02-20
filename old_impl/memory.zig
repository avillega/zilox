const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn allocateElement(allocator: *Allocator, comptime T: type) *T {
    return allocator.create(T) catch @panic("Error creating an element");
}

pub fn allocate(allocator: *Allocator, comptime T: type, size: usize) []T {
    return allocator.alloc(T, size) catch @panic("Failed when allocating");
}

pub fn reallocate(allocator: *Allocator, old_mem: anytype, old_size: usize, new_size: usize) @TypeOf(old_mem) {
    if (old_size == 0) {
        return allocator.alloc(@typeInfo(@TypeOf(old_mem)).Pointer.child, new_size) catch @panic("Failed when allocating");
    } else {
        return allocator.realloc(old_mem, new_size) catch @panic("Failed when reallocating");
    }
}
