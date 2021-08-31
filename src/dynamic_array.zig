const std = @import("std");
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;


pub fn DynamicArray(comptime T: type) type {
    return struct {
        const Self = @This();

        count: usize,
        capacity: usize,
        items: []T,
        allocator: *Allocator,

        pub fn init(allocator: *Allocator) Self {
            return Self{
                .count = 0,
                .capacity = 0,
                .items = &[_]T{},
                .allocator = allocator,
            };
        }

        pub fn append_item(self: *Self, item: T) !void {
            if (self.capacity < self.count + 1) {
                const old_capacity = self.capacity;
                self.capacity = if (old_capacity < 8) 8 else old_capacity * 2;

                self.items = try reallocate(self.allocator, self.items, old_capacity, self.capacity);
            }
            self.items[self.count] = item;
            self.count += 1;
        }

        pub fn deinit(self: *Self) void {
            if (self.capacity == 0) return;

            self.allocator.free(self.items);
            self.* = Self.init(self.allocator);
        }

        fn grow_capacity(capacity: usize) usize {
            return if (capacity < 8) 8 else capacity * 2;
        }
    };
}

fn reallocate(allocator: *Allocator, old_mem: anytype, old_size: usize, new_size: usize) !@TypeOf(old_mem) {
    if (old_size == 0) {
        return try allocator.alloc(@typeInfo(@TypeOf(old_mem)).Pointer.child, new_size);
    } else {
        return try allocator.realloc(old_mem, new_size);
    }
}

test "create a DynamicArray" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const leaked = gpa.deinit();
        if (leaked) expect(false) catch @panic("The list is leaking");
    }

    var arr = DynamicArray(u8).init(&gpa.allocator);
    defer arr.deinit();

    try arr.append_item(5);
    try expect(arr.items[0] == 5);
    try expect(arr.count == 1);

    try arr.append_item(1);
    try arr.append_item(2);
    try arr.append_item(3);
    try arr.append_item(4);
    try arr.append_item(5);
    try arr.append_item(6);
    try arr.append_item(7);
    try arr.append_item(8);
    try arr.append_item(9);
    try arr.append_item(10);
    try arr.append_item(11);
    try arr.append_item(12);
    try arr.append_item(13);
    try arr.append_item(14);
    try expect(arr.items[10] == 10);
    arr.deinit();

    try expect(arr.count == 0);
    try expect(arr.capacity == 0);
}
