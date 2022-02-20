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

        pub fn appendItem(self: *Self, item: T) void {
            if (self.capacity < self.count + 1) {
                self.capacity = growCapacity(self.capacity);
                self.items = self.allocator.realloc(self.items, self.capacity) catch @panic("Error allocating new memory");
            }
            self.items[self.count] = item;
            self.count += 1;
        }

        pub fn deinit(self: *Self) void {
            if (self.capacity == 0) return;

            self.allocator.free(self.items);
            self.* = Self.init(self.allocator);
        }

        fn growCapacity(capacity: usize) usize {
            return if (capacity < 8) 8 else capacity * 2;
        }
    };
}

test "create a DynamicArray" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const leaked = gpa.deinit();
        if (leaked) expect(false) catch @panic("The list is leaking");
    }

    var arr = DynamicArray(u8).init(&gpa.allocator);
    defer arr.deinit();

    arr.appendItem(5);
    try expect(arr.items[0] == 5);
    try expect(arr.count == 1);

    arr.appendItem(1);
    arr.appendItem(2);
    arr.appendItem(3);
    arr.appendItem(4);
    arr.appendItem(5);
    arr.appendItem(6);
    arr.appendItem(7);
    arr.appendItem(8);
    arr.appendItem(9);
    arr.appendItem(10);
    arr.appendItem(11);
    arr.appendItem(12);
    arr.appendItem(13);
    arr.appendItem(14);
    try expect(arr.items[10] == 10);
    arr.deinit();

    try expect(arr.count == 0);
    try expect(arr.capacity == 0);
}
