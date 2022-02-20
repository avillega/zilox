const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("./value.zig").Value;
const String = @import("./object.zig").Obj.String;

const table_max_load = 0.75;

const Entry = struct {
    key: ?*String = null,
    value: Value = Value.nil,
};

pub const Table = struct {
    count: u32,
    entries: []Entry,
    allocator: *Allocator,

    pub fn init(allocator: *Allocator) Table {
        return Table{
            .count = 0,
            .entries = &[_]Entry{},
            .allocator = allocator,
        };
    }

    /// adds the entry .{key, value} to the map, if the key already exists repalce the value,
    /// return true when the key is inserted, false when key was already present 
    pub fn set(self: *Table, key: *String, value: Value) bool {
        const capacity = self.entries.len;
        if (@intToFloat(f64, self.count + 1) > @intToFloat(f64, capacity) * table_max_load) {
            const new_capacity = if (capacity < 8) 8 else capacity * 2;
            self.adjustCapacity(new_capacity);
        }

        const entry = findEntry(self.entries, key);
        const is_new_key = entry.key == null;
        if (is_new_key and entry.value.isNil()) self.count += 1;

        entry.key = key;
        entry.value = value;
        return is_new_key;
    }

    pub fn get(self: *Table, key: *String) ?*Value {
        if (self.count == 0) return null;
        const entry = findEntry(self.entries, key);
        if (entry.key == null) return null;
        return &entry.value;
    }

    /// Return true when the entry is deleted from the table, false otherwise
    pub fn delete(self: *Table, key: *String) bool {
        if (self.count == 0) return false;

        const entry = findEntry(self.entries, key);
        if (entry.key == null) return false;

        entry.key = null;
        entry.value = Value.fromBool(true);
        return true;
    }

    pub fn findString(self: *Table, bytes: []const u8, hash: u32) ?*String {
        if (self.count == 0) return null;
        var index = hash % self.entries.len;
        while (true) {
            const entry = &self.entries[index];
            if (entry.key == null and entry.value.isNil()) return null;
            if (hash == entry.key.?.hash and std.mem.eql(u8, entry.key.?.bytes, bytes)) return entry.key.?;

            index = (index + 1) % self.entries.len;
        }
    }

    pub fn deinit(self: *Table) void {
        self.allocator.free(self.entries);
        self.count = 0;
    }

    pub fn addAll(self: *Table, from: *Table) void {
        for (from.entries) |*entry| {
            if (entry.key != null) {
                self.set(entry.key, entry.value);
            }
        }
    }

    fn adjustCapacity(self: *Table, new_capacity: usize) void {
        const entries = self.allocator.alloc(Entry, new_capacity) catch @panic("Error allocating memery for new entries!");
        for (entries) |*e| {
            e.* = Entry{};
        }

        self.count = 0;
        for (self.entries) |e| {
            if (e.key == null) continue;
            const dst: *Entry = findEntry(entries, e.key.?);
            dst.key = e.key;
            dst.value = e.value;
            self.count += 1;
        }

        self.allocator.free(self.entries);

        self.entries = entries;
    }

    fn findEntry(entries: []Entry, key: *String) *Entry {
        const capacity = entries.len;
        var index = key.hash % capacity;
        var tombstone: ?*Entry = null;

        while (true) {
            const entry = &entries[index];
            if (entry.key == null) {
                if (entry.value.isNil()) {
                    // not a tombstone
                    return tombstone orelse entry;
                } else {
                    // Found a tombstone
                    tombstone = entry;
                }
                return entry;
            } else if (entry.key == key) {
                return entry;
            }

            index = (index + 1) % capacity;
        }
    }

    pub fn debug(self: *Table) void {
        std.debug.print("[\n", .{});
        for (self.entries) |entry| {
            if (entry.key) |e| {
                std.debug.print("  {s} => ", .{e.bytes});
            } else {
                std.debug.print("  -- => ", .{});
            }
            std.debug.print("{any}\n", .{entry.value});
        }
        std.debug.print("]\n", .{});
    }
};

test "Add entry" {
    const Vm = @import("./vm.zig").Vm;
    const testing = std.testing;

    var vm = Vm.init(testing.allocator);
    defer vm.deinit();

    var table = Table.init(testing.allocator);
    defer table.deinit();

    var k1 = String.copy(&vm, "Key1");
    var result = table.set(k1, Value.fromBool(true));
    try testing.expect(result);

    result = table.set(k1, Value.fromNumber(13.0));
    try testing.expect(!result);
}

test "query entry" {
    const Vm = @import("./vm.zig").Vm;
    const testing = std.testing;

    var vm = Vm.init(testing.allocator);
    defer vm.deinit();

    var table = Table.init(testing.allocator);
    defer table.deinit();

    var k1 = String.copy(&vm, "Key1");
    var result = table.set(k1, Value.fromNumber(42.0));
    try testing.expect(result);

    var val = table.get(k1);
    try testing.expect(val != null);
    try testing.expect(val.?.equals(Value.fromNumber(42.0)));

    var k2 = String.copy(&vm, "NoExistant");
    val = table.get(k2);
    try testing.expectEqual(val, null);
}

test "delete entry" {
    const Vm = @import("./vm.zig").Vm;
    const testing = std.testing;

    var vm = Vm.init(testing.allocator);
    defer vm.deinit();

    var table = Table.init(testing.allocator);
    defer table.deinit();

    var k1 = String.copy(&vm, "Key1");
    var result = table.set(k1, Value.fromNumber(42.0));
    try testing.expect(result);

    result = table.delete(k1);
    try testing.expect(result);

    const val = table.get(k1);
    try testing.expectEqual(val, null);

    result = table.delete(k1);
    try testing.expect(!result);
}

test "find string" {
    const Vm = @import("./vm.zig").Vm;
    const testing = std.testing;

    var vm = Vm.init(testing.allocator);
    defer vm.deinit();

    var table = Table.init(testing.allocator);
    defer table.deinit();

    var k1 = String.copy(&vm, "Key1");
    var result = table.set(k1, Value.fromNumber(42.0));
    try testing.expect(result);

    var k2 = table.findString("Key1", 506120967).?;
    testing.expect(k1 == k2) catch {
        std.debug.print("Expected k1: {*} == k2: {*}", .{ k1, k2 });
        return error.TestUnexpectedResult;
    };
}

test "add two" {
    const Vm = @import("./vm.zig").Vm;
    const testing = std.testing;

    var vm = Vm.init(testing.allocator);
    defer vm.deinit();

    var table = Table.init(testing.allocator);
    defer table.deinit();

    var k1 = String.copy(&vm, "one");
    var k2 = String.copy(&vm, "two");
    var k3 = String.copy(&vm, "three");
    var k4 = String.copy(&vm, "four");
    var result = table.set(k1, Value.fromNumber(1.0));
    try testing.expect(result);
    result = table.set(k2, Value.fromNumber(2.0));
    try testing.expect(result);
    result = table.set(k3, Value.fromNumber(3.0));
    try testing.expect(result);
    result = table.set(k4, Value.fromNumber(4.0));
    try testing.expect(result);

    var val = table.get(k1);
    try testing.expect(val != null);

    val = table.get(k1);
    try testing.expect(val != null);
}
