const std = @import("std");
const DynamicArray = @import("./dynamic_array.zig").DynamicArray;
const Value = @import("./value.zig").Value;

const Allocator = std.mem.Allocator;

const expect = std.testing.expect;

pub const OpCode = enum(u8) {
    const Self = @This();

    op_ret,
    op_constant,

    pub fn toU8(self: Self) u8 {
        return @enumToInt(self);
    }

    pub fn num_operands(self: Self) usize {
        return switch (self) {
            .op_ret => 0,
            .op_constant => 1,
        };
    }
};

pub const Chunk = struct {
    const Self = @This();
    const BytesArray = DynamicArray(u8);
    const ValuesArray = DynamicArray(Value);
    const LinesArray = DynamicArray(u16);

    code: BytesArray,
    constants: ValuesArray,
    lines: LinesArray,

    pub fn init(allocator: *Allocator) Chunk {
        return Self{
            .code = BytesArray.init(allocator),
            .constants = ValuesArray.init(allocator),
            .lines = LinesArray.init(allocator),
        };
    }

    pub fn write_chunk(self: *Self, byte: u8, line: u16) !void {
        try self.code.append_item(byte);
        try self.lines.append_item(line);
    }

    pub fn free_chunk(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
    }

    pub fn add_constant(self: *Self, value: Value) !u8 {
        try self.constants.append_item(value);
        return @intCast(u8, self.constants.count - 1);
    }
};

test "create a Chunk" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const leaked = gpa.deinit();
        if (leaked) expect(false) catch @panic("The list is leaking");
    }

    var chunk = Chunk.init(&gpa.allocator);
    defer chunk.free_chunk();

    try chunk.write_chunk(OpCode.op_ret);
    try expect(chunk.code.items[0] == OpCode.op_ret);

    try chunk.write_chunk(OpCode.op_ret);
    try chunk.write_chunk(OpCode.op_ret);
    try chunk.write_chunk(OpCode.op_ret);
    try chunk.write_chunk(OpCode.op_ret);
    try chunk.write_chunk(OpCode.op_ret);

    try expect(chunk.code.items[4] == OpCode.op_ret);
    chunk.free_chunk();
}
