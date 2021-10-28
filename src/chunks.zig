const std = @import("std");
const DynamicArray = @import("./dynamic_array.zig").DynamicArray;
const Value = @import("./value.zig").Value;

const Allocator = std.mem.Allocator;

const expect = std.testing.expect;

pub const OpCode = enum(u8) {
    const Self = @This();

    op_ret,
    op_constant,
    op_nil,
    op_true,
    op_false,
    op_negate,
    op_not,
    op_equal,
    op_greater,
    op_less,
    op_add,
    op_sub,
    op_mul,
    op_div,
    op_print,
    op_pop,
    op_define_gloabl,
    op_get_global,
    op_set_global,

    pub fn toU8(self: Self) u8 {
        return @enumToInt(self);
    }

    pub fn num_operands(self: Self) usize {
        return switch (self) {
            .op_constant => 1,
            .op_define_gloabl => 1,
            .op_get_global => 1,
            .op_set_global => 1,
            else => 0,
        };
    }
};

pub const Chunk = struct {
    const Self = @This();
    const BytesArray = DynamicArray(u8);
    const ValuesArray = DynamicArray(Value);
    const LinesArray = DynamicArray(usize);

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

    pub fn write(self: *Self, byte: u8, line: usize) void {
        self.code.appendItem(byte);
        self.lines.appendItem(line);
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
    }

    pub fn addConstant(self: *Self, value: Value) u16 {
        self.constants.appendItem(value);
        return @intCast(u16, self.constants.count - 1);
    }
};

test "create a Chunk" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const leaked = gpa.deinit();
        if (leaked) expect(false) catch @panic("The list is leaking");
    }

    var chunk = Chunk.init(&gpa.allocator);
    defer chunk.deinit();

    chunk.write(OpCode.op_ret.toU8(), 1);
    try expect(chunk.code.items[0] == OpCode.op_ret.toU8());

    chunk.write(OpCode.op_ret.toU8(), 1);
    chunk.write(OpCode.op_ret.toU8(), 1);
    chunk.write(OpCode.op_ret.toU8(), 1);
    chunk.write(OpCode.op_ret.toU8(), 1);
    chunk.write(OpCode.op_ret.toU8(), 1);

    try expect(chunk.code.items[4] == OpCode.op_ret.toU8());
    chunk.deinit();
}
