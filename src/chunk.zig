const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("value.zig").Value;

pub const OpCode = enum(u8) {
    op_neg,
    op_add,
    op_sub,
    op_mul,
    op_div,
    op_const,
    op_nil,
    op_true,
    op_false,
    op_not,
    op_equal,
    op_greater,
    op_less,
    op_print,
    op_pop,
    op_define_global,
    op_get_global,
    op_set_global,
    op_set_local,
    op_get_local,
    op_jmp,
    op_jmp_if_false,
    op_ret,
};

pub const Chunk = struct {
    const Self = @This();
    const CodeArrayList = std.ArrayList(u8);
    const ValueArrayList = std.ArrayList(Value);
    const LinesArratList = std.ArrayList(usize);

    code: CodeArrayList,
    values: ValueArrayList,
    lines: LinesArratList,

    pub fn init(allocator: Allocator) Self {
        return Self{
            .code = CodeArrayList.init(allocator),
            .values = ValueArrayList.init(allocator),
            .lines = LinesArratList.init(allocator),
        };
    }

    pub fn writeOpCode(self: *Self, op_code: OpCode, line: usize) void {
        self.writeByte(@enumToInt(op_code), line);
    }

    pub fn addConstant(self: *Self, val: Value) usize {
        const constant_idx = self.values.items.len;
        self.values.append(val) catch {
            std.debug.panic("Not enough memory, you have dificult problems to solve, zilox is not for you!\n", .{});
        };
        return constant_idx;
    }

    pub fn writeByte(self: *Self, byte: u8, line: usize) void {
        self.code.append(byte) catch {
            std.debug.panic("Error in line {d}: Not enough memory, you have dificult problems to solve, zilox is not for you!\n", .{line});
        };
        self.lines.append(line) catch {
            std.debug.panic("Error in line {d}: Not enough memory, you have dificult problems to solve, zilox is not for you!\n", .{line});
        };
    }

    pub fn deinit(self: *Self) void {
        self.code.deinit();
        self.values.deinit();
        self.lines.deinit();
    }
};
