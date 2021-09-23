const std = @import("std");
const chunks = @import("./chunks.zig");
const values = @import("./value.zig");
const debug = @import("./debug.zig");
const compiler = @import("./compiler.zig");
const Allocator = std.mem.Allocator;
const Value = values.Value;
const Chunk = chunks.Chunk;
const OpCode = chunks.OpCode;
const print = std.debug.print;

const DEBUG_TRACE_EXECUTION = false;

const STACK_MAX = 256;

pub const InterpretError = error{
    CompileError,
    RuntimeError,
};

const BinaryOp = enum {
    add,
    sub,
    div,
    mul,
};

pub const Vm = struct {
    const Self = @This();
    chunk: *Chunk,
    ip: [*]u8,
    stack: [STACK_MAX]Value,
    stack_top: usize,

    pub fn init() Self {
        return Self{
            .chunk = undefined,
            .ip = undefined,
            .stack = undefined,
            .stack_top = 0,
        };
    }

    pub fn interpret(self: *Self, source: []const u8, allocator: *Allocator) InterpretError!void {
        _ = self;
        var chunk = Chunk.init(allocator);
        defer chunk.deinit();

        compiler.compile(source, &chunk) catch return InterpretError.CompileError;
        self.chunk = &chunk;
        self.ip = chunk.code.items.ptr;

        try self.run();
    }

    fn run(self: *Self) InterpretError!void {
        while (true) {
            if (comptime DEBUG_TRACE_EXECUTION) {
                print_stack(self.stack[0..self.stack_top]);
                debug.disassembleInstruction(self.chunk, @ptrToInt(self.ip) - @ptrToInt(self.chunk.code.items.ptr));
            }

            const instruction = self.read_instruction();
            switch (instruction) {
                .op_constant => {
                    const constant = self.read_constant();
                    self.push(constant);
                },
                .op_negate => self.push(-self.pop()),
                .op_add => self.binary_op(.add),
                .op_sub => self.binary_op(.sub),
                .op_mul => self.binary_op(.mul),
                .op_div => self.binary_op(.div),
                .op_ret => {
                    values.printValue(self.pop());
                    print("\n", .{});
                    return;
                },
            }
        }
    }

    fn binary_op(self: *Self, comptime op: BinaryOp) void {
        const b = self.pop();
        const a = self.pop();
        const result = switch (op) {
            .add => a + b,
            .sub => a - b,
            .mul => a * b,
            .div => a / b,
        };
        self.push(result);
    }

    pub fn push(self: *Self, value: Value) void {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    pub fn pop(self: *Self) Value {
        self.stack_top -= 1;
        return self.stack[self.stack_top];
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    inline fn read_instruction(self: *Self) OpCode {
        const instruction = @intToEnum(OpCode, self.ip[0]);
        self.ip += 1;
        return instruction;
    }

    inline fn read_constant(self: *Self) Value {
        const constant = self.chunk.constants.items[self.ip[0]];
        self.ip += 1;
        return constant;
    }
};

fn print_stack(stack: []Value) void {
    print("          ", .{});
    for (stack) |value| {
        print("[", .{});
        values.printValue(value);
        print("]", .{});
    }
    print("\n", .{});
}
