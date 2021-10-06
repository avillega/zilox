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
const valuesEq = values.valuesEq;

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
    gt,
    lt,
};

pub const Vm = struct {
    const Self = @This();
    chunk: *Chunk,
    ip: usize,
    stack: [STACK_MAX]Value,
    stackTop: usize,

    pub fn init() Self {
        return Self{
            .ip = 0,
            .stackTop = 0,
            .chunk = undefined,
            .stack = undefined,
        };
    }

    pub fn interpret(self: *Self, source: []const u8, allocator: *Allocator) InterpretError!void {
        var chunk = Chunk.init(allocator);
        defer chunk.deinit();

        compiler.compile(source, &chunk) catch return InterpretError.CompileError;
        self.chunk = &chunk;
        self.ip = 0;

        try self.run();
    }

    fn run(self: *Self) InterpretError!void {
        while (true) {
            if (comptime DEBUG_TRACE_EXECUTION) {
                print_stack(self.stack[0..self.stackTop]);
                debug.disassembleInstruction(self.chunk, self.ip);
            }

            const instruction = self.read_instruction();
            try switch (instruction) {
                .op_constant => {
                    const constant = self.read_constant();
                    self.push(constant);
                },
                .op_negate => {
                    if (self.peek(0) == .number) {
                        self.runtimeErr("Operand must be a number", .{});
                        return InterpretError.RuntimeError;
                    }
                    self.push(Value{ .number = -self.pop().number });
                },
                .op_not => self.push(Value{ .boolean = isFalsey(self.pop()) }),
                .op_nil => self.push(Value.nil),
                .op_false => self.push(Value{ .boolean = false }),
                .op_true => self.push(Value{ .boolean = true }),
                .op_add => self.binary_op(.add),
                .op_sub => self.binary_op(.sub),
                .op_mul => self.binary_op(.mul),
                .op_div => self.binary_op(.div),
                .op_greater => self.binary_op(.gt),
                .op_less => self.binary_op(.lt),
                .op_equal => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(Value{ .boolean = valuesEq(a, b) });
                },
                .op_ret => {
                    values.printValue(self.pop());
                    print("\n", .{});
                    return;
                },
            };
        }
    }

    fn resetStack(self: *Self) void {
        self.stackTop = 0;
    }

    fn runtimeErr(self: *Self, comptime fmt: []const u8, args: anytype) void {
        const errWriter = std.io.getStdErr().writer();
        errWriter.print(fmt ++ "\n", args) catch {};

        errWriter.print("[line {d}] in script.\n", .{self.chunk.lines.items[self.ip]}) catch {};
        self.resetStack();
    }

    fn binary_op(self: *Self, comptime op: BinaryOp) InterpretError!void {
        if (self.peek(0) != .number or self.peek(1) != .number) {
            self.runtimeErr("Operands must be numbers.", .{});
            return InterpretError.RuntimeError;
        }

        const b = self.pop().number;
        const a = self.pop().number;
        const result = switch (op) {
            .add => a + b,
            .sub => a - b,
            .mul => a * b,
            .div => a / b,
            .gt => a > b,
            .lt => a < b,
        };
        switch (@TypeOf(result)) {
            bool => self.push(Value{ .boolean = result }),
            f64 => self.push(Value{ .number = result }),
            else => unreachable,
        }
    }

    inline fn push(self: *Self, value: Value) void {
        self.stack[self.stackTop] = value;
        self.stackTop += 1;
    }

    inline fn peek(self: *Self, distance: usize) Value {
        return self.stack[self.stackTop - distance - 1];
    }

    fn pop(self: *Self) Value {
        self.stackTop -= 1;
        return self.stack[self.stackTop];
    }

    pub fn deinit(self: *Self) void {
        self.resetStack();
    }

    inline fn read_instruction(self: *Self) OpCode {
        const instruction = @intToEnum(OpCode, self.chunk.code.items[self.ip]);
        self.ip += 1;
        return instruction;
    }

    inline fn read_constant(self: *Self) Value {
        const constant = self.chunk.constants.items[self.chunk.code.items[self.ip]];
        self.ip += 1;
        return constant;
    }
};

fn isFalsey(value: Value) bool {
    return switch (value) {
        .nil => true,
        .boolean => |b| !b,
        else => false,
    };
}

fn print_stack(stack: []Value) void {
    print("          ", .{});
    for (stack) |value| {
        print("[", .{});
        values.printValue(value);
        print("]", .{});
    }
    print("\n", .{});
}
