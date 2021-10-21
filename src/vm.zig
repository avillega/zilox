const std = @import("std");
const Allocator = std.mem.Allocator;
const chunks = @import("./chunks.zig");
const values = @import("./value.zig");
const debug = @import("./debug.zig");
const compiler = @import("./compiler.zig");
const Obj = @import("object.zig").Obj;
const Value = values.Value;
const Chunk = chunks.Chunk;
const OpCode = chunks.OpCode;
const valuesEq = values.valuesEq;
const Table = @import("./table.zig").Table;

const debug_trace_execution = false;
const debug_gc = false;
const stack_max = 256;

pub const InterpretError = error{
    CompileError,
    RuntimeError,
};

const BinaryOp = enum {
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
    stack: [stack_max]Value,
    stack_top: usize,
    allocator: *Allocator,
    objects: ?*Obj,
    strings: Table,

    pub fn init(allocator: *Allocator) Self {
        return Self{
            .ip = 0,
            .stack_top = 0,
            .chunk = undefined,
            .stack = undefined,
            .allocator = allocator,
            .objects = null,
            .strings = Table.init(allocator),
        };
    }

    pub fn interpret(self: *Self, source: []const u8) InterpretError!void {
        var chunk = Chunk.init(self.allocator);
        defer chunk.deinit();

        compiler.compile(source, &chunk, self) catch return InterpretError.CompileError;
        self.chunk = &chunk;
        self.ip = 0;

        try self.run();
    }

    fn run(self: *Self) InterpretError!void {
        while (true) {
            if (comptime debug_trace_execution) {
                printStack(self.stack[0..self.stack_top]);
                debug.disassembleInstruction(self.chunk, self.ip);
            }

            const instruction = self.readInstruction();
            try switch (instruction) {
                .op_constant => {
                    const constant = self.readConstant();
                    self.push(constant);
                },
                .op_negate => {
                    if (!self.peek(0).isNumber()) {
                        self.runtimeErr("Operand must be a number", .{});
                        return InterpretError.RuntimeError;
                    }
                    self.push(Value.fromNumber(-self.pop().asNumber()));
                },
                .op_not => self.push(Value.fromBool(self.pop().isFalsey())),
                .op_nil => self.push(Value.nil),
                .op_false => self.push(Value.fromBool(false)),
                .op_true => self.push(Value.fromBool(true)),
                .op_add => self.add(),
                .op_sub => self.binary_op(.sub),
                .op_mul => self.binary_op(.mul),
                .op_div => self.binary_op(.div),
                .op_greater => self.binary_op(.gt),
                .op_less => self.binary_op(.lt),
                .op_equal => self.equal(),
                .op_ret => {
                    std.debug.print("{}\n", .{self.pop()});
                    return;
                },
            };
        }
    }

    fn resetStack(self: *Self) void {
        self.stack_top = 0;
    }

    fn runtimeErr(self: *Self, comptime fmt: []const u8, args: anytype) void {
        const err_writer = std.io.getStdErr().writer();
        err_writer.print(fmt ++ "\n", args) catch {};

        err_writer.print("[line {d}] in script.\n", .{self.chunk.lines.items[self.ip]}) catch {};
        self.resetStack();
    }

    fn binary_op(self: *Self, comptime op: BinaryOp) InterpretError!void {
        if (!self.peek(0).isNumber() or !self.peek(1).isNumber()) {
            self.runtimeErr("Operands must be numbers.", .{});
            return InterpretError.RuntimeError;
        }

        const b = self.pop().asNumber();
        const a = self.pop().asNumber();
        const result = switch (op) {
            .sub => a - b,
            .mul => a * b,
            .div => a / b,
            .gt => a > b,
            .lt => a < b,
        };
        switch (@TypeOf(result)) {
            bool => self.push(Value.fromBool(result)),
            f64 => self.push(Value.fromNumber(result)),
            else => unreachable,
        }
    }

    inline fn equal(self: *Self) void {
        const b = self.pop();
        const a = self.pop();
        self.push(Value.fromBool(a.equals(b)));
    }

    inline fn add(self: *Self) !void {
        if (self.peek(0).isString() and self.peek(1).isString()) {
            self.concat();
        } else if (self.peek(0).isNumber() and self.peek(1).isNumber()) {
            const b = self.pop().asNumber();
            const a = self.pop().asNumber();
            self.push(Value.fromNumber(a + b));
        } else {
            self.runtimeErr("Operands must be two numbers or two strings.", .{});
            return InterpretError.RuntimeError;
        }
    }

    inline fn concat(self: *Self) void {
        const b = self.pop().asObj().asString();
        const a = self.pop().asObj().asString();
        const result = std.mem.concat(self.allocator, u8, &[_][]const u8{ a.bytes, b.bytes }) catch unreachable;
        const str = Obj.String.take(self, result);
        self.push(str.obj.toValue());
    }

    inline fn push(self: *Self, value: Value) void {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    inline fn peek(self: *Self, distance: usize) Value {
        return self.stack[self.stack_top - distance - 1];
    }

    fn pop(self: *Self) Value {
        self.stack_top -= 1;
        return self.stack[self.stack_top];
    }

    inline fn readInstruction(self: *Self) OpCode {
        const instruction = @intToEnum(OpCode, self.chunk.code.items[self.ip]);
        self.ip += 1;
        return instruction;
    }

    inline fn readConstant(self: *Self) Value {
        const constant = self.chunk.constants.items[self.chunk.code.items[self.ip]];
        self.ip += 1;
        return constant;
    }

    pub fn deinit(self: *Self) void {
        if (comptime debug_gc) {
            std.debug.print("Uninitializing VM\n", .{});
        }
        self.resetStack();
        self.freeObjects();
        self.strings.deinit();
    }

    fn freeObjects(self: *Self) void {
        var obj = self.objects;
        var total_objects: u64 = 0;
        while (obj) |object| {
            if (comptime debug_gc) {
                total_objects += 1;
            }
            const next = object.next;
            object.destroy(self);
            obj = next;
        }
        if (comptime debug_gc) {
            std.debug.print("Objects freed {d}\n", .{total_objects});
        }
    }
};

fn printStack(stack: []Value) void {
    std.debug.print("          ", .{});
    for (stack) |value| {
        std.debug.print("[", .{});
        values.printValue(value);
        std.debug.print("]", .{});
    }
    std.debug.print("\n", .{});
}
