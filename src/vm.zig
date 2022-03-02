const std = @import("std");
const chunk_mod = @import("./chunk.zig");
const value_mod = @import("./value.zig");
const compile = @import("compiler.zig").compile;
const dissasembleInstruction = @import("./debug.zig").dissasembleInstruction;
const Chunk = chunk_mod.Chunk;
const OpCode = chunk_mod.OpCode;
const Value = value_mod.Value;
const Obj = value_mod.Obj;
const ObjString = value_mod.ObjString;

const trace_execution = true;
const trace_stack = false;
const stack_max = 256;

pub const InterpretError = error{
    compile_error,
    runtime_error,
};

const BinaryOperation = enum { add, sub, mul, div, less, greater };

pub const Vm = struct {
    const Self = @This();
    const out_writer = std.io.getStdOut().writer();
    const StringsHashMap = std.StringHashMap(*ObjString);
    const GlobalsHashMap = std.StringHashMap(Value);

    chunk: *Chunk = undefined,
    ip: usize,
    stack: [stack_max]Value = undefined,
    stack_top: usize,
    allocator: std.mem.Allocator,
    strings: StringsHashMap,
    globals: GlobalsHashMap,
    objects: ?*Obj = null,

    pub fn init(allocator: std.mem.Allocator) Vm {
        return Vm{
            .ip = 0,
            .stack_top = 0,
            .allocator = allocator,
            .strings = StringsHashMap.init(allocator),
            .globals = GlobalsHashMap.init(allocator),
        };
    }

    pub fn interpret(self: *Self, src: []const u8) InterpretError!void {
        var chunk = Chunk.init(self.allocator);
        defer chunk.deinit();
        compile(src, &chunk, self) catch return InterpretError.compile_error;
        self.chunk = &chunk;
        self.ip = 0;
        return self.run();
    }

    pub fn push(self: *Self, value: Value) void {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    pub fn pop(self: *Self) Value {
        self.stack_top -= 1;
        return self.stack[self.stack_top];
    }

    fn run(self: *Self) InterpretError!void {
        while (true) {
            const instruction = @intToEnum(OpCode, self.readByte());

            if (comptime trace_stack) {
                self.trace_stack_execution();
            }

            if (comptime trace_execution) {
                _ = dissasembleInstruction(self.chunk, instruction, self.ip - 1);
            }

            try switch (instruction) {
                .op_const => self.opConst(),
                .op_neg => self.opNeg(),
                .op_not => self.opNot(),
                .op_add => self.opAdd(),
                .op_sub => self.opBinary(.sub),
                .op_mul => self.opBinary(.mul),
                .op_div => self.opBinary(.div),
                .op_less => self.opBinary(.less),
                .op_greater => self.opBinary(.greater),
                .op_true => self.push(Value{ .bool = true }),
                .op_false => self.push(Value{ .bool = false }),
                .op_nil => self.push(Value.nil),
                .op_equal => self.opEqual(),
                .op_print => self.opPrint(),
                .op_pop => _ = self.pop(),
                .op_define_global => self.opDefineGlobal(),
                .op_get_global => self.opGetGlobal(),
                .op_set_global => self.opSetGlobal(),
                .op_get_local => self.opGetLocal(),
                .op_set_local => self.opSetLocal(),
                .op_jmp => self.opJmp(),
                .op_jmp_if_false => self.opJmpIfFalse(),
                .op_loop => self.opLoop(),
                .op_ret => {
                    return;
                },
            };
        }
    }

    inline fn opLoop(self: *Self) void {
        const offset = self.readU16();
        self.ip -= offset;
    }

    inline fn opJmp(self: *Self) void {
        const offset = self.readU16();
        self.ip += offset;
    }

    inline fn opJmpIfFalse(self: *Self) void {
        const offset = self.readU16();
        if (isFalsey(self.peek(0))) {
            self.ip += offset;
        }
    }

    inline fn opGetLocal(self: *Self) void {
        const slot = self.readByte();
        self.push(self.stack[slot]);
    }

    inline fn opSetLocal(self: *Self) void {
        const slot = self.readByte();
        self.stack[slot] = self.peek(0);

    }

    inline fn opSetGlobal(self: *Self) InterpretError!void {
        const name = self.readString();
        if (!self.globals.contains(name.chars)) {
            self.runtimeError("Undefined variable '{s}'", .{name.chars});
            return InterpretError.runtime_error;
        }
        self.globals.put(name.chars, self.peek(0)) catch {};
    }

    inline fn opGetGlobal(self: *Self) InterpretError!void {
        const name = self.readString();
        const val = self.globals.get(name.chars) orelse {
            self.runtimeError("Undefined variable {s}", .{name.chars});
            return InterpretError.runtime_error;
        };
        self.push(val);
    }

    inline fn opDefineGlobal(self: *Self) void {
        const name = self.readString();
        self.globals.put(name.chars, self.peek(0)) catch {};
        _ = self.pop();
    }

    inline fn readString(self: *Self) *ObjString {
        return self.chunk.values.items[self.readByte()].string;
    }

    inline fn opPrint(self: *Self) void {
        out_writer.print("{}\n", .{self.pop()}) catch {
            std.debug.panic("Couldn't write to std output\n", .{});
        };
    }

    inline fn opConst(self: *Self) void {
        const constant = self.chunk.values.items[self.readByte()];
        self.push(constant);
    }

    inline fn opNeg(self: *Self) InterpretError!void {
        if (self.peek(0) != .number) {
            self.runtimeError("Operand must be a number.", .{});
            return InterpretError.runtime_error;
        }
        const val = Value{ .number = -(self.pop().number) };
        self.push(val);
    }

    inline fn opAdd(self: *Self) InterpretError!void {
        if (self.peek(0) == .string and self.peek(1) == .string) {
            self.concat();
        } else if (self.peek(0) == .number and self.peek(1) == .number) {
            try self.opBinary(.add);
        } else {
            self.runtimeError("Operands must be two numbers or two strings.", .{});
            return InterpretError.runtime_error;
        }
    }

    inline fn concat(self: *Self) void {
        const b = self.pop().string;
        const a = self.pop().string;

        const new_chars = std.mem.concat(self.allocator, u8, &[_][]const u8{ a.chars, b.chars }) catch std.debug.panic("Not enough memory!", .{});
        const val = Value{ .string = ObjString.takeString(new_chars, self) };
        self.push(val);
    }

    inline fn opNot(self: *Self) InterpretError!void {
        const val = Value{ .bool = isFalsey(self.pop()) };
        self.push(val);
    }

    inline fn opBinary(self: *Self, comptime binary_operation: BinaryOperation) InterpretError!void {
        if (self.peek(0) != .number or self.peek(1) != .number) {
            self.runtimeError("Operands must be numbers.", .{});
            return InterpretError.runtime_error;
        }

        const b = self.pop().number;
        const a = self.pop().number;
        const result = switch (binary_operation) {
            .add => a + b,
            .sub => a - b,
            .mul => a * b,
            .div => a / b,
            .less => a < b,
            .greater => a > b,
        };
        if (@TypeOf(result) == bool) {
            self.push(Value{ .bool = result });
        } else {
            self.push(Value{ .number = result });
        }
    }

    inline fn opEqual(self: *Self) void {
        const b = self.pop();
        const a = self.pop();
        self.push(Value{ .bool = Value.eql(a, b) });
    }

    inline fn readByte(self: *Self) u8 {
        const byte = self.chunk.code.items[self.ip];
        self.ip += 1;
        return byte;
    }

    inline fn readU16(self: *Self) u16 {
        const b1 = @as(u16, self.chunk.code.items[self.ip]);
        const b2 = self.chunk.code.items[self.ip+1];
        self.ip += 2;
        return (b1 << 8) | b2;
    }

    fn peek(self: *Self, distance: usize) Value {
        return self.stack[self.stack_top - 1 - distance];
    }

    fn isFalsey(val: Value) bool {
        return val == .nil or (val == .bool and !val.bool);
    }

    fn runtimeError(self: *Self, comptime fmt: []const u8, args: anytype) void {
        const errout = std.io.getStdErr().writer();
        errout.print(fmt ++ "\n", args) catch {};
        errout.print("[line {d}] in script\n", .{self.chunk.lines.items[self.ip - 1]}) catch {};
        self.reset();
    }

    inline fn trace_stack_execution(self: *Self) void {
        std.debug.print("       ", .{});
        var i: usize = 0;
        while (i < self.stack_top) : (i += 1) {
            std.debug.print("[ {d} ]", .{self.stack[i]});
        }
        std.debug.print("\n", .{});
    }

    pub fn reset(self: *Self) void {
        self.stack_top = 0;
        self.ip = 0;
    }

    pub fn deinit(self: *Self) void {
        self.deinitObjs();
        self.strings.deinit();
        self.globals.deinit();
        self.stack_top = 0;
        self.ip = 0;
    }

    fn deinitObjs(self: *Self) void {
        var current = self.objects;
        while (current) |obj| {
            const next = obj.next;
            obj.deinit(self);
            current = next;
        }
    }
};
