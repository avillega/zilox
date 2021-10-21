const std = @import("std");
const chunks = @import("./chunks.zig");
const values = @import("./value.zig");
const Chunk = chunks.Chunk;
const OpCode = chunks.OpCode;

pub fn dissasembleChunk(chunk: *Chunk, comptime name: []const u8) void {
    std.debug.print("=== {s} ===\n", .{name});
    const code = chunk.code;
    var offset: usize = 0;
    while (offset < code.count) {
        disassembleInstruction(chunk, offset);
        offset = calcOffset(code.items[offset], offset);
    }
}

fn calcOffset(instruction_code: u8, current_offset: usize) usize {
    const instruction = @intToEnum(OpCode, instruction_code);
    return current_offset + instruction.num_operands() + 1;
}

pub fn disassembleInstruction(chunk: *Chunk, offset: usize) void {
    std.debug.print("{d:0>4} ", .{offset});
    const code = chunk.code;
    const lines = chunk.lines;

    if (offset > 0 and lines.items[offset] == lines.items[offset - 1]) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d: >4} ", .{lines.items[offset]});
    }

    const instruction = @intToEnum(OpCode, code.items[offset]);
    switch (instruction) {
        .op_constant => constantInstruction("OP_CONSTANT", chunk, offset),
        .op_negate => simpleInstruction("OP_NEGATE"),
        .op_not => simpleInstruction("OP_NOT"),
        .op_nil => simpleInstruction("OP_NIL"),
        .op_false => simpleInstruction("OP_EQUAL"),
        .op_equal => simpleInstruction("OP_FALSE"),
        .op_greater => simpleInstruction("OP_GREATER"),
        .op_less => simpleInstruction("OP_LESS"),
        .op_true => simpleInstruction("OP_TRUE"),
        .op_add => simpleInstruction("OP_ADD"),
        .op_sub => simpleInstruction("OP_SUBSTRACT"),
        .op_mul => simpleInstruction("OP_MULTIPLY"),
        .op_div => simpleInstruction("OP_DIVIDE"),
        .op_ret => simpleInstruction("OP_RETURN"),
    }
}

fn simpleInstruction(comptime name: []const u8) void {
    std.debug.print("{s}\n", .{name});
}

fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize) void {
    const constant_idx = chunk.code.items[offset + 1];
    const constant = chunk.constants.items[constant_idx];
    std.debug.print("{s: <16} {d: >4} '{}'\n", .{ name, constant_idx, constant });
}
