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
        offset = calc_offset(code.items[offset], offset);
    }
}

fn calc_offset(instruction_code: u8, current_offset: usize) usize {
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
        .op_constant => constant_instruction("OP_CONSTANT", chunk, offset),
        .op_negate => simple_instruction("OP_NEGATE"),
        .op_not => simple_instruction("OP_NOT"),
        .op_nil => simple_instruction("OP_NIL"),
        .op_false => simple_instruction("OP_EQUAL"),
        .op_equal => simple_instruction("OP_FALSE"),
        .op_greater => simple_instruction("OP_GREATER"),
        .op_less => simple_instruction("OP_LESS"),
        .op_true => simple_instruction("OP_TRUE"),
        .op_add => simple_instruction("OP_ADD"),
        .op_sub => simple_instruction("OP_SUBSTRACT"),
        .op_mul => simple_instruction("OP_MULTIPLY"),
        .op_div => simple_instruction("OP_DIVIDE"),
        .op_ret => simple_instruction("OP_RETURN"),
    }
}

fn simple_instruction(comptime name: []const u8) void {
    std.debug.print("{s}\n", .{name});
}

fn constant_instruction(name: []const u8, chunk: *Chunk, offset: usize) void {
    const constant_idx = chunk.code.items[offset + 1];
    const constant = chunk.constants.items[constant_idx];
    std.debug.print("{s: <16} {d: >4} '", .{ name, constant_idx });
    values.printValue(constant);
    std.debug.print("'\n", .{});
}
