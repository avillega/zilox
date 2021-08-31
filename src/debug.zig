const std = @import("std");
const chunks = @import("./chunks.zig");
const values = @import("./value.zig");
const Chunk = chunks.Chunk;
const OpCode = chunks.OpCode;

pub fn dissasemble_chunk(chunk: *Chunk, comptime name: []const u8) void {
    std.debug.print("=== {s} ===\n", .{name});
    const code = chunk.code;
    var offset: usize = 0;
    while (offset < code.count) {
        disassemble_instruction(chunk, offset);
        offset = calc_offset(code.items[offset], offset);
    }
}

fn calc_offset(instruction_code: u8, current_offset: usize) usize {
    const instruction = @intToEnum(OpCode, instruction_code);
    return current_offset + instruction.num_operands() + 1;
}

fn disassemble_instruction(chunk: *Chunk, offset: usize) void {
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
        .op_ret => simple_instruction("OP_RETURN"),
        .op_constant => constant_instruction("OP_CONSTANT", chunk, offset),
    }
}

fn simple_instruction(comptime name: []const u8) void {
    std.debug.print("{s}\n", .{ name });
}

fn constant_instruction(name: []const u8, chunk: *Chunk, offset: usize) void {
    const constant_idx = chunk.code.items[offset + 1];
    const constant = chunk.constants.items[constant_idx];
    std.debug.print("{s: <16} {d: >4} '", .{ name, constant_idx });
    values.printValue(constant);
    std.debug.print("'\n", .{});
}
