const std = @import("std");
const chunk_mod = @import("./chunk.zig");
const Chunk = chunk_mod.Chunk;
const OpCode = chunk_mod.OpCode;
const print = std.debug.print;

const Sign = enum {
    neg,
    pos,
};

pub fn dissasembleChunk(chunk: *const Chunk, name: []const u8) void {
    print("==== {s} ====\n", .{name});

    var idx: usize = 0;
    while (idx < chunk.code.items.len) {
        const item = @intToEnum(OpCode, chunk.code.items[idx]);
        idx = dissasembleInstruction(chunk, item, idx);
    }
}

pub fn dissasembleInstruction(chunk: *const Chunk, op_code: OpCode, idx: usize) usize {
    print("{d:0>4} ", .{idx});
    if (idx > 0 and chunk.lines.items[idx] == chunk.lines.items[idx - 1]) {
        print("   | ", .{});
    } else {
        print("{d:4} ", .{chunk.lines.items[idx]});
    }

    return switch (op_code) {
        .op_const => constantInstruction("op_const", chunk, idx),
        .op_neg => simpleInstruction("op_neg", idx),
        .op_add => simpleInstruction("op_add", idx),
        .op_sub => simpleInstruction("op_sub", idx),
        .op_mul => simpleInstruction("op_mul", idx),
        .op_div => simpleInstruction("op_div", idx),
        .op_nil => simpleInstruction("op_nil", idx),
        .op_true => simpleInstruction("op_true", idx),
        .op_false => simpleInstruction("op_false", idx),
        .op_not => simpleInstruction("op_not", idx),
        .op_ret => simpleInstruction("op_ret", idx),
        .op_less => simpleInstruction("op_less", idx),
        .op_greater => simpleInstruction("op_greater", idx),
        .op_print => simpleInstruction("op_print", idx),
        .op_pop => simpleInstruction("op_pop", idx),
        .op_equal => simpleInstruction("op_equal", idx),
        .op_define_global => constantInstruction("op_define_global", chunk, idx),
        .op_get_global => constantInstruction("op_get_global", chunk, idx),
        .op_set_global => constantInstruction("op_set_global", chunk, idx),
        .op_get_local => byteInstruction("op_get_local", chunk, idx),
        .op_set_local => byteInstruction("op_set_local", chunk, idx),
        .op_jmp => jmpInstruction("op_jmp", .pos, chunk, idx),
        .op_jmp_if_false => jmpInstruction("op_jmp_if_false", .pos, chunk, idx),
    };
}

pub fn byteInstruction(name: []const u8, chunk: *const Chunk, idx: usize) usize {
    const slot = chunk.code.items[idx + 1];
    print("{s: <16} {d:4}\n", .{ name, slot });
    return idx + 2;
}

pub fn jmpInstruction(name: []const u8, comptime sign: Sign, chunk: *const Chunk, idx: usize) usize {
    const b1 = @as(u16, chunk.code.items[idx + 1]);
    const b2 = chunk.code.items[idx + 2];
    const jmp = (b1 << 8) | b2;
    const jmp_addr = switch (sign) {
        .neg => idx + 3 - jmp,
        .pos => idx + 3 + jmp,
    };
    print("{s: <16} {d:4} -> {d}\n", .{ name, jmp, jmp_addr});
    return idx + 3;
}

pub fn constantInstruction(name: []const u8, chunk: *const Chunk, idx: usize) usize {
    const constant_idx = chunk.code.items[idx + 1];
    print("{s: <16} {d:4} '{}'\n", .{ name, constant_idx, chunk.values.items[constant_idx] });
    return idx + 2;
}

pub fn simpleInstruction(name: []const u8, idx: usize) usize {
    print("{s}\n", .{name});
    return idx + 1;
}
