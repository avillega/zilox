const std = @import("std");
const chunks = @import("./chunks.zig");
const debug = @import("./debug.zig");
const OpCode = chunks.OpCode;
const Chunk = chunks.Chunk;

pub fn main() anyerror!u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var chunk = Chunk.init(&gpa.allocator);
    defer chunk.free_chunk();

    const constant_idx = try chunk.add_constant(1.2);
    try chunk.write_chunk(OpCode.op_constant.toU8(), 123);
    try chunk.write_chunk(constant_idx, 123);

    try chunk.write_chunk(OpCode.op_ret.toU8(), 123);

    debug.dissasemble_chunk(&chunk, "test chunk");

    return 0;
}
