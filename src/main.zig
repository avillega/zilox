const std = @import("std");
const chunks = @import("./chunks.zig");
const debug = @import("./debug.zig");
const OpCode = chunks.OpCode;
const Chunk = chunks.Chunk;
const Vm = @import("./vm.zig").Vm;

pub fn main() anyerror!u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var vm = Vm.init();
    defer vm.deinit();

    var chunk = Chunk.init(&gpa.allocator);
    defer chunk.free_chunk();

    var constant_idx = try chunk.add_constant(1.2);
    try chunk.write_chunk(OpCode.op_constant.toU8(), 123);
    try chunk.write_chunk(constant_idx, 123);

    constant_idx = try chunk.add_constant(3.2);
    try chunk.write_chunk(OpCode.op_constant.toU8(), 123);
    try chunk.write_chunk(constant_idx, 123);

    try chunk.write_chunk(OpCode.op_add.toU8(), 123);

    constant_idx = try chunk.add_constant(5.6);
    try chunk.write_chunk(OpCode.op_constant.toU8(), 123);
    try chunk.write_chunk(constant_idx, 123);

    try chunk.write_chunk(OpCode.op_div.toU8(), 123);
    try chunk.write_chunk(OpCode.op_negate.toU8(), 123);

    try chunk.write_chunk(OpCode.op_ret.toU8(), 123);

    //debug.dissasemble_chunk(&chunk, "test chunk");
    try vm.interpret(&chunk);

    return 0;
}
