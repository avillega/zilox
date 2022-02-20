const std = @import("std");
const process = std.process;
const chunks = @import("./chunks.zig");
const debug = @import("./debug.zig");
const Allocator = std.mem.Allocator;
const OpCode = chunks.OpCode;
const Chunk = chunks.Chunk;
const Vm = @import("./vm.zig").Vm;

pub fn main() anyerror!u8 {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = general_purpose_allocator.deinit();
    const gpa = &general_purpose_allocator.allocator;

    const args = try process.argsAlloc(gpa);
    defer process.argsFree(gpa, args);

    var vm = Vm.init(gpa);
    defer vm.deinit();

    switch (args.len) {
        1 => try repl(&vm),
        2 => runFile(args[1], &vm, gpa),
        else => {
            std.log.err("Usage: zilox [path]\n", .{});
            process.exit(64);
        },
    }

    return 0;
}

fn repl(vm: *Vm) !void {
    const in = std.io.getStdIn();
    const stdout = std.io.getStdOut();

    var buf = std.io.bufferedReader(in.reader());
    var reader = buf.reader();
    var line_buf: [1024]u8 = undefined;

    while (true) {
        try stdout.writeAll("> ");
        var line = (try reader.readUntilDelimiterOrEof(&line_buf, '\n')) orelse {
            try stdout.writeAll("\n");
            break;
        };

        vm.interpret(line) catch {};
    }
}

fn runFile(fileName: []const u8, vm: *Vm, allocator: *Allocator) void {
    const source = readFile(fileName, allocator);
    defer allocator.free(source);

    vm.interpret(source) catch {};
}

fn readFile(path: []const u8, allocator: *Allocator) []const u8 {
    const file = std.fs.cwd().openFile(
        path,
        .{ .read = true },
    ) catch |err| {
        std.log.err("Could not open file \"{s}\", error: {any}.\n", .{ path, err });
        process.exit(74);
    };
    defer file.close();

    return file.readToEndAlloc(allocator, 100_000_000) catch |err| {
        std.log.err("Could not read file \"{s}\", error: {any}.\n", .{ path, err });
        process.exit(74);
    };
}
