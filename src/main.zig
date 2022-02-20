const std = @import("std");
const Allocator = std.mem.Allocator;
const chunk_mod = @import("./chunk.zig");
const dbg = @import("./debug.zig");
const Vm = @import("./vm.zig").Vm;
const InterpretError = @import("./vm.zig").InterpretError;
const Chunk = chunk_mod.Chunk;

const errout = std.io.getStdErr().writer();
const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

pub fn main() anyerror!u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var vm = Vm.init(allocator);
    defer vm.deinit();

    switch (args.len) {
        1 => repl(&vm),
        2 => runFile(args[1], &vm, allocator),
        else => {
            errout.print("Usage: zilox [path]\n", .{}) catch {};
            std.process.exit(64);
        },
    }
    return 0;
}

fn repl(vm: *Vm) void {
    var buf = std.io.bufferedReader(stdin);
    var reader = buf.reader();
    var line_buf: [1024]u8 = undefined;

    while (true) {
        stdout.writeAll("> ") catch std.debug.panic("Couldn't write to stdout you have serious problems", .{});
        var line = reader.readUntilDelimiterOrEof(&line_buf, '\n') catch {
            std.debug.panic("Couldn't read from stdin in repl you have serious problems", .{});
        } orelse {
            stdout.writeAll("\n") catch std.debug.panic("Couldn't write to stdout you have serious problems", .{});
            break;
        };

        vm.interpret(line) catch {};
    }
}

fn runFile(fileName: []const u8, vm: *Vm, allocator: Allocator) void {
    const source = readFile(fileName, allocator);
    defer allocator.free(source);

    vm.interpret(source) catch |e| {
        switch (e) {
            InterpretError.runtime_error => std.process.exit(70),
            InterpretError.compile_error => std.process.exit(65),
        }
    };
}

fn readFile(path: []const u8, allocator: Allocator) []const u8 {
    const file = std.fs.cwd().openFile(path, .{ .read = true }) catch |err| {
        errout.print("Could not open file \"{s}\", error: {any}.\n", .{ path, err }) catch {};
        std.process.exit(74);
    };
    defer file.close();

    return file.readToEndAlloc(allocator, 100_000_000) catch |err| {
        errout.print("Could not read file \"{s}\", error: {any}.\n", .{ path, err }) catch {};
        std.process.exit(74);
    };
}
