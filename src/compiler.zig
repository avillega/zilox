const std = @import("std");
const scanner_ = @import("./scanner.zig");
const Scanner = scanner_.Scanner;

pub fn compile(source: []const u8) void {
    var scanner = Scanner.init(source);
    var line: u64 = 0;

    while (scanner.nextToken()) |token| {
        if (token.line != line) {
            std.debug.print("{d: >4} ", .{token.line});
            line = token.line;
        } else {
            std.debug.print("   | ", .{});
        }
        std.debug.print(".{s: <13} '{s}'\n", .{@tagName(token.ty), token.lexeme});
    }
}
