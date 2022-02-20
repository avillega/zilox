const std = @import("std");
const expect = std.testing.expect;

test "many pointers" {
    var h : []const u8 = "Hello World";
    var b : [*]const u8 = h.ptr;
    var c : [*]const u8 = h.ptr;

    b += 1;
    c += 4;

    const start = @ptrToInt(b);
    const end = @ptrToInt(c);
    std.debug.print("{s} b:{*} c:{*} {s} {c} {}\n", .{h, b, c, b[0..(end-start)], c[0], end - start});
    try expect(true);
    
}