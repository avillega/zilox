const std = @import("std");
const Scanner = @import("scanner.zig").Scanner;
const TokenType = @import("scanner.zig").TokenType;
const Token = @import("scanner.zig").Token;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const Obj = @import("value.zig").Obj;
const ObjString = @import("value.zig").ObjString;
const Vm = @import("vm.zig").Vm;
const dissasembleChunk = @import("debug.zig").dissasembleChunk;

const errout = std.io.getStdErr().writer();

const Parser = struct {
    vm: *Vm = undefined,
    previous: Token = undefined,
    current: Token = undefined,
    hadError: bool = false,
    panicMode: bool = false,
};

const ParseFn = fn () void;

const ParseRule = struct {
    prefix: ?ParseFn = null,
    infix: ?ParseFn = null,
    precedence: Precedence = Precedence.prec_none,
};

var scanner: Scanner = undefined;
var parser = Parser{};
var compiling_chunk: *Chunk = undefined;

pub fn compile(src: []const u8, chunk: *Chunk, vm: *Vm) !void {
    scanner = Scanner.init(src);
    parser.panicMode = false;
    parser.hadError = false;
    parser.vm = vm;
    compiling_chunk = chunk;
    advance();
    expression();
    consume(.eof, "Expect end of expression.");
    endCompiler();
    if (parser.hadError) return error.compiler_error;
}

fn advance() void {
    parser.previous = parser.current;
    while (true) {
        parser.current = scanner.nextToken();
        if (parser.current.type != .token_error) break;

        errorAtCurrent(parser.current.lexeme);
    }
}

fn expression() void {
    parsePrecedence(.prec_assignment);
}

fn number() void {
    const value = std.fmt.parseFloat(f64, parser.previous.lexeme) catch {
        std.debug.panic("Unexpected error trying to convert {s} to a number", .{parser.previous.lexeme});
    };
    emitConstant(Value{ .number = value });
}

fn string() void {
    const chars = parser.previous.lexeme[1 .. parser.previous.lexeme.len - 1];
    emitConstant(Value{ .string = ObjString.copyString(chars, parser.vm) });
}

fn literal() void {
    switch (parser.previous.type) {
        .token_true => emitOpCode(.op_true),
        .token_false => emitOpCode(.op_false),
        .nil => emitOpCode(.op_nil),
        else => unreachable,
    }
}

fn grouping() void {
    expression();
    consume(.right_paren, "Expect ')' after expression.");
}

fn unary() void {
    const operator_type = parser.previous.type;
    parsePrecedence(.prec_unary);
    switch (operator_type) {
        .minus => emitOpCode(.op_neg),
        .bang => emitOpCode(.op_not),
        else => unreachable,
    }
}

fn binary() void {
    const operator_type = parser.previous.type;
    const rule = getRule(operator_type);
    parsePrecedence(@intToEnum(Precedence, @enumToInt(rule.precedence) + 1));

    switch (operator_type) {
        .plus => emitOpCode(.op_add),
        .minus => emitOpCode(.op_sub),
        .star => emitOpCode(.op_mul),
        .slash => emitOpCode(.op_div),
        .bang_equal => emitOpCodes(.op_equal, .op_not),
        .equal_equal => emitOpCode(.op_equal),
        .greater => emitOpCode(.op_greater),
        .greater_equal => emitOpCodes(.op_less, .op_not),
        .less => emitOpCode(.op_less),
        .less_equal => emitOpCodes(.op_greater, .op_not),
        else => unreachable,
    }
}

fn parsePrecedence(precedence: Precedence) void {
    advance();
    const prefix_rule = getRule(parser.previous.type).prefix orelse {
        err("Expect expression.");
        return;
    };

    prefix_rule();

    while (@enumToInt(precedence) <= @enumToInt(getRule(parser.current.type).precedence)) {
        advance();
        const infix_rule = getRule(parser.previous.type).infix orelse unreachable;
        infix_rule();
    }
}

fn consume(ty: TokenType, msg: []const u8) void {
    if (parser.current.type == ty) {
        advance();
        return;
    }

    errorAtCurrent(msg);
}

fn endCompiler() void {
    emitOpCode(.op_ret);
    const debug_print_code = true;
    if (comptime debug_print_code) {
        if (!parser.hadError) {
            dissasembleChunk(currentChunk(), "code");
        }
    }
}

fn emitBytes(b1: u8, b2: u8) void {
    emitByte(b1);
    emitByte(b2);
}

fn emitOpCodes(code1: OpCode, code2: OpCode) void {
    emitOpCode(code1);
    emitOpCode(code2);
}

fn emitConstant(value: Value) void {
    emitBytes(@enumToInt(OpCode.op_const), makeConstant(value));
}

fn makeConstant(value: Value) u8 {
    const max_u8 = std.math.maxInt(u8);
    const constant_idx = currentChunk().addConstant(value);
    if (constant_idx > max_u8) {
        err("Too many constants in one chunk.");
        return 0;
    }

    return @intCast(u8, constant_idx);
}

fn emitByte(byte: u8) void {
    currentChunk().writeByte(byte, parser.previous.line);
}

fn emitOpCode(op_code: OpCode) void {
    currentChunk().writeOpCode(op_code, parser.previous.line);
}

fn currentChunk() *Chunk {
    return compiling_chunk;
}

fn errorAtCurrent(msg: []const u8) void {
    errorAt(parser.current, msg);
}

fn err(msg: []const u8) void {
    errorAt(parser.previous, msg);
}

fn errorAt(token: Token, msg: []const u8) void {
    if (parser.panicMode) return;
    parser.panicMode = true;
    errout.print("[line {d}] Error", .{token.line}) catch {};
    switch (token.type) {
        .eof => errout.writeAll(" at end") catch {},
        .token_error => {},
        else => errout.print(" at '{s}'", .{token.lexeme}) catch {},
    }

    errout.print(": {s}\n", .{msg}) catch {};
    parser.hadError = true;
}

const Precedence = enum {
    prec_none,
    prec_assignment, // =
    prec_or, // or
    prec_and, // and
    prec_equality, // == !=
    prec_comparison, // < > <= >=
    prec_term, // + -
    prec_factor, // * /
    prec_unary, // ! -
    prec_call, // . ()
    prec_primary,
};

fn getRule(ty: TokenType) ParseRule {
    return switch (ty) {
        .left_paren => ParseRule{ .prefix = grouping },
        .right_paren => ParseRule{},
        .minus => ParseRule{ .prefix = unary, .infix = binary, .precedence = .prec_term },
        .plus => ParseRule{ .infix = binary, .precedence = .prec_term },
        .slash => ParseRule{ .infix = binary, .precedence = .prec_factor },
        .star => ParseRule{ .infix = binary, .precedence = .prec_factor },
        .bang => ParseRule{ .prefix = unary, .precedence = .prec_unary },
        .number => ParseRule{ .prefix = number },
        .token_false => ParseRule{ .prefix = literal },
        .nil => ParseRule{ .prefix = literal },
        .token_true => ParseRule{ .prefix = literal },
        .bang_equal => ParseRule{ .infix = binary, .precedence = .prec_equality },
        .equal_equal => ParseRule{ .infix = binary, .precedence = .prec_equality },
        .greater => ParseRule{ .infix = binary, .precedence = .prec_comparison },
        .greater_equal => ParseRule{ .infix = binary, .precedence = .prec_comparison },
        .less => ParseRule{ .infix = binary, .precedence = .prec_comparison },
        .less_equal => ParseRule{ .infix = binary, .precedence = .prec_comparison },
        .string => ParseRule{ .prefix = string },
        .eof => ParseRule{},
        else => std.debug.panic("there is no rule for token {}\n", .{ty}),
    };
}
