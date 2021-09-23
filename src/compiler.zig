const std = @import("std");
const scanner_mod = @import("./scanner.zig");
const chunks_mod = @import("./chunks.zig");
const debug_mod = @import("./debug.zig");
const Scanner = scanner_mod.Scanner;
const Token = scanner_mod.Token;
const TokenType = scanner_mod.TokenType;
const Chunk = chunks_mod.Chunk;
const OpCode = chunks_mod.OpCode;
const Value = @import("./value.zig").Value;

const DEBUG_PRINT_CODE = true;

const CompileError = error{ CompileError, TooManyConstants };

const Precedence = enum {
    precNone,
    precAssignment, // =
    precOr, // or
    precAnd, // and
    precEquality, // == !=
    precComparison, // < > <= >=
    precTerm, // + -
    precFactor, // * /
    precUnary, // ! -
    precCall, // . ()
    precPrimary,
};

pub fn compile(source: []const u8, chunk: *Chunk) !void {
    var scanner = Scanner.init(source);
    var parser = Parser.init(&scanner, chunk);
    try parser.advance();
    try parser.expression();

    // Make sure there are no tokens left. if there are, it is an error.
    if (scanner.nextToken()) |_| {
        parser.errAtCurrent("Expect end of expression.");
        return CompileError.CompileError;
    }
    parser.endCompiler();
}

const ParseFn = fn (parser: *Parser) anyerror!void;

const ParseRule = struct {
    prefix: ?ParseFn,
    infix: ?ParseFn,
    precedence: Precedence,

    pub fn init(prefix: ?ParseFn, infix: ?ParseFn, precedence: Precedence) ParseRule {
        return .{
            .prefix = prefix,
            .infix = infix,
            .precedence = precedence,
        };
    }
};

fn getRule(ty: TokenType) ParseRule {
    return switch (ty) {
        .LEFT_PAREN => ParseRule.init(Parser.grouping, null, .precNone),
        .RIGHT_PAREN => ParseRule.init(null, null, .precNone),
        .LEFT_BRACE => ParseRule.init(null, null, .precNone),
        .RIGHT_BRACE => ParseRule.init(null, null, .precNone),
        .COMMA => ParseRule.init(null, null, .precNone),
        .DOT => ParseRule.init(null, null, .precNone),
        .MINUS => ParseRule.init(Parser.unary, Parser.binary, .precTerm),
        .PLUS => ParseRule.init(null, Parser.binary, .precTerm),
        .SEMICOLON => ParseRule.init(null, null, .precNone),
        .SLASH => ParseRule.init(null, Parser.binary, .precFactor),
        .STAR => ParseRule.init(null, Parser.binary, .precFactor),
        .BANG => ParseRule.init(null, null, .precNone),
        .BANG_EQUAL => ParseRule.init(null, null, .precNone),
        .EQUAL => ParseRule.init(null, null, .precNone),
        .EQUAL_EQUAL => ParseRule.init(null, null, .precNone),
        .GREATER => ParseRule.init(null, null, .precNone),
        .GREATER_EQUAL => ParseRule.init(null, null, .precNone),
        .LESS => ParseRule.init(null, null, .precNone),
        .LESS_EQUAL => ParseRule.init(null, null, .precNone),
        .IDENTIFIER => ParseRule.init(null, null, .precNone),
        .STRING => ParseRule.init(null, null, .precNone),
        .NUMBER => ParseRule.init(Parser.number, null, .precNone),
        .AND => ParseRule.init(null, null, .precNone),
        .CLASS => ParseRule.init(null, null, .precNone),
        .ELSE => ParseRule.init(null, null, .precNone),
        .FALSE => ParseRule.init(null, null, .precNone),
        .FOR => ParseRule.init(null, null, .precNone),
        .FUN => ParseRule.init(null, null, .precNone),
        .IF => ParseRule.init(null, null, .precNone),
        .NIL => ParseRule.init(null, null, .precNone),
        .OR => ParseRule.init(null, null, .precNone),
        .PRINT => ParseRule.init(null, null, .precNone),
        .RETURN => ParseRule.init(null, null, .precNone),
        .SUPER => ParseRule.init(null, null, .precNone),
        .THIS => ParseRule.init(null, null, .precNone),
        .TRUE => ParseRule.init(null, null, .precNone),
        .VAR => ParseRule.init(null, null, .precNone),
        .WHILE => ParseRule.init(null, null, .precNone),
        .ERROR => ParseRule.init(null, null, .precNone),
        .EOF => ParseRule.init(null, null, .precNone),
    };
}

const Parser = struct {
    const Self = @This();
    current: Token = undefined,
    previous: Token = undefined,
    panicMode: bool = false,
    scanner: *Scanner,
    compilingChunk: *Chunk,

    pub fn init(scanner: *Scanner, chunk: *Chunk) Parser {
        return .{
            .scanner = scanner,
            .compilingChunk = chunk,
        };
    }

    pub fn advance(self: *Self) CompileError!void {
        self.previous = self.current;
        while (self.scanner.nextToken()) |token| {
            self.current = token;
            if (token.ty != .ERROR) break;

            self.errAtCurrent(self.current.lexeme);
            return CompileError.CompileError;
        }
    }

    pub fn consume(self: *Self, ty: TokenType, message: []const u8) CompileError!void {
        if (self.current.ty == ty) {
            try self.advance();
            return;
        }
        self.errAtCurrent(message);
        return CompileError.CompileError;
    }

    fn errAtCurrent(self: *Self, message: []const u8) void {
        self.errAt(self.current, message);
    }

    fn err(self: *Self, message: []const u8) void {
        self.errAt(self.previous, message);
    }

    fn errAt(self: *Self, token: Token, message: []const u8) void {
        if (self.panicMode) return;

        self.panicMode = true;
        const errWriter = std.io.getStdErr().writer();
        errWriter.print("[line {d}] Error", .{token.line}) catch unreachable;

        switch (token.ty) {
            .EOF => errWriter.writeAll(" at end") catch unreachable,
            .ERROR => {},
            else => errWriter.print(" at '{s}'", .{token.lexeme}) catch unreachable,
        }
        errWriter.print(": {s}\n", .{message}) catch unreachable;
    }

    // Compiler Bakckend
    fn expression(self: *Self) !void {
        try self.parsePrecendece(.precAssignment);
    }

    fn number(self: *Self) !void {
        const value = std.fmt.parseFloat(f64, self.previous.lexeme) catch unreachable;
        try self.emitConstant(value);
    }

    fn grouping(self: *Self) !void {
        try self.expression();
        try self.consume(.RIGHT_PAREN, "Expect ')' after expression.");
    }

    fn unary(self: *Self) !void {
        const operatorType = self.previous.ty;
        try self.parsePrecendece(.precUnary);
        switch (operatorType) {
            .MINUS => self.emitByte(OpCode.op_negate.toU8()),
            else => unreachable,
        }
    }

    fn binary(self: *Self) !void {
        const operatorType = self.previous.ty;
        const rule = getRule(operatorType);
        try self.parsePrecendece(@intToEnum(Precedence, @enumToInt(rule.precedence) + 1));

        switch (operatorType) {
            .PLUS => self.emitByte(OpCode.op_add.toU8()),
            .MINUS => self.emitByte(OpCode.op_sub.toU8()),
            .STAR => self.emitByte(OpCode.op_mul.toU8()),
            .SLASH => self.emitByte(OpCode.op_div.toU8()),
            else => unreachable,
        }
    }

    fn parsePrecendece(self: *Self, precedence: Precedence) !void {
        try self.advance();
        const prefixRule = getRule(self.previous.ty).prefix orelse {
            self.err("Expect expression.");
            return CompileError.CompileError;
        };

        try prefixRule(self);

        while (@enumToInt(precedence) <= @enumToInt(getRule(self.current.ty).precedence)) {
            try self.advance();
            const rule = getRule(self.previous.ty);
            std.debug.print("{any}\n", .{rule});
            const infixRule = rule.infix orelse {
                self.err("Unreachable????");
                return CompileError.CompileError;
            };

            try infixRule(self);
        }
    }

    fn emitConstant(self: *Self, value: Value) !void {
        const constIdx = try self.makeConstant(value);
        self.emitBytes(OpCode.op_constant.toU8(), constIdx);
    }

    fn makeConstant(self: *Self, value: Value) !u8 {
        const constant = self.currentChunk().addConstant(value) catch {
            self.err("Err adding constant.");
            return CompileError.CompileError;
        };
        if (constant > std.math.maxInt(u8)) {
            self.err("Too many constants in a chunk.");
            return CompileError.TooManyConstants;
        }

        return @intCast(u8, constant);
    }

    pub fn emitByte(self: *Self, byte: u8) void {
        self.currentChunk().write(byte, self.previous.line) catch |err| {
            std.log.err("Error {any} trying to emit byte", .{err});
            std.process.exit(1);
        };
    }

    pub fn emitBytes(self: *Self, byte1: u8, byte2: u8) void {
        self.emitByte(byte1);
        self.emitByte(byte2);
    }

    pub fn endCompiler(self: *Self) void {
        self.emitReturn();
        if (comptime DEBUG_PRINT_CODE) {
            debug_mod.dissasembleChunk(self.currentChunk(), "code");
        }
    }

    fn emitReturn(self: *Self) void {
        self.emitByte(OpCode.op_ret.toU8());
    }

    fn currentChunk(self: *Self) *Chunk {
        return self.compilingChunk;
    }
};

// Function used to test the scanner.
fn scanner_test(source: []const u8) void {
    var scanner = Scanner.init(source);
    var line: u64 = 0;

    while (scanner.nextToken()) |token| {
        if (token.line != line) {
            std.debug.print("{d: >4} ", .{token.line});
            line = token.line;
        } else {
            std.debug.print("   | ", .{});
        }
        std.debug.print(".{s: <13} '{s}'\n", .{ @tagName(token.ty), token.lexeme });
    }
}
