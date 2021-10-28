const std = @import("std");
const Allocator = std.mem.Allocator;
const scanner_mod = @import("./scanner.zig");
const chunks_mod = @import("./chunks.zig");
const debug_mod = @import("./debug.zig");
const Scanner = scanner_mod.Scanner;
const Token = scanner_mod.Token;
const TokenType = scanner_mod.TokenType;
const Chunk = chunks_mod.Chunk;
const OpCode = chunks_mod.OpCode;
const Value = @import("./value.zig").Value;
const Obj = @import("./object.zig").Obj;
const Vm = @import("vm.zig").Vm;

const debug_print_code = true;

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

pub fn compile(source: []const u8, chunk: *Chunk, vm: *Vm) !void {
    var scanner = Scanner.init(source);
    var parser = Parser.init(&scanner, chunk, vm);
    try parser.advance();
    while (scanner.hasNextToken()) {
        try parser.declaration();
    }
    parser.endCompiler();
}

const ParseFn = fn (parser: *Parser, can_assign: bool) anyerror!void;

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
        .BANG => ParseRule.init(Parser.unary, null, .precNone),
        .BANG_EQUAL => ParseRule.init(null, Parser.binary, .precEquality),
        .EQUAL => ParseRule.init(null, null, .precNone),
        .EQUAL_EQUAL => ParseRule.init(null, Parser.binary, .precEquality),
        .GREATER => ParseRule.init(null, Parser.binary, .precComparison),
        .GREATER_EQUAL => ParseRule.init(null, Parser.binary, .precComparison),
        .LESS => ParseRule.init(null, Parser.binary, .precComparison),
        .LESS_EQUAL => ParseRule.init(null, Parser.binary, .precComparison),
        .IDENTIFIER => ParseRule.init(Parser.variable, null, .precNone),
        .STRING => ParseRule.init(Parser.string, null, .precNone),
        .NUMBER => ParseRule.init(Parser.number, null, .precNone),
        .AND => ParseRule.init(null, null, .precNone),
        .CLASS => ParseRule.init(null, null, .precNone),
        .ELSE => ParseRule.init(null, null, .precNone),
        .FALSE => ParseRule.init(Parser.literal, null, .precNone),
        .FOR => ParseRule.init(null, null, .precNone),
        .FUN => ParseRule.init(null, null, .precNone),
        .IF => ParseRule.init(null, null, .precNone),
        .NIL => ParseRule.init(Parser.literal, null, .precNone),
        .OR => ParseRule.init(null, null, .precNone),
        .PRINT => ParseRule.init(null, null, .precNone),
        .RETURN => ParseRule.init(null, null, .precNone),
        .SUPER => ParseRule.init(null, null, .precNone),
        .THIS => ParseRule.init(null, null, .precNone),
        .TRUE => ParseRule.init(Parser.literal, null, .precNone),
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
    vm: *Vm,

    pub fn init(scanner: *Scanner, chunk: *Chunk, vm: *Vm) Parser {
        return .{
            .scanner = scanner,
            .compilingChunk = chunk,
            .vm = vm,
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

    pub fn consume(self: *Self, ty: TokenType, comptime message: []const u8) CompileError!void {
        if (self.current.ty == ty) {
            try self.advance();
            return;
        }
        self.errAtCurrent(message);
        return CompileError.CompileError;
    }

    fn match(self: *Self, ty: TokenType) !bool {
        if (!self.check(ty)) return false;
        try self.advance();
        return true;
    }

    fn check(self: *Self, ty: TokenType) bool {
        return self.current.ty == ty;
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
    pub fn declaration(self: *Self) !void {
        if (try self.match(.VAR)) {
            try self.varDeclaration();
        } else {
            try self.statement();
        }

        if (self.panicMode) try self.synchronize();
    }

    fn statement(self: *Self) !void {
        if (try self.match(.PRINT)) {
            try self.printStatement();
        } else {
            try self.expressionStatement();
        }
    }

    fn printStatement(self: *Self) !void {
        try self.expression();
        try self.consume(.SEMICOLON, "Expect ';' after value");
        self.emitOpCode(.op_print);
    }

    fn varDeclaration(self: *Self) !void {
        const global = try self.parseVariable("Expect variable name.");

        if (try self.match(.EQUAL)) {
            try self.expression();
        } else {
            self.emitOpCode(.op_nil);
        }
        try self.consume(.SEMICOLON, "Expect ';' after varaible declaration.");
        try self.defineVariable(global);
    }

    fn parseVariable(self: *Self, comptime error_message: []const u8) !u8 {
        try self.consume(.IDENTIFIER, error_message);
        return self.identifierConstant(self.previous);
    }

    fn identifierConstant(self: *Self, token: Token) !u8 {
        return try self.makeConstant(Obj.String.copy(self.vm, token.lexeme).obj.toValue());
    }

    fn defineVariable(self: *Self, global: u8) !void {
        self.emitBytes(OpCode.op_define_gloabl.toU8(), global);
    }

    fn expressionStatement(self: *Self) !void {
        try self.expression();
        try self.consume(.SEMICOLON, "Expect ; after expression");
        self.emitOpCode(.op_pop);
    }

    fn synchronize(self: *Self) !void {
        self.panicMode = false;

        while (self.scanner.hasNextToken()) {
            if (self.previous.ty == .SEMICOLON) return;
            switch (self.current.ty) {
                .CLASS, .FUN, .VAR, .FOR, .IF, .WHILE, .PRINT, .RETURN => return,
                else => try self.advance(),
            }
        }
    }

    fn expression(self: *Self) !void {
        try self.parsePrecendece(.precAssignment);
    }

    fn number(self: *Self, can_assign: bool) !void {
        _ = can_assign;
        const x = std.fmt.parseFloat(f64, self.previous.lexeme) catch unreachable;
        try self.emitConstant(Value.fromNumber(x));
    }

    fn string(self: *Self, can_assign: bool) !void {
        _ = can_assign;
        const lexeme_len = self.previous.lexeme.len;
        const str = Obj.String.copy(self.vm, self.previous.lexeme[1 .. lexeme_len - 1]);
        try self.emitConstant(str.obj.toValue());
    }

    fn variable(self: *Self, can_assign: bool) !void {
        try self.namedVariable(self.previous, can_assign);
    }

    fn namedVariable(self: *Self, name: Token, can_assign: bool) !void {
        const arg = try self.identifierConstant(name);
        if (can_assign and try self.match(.EQUAL)) {
            try self.expression();
            self.emitBytes(OpCode.op_set_global.toU8(), arg);
        } else {
            self.emitBytes(OpCode.op_get_global.toU8(), arg);
        }
    }

    fn grouping(self: *Self, can_assign: bool) !void {
        _ = can_assign;
        try self.expression();
        try self.consume(.RIGHT_PAREN, "Expect ')' after expression.");
    }

    fn unary(self: *Self, can_assign: bool) !void {
        _ = can_assign;
        const operatorType = self.previous.ty;
        try self.parsePrecendece(.precUnary);
        switch (operatorType) {
            .BANG => self.emitOpCode(.op_not),
            .MINUS => self.emitOpCode(.op_negate),
            else => unreachable,
        }
    }

    fn binary(self: *Self, can_assign: bool) !void {
        _ = can_assign;
        const operatorType = self.previous.ty;
        const rule = getRule(operatorType);
        try self.parsePrecendece(@intToEnum(Precedence, @enumToInt(rule.precedence) + 1));

        switch (operatorType) {
            .EQUAL_EQUAL => self.emitOpCode(.op_equal),
            .GREATER => self.emitOpCode(.op_greater),
            .LESS => self.emitOpCode(.op_less),
            .BANG_EQUAL => self.emitTwoOpCodes(.op_equal, .op_not),
            .GREATER_EQUAL => self.emitTwoOpCodes(.op_less, .op_not),
            .LESS_EQUAL => self.emitTwoOpCodes(.op_greater, .op_not),
            .PLUS => self.emitOpCode(.op_add),
            .MINUS => self.emitOpCode(.op_sub),
            .STAR => self.emitOpCode(.op_mul),
            .SLASH => self.emitOpCode(.op_div),
            else => unreachable,
        }
    }

    fn literal(self: *Self, can_assign: bool) !void {
        _ = can_assign;
        switch (self.previous.ty) {
            .FALSE => self.emitOpCode(.op_false),
            .NIL => self.emitOpCode(.op_nil),
            .TRUE => self.emitOpCode(.op_true),
            else => unreachable,
        }
    }

    fn parsePrecendece(self: *Self, precedence: Precedence) !void {
        try self.advance();
        const prefix_rule = getRule(self.previous.ty).prefix orelse {
            self.err("Expect expression.");
            return CompileError.CompileError;
        };

        const can_assign = @enumToInt(precedence) <= @enumToInt(Precedence.precAssignment);
        try prefix_rule(self, can_assign);

        while (@enumToInt(precedence) <= @enumToInt(getRule(self.current.ty).precedence)) {
            try self.advance();
            const rule = getRule(self.previous.ty);
            const infix_rule = rule.infix orelse {
                self.err("Unreachable????");
                return CompileError.CompileError;
            };

            try infix_rule(self, can_assign);
        }

        if (can_assign and try self.match(.EQUAL)) {
            self.err("Invalid assignment target.");
        }
    }

    fn emitConstant(self: *Self, value: Value) !void {
        const const_idx = try self.makeConstant(value);
        self.emitBytes(OpCode.op_constant.toU8(), const_idx);
    }

    fn makeConstant(self: *Self, value: Value) !u8 {
        const constant = self.currentChunk().addConstant(value);
        if (constant > std.math.maxInt(u8)) {
            self.err("Too many constants in a chunk.");
            return CompileError.TooManyConstants;
        }

        return @intCast(u8, constant);
    }

    fn emitTwoOpCodes(self: *Self, op_code1: OpCode, op_code2: OpCode) void {
        self.emitBytes(op_code1.toU8(), op_code2.toU8());
    }

    fn emitOpCode(self: *Self, opCode: OpCode) void {
        self.emitByte(opCode.toU8());
    }

    fn emitByte(self: *Self, byte: u8) void {
        self.currentChunk().write(byte, self.previous.line);
    }

    fn emitBytes(self: *Self, byte1: u8, byte2: u8) void {
        self.emitByte(byte1);
        self.emitByte(byte2);
    }

    pub fn endCompiler(self: *Self) void {
        self.emitReturn();
        if (comptime debug_print_code) {
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
fn scannerTest(source: []const u8) void {
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
