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
const u8_count = std.math.maxInt(u8) + 1;

const CompileCtx = struct {
    can_assign: bool,
};

const Parser = struct {
    vm: *Vm = undefined,
    previous: Token = undefined,
    current: Token = undefined,
    hadError: bool = false,
    panicMode: bool = false,
};

const Compiler = struct {
    locals: [u8_count]Local = undefined,
    local_count: u8 = 0,
    scope_depth: u32 = 0,

    pub fn init() Compiler {
        return .{};
    }
};

const Local = struct {
    name: Token,
    depth: ?u32,
};

const ParseFn = fn (CompileCtx) void;

const ParseRule = struct {
    prefix: ?ParseFn = null,
    infix: ?ParseFn = null,
    precedence: Precedence = Precedence.prec_none,
};

var scanner: Scanner = undefined;
var parser = Parser{};
var compiling_chunk: *Chunk = undefined;
var current: *Compiler = undefined;

pub fn compile(src: []const u8, chunk: *Chunk, vm: *Vm) !void {
    scanner = Scanner.init(src);
    parser.panicMode = false;
    parser.hadError = false;
    parser.vm = vm;
    compiling_chunk = chunk;
    current = &Compiler.init();
    advance();
    while (!match(.eof)) {
        declaration();
    }
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

fn declaration() void {
    if (match(.token_var)) {
        varDeclaration();
    } else {
        statement();
    }

    if (parser.panicMode) synchronize();
}

fn varDeclaration() void {
    const global = parseVariable("Expect variable name.");
    if (match(.equal)) {
        expression();
    } else {
        emitOpCode(.op_nil);
    }
    consume(.semicolon, "Expect ';' after variable declaration");
    defineVariable(global);
}

fn parseVariable(error_msg: []const u8) u8 {
    consume(.identifier, error_msg);

    declareVar();
    if (current.scope_depth > 0) return 0;

    return identifierConstant(parser.previous);
}

fn identifierConstant(name: Token) u8 {
    return makeConstant(Value{ .string = ObjString.copyString(name.lexeme, parser.vm) });
}

fn resolveLocal(compiler: *Compiler, name: Token) ?u8 {
    var i: usize = compiler.local_count;
    while (i > 0)  {
        i -= 1;
        const local = compiler.locals[i];
        if (std.mem.eql(u8, name.lexeme, local.name.lexeme)) {
            if (local.depth == null) {
                err("Can't read local variable in its own initializer.");
            }
            return @intCast(u8, i);
        }
    }

    return null;
}

fn defineVariable(global: u8) void {
    if (current.scope_depth > 0) {
        markInitialized();
        return;
    } 

    emitBytes(@enumToInt(OpCode.op_define_global), global);
}

fn markInitialized() void {
    current.locals[current.local_count - 1].depth = current.scope_depth;
}

fn declareVar() void {
    if (current.scope_depth == 0) return;
    const name = parser.previous;
    var idx: usize = current.local_count;
    while (idx > 0) {
        idx -= 1;
        const local = current.locals[idx];
        if (local.depth != null and local.depth.? < current.scope_depth) break;

        if (std.mem.eql(u8, name.lexeme, local.name.lexeme)) {
            err("Already a variable with this name in scope.");
        }
    }
    addLocal(name);
}

fn addLocal(name: Token) void {
    if (current.local_count == u8_count) {
        err("Too may local variables in function.");
        return;
    }
    current.locals[current.local_count] = Local{ .name = name, .depth = null };
    current.local_count += 1;
}

fn statement() void {
    if (match(.print)) {
        printStmt();
    } else if (match(.left_brace)) {
        beginScope();
        block();
        endScope();
    } else if (match(.token_if)) {
        ifStmt();
    } else {
        expressionStmt();
    }
}

fn ifStmt() void {
    consume(.left_paren, "Expect '(' after if.");
    expression();
    consume(.right_paren, "Expect ')' after if.");

    const thenJmp = emitJmp(.op_jmp_if_false);
    emitOpCode(.op_pop);
    statement();

    const elseJmp = emitJmp(.op_jmp);

    patchJmp(thenJmp);
    emitOpCode(.op_pop);

    if (match(.token_else)) statement();
    patchJmp(elseJmp);
}

fn patchJmp(addr: usize) void {
    const jmp = currentChunk().code.items.len - addr - 2;

    if (jmp > std.math.maxInt(u16)) {
        err("Too much code to jump over.");
    }

    currentChunk().code.items[addr] = @truncate(u8, jmp >> 8) & 0xff;
    currentChunk().code.items[addr+1] = @truncate(u8, jmp) & 0xff;
}

fn beginScope() void {
    current.scope_depth += 1;
}

fn block() void {
    while (!check(.right_brace) and !check(.eof)) {
        declaration();
    }

    consume(.right_brace, "Expect '}' after block.");
}

fn endScope() void {
    current.scope_depth -= 1;
    while (current.local_count > 0 and
        current.locals[current.local_count - 1].depth.? > current.scope_depth)
    {
        emitOpCode(.op_pop);
        current.local_count -= 1;
    }
}

fn printStmt() void {
    expression();
    consume(.semicolon, "Expect ';' after value.");
    emitOpCode(.op_print);
}

fn expressionStmt() void {
    expression();
    consume(.semicolon, "Expect ';' after value.");
    emitOpCode(.op_pop);
}

fn synchronize() void {
    parser.panicMode = false;

    while (parser.current.type != .eof) {
        if (parser.previous.type == .semicolon) return;
        switch (parser.current.type) {
            .class,
            .fun,
            .token_var,
            .token_for,
            .token_while,
            .token_if,
            .print,
            .token_return,
            => return,
            else => {},
        }
        advance();
    }
}

fn variable(ctx: CompileCtx) void {
    namedVariable(parser.previous, ctx);
}

fn namedVariable(name: Token, ctx: CompileCtx) void {
    var get_op: OpCode = undefined;
    var set_op: OpCode = undefined;

    var arg: ?u8 = resolveLocal(current, name);
    if (arg) |_| {
        get_op = .op_get_local;
        set_op = .op_set_local;
    } else {
        arg = identifierConstant(name);
        get_op = .op_get_global;
        set_op = .op_set_global;
    }

    if (ctx.can_assign and match(.equal)) {
        expression();
        emitBytes(@enumToInt(set_op), arg.?);
    } else {
        emitBytes(@enumToInt(get_op), arg.?);
    }
}

fn number(ctx: CompileCtx) void {
    _ = ctx;
    const value = std.fmt.parseFloat(f64, parser.previous.lexeme) catch {
        std.debug.panic("Unexpected error trying to convert {s} to a number", .{parser.previous.lexeme});
    };
    emitConstant(Value{ .number = value });
}

fn string(ctx: CompileCtx) void {
    _ = ctx;
    const chars = parser.previous.lexeme[1 .. parser.previous.lexeme.len - 1];
    emitConstant(Value{ .string = ObjString.copyString(chars, parser.vm) });
}

fn literal(ctx: CompileCtx) void {
    _ = ctx;
    switch (parser.previous.type) {
        .token_true => emitOpCode(.op_true),
        .token_false => emitOpCode(.op_false),
        .nil => emitOpCode(.op_nil),
        else => unreachable,
    }
}

fn grouping(ctx: CompileCtx) void {
    _ = ctx;
    expression();
    consume(.right_paren, "Expect ')' after expression.");
}

fn unary(ctx: CompileCtx) void {
    _ = ctx;
    const operator_type = parser.previous.type;
    parsePrecedence(.prec_unary);
    switch (operator_type) {
        .minus => emitOpCode(.op_neg),
        .bang => emitOpCode(.op_not),
        else => unreachable,
    }
}

fn binary(ctx: CompileCtx) void {
    _ = ctx;
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

    const can_assign = @enumToInt(precedence) <= @enumToInt(Precedence.prec_assignment);
    prefix_rule(.{ .can_assign = can_assign });

    while (@enumToInt(precedence) <= @enumToInt(getRule(parser.current.type).precedence)) {
        advance();
        const infix_rule = getRule(parser.previous.type).infix orelse unreachable;
        infix_rule(.{ .can_assign = can_assign });
    }

    if (can_assign and match(.equal)) {
        err("Invalid assignment target.");
    }
}

fn consume(ty: TokenType, msg: []const u8) void {
    if (parser.current.type == ty) {
        advance();
        return;
    }

    errorAtCurrent(msg);
}

fn match(ty: TokenType) bool {
    if (!check(ty)) return false;
    advance();
    return true;
}

fn check(ty: TokenType) bool {
    return parser.current.type == ty;
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

fn emitJmp(jmp: OpCode) usize {
    emitOpCode(jmp);
    emitBytes(0xff, 0xff);
    return currentChunk().code.items.len - 2;
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
        .print => ParseRule{},
        .semicolon => ParseRule{},
        .token_var => ParseRule{},
        .identifier => ParseRule{ .prefix = variable },
        .equal => ParseRule{},
        .token_if => ParseRule{},
        .token_else => ParseRule{},
        .eof => ParseRule{},
        else => std.debug.panic("there is no rule for token {}\n", .{ty}),
    };
}
