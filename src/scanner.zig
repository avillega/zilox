const std = @import("std");

pub const Scanner = struct {
    const Self = @This();
    src: []const u8,
    start: [*]const u8,
    current: [*]const u8,
    line: u32 = 1,

    pub fn init(src: []const u8) Self {
        return Scanner{
            .src = src,
            .start = src.ptr,
            .current = src.ptr,
        };
    }

    pub fn nextToken(self: *Self) Token {
        self.skipWhitespace();
        self.start = self.current;

        if (self.isAtEnd()) return self.makeToken(.eof);

        const c = self.advance();
        if (isAlpha(c)) return self.identifier();
        switch (c) {
            ')' => return self.makeToken(.right_paren),
            '(' => return self.makeToken(.left_paren),
            '{' => return self.makeToken(.left_brace),
            '}' => return self.makeToken(.right_brace),
            ';' => return self.makeToken(.semicolon),
            ',' => return self.makeToken(.comma),
            '.' => return self.makeToken(.dot),
            '-' => return self.makeToken(.minus),
            '+' => return self.makeToken(.plus),
            '/' => return self.makeToken(.slash),
            '*' => return self.makeToken(.star),
            '!' => return if (self.match('=')) self.makeToken(.bang_equal) else self.makeToken(.bang),
            '=' => return if (self.match('=')) self.makeToken(.equal_equal) else self.makeToken(.equal),
            '<' => return if (self.match('=')) self.makeToken(.less_equal) else self.makeToken(.less),
            '>' => return if (self.match('=')) self.makeToken(.greater_equal) else self.makeToken(.greater),
            '"' => return self.string(),
            '0'...'9' => return self.number(),
            else => {},
        }

        return self.errorToken("Unexpected character!");
    }

    inline fn advance(self: *Self) u8 {
        defer self.current += 1;
        return self.current[0];
    }

    inline fn match(self: *Self, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.current[0] != expected) return false;
        self.current += 1;
        return true;
    }

    fn isAtEnd(self: *Self) bool {
        const curr_len = @ptrToInt(self.current) - @ptrToInt(self.src.ptr);
        return curr_len >= self.src.len;
    }

    fn skipWhitespace(self: *Self) void {
        while (true) {
            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => _ = self.advance(),
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        while (self.peek() != '\n' and !self.isAtEnd()) _ = self.advance();
                    }
                },
                else => return,
            }
        }
    }

    fn number(self: *Self) Token {
        while (isDigit(self.peek())) _ = self.advance();
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            _ = self.advance();
            while (isDigit(self.peek())) _ = self.advance();
        }
        return self.makeToken(.number);
    }

    fn identifier(self: *Self) Token {
        while (isDigit(self.peek()) or isAlpha(self.peek())) _ = self.advance();
        return self.makeToken(self.identifierType());
    }

    fn identifierType(self: *Self) TokenType {
        const eql = std.mem.eql;
        const lexeme = self.currentLexeme();
        if (eql(u8, lexeme, "and")) return .token_and;
        if (eql(u8, lexeme, "class")) return .class;
        if (eql(u8, lexeme, "else")) return .token_else;
        if (eql(u8, lexeme, "false")) return .token_false;
        if (eql(u8, lexeme, "for")) return .token_for;
        if (eql(u8, lexeme, "fun")) return .fun;
        if (eql(u8, lexeme, "if")) return .token_if;
        if (eql(u8, lexeme, "nil")) return .nil;
        if (eql(u8, lexeme, "or")) return .token_or;
        if (eql(u8, lexeme, "print")) return .print;
        if (eql(u8, lexeme, "return")) return .token_return;
        if (eql(u8, lexeme, "super")) return .super;
        if (eql(u8, lexeme, "this")) return .this;
        if (eql(u8, lexeme, "true")) return .token_true;
        if (eql(u8, lexeme, "var")) return .token_var;
        if (eql(u8, lexeme, "while")) return .token_while;

        return .identifier;
    } 

    fn string(self: *Self) Token {   
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        } 

        if (self.isAtEnd()) return self.errorToken("Unterminated string.");

        _ = self.advance();
        return self.makeToken(.string);
    }

    fn currentLexeme(self: *Self) []const u8 {
        const end = @ptrToInt(self.current) - @ptrToInt(self.start);
        return self.start[0..end];
    }

    inline fn peek(self: *Self) u8 {
        return self.current[0];
    }

    inline fn peekNext(self: *Self) u8 {
        if (self.isAtEnd()) return 0;
        return self.current[1];
    }

    fn makeToken(self: *Self, ty: TokenType) Token {
        return Token{
            .line = self.line,
            .type = ty,
            .lexeme = self.currentLexeme(),
        };
    }

    fn errorToken(self: *Self, comptime msg: []const u8) Token {
        return Token{
            .line = self.line,
            .type = .token_error,
            .lexeme = msg,
        };
    }
};

fn isDigit(c: u8) bool {
    return switch (c) {
        '0'...'9' => true,
        else => false,
    };
}

fn isAlpha(c: u8) bool {
    return switch (c) {
        'a'...'z', 'A'...'Z', '_' => true,
        else => false,
    };
}

pub const Token = struct {
    lexeme: []const u8,
    line: usize,
    type: TokenType,
};

pub const TokenType = enum {
    // single-character tokens.
    left_paren,
    right_paren,
    left_brace,
    right_brace,
    comma,
    dot,
    minus,
    plus,
    semicolon,
    slash,
    star,
    // one or two character tokens.
    bang,
    bang_equal,
    equal,
    equal_equal,
    greater,
    greater_equal,
    less,
    less_equal,
    // literals.
    identifier,
    string,
    number,
    // keywords.
    token_and,
    class,
    token_else,
    token_false,
    token_for,
    fun,
    token_if,
    nil,
    token_or,
    print,
    token_return,
    super,
    this,
    token_true,
    token_var,
    token_while,

    token_error,
    eof,
};
