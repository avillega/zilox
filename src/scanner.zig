const std = @import("std");

pub const Scanner = struct {
    const Self = @This();
    source: []const u8,
    start: usize = 0,
    current: usize = 0,
    line: usize = 1,

    pub fn init(source: []const u8) Self {
        return Self{ .source = source };
    }

    pub fn nextToken(self: *Self) ?Token {
        self.skipWhiteSpace();
        if (self.isAtEnd()) return null;
        self.start = self.current;

        const c = self.advance();
        if (isAlpha(c)) return self.handleIdentifier();
        if (isDigit(c)) return self.handleNumber();

        return switch (c) {
            '(' => self.makeToken(.LEFT_PAREN),
            ')' => self.makeToken(.RIGHT_PAREN),
            '{' => self.makeToken(.LEFT_BRACE),
            '}' => self.makeToken(.RIGHT_BRACE),
            ';' => self.makeToken(.SEMICOLON),
            ',' => self.makeToken(.COMMA),
            '.' => self.makeToken(.DOT),
            '-' => self.makeToken(.MINUS),
            '+' => self.makeToken(.PLUS),
            '/' => self.makeToken(.SLASH),
            '*' => self.makeToken(.STAR),
            '!' => if (self.match('=')) self.makeToken(.BANG_EQUAL) else self.makeToken(.BANG),
            '=' => if (self.match('=')) self.makeToken(.EQUAL_EQUAL) else self.makeToken(.EQUAL),
            '<' => if (self.match('=')) self.makeToken(.LESS_EQUAL) else self.makeToken(.LESS),
            '>' => if (self.match('=')) self.makeToken(.GREATER_EQUAL) else self.makeToken(.GREATER),
            '"' => self.handleString(),
            else => return self.errorToken("Unexpected character."),
        };
    }

    fn handleIdentifier(self: *Self) Token {
        while (isAlpha(self.peek()) or isDigit(self.peek())) {
            self.current += 1;
        }
        return self.makeToken(self.identifierType());
    }

    fn handleNumber(self: *Self) Token {
        while (isDigit(self.peek())) {
            self.current += 1;
        }

        if (self.peek() == '.' and isDigit(self.peekNext())) {
            // consume the dot
            self.current += 1;

            while (isDigit(self.peek())) {
                self.current += 1;
            }
        }

        return self.makeToken(.NUMBER);
    }

    fn handleString(self: *Self) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }
            self.current += 1;
        }

        if (self.isAtEnd()) return self.errorToken("Unterminated string.");

        // For the closing quote
        self.current += 1;
        return self.makeToken(.STRING);
    }

    fn skipWhiteSpace(self: *Self) void {
        while (true) {
            switch (self.peek()) {
                ' ', '\r', '\t' => self.current += 1,
                '\n' => {
                    self.line += 1;
                    self.current += 1;
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        while (self.peek() != '\n' and !self.isAtEnd()) {
                            self.current += 1;
                        }
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    inline fn peek(self: *Self) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.current];
    }

    inline fn peekNext(self: *Self) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.current + 1];
    }

    fn advance(self: *Self) u8 {
        self.current += 1;
        return self.source[self.current - 1];
    }

    fn match(self: *Self, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.current] != expected) return false;

        self.current += 1;
        return true;
    }

    fn isAtEnd(self: *Self) bool {
        return self.current >= self.source.len;
    }

    fn makeToken(self: *Self, tokenType: TokenType) Token {
        return Token{
            .ty = tokenType,
            .lexeme = self.source[self.start..self.current],
            .line = self.line,
        };
    }

    fn errorToken(self: *Self, message: []const u8) Token {
        return Token{
            .ty = .ERROR,
            .lexeme = message,
            .line = self.line,
        };
    }

    fn identifierType(self: *Self) TokenType {
        return switch (self.source[self.start]) {
            'a' => self.checkKeyword("and", .AND),
            'c' => self.checkKeyword("class", .CLASS),
            'e' => self.checkKeyword("else", .ELSE),
            'f' => switch (self.source[self.start + 1]) {
                'a' => self.checkKeyword("false", .FALSE),
                'o' => self.checkKeyword("for", .FOR),
                'u' => self.checkKeyword("fun", .FUN),
                else => .IDENTIFIER,
            },
            'i' => self.checkKeyword("if", .IF),
            'n' => self.checkKeyword("nil", .NIL),
            'o' => self.checkKeyword("or", .OR),
            'p' => self.checkKeyword("print", .PRINT),
            'r' => self.checkKeyword("return", .RETURN),
            's' => self.checkKeyword("super", .SUPER),
            't' => switch (self.source[self.start + 1]) {
                'h' => self.checkKeyword("this", .THIS),
                'r' => self.checkKeyword("true", .TRUE),
                else => .IDENTIFIER,
            },
            'v' => self.checkKeyword("var", .VAR),
            'w' => self.checkKeyword("while", .WHILE),
            else => return .IDENTIFIER,
        };
    }

    fn checkKeyword(self: *Self, keyword: []const u8, ty: TokenType) TokenType {
        if (std.mem.eql(u8, keyword, self.source[self.start..self.current])) {
            return ty;
        }

        return .IDENTIFIER;
    }
};

fn isDigit(char: u8) bool {
    return char >= '0' and char <= '9';
}

fn isAlpha(char: u8) bool {
    return (char >= 'a' and char <= 'z') or (char >= 'A' and char <= 'Z') or (char == '_');
}

pub const Token = struct {
    ty: TokenType,
    lexeme: []const u8,
    line: u64,
};

pub const TokenType = enum {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,
    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,
    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FOR,
    FUN,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,
    // Other
    ERROR,
    EOF,
};
