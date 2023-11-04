use crate::scanner::TokenType::Identifier;

// todo: make file a reference, cause currently we're cloning the filename each time smh
#[derive(Debug, Clone)]
pub struct Scanner {
    pub file: String,
    pub code: String,
    pub cur_line: usize,
    pub start_pos: usize,
    pub cur_pos: usize,
}

impl Default for Scanner {
    fn default() -> Self {
        Scanner {
            file: String::new(),
            code: "".to_string(),
            cur_line: 0,
            start_pos: 0,
            cur_pos: 0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub line_num: usize,
    pub lexeme: String,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    /// (
    LeftParen,
    /// )
    RightParen,
    /// {
    LeftBrace,
    /// }
    RightBrace,
    /// [
    LeftBracket,
    /// [
    RightBracket,
    Comma,
    Dot,
    Semicolon,
    Minus,
    MinusAssign,
    MinusMinus,
    Plus,
    PlusAssign,
    PlusPlus,
    Slash,
    SlashAssign,
    HashTag,
    Star,
    StarAssign,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Colon,

    Identifier,
    String,
    Number,

    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Error,
    EOF,
    Import,
}

impl Scanner {
    pub fn new(file: String, code: String, cur_line: usize) -> Scanner {
        Scanner {
            file,
            code,
            cur_line,
            start_pos: 0,
            cur_pos: 0,
        }
    }

    fn create_token(&self, token_type: TokenType) -> Token {
        // println!("code: {}|", self.code);
        Token {
            token_type,
            line_num: self.cur_line,
            lexeme: if self.code.is_empty() {
                "".to_string()
            } else {
                self.code[self.start_pos..self.cur_pos].to_string()
            },
        }
    }

    fn error_token(&self, message: &str) -> Token {
        Token {
            token_type: TokenType::Error,
            line_num: self.cur_line,
            lexeme: message.to_string(),
        }
    }

    pub fn scan_token(&mut self) -> Token {
        self.skip_whitespace();
        self.start_pos = self.cur_pos;

        if self.is_at_end() {
            return self.create_token(TokenType::EOF);
        }

        let c = self.advance();

        if Self::is_digit(c) {
            return self.number();
        }

        if Self::is_alpha(c) {
            return self.identifier();
        }

        match c {
            b'(' => self.create_token(TokenType::LeftParen),
            b')' => self.create_token(TokenType::RightParen),
            b'{' => self.create_token(TokenType::LeftBrace),
            b'}' => self.create_token(TokenType::RightBrace),
            b'[' => self.create_token(TokenType::LeftBracket),
            b']' => self.create_token(TokenType::RightBracket),
            b',' => self.create_token(TokenType::Comma),
            b'.' => self.create_token(TokenType::Dot),
            b'-' => {
                if self.match_char(b'=') {
                    self.create_token(TokenType::MinusAssign)
                } else if self.match_char(b'-') {
                    self.create_token(TokenType::MinusMinus)
                } else {
                    self.create_token(TokenType::Minus)
                }
            }
            b'+' => {
                if self.match_char(b'=') {
                    self.create_token(TokenType::PlusAssign)
                } else if self.match_char(b'+') {
                    self.create_token(TokenType::PlusPlus)
                } else {
                    self.create_token(TokenType::Plus)
                }
            }
            b';' => self.create_token(TokenType::Semicolon),
            b'*' => {
                if self.match_char(b'=') {
                    self.create_token(TokenType::StarAssign)
                } else {
                    self.create_token(TokenType::Star)
                }
            }
            b'/' => {
                if self.match_char(b'=') {
                    self.create_token(TokenType::SlashAssign)
                } else {
                    self.create_token(TokenType::Slash)
                }
            }
            b'#' => self.create_token(TokenType::HashTag),
            b'!' => {
                if self.match_char(b'=') {
                    self.create_token(TokenType::BangEqual)
                } else {
                    self.create_token(TokenType::Bang)
                }
            }
            b'=' => {
                if self.match_char(b'=') {
                    self.create_token(TokenType::EqualEqual)
                } else {
                    self.create_token(TokenType::Equal)
                }
            }
            b'<' => {
                if self.match_char(b'=') {
                    self.create_token(TokenType::LessEqual)
                } else {
                    self.create_token(TokenType::Less)
                }
            }
            b'>' => {
                if self.match_char(b'=') {
                    self.create_token(TokenType::GreaterEqual)
                } else {
                    self.create_token(TokenType::Greater)
                }
            }
            b'"' => self.string(),
            b':' => self.create_token(TokenType::Colon),
            _ => self.error_token("Unexpected character."),
        }
    }

    fn is_at_end(&self) -> bool {
        self.cur_pos >= self.code.len()
    }

    fn is_digit(c: u8) -> bool {
        c.is_ascii_digit()
    }

    fn is_alpha(c: u8) -> bool {
        c.is_ascii_lowercase() || c.is_ascii_uppercase() || c == b'_'
    }

    fn can_peek_next(&self) -> bool {
        self.cur_pos + 2 <= self.code.len()
    }

    fn advance(&mut self) -> u8 {
        let ret = self.peek();
        self.cur_pos += 1;
        ret
    }

    fn match_char(&mut self, expected: u8) -> bool {
        if self.is_at_end() || self.peek() != expected {
            false
        } else {
            self.cur_pos += 1;
            true
        }
    }

    fn peek(&self) -> u8 {
        if self.is_at_end() {
            return b'\0';
        }
        self.code.as_bytes()[self.cur_pos]
    }

    fn peek_next(&self) -> u8 {
        if self.is_at_end() {
            return b'\0';
        }
        self.code.as_bytes()[self.cur_pos + 1]
    }

    fn skip_whitespace(&mut self) {
        while !self.is_at_end() {
            let next = self.peek();
            if (next == b' ') || (next == b'\t') || (next == b'\r') {
                self.advance();
            } else if next == b'\n' {
                self.advance();
                self.cur_line += 1;
            } else if next == b'/' {
                if self.can_peek_next() && self.peek_next() == b'/' {
                    while !self.is_at_end() && self.peek() != b'\n' {
                        self.advance(); // Advance over the second '/' and the rest of the line, or until we hit the end of the file
                    }

                    if !self.is_at_end() {
                        self.advance(); // consume the \n
                        self.cur_line += 1;
                    }
                } else {
                    return; // Return on single slash
                }
                // check for #! at the start of the file
            } else if next == b'#' && self.cur_pos == 0 {
                while !self.is_at_end() && self.peek() != b'\n' {
                    self.advance();
                }

                if !self.is_at_end() {
                    self.advance();
                    self.cur_line += 1;
                }
            } else {
                return;
            }
        }
    }

    fn string(&mut self) -> Token {
        while !self.is_at_end() && self.peek() != b'"' {
            if self.peek() == b'\n' {
                self.cur_line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            return self.error_token("Unterminated string.");
        }

        self.advance(); // consume the closing "

        self.create_token(TokenType::String)
    }

    fn number(&mut self) -> Token {
        if self.cur_pos == self.code.len() {
            return self.error_token("Unexpected end of file.");
        }
        while Self::is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == b'.' && Self::is_digit(self.peek_next()) {
            self.advance(); // consume the .

            while Self::is_digit(self.peek()) {
                self.advance();
            }
        }

        // also take an f after the number to make it a float
        if self.peek() == b'f' {
            self.advance();
        }

        self.create_token(TokenType::Number)
    }

    fn identifier(&mut self) -> Token {
        if self.cur_pos == self.code.len() {
            return self.error_token("Unexpected end of file.");
        }
        while Self::is_alpha(self.peek()) || Self::is_digit(self.peek()) {
            self.advance();
        }

        self.create_token(self.identifier_type())
    }

    fn identifier_type(&self) -> TokenType {
        let c = self.code.as_bytes()[self.start_pos];
        return match c {
            b'a' => self.check_for_keyword(1, 2, "nd", TokenType::And),
            b'c' => self.check_for_keyword(1, 4, "lass", TokenType::Class),
            b'e' => self.check_for_keyword(1, 3, "lse", TokenType::Else),
            b'i' => {
                if self.cur_pos - self.start_pos > 1 {
                    // more than 1 char in this maybe keyword
                    match self.code.as_bytes()[self.start_pos + 1] {
                        b'f' => TokenType::If,
                        b'm' => self.check_for_keyword(2, 4, "port", TokenType::Import),
                        _ => Identifier,
                    }
                } else {
                    Identifier
                }
            }
            b'n' => self.check_for_keyword(1, 2, "il", TokenType::Nil),
            b'o' => self.check_for_keyword(1, 1, "r", TokenType::Or),
            b'p' => self.check_for_keyword(1, 4, "rint", TokenType::Print),
            b'r' => self.check_for_keyword(1, 5, "eturn", TokenType::Return),
            b's' => self.check_for_keyword(1, 4, "uper", TokenType::Super),
            b'v' => self.check_for_keyword(1, 2, "ar", TokenType::Var),
            b'w' => self.check_for_keyword(1, 4, "hile", TokenType::While),
            b'f' => {
                if self.cur_pos - self.start_pos > 1 {
                    // more than 1 char in this maybe keyword
                    match self.code.as_bytes()[self.start_pos + 1] {
                        b'a' => self.check_for_keyword(2, 3, "lse", TokenType::False),
                        b'o' => self.check_for_keyword(2, 1, "r", TokenType::For),
                        b'u' => self.check_for_keyword(2, 1, "n", TokenType::Fun),
                        _ => Identifier,
                    }
                } else {
                    Identifier
                }
            }
            b't' => {
                if self.cur_pos - self.start_pos > 1 {
                    // more than 1 char in this maybe keyword
                    match self.code.as_bytes()[self.start_pos + 1] {
                        b'h' => self.check_for_keyword(2, 2, "is", TokenType::This),
                        b'r' => self.check_for_keyword(2, 2, "ue", TokenType::True),
                        _ => Identifier,
                    }
                } else {
                    Identifier
                }
            }
            _ => Identifier,
        };
    }

    fn check_for_keyword(
        &self,
        start: usize,
        length: usize,
        rest: &str,
        keyword_type: TokenType,
    ) -> TokenType {
        if self.cur_pos - self.start_pos == start + length {
            // this will check that begin + length is within the array, since we already moved cur_pos exactly that far
            let begin = self.start_pos + start;
            if &self.code[begin..begin + length] == rest {
                return keyword_type;
            }
        }
        Identifier
    }
}
