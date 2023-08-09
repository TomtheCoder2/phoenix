use serde::{Deserialize, Serialize};

use crate::scanner::TokenType;

#[derive(Debug, PartialEq, PartialOrd, Serialize, Deserialize)]
pub enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Increment,
    Unary,
    Call,
    Primary,
}

#[derive(Serialize, Deserialize, Debug)]
pub enum ParseFn {
    None,
    Unary,
    Increment,
    Grouping,
    Number,
    Binary,
    Literal,
    String,
    Variable,
    List,
    And,
    Or,
    Call,
    Dot,
    This,
    Super,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ParseRule {
    pub prefix: ParseFn,
    pub infix: ParseFn,
    pub precedence: Precedence,
}

impl ParseRule {
    pub fn next_precedence(&self) -> Precedence {
        match self.precedence {
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Increment,
            Precedence::Increment => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => Precedence::Primary,
        }
    }
}

const PARSE_RULE_NONE: ParseRule = ParseRule {
    prefix: ParseFn::None,
    infix: ParseFn::None,
    precedence: Precedence::None,
};

const PARSE_RULE_LP: ParseRule = ParseRule {
    prefix: ParseFn::Grouping,
    infix: ParseFn::Call,
    precedence: Precedence::Call,
};

const PARSE_RULE_MINUS: ParseRule = ParseRule {
    prefix: ParseFn::Unary,
    infix: ParseFn::Binary,
    precedence: Precedence::Term,
};

const PARSE_RULE_MINUS_ASSIGN: ParseRule = ParseRule {
    prefix: ParseFn::None,
    infix: ParseFn::Variable,
    precedence: Precedence::Assignment,
};

const PARSE_RULE_MINUS_MINUS: ParseRule = ParseRule {
    prefix: ParseFn::Grouping,
    infix: ParseFn::Increment,
    precedence: Precedence::Increment,
};

const PARSE_RULE_PLUS: ParseRule = ParseRule {
    prefix: ParseFn::None,
    infix: ParseFn::Binary,
    precedence: Precedence::Term,
};

const PARSE_RULE_PLUS_ASSIGN: ParseRule = ParseRule {
    prefix: ParseFn::None,
    infix: ParseFn::Binary,
    precedence: Precedence::Assignment,
};

const PARSE_RULE_PLUS_PLUS: ParseRule = ParseRule {
    prefix: ParseFn::Grouping,
    infix: ParseFn::Increment,
    precedence: Precedence::Increment,
};

const PARSE_RULE_SLASH: ParseRule = ParseRule {
    prefix: ParseFn::None,
    infix: ParseFn::Binary,
    precedence: Precedence::Factor,
};

const PARSE_RULE_SLASH_ASSIGN: ParseRule = ParseRule {
    prefix: ParseFn::None,
    infix: ParseFn::Binary,
    precedence: Precedence::Assignment,
};

const PARSE_RULE_STAR: ParseRule = ParseRule {
    prefix: ParseFn::None,
    infix: ParseFn::Binary,
    precedence: Precedence::Factor,
};

const PARSE_RULE_STAR_ASSIGN: ParseRule = ParseRule {
    prefix: ParseFn::None,
    infix: ParseFn::Binary,
    precedence: Precedence::Assignment,
};

const PARSE_RULE_NUM: ParseRule = ParseRule {
    prefix: ParseFn::Number,
    infix: ParseFn::None,
    precedence: Precedence::None,
};

const PARSE_RULE_TRUE: ParseRule = ParseRule {
    prefix: ParseFn::Literal,
    infix: ParseFn::None,
    precedence: Precedence::None,
};

const PARSE_RULE_FALSE: ParseRule = ParseRule {
    prefix: ParseFn::Literal,
    infix: ParseFn::None,
    precedence: Precedence::None,
};

const PARSE_RULE_NIL: ParseRule = ParseRule {
    prefix: ParseFn::Literal,
    infix: ParseFn::None,
    precedence: Precedence::None,
};

const PARSE_RULE_BANG: ParseRule = ParseRule {
    prefix: ParseFn::Unary,
    infix: ParseFn::None,
    precedence: Precedence::None,
};

const PARSE_RULE_BE: ParseRule = ParseRule {
    prefix: ParseFn::None,
    infix: ParseFn::Binary,
    precedence: Precedence::Equality,
};

const PARSE_RULE_EE: ParseRule = ParseRule {
    prefix: ParseFn::None,
    infix: ParseFn::Binary,
    precedence: Precedence::Equality,
};

const PARSE_RULE_G: ParseRule = ParseRule {
    prefix: ParseFn::None,
    infix: ParseFn::Binary,
    precedence: Precedence::Comparison,
};

const PARSE_RULE_GE: ParseRule = ParseRule {
    prefix: ParseFn::None,
    infix: ParseFn::Binary,
    precedence: Precedence::Comparison,
};
const PARSE_RULE_L: ParseRule = ParseRule {
    prefix: ParseFn::None,
    infix: ParseFn::Binary,
    precedence: Precedence::Comparison,
};
const PARSE_RULE_LE: ParseRule = ParseRule {
    prefix: ParseFn::None,
    infix: ParseFn::Binary,
    precedence: Precedence::Comparison,
};
const PARSE_RULE_STR: ParseRule = ParseRule {
    prefix: ParseFn::String,
    infix: ParseFn::None,
    precedence: Precedence::None,
};
const PARSE_RULE_ID: ParseRule = ParseRule {
    prefix: ParseFn::Variable,
    infix: ParseFn::None,
    precedence: Precedence::None,
};
const PARSE_RULE_AND: ParseRule = ParseRule {
    prefix: ParseFn::None,
    infix: ParseFn::And,
    precedence: Precedence::And,
};
const PARSE_RULE_OR: ParseRule = ParseRule {
    prefix: ParseFn::None,
    infix: ParseFn::Or,
    precedence: Precedence::Or,
};
const PARSE_RULE_DOT: ParseRule = ParseRule {
    prefix: ParseFn::None,
    infix: ParseFn::Dot,
    precedence: Precedence::Call,
};
const PARSE_RULE_THIS: ParseRule = ParseRule {
    prefix: ParseFn::This,
    infix: ParseFn::None,
    precedence: Precedence::None,
};
const PARSE_RULE_SUPER: ParseRule = ParseRule {
    prefix: ParseFn::Super,
    infix: ParseFn::None,
    precedence: Precedence::None,
};
const PARSE_RULE_LB: ParseRule = ParseRule {
    prefix: ParseFn::List,
    infix: ParseFn::None,
    precedence: Precedence::None,
};

pub fn get_rule(operator: TokenType) -> ParseRule {
    match operator {
        TokenType::LeftParen => PARSE_RULE_LP,
        TokenType::LeftBracket => PARSE_RULE_LB,
        TokenType::Minus => PARSE_RULE_MINUS,
        TokenType::MinusMinus => PARSE_RULE_MINUS_MINUS,
        TokenType::MinusAssign => PARSE_RULE_MINUS_ASSIGN,
        TokenType::Plus => PARSE_RULE_PLUS,
        TokenType::PlusPlus => PARSE_RULE_PLUS_PLUS,
        TokenType::PlusAssign => PARSE_RULE_PLUS_ASSIGN,
        TokenType::Slash => PARSE_RULE_SLASH,
        TokenType::SlashAssign => PARSE_RULE_SLASH_ASSIGN,
        TokenType::Star => PARSE_RULE_STAR,
        TokenType::StarAssign => PARSE_RULE_STAR_ASSIGN,
        TokenType::Number => PARSE_RULE_NUM,
        TokenType::True => PARSE_RULE_TRUE,
        TokenType::False => PARSE_RULE_FALSE,
        TokenType::Nil => PARSE_RULE_NIL,
        TokenType::Bang => PARSE_RULE_BANG,
        TokenType::BangEqual => PARSE_RULE_BE,
        TokenType::EqualEqual => PARSE_RULE_EE,
        TokenType::Greater => PARSE_RULE_G,
        TokenType::GreaterEqual => PARSE_RULE_GE,
        TokenType::Less => PARSE_RULE_L,
        TokenType::LessEqual => PARSE_RULE_LE,
        TokenType::String => PARSE_RULE_STR,
        TokenType::Identifier => PARSE_RULE_ID,
        TokenType::And => PARSE_RULE_AND,
        TokenType::Or => PARSE_RULE_OR,
        TokenType::Dot => PARSE_RULE_DOT,
        TokenType::This => PARSE_RULE_THIS,
        TokenType::Super => PARSE_RULE_SUPER,
        _ => PARSE_RULE_NONE,
    }
}
