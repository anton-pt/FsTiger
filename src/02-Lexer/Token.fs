module Token

type Keyword =
    | TYPE
    | VAR
    | FUNCTION
    | INT
    | STRING
    | ARRAY
    | OF
    | NIL
    | LET
    | IN
    | END
    | IF
    | THEN
    | ELSE
    | WHILE
    | DO
    | FOR
    | TO
    | BREAK

type Operation =
    | PLUS
    | MINUS
    | TIMES
    | DIV
    | ASSIGN
    | EQ
    | NEQ
    | GTEQ
    | LTEQ
    | GT
    | LT
    | AND
    | OR

type Punctuation =
    | LPAREN
    | RPAREN
    | LBRACE
    | RBRACE
    | LBRACKET
    | RBRACKET
    | DOT
    | COMMA
    | COLON
    | SEMICOLON

type Token =
    | KEYW of Keyword
    | OP of Operation
    | PUNCT of Punctuation
    | ID of string
    | STRING_LIT of string
    | INT_LIT of int
    | EOF