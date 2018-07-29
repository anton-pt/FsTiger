module Ast

open Microsoft.FSharp.Text.Lexing

type Symbol = Symbol of string

type Var =
    | SimpleVar of Symbol * Position
    | FieldVar of Var * Symbol * Position
    | SubscriptVar of Var * Exp * Position
and Exp =
    | VarExp of Var
    | NilExp
    | IntExp of int
    | StringExp of string * Position
    | CallExp of func: Symbol * args: Exp list * pos: Position
    | OpExp of left: Exp * oper: Oper * right: Exp * pos: Position
    | RecordExp of fields: (Symbol * Exp * Position) list * typ: Symbol * pos: Position
    | SeqExp of (Exp * Position) list
    | AssignExp of var: Var * exp: Exp * pos: Position
    | IfExp of testExp: Exp * thenExp: Exp * elseExp: Exp option * pos: Position
    | WhileExp of testExp: Exp * body: Exp * pos: Position
    | ForExp of var: Symbol * escape: bool ref * lo: Exp * hi: Exp * body: Exp * pos: Position
    | BreakExp of Position
    | LetExp of decs: Dec list * body: Exp * pos: Position
    | ArrayExp of typ: Symbol * size: Exp * init: Exp * pos: Position
and Dec =
    | FunDec of FunDec list
    | VarDec of name: Symbol * escape: bool ref * typ: (Symbol * Position) option * init: Exp * pos: Position
    | TypeDec of TypeDec list
and Field = 
    { FieldName: Symbol 
      Escape: bool ref
      Typ: Symbol
      Pos: Position }
and FunDec =
    { FunName: Symbol
      Params: Field list 
      Result: (Symbol * Position) option
      Body: Exp
      Pos: Position }
and TypeDec =
    { TypeName: Symbol
      Ty: Ty
      Pos: Position }
and Ty =
    | NameTy of Symbol * Position
    | RecordTy of Field list
    | ArrayTy of Symbol * Position
and Oper =
    | PlusOp | MinusOp | TimesOp | DivideOp
    | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp
