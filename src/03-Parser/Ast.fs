module Ast

type Id = string
type TypeId = string

type ArithmeticOp =
    | PlusOp
    | MinusOp
    | TimesOp
    | DivOp

type ComparisonOp =
    | EqualsOp
    | NotEqualsOp
    | GreaterThanOp
    | LessThanOp
    | GreaterThanOrEqualsOp
    | LessThanOrEqualsOp

type LogicOp =
    | AndOp
    | OrOp

type TypeDef =
    | TypeAliasDef of TypeId
    | RecordTypeDef of (Id * TypeId) list
    | ArrayTypeDef of TypeId
    
type LValue =
    | LId of Id
    | LProperty of LValue * Id
    | LArrayElem of LValue * Expr
and Expr =
    | NilExpr
    | UnitExpr
    | EvalExpr of LValue
    | ExprSeqExpr of Expr list
    | IntLiteralExpr of int
    | StringLiteralExpr of string
    | FunctionCallExpr of Id * Expr list
    | NegationExpr of Expr
    | ArithmeticExpr of Expr * ArithmeticOp * Expr
    | ComparisonExpr of Expr * ComparisonOp * Expr
    | LogicExpr of Expr * LogicOp * Expr
    | RecordCreationExpr of TypeId * (Id * Expr) list
    | ArrayCreationExpr of TypeId * Expr * Expr
    | AssignmentExpr of LValue * Expr
    | IfThenExpr of Expr * Expr
    | IfThenElseExpr of Expr * Expr * Expr
    | WhileDoExpr of Expr * Expr
    | ForDoExpr of Id * Expr * Expr * Expr
    | BreakExpr
    | LetInExpr of Dec list * Expr
and Dec =
    | TDec of TypeDec
    | VDec of VarDec
    | FDec of FunDec
and TypeDec = TypeDec of TypeId * TypeDef
and VarDec = 
    | VarDec of Id * Expr
    | VarDecWithType of Id * TypeId * Expr
and FunDec =
    | FunDec of Id * (Id * TypeId) list * Expr
    | FunDecWithType of Id * (Id * TypeId) list * TypeId * Expr