module Semant

open System

open Ast
open Symbol
open Types
open Env

type VEnv = Table<EnvEntry>
type TEnv = Table<Type>

type ExpTy = 
    { Exp : unit // placeholder
      Type : Type }

let baseVEnv = Table.empty
let baseTEnv = Table.empty

let private error pos msg =
    failwith msg

let private checkInt expTy pos =
    if expTy.Type <> Int then
        error pos "Integer required"

let private actualTy ty = ty

let rec transVar (venv: VEnv) (tenv: TEnv) (var: Var) : ExpTy =
    let trVar = transVar venv tenv

    match var with
    | SimpleVar(id, pos) ->
        match Table.look venv id with
        | Some (VarEntry(ty)) -> { Exp = () ; Type = actualTy ty }
        | Some (FunEntry(_, _)) -> error pos "Expected value but found function"
        | None -> error pos (sprintf "Undefined variable %s" (name id))
    | _ ->
        raise (NotImplementedException())

and transExp (venv: VEnv) (tenv: TEnv) (exp: Exp) : ExpTy =
    let trExp = transExp venv tenv

    match exp with
    | VarExp var -> transVar venv tenv var
    | NilExp -> { Exp = () ; Type = Nil }
    | IntExp _ -> { Exp = () ; Type = Int }
    | StringExp _ -> { Exp = () ; Type = String }
    | CallExp(func, args, pos) ->
        match Table.look venv func with
        | None -> error pos (sprintf "Undefined function %s" (name func))
        | Some (VarEntry _) -> error pos "Expected function but found value"
        | Some (FunEntry (formals, result)) ->
            if List.length formals <> List.length args then
                error pos "Unexpected number of arguments in function call"
            else
                List.zip formals args
                |> List.iteri (fun i (reqType, arg) -> 
                    let argType = (transExp venv tenv arg).Type
                    if not (argType |> Type.equals reqType) then
                        error pos (sprintf "Argument %d does not have the correct type" i))
                { Exp = () ; Type = result }
    | OpExp(left, oper, right, pos) when oper = EqOp || oper = NeqOp ->
        match ((trExp left).Type, (trExp right).Type) with
        | (Int, Int) -> ()
        | (Record (_, lid), Record (_, rid)) when lid = rid -> ()
        | (Array (_, lid), Array (_, rid)) when lid = rid -> ()
        | _ -> error pos "Attempted equality comparison of mismatching types"
        { Exp = () ; Type = Int }
    | OpExp(left, oper, right, pos) ->
        checkInt (trExp left) pos
        checkInt (trExp right) pos
        { Exp = () ; Type = Int }
    | LetExp (decs, body, pos) ->
        let (venv', tenv') = 
            decs |> List.fold 
                (fun (venv, tenv) dec -> transDec venv tenv dec)
                (venv, tenv)
        transExp venv' tenv' body
    | _ -> raise (NotImplementedException())

and transDec (venv: VEnv) (tenv: TEnv) (dec: Dec) : (VEnv * TEnv) =
    match dec with
    | VarDec(id, _, None, init, pos) ->
        let initType = (transExp venv tenv init).Type
        if initType = Nil then
            error pos "Must specify type when initializing nil value"
        else 
            let entry = VarEntry initType
            (Table.enter venv id entry, tenv)
    | VarDec(id, _, Some (decTypeId, pos), init, _) ->
        let initType = (transExp venv tenv init).Type
        let decType = Table.look tenv decTypeId
        match decType with
        | None -> error pos "Unknown type"
        | Some ty when not (Type.isRecord ty) && initType = Nil ->
            error pos "Nil is not a valid value of the given type"
        | Some ty when not (ty |> Type.equals initType) && initType <> Nil ->
            error pos "Expression does not match type declaration"
        | _ ->
            let entry = VarEntry initType
            (Table.enter venv id entry, tenv)
    | TypeDec({ TypeName = id ; Ty = ty ; Pos = pos }::decs) ->
        let ty' = transTy tenv ty
        let tenv' = Table.enter tenv id ty'
        transDec venv tenv' (TypeDec(decs))
    | TypeDec([]) ->
        (venv, tenv)
    | _ ->
        raise (NotImplementedException())

and transTy (tenv: TEnv) (ty: Ty) : Type =
    raise (NotImplementedException())


let transProg (prog: Exp) = ()