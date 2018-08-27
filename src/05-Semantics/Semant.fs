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

let baseVEnv =
    [ (Symbol.symbol "print", FunEntry([String], Unit))
      (Symbol.symbol "flush", FunEntry([], Unit))
      (Symbol.symbol "getchar", FunEntry([], String))
      (Symbol.symbol "ord", FunEntry([String], Int))
      (Symbol.symbol "chr", FunEntry([Int], String))
      (Symbol.symbol "size", FunEntry([String], Int))
      (Symbol.symbol "substring", FunEntry([String ; Int ; Int], String))
      (Symbol.symbol "concat", FunEntry([String ; String], String))
      (Symbol.symbol "not", FunEntry([Int], Int))
      (Symbol.symbol "exit", FunEntry([Int], Unit)) ]
    |> List.fold (fun env (symbol, entry) -> Table.enter env symbol entry) Table.empty

let baseTEnv =
    [ (Symbol.symbol "int", Int)
      (Symbol.symbol "string", String) ]
    |> List.fold (fun env (symbol, entry) -> Table.enter env symbol entry) Table.empty

let private error pos msg =
    failwith msg

let rec private actualTy ty =
    match ty with
    | Name (_, actual) -> actualTy (Option.get !actual)
    | _ -> ty

let rec transVar (venv: VEnv) (tenv: TEnv) (var: Var) : ExpTy =
    let trVar = transVar venv tenv
    match var with
    | SimpleVar(id, pos) ->
        match Table.look venv id with
        | Some (VarEntry(ty)) -> { Exp = () ; Type = actualTy ty }
        | Some (FunEntry(_, _)) -> error pos "Expected value but found function"
        | None -> error pos (sprintf "Undefined variable %s" (name id))
    | FieldVar(var, id, pos) ->
        let { Exp = () ; Type = varTy } = trVar var
        match varTy with
        | Record(fields, _) ->
            let fieldTy =
                fields |> List.tryPick (fun (fieldId, fieldTy) -> 
                    if id = fieldId then Some fieldTy else None)
            match fieldTy with
            | Some fieldTy -> { Exp = () ; Type = actualTy fieldTy }
            | None -> error pos (sprintf "Type does not contain field %s" (name id))
        | _ -> error pos "Cannot access member field of type which is not a record type"
    | SubscriptVar(var, exp, pos) ->
        let { Exp = () ; Type = varTy } = trVar var
        match varTy with
        | Array(elemTy, _) ->
            let { Exp = () ; Type = expTy } = transExp false venv tenv exp
            if expTy = Int
            then { Exp = () ; Type = actualTy elemTy }
            else error pos "Cannot use non-integer expression as array index"
        | _ -> error pos "Cannot subscript type which is not an array type"

and transExp allowBreak (venv: VEnv) (tenv: TEnv) (exp: Exp) : ExpTy =
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
                    let argType = (transExp false venv tenv arg).Type
                    if not (argType |> Type.equals reqType) then
                        error pos (sprintf "Function argument %d does not have the correct type" i))
                { Exp = () ; Type = result }
    | OpExp(left, oper, right, pos) when oper = EqOp || oper = NeqOp ->
        match ((transExp false venv tenv left).Type, (transExp false venv tenv right).Type) with
        | (Int, Int) -> ()
        | (String, String) -> ()
        | (Record (_, lid), Record (_, rid)) when lid = rid -> ()
        | (Record _, Nil) -> ()
        | (Nil, Record _) -> ()
        | (Array (_, lid), Array (_, rid)) when lid = rid -> ()
        | _ -> error pos "Attempted equality comparison of mismatching types"
        { Exp = () ; Type = Int }
    | OpExp(left, oper, right, pos) ->
        let lType = (transExp false venv tenv left).Type
        let rType = (transExp false venv tenv right).Type
        if lType <> Int || rType <> Int then
            failwithf "Integer expected in operator expression"
        { Exp = () ; Type = Int }
    | RecordExp(fields, typ, pos) ->
        match Table.look tenv typ with
        | None -> error pos "Unknown type"
        | Some ty ->
            let actual = actualTy ty
            match actual with
            | Record(reqFields, _) ->
                let fields' = fields |> List.map (fun (s, exp, pos) -> (s, (exp, pos))) |> Map.ofList
                let reqFields' = Map.ofList reqFields
                if (fields' |> Seq.map (fun kvp -> kvp.Key) |> Set.ofSeq)
                        <> (reqFields' |> Seq.map (fun kvp -> kvp.Key) |> Set.ofSeq)
                then error pos "Record fields do not match those of the type"
                else { Exp = () ; Type = actual }
            | _ -> error pos "Record type expected"
    | SeqExp exps ->
        let len = List.length exps
        let exps' = exps |> List.mapi (fun i exp -> fst exp |> transExp (i = len - 1) venv tenv)
        if List.isEmpty exps'
        then { Exp = () ; Type = Unit }
        else { Exp = () ; Type = (List.last exps').Type }
    | AssignExp (var, exp, pos) ->
        let { Exp = () ; Type = varType } = transVar venv tenv var
        let { Exp = () ; Type = expType } = transExp false venv tenv exp
        if varType = expType
        then { Exp = () ; Type = Unit }
        else error pos "Type mismatch in assignment expression"
    | IfExp (testExp, thenExp, elseExp, pos) as exp ->
        let { Exp = () ; Type = testType } = transExp false venv tenv testExp
        if testType <> Int
        then error pos "Integer expression expected in condition"
        else let { Exp = () ; Type = thenType } = transExp allowBreak venv tenv thenExp
             match elseExp with
             | Some elseExp ->
                 let { Exp = () ; Type = elseType } = transExp allowBreak venv tenv elseExp
                 match (thenType, elseType) with
                 | (thenType, elseType) when Type.equals thenType elseType -> { Exp = () ; Type = thenType }
                 | (thenType, Nil) when thenType <> Nil -> { Exp = () ; Type = thenType }
                 | (Nil, elseType) when elseType <> Nil -> { Exp = () ; Type = elseType }
                 | (Nil, Nil) -> error pos "Both if expression branches are of nil type"
                 | _ -> error pos "If expression branches must have the same type"
             | None ->
                 if thenType <> Unit
                 then error pos "Else branch required if the then branch has a non-unit type"
                 else { Exp = () ; Type = Unit }
    | WhileExp (testExp, body, pos) ->
        let { Exp = () ; Type = testType } = transExp false venv tenv testExp
        if testType <> Int
        then error pos "Integer expression expected in condition"
        else let { Exp = () ; Type = bodyType } = transExp true venv tenv body
             if bodyType <> Unit
             then error pos "Unit expression expected in body"
             else { Exp = () ; Type = Unit }
    | ForExp (var, escape, lo, hi, body, pos) ->
        let { Exp = () ; Type = loType } = transExp false venv tenv lo
        let { Exp = () ; Type = hiType } = transExp false venv tenv hi
        if loType <> Int || hiType <> Int
        then error pos "Integer expression expected in for loop bound"
        else let venv' = Table.enter venv var (VarEntry(Int))
             let { Exp = () ; Type = bodyType } = transExp true venv' tenv body
             if bodyType <> Unit
             then error pos "Unit expression expected in body"
             else { Exp = () ; Type = Unit }
    | BreakExp pos -> 
        if allowBreak
        then { Exp = () ; Type = Unit }
        else error pos "Unexpected keyword break"
    | LetExp (decs, body, pos) ->
        let (venv', tenv') = 
            decs |> List.fold 
                (fun (venv, tenv) dec -> transDec venv tenv dec)
                (venv, tenv)
        transExp allowBreak venv' tenv' body
    | ArrayExp (typ, size, init, pos) ->
        match Table.look tenv typ |> Option.map actualTy with
        | Some ((Array (elType, guid)) as arrayType) ->
            let { Exp = () ; Type = initType } = transExp false venv tenv init
            if actualTy elType <> initType
            then error pos "Initialiser type must match element type"
            else { Exp = () ; Type = arrayType }
        | Some _ -> error pos "Array type expected"
        | None -> error pos "Unknown type"

and transDec (venv: VEnv) (tenv: TEnv) (dec: Dec) : (VEnv * TEnv) =
    match dec with
    | VarDec(id, _, None, init, pos) ->
        let initType = (transExp false venv tenv init).Type
        if initType = Nil then
            error pos "Must specify type when initializing nil value"
        else 
            let entry = VarEntry initType
            (Table.enter venv id entry, tenv)
    | VarDec(id, _, Some (decTypeId, pos), init, _) ->
        let initType = (transExp false venv tenv init).Type
        let decType = Table.look tenv decTypeId
        match decType with
        | None -> error pos "Unknown type"
        | Some ty when not (Type.isRecord ty) && initType = Nil ->
            error pos "Nil is not a valid value of the given type"
        | Some ty when not (actualTy ty |> Type.equals initType) && initType <> Nil ->
            error pos "Expression does not match type declaration"
        | Some decType ->
            let entry = VarEntry decType
            (Table.enter venv id entry, tenv)
    | TypeDec decs ->
        let nameCollision =
            decs
            |> List.groupBy (fun dec -> dec.TypeName)
            |> List.tryPick (fun (name, decsByName) ->
                if List.length decsByName > 1
                then Some (List.last decsByName)
                else None)
        match nameCollision with
        | Some dec -> error dec.Pos "Repeated type name in same batch of declarations"
        | None -> ()
        let nameCycle =
            let decs' = Array.ofList decs
            let visited = Array.zeroCreate decs'.Length 
            let recStack = Array.zeroCreate decs'.Length
            let rec findCycle n =
                if recStack.[n] then
                    (Some decs'.[n])
                else if visited.[n] then
                    None
                else 
                    visited.[n] <- true
                    recStack.[n] <- true
                    let child =
                        match decs'.[n].Ty with
                        | NameTy (actual, _) -> decs' |> Array.tryFindIndex (fun dec -> dec.TypeName = actual)
                        | _ -> None
                    match child |> Option.bind findCycle with
                    | Some cycle -> Some cycle
                    | _ -> recStack.[n] <- false ; None
            seq { 0 .. decs'.Length - 1 } |> Seq.tryPick (fun n -> findCycle n)
        match nameCycle with
        | Some dec -> error dec.Pos "Type definition contains illegal cycle"
        | None -> ()
        let (tenv', tys) =
            decs 
            |> List.fold (fun (env, tys) dec -> 
                    let ty = ref None in (Table.enter env dec.TypeName (Name (dec.TypeName, ty)), ty::tys))
                (tenv, List.empty)
        let tenv'' =
            decs
            |> List.map (fun dec -> (dec.TypeName, transTy tenv' dec.Ty))
            |> List.zip (List.rev tys)
            |> List.fold (fun env (ty', (name, ty)) -> 
                ty' := Some ty
                Table.enter env name ty) tenv'
        (venv, tenv'')
    | FunDec decs ->
        let nameCollision =
            decs
            |> List.groupBy (fun dec -> dec.FunName)
            |> List.tryPick (fun (name, decsByName) ->
                if List.length decsByName > 1
                then Some (List.last decsByName)
                else None)
        match nameCollision with
        | Some dec -> error dec.Pos "Repeated function name in same batch of declarations"
        | None -> ()
        let (venv', funData) =
            decs
            |> List.fold (fun (env, funData) dec ->
                let parameters = 
                    dec.Params
                    |> List.fold (fun parameters param -> 
                            match Table.look tenv param.Typ with
                            | Some paramType -> (param.FieldName, paramType)::parameters
                            | None -> error param.Pos "Unknown parameter type")
                        List.empty
                let resultType =
                    match dec.Result with
                    | Some (resultTyp, pos) ->
                        match Table.look tenv resultTyp with
                        | Some resultType -> resultType
                        | None -> error pos "Unknown result type"
                    | None -> Unit
                let entry = FunEntry(List.rev parameters |> List.map snd, resultType)
                (Table.enter env dec.FunName entry, (parameters, resultType)::funData)) 
                (venv, List.empty)
        decs
        |> List.zip (List.rev funData)
        |> List.iter (fun ((parameters, resultType), dec) ->
            let funEnv = parameters |> List.fold (fun env (name, paramType) -> Table.enter env name (VarEntry paramType)) venv'
            let { Exp = () ; Type = bodyType } = transExp false funEnv tenv dec.Body
            if not (Type.equals bodyType resultType)
            then error dec.Pos "Function body return type does not match declared result type")
        (venv', tenv)

and transTy (tenv: TEnv) (ty: Ty) : Type =
    match ty with
    | NameTy (actualTy, pos) ->
        match Table.look tenv actualTy with
        | Some actual -> actual
        | None -> error pos "Cannot alias unknown type"
    | RecordTy fields ->
        let fieldTypes =
            fields
            |> List.map (fun field ->
                match Table.look tenv field.Typ with
                | Some fieldType -> (field.FieldName, fieldType)
                | None -> error field.Pos "Unknown field type")
        Record (fieldTypes, Guid.NewGuid())
    | ArrayTy (elTyp, pos) ->
        match Table.look tenv elTyp with
        | Some elType -> Array(elType, Guid.NewGuid())
        | None -> error pos "Unknown element type"

let transProg (prog: Exp) =
    transExp false baseVEnv baseTEnv prog
    