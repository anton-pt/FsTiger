module Types

open System
open Symbol


type Type =
    | Int
    | String
    | Record of (Symbol * Type) list * Guid
    | Array of Type * Guid
    | Nil
    | Unit
    | Name of Symbol * Type option ref

[<RequireQualifiedAccess>]
module Type =
    let equals a b =
        match (a, b) with
        | (Int, Int) -> true
        | (String, String) -> true
        | (Record (_, lguid), Record (_, rguid)) when lguid = rguid -> true
        | (Array (_, lguid), Array (_, rguid)) when lguid = rguid -> true
        | (Unit, Unit) -> true
        | _ -> false

    let isRecord a =
        match a with
        | Record _ -> true
        | _ -> false