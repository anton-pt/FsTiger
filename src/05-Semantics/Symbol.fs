module Symbol

open System.Collections.Generic

type Symbol = private Symbol of string * int

let private nextSym = ref 0
let private symbolDictionary = Dictionary<string, int>()

let symbol name =
    if symbolDictionary.ContainsKey name then
        Symbol (name, symbolDictionary.[name])
    else
        symbolDictionary.[name] <- !nextSym
        nextSym := !nextSym + 1
        Symbol (name, symbolDictionary.[name])

let name (Symbol (n, _)) = n

type Table<'a> = private Table of Map<Symbol, 'a>

[<RequireQualifiedAccess>]
module Table = 
    let empty = Table (Map.empty)
    let enter (Table tbl) s x = Table (tbl |> Map.add s x)
    let look (Table tbl) s = tbl |> Map.tryFind s