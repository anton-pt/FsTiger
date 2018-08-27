// Learn more about F# at http://fsharp.org

open System.IO
open Parser
open Microsoft.FSharp.Text.Lexing

[<EntryPoint>]
let main argv =
    Directory.GetFiles "../../testcases/"
    |> Seq.sort
    |> Seq.filter (fun path -> Path.GetExtension path = ".tig")
    |> Seq.iter (fun path ->
        printfn "PARSING: %s" path
        use sr = File.OpenText path
        let buf = LexBuffer<_>.FromTextReader sr
        let prog = start Lexer.token buf
        try
            Semant.transProg prog |> printfn "SUCCESS: %A\n\n\n"
        with
        | exn -> printfn "ERROR: %s\n\n\n" exn.Message)

    0