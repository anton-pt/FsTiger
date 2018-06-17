#r "../../packages/FsLexYacc.Runtime.7.0.6/lib/portable-net45+netcore45+wpa81+wp8+MonoAndroid10+MonoTouch10/FsLexYacc.Runtime.dll"
#r "bin/Debug/03-Parser.dll"

open System.IO
open Parser
open Microsoft.FSharp.Text.Lexing

Directory.GetFiles "../../testcases/"
|> Seq.filter (fun path -> Path.GetExtension path = ".tig")
|> Seq.iter (fun path ->
    printfn "PARSING: %s" path
    use sr = File.OpenText path 
    let buf = LexBuffer<_>.FromTextReader sr
    let ast = start Lexer.token buf
    printfn "%A" ast
    printfn "\n\n\n\n")