#r "../../packages/FsLexYacc.Runtime.7.0.6/lib/portable-net45+netcore45+wpa81+wp8+MonoAndroid10+MonoTouch10/FsLexYacc.Runtime.dll"
#r "bin/Debug/03-Parser.dll"

open Parser
open Microsoft.FSharp.Text.Lexing

let prog = """(a := 5; a)"""

let buf = LexBuffer<_>.FromString prog
let ast = start Lexer.token buf