module Compiler

open System
open Ast
open TypeValidator

let private parseInternal streamName schizo =
    let lexbuf = FSharp.Text.Lexing.LexBuffer<char>.FromString schizo
    try
        let res = Parser.start (Lexer.read streamName) lexbuf
        res
    with e ->
        let pos = lexbuf.StartPos
        let line = pos.Line + 1
        let column = pos.Column
        let lastToken = String(lexbuf.Lexeme)
        printfn "%s(%d, %d): %s on token %s" streamName line column e.Message lastToken
        []

let parse fileEnv streamName stream =
    let decls = parseInternal streamName stream
    let ds, env, s = openImports parseInternal fileEnv Map.empty (Set.empty.Add streamName) decls
    let m
        = { Module.name   = streamName
            decls         = ds }
    m, validate fileEnv m

