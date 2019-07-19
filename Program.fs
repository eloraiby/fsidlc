﻿//
// IDLC - Interface Definition Language Compiler
//
// Copyright (C) 2018-2019  Wael El Oraiby
//
// All rights reserved.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//

open System
open FSharp.Text.Lexing

let parse streamName schizo =
    let lexbuf = LexBuffer<char>.FromString schizo
    let res = Parser.start (Lexer.read streamName) lexbuf
    res

let (|>!) (a: 'A) (b: 'A -> 'B) = b a |> ignore; a

[<EntryPoint>]
let main argv =

    let test =
        """
interface T {
    field0: String,
    fieldSomething: Int,
    arrayField : [Float * 4],
    vectorField : [Char]
}
        """
    let result = parse "test" test
    printfn "%A" result
    0 // return an integer exit code
