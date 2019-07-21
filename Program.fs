//
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
open Validator
open Argu

type Arguments =
    | [<AltCommandLine("-i"); Unique>] Import_Directory      of path: string
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Import_Directory _    -> "specify an import directory"

let (|>!) (a: 'A) (b: 'A -> 'B) = b a |> ignore; a

[<EntryPoint>]
let main argv =

    let argParser = ArgumentParser.Create<Arguments>();
    let usage = argParser.PrintUsage()

    if argv.Length = 0 || argv.Length > 2
    then
        printfn "%s" usage
        1
    else
        let streamName = argv.[0]
        match IO.File.Exists (streamName + ".fsidl") with
        | true ->
            try
                let stream = IO.File.ReadAllText (streamName + ".fsidl")
                let res = parse streamName stream
                printfn "%A" res
                0
            with e ->
                printfn "Error: %s" e.Message
                3
        | false ->
            printfn "Module %s (file %s.fsidl) does not exist" streamName streamName
            2
