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
open System.IO
open Argu

type Arguments =
    | [<AltCommandLine("-i")>] Import_Directory      of paths: string list
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Import_Directory _    -> "specify import directories"

let (|>!) (a: 'A) (b: 'A -> 'B) = b a |> ignore; a

let isUpper a =
    a >= 'A' && a <= 'Z'

let isValid a =
    (a >= 'A' && a <= 'Z') || (a >= 'a' && a <= 'z') || (a >= '0' && a <= '9')

let checkModuleName (name: string) =
    if name.[0] |> isUpper then
        (name
        |> Seq.filter isValid
        |> Seq.length)
         = name.Length
    else false

let getFilesInFolders (folders: string list) : Map<string, string> =
    folders
    |> List.map (fun f -> IO.Directory.GetFiles f |> Array.toList)
    |> List.concat
    |> List.filter (fun fname -> IO.Path.GetExtension fname = ".fsidl")
    |> List.map (fun fname ->
        (IO.Path.GetFileNameWithoutExtension fname), fname)
    |> List.filter (fun (m, fname) ->
        if checkModuleName m
        then true
        else
            printfn "warning: igonring %s, module name (%s) is not comformat" fname m
            false)
    |> Map.ofList

module List =
    let cons h t = List.Cons(h, t)

[<EntryPoint>]
let main argv =

    let argParser = ArgumentParser.Create<Arguments>();
    let usage = argParser.PrintUsage()

    if argv.Length = 0
    then
        printfn "%s" usage
        1
    else
        let fileName    = argv.[0]
        let streamName  = fileName |> Path.GetFileNameWithoutExtension
        let folder      =
            match fileName |> Path.GetDirectoryName with
            | "" -> Directory.GetCurrentDirectory ()
            | x -> x

        let ext         =
            match fileName |> Path.GetExtension with
            | "" -> ".fsidl"
            | x -> x

        let options = if argv.Length > 1 then argv.[1..] else [||]
        printfn "Options: %A" options
        try
            let results =
                match options with
                | [||] -> []
                | _ -> (argParser.Parse options).GetResult <@Import_Directory@>

            let files =
                results
                |> List.cons folder
                |> getFilesInFolders


            if not (checkModuleName streamName)
            then failwith (sprintf "Invalid module name: %s (not respecting module name rules: Start with uppercase and contains letters and digits only)" streamName)

            let rootFileName = folder + "/" + streamName + ext
            let files = files.Add(streamName, rootFileName)
            match IO.File.Exists rootFileName with
            | true ->
                try
                    let stream = IO.File.ReadAllText rootFileName
                    let m, errs = Compiler.parse files streamName stream
                    printfn "%A" errs
                    0
                with e ->
                    printfn "Error: %s" e.Message
                    3
            | false ->
                printfn "Module %s (file %s) does not exist" streamName rootFileName
                2

        with e ->
            printfn "%s" e.Message
            1

