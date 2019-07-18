open System
open FSharp.Text.Lexing

let parse schizo =
    let lexbuf = LexBuffer<char>.FromString schizo
    let res = Parser.start (Lexer.read) lexbuf
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
    let result = parse test
    printfn "%A" result
    0 // return an integer exit code
