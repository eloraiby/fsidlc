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
