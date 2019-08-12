module Backend

open Ast

let reorder (m: Module) =

    let decls   = m.decls

    let refTys
        = m.decls
        |> List.filter (
            function
            | Decl.DeclObj _
            | Decl.DeclInterface _ -> true
            | _ -> false)
    let vTys
        = m.decls
        |> List.filter (
            function
            | Decl.DeclObj _
            | Decl.DeclInterface _ -> false
            | _ -> true)


    failwith "todo"