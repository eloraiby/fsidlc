module CBackend

open System.Text

let private append (sb: StringBuilder) (s: string) = sb.Append s |> ignore

type ISerialize =
    abstract toString: System.Text.StringBuilder -> System.Text.StringBuilder

type Expr
    = Field     of {| expr: Expr;  fieldName: string |}
    | Var       of string
    | Add       of Expr * Expr
    | Sub       of Expr * Expr
    | Mul       of Expr * Expr
    | Div       of Expr * Expr
    | Mod       of Expr * Expr
    | Eq        of Expr * Expr
    | LT        of Expr * Expr
    | GT        of Expr * Expr
    | LEq       of Expr * Expr
    | GEq       of Expr * Expr
    | Not       of Expr
    | Or        of Expr * Expr
    | And       of Expr * Expr
    | Xor       of Expr * Expr
    | BNot      of Expr
    | BOr       of Expr * Expr
    | BAnd      of Expr * Expr
    | Bool      of bool
    | Char      of char
    | UInt8     of uint8
    | UInt16    of uint16
    | UInt32    of uint32
    | UInt64    of uint64
    | SInt8     of int8
    | SInt16    of int16
    | SInt32    of int32
    | SInt64    of int64
    | Real32    of single
    | Real64    of float
    | String    of string
    | Call      of {| e: Expr; args: Expr[] |}
    | DRef      of Expr
    | Getter    of {| e: Expr; index: Expr |}
    | Inc       of Expr
    | Dec       of Expr
    | PostInc   of Expr
    | PostDec   of Expr
with
    member private x.isOp =
        match x with
        | Not       _ -> true
        | BNot      _ -> true
        | Add       _ -> true
        | Sub       _ -> true
        | Mul       _ -> true
        | Div       _ -> true
        | Mod       _ -> true
        | Eq        _ -> true
        | LT        _ -> true
        | GT        _ -> true
        | LEq       _ -> true
        | GEq       _ -> true
        | Or        _ -> true
        | And       _ -> true
        | Xor       _ -> true
        | BOr       _ -> true
        | BAnd      _ -> true
        | Inc       _ -> true
        | Dec       _ -> true
        | PostInc   _ -> true
        | PostDec   _ -> true
        | _ -> false

    member x.paren =
        if x.isOp
        then sprintf "(%s)" x.asString
        else sprintf "%s" x.asString

    member x.asString =
        match x with
        | Var   x -> x
        | Bool  b -> if b then "true" else "false"
        | Char  c -> sprintf "'%c'" c
        | UInt8     n -> sprintf "%d" n
        | UInt16    n -> sprintf "%d" n
        | UInt32    n -> sprintf "%d" n
        | UInt64    n -> sprintf "%dL" n
        | SInt8     n -> sprintf "%d" n
        | SInt16    n -> sprintf "%d" n
        | SInt32    n -> sprintf "%d" n
        | SInt64    n -> sprintf "%dL" n
        | Real32    n -> sprintf "%f" n
        | Real64    n -> sprintf "%ff" n
        | String    s -> sprintf "\"%s\"" s
        | Field     f ->
            match f.expr with
            | Var name  -> sprintf "%s.%s" name f.fieldName
            | _         -> sprintf "(%s).%s" f.expr.asString f.fieldName
        | Call      f ->
            let sb = StringBuilder()
            let args =
                f.args
                |> Array.iteri(fun i x ->
                    if i = 0
                    then x.asString |> append sb
                    else sprintf ", %s" (x.asString) |> append sb)

            sprintf "%s(%s)" f.e.asString (sb.ToString())
        | DRef      e -> sprintf "*%s" e.asString
        | Getter    g -> sprintf "%s[%s]" g.e.asString (g.index.asString)
        | Not       e           -> sprintf "~%s"        e.paren
        | BNot      e           -> sprintf "!%s"        e.paren
        | Add       (e0, e1)    -> sprintf "%s + %s"    e0.paren e1.paren
        | Sub       (e0, e1)    -> sprintf "%s - %s"    e0.paren e1.paren
        | Mul       (e0, e1)    -> sprintf "%s * %s"    e0.paren e1.paren
        | Div       (e0, e1)    -> sprintf "%s / %s"    e0.paren e1.paren
        | Mod       (e0, e1)    -> sprintf "%s %% %s"   e0.paren e1.paren
        | Eq        (e0, e1)    -> sprintf "%s == %s"   e0.paren e1.paren
        | LT        (e0, e1)    -> sprintf "%s < %s"    e0.paren e1.paren
        | GT        (e0, e1)    -> sprintf "%s > %s"    e0.paren e1.paren
        | LEq       (e0, e1)    -> sprintf "%s <= %s"   e0.paren e1.paren
        | GEq       (e0, e1)    -> sprintf "%s >= %s"   e0.paren e1.paren
        | Or        (e0, e1)    -> sprintf "%s | %s"    e0.paren e1.paren
        | And       (e0, e1)    -> sprintf "%s & %s"    e0.paren e1.paren
        | Xor       (e0, e1)    -> sprintf "%s ^ %s"    e0.paren e1.paren
        | BOr       (e0, e1)    -> sprintf "%s || %s"   e0.paren e1.paren
        | BAnd      (e0, e1)    -> sprintf "%s && %s"   e0.paren e1.paren
        | Inc       e           -> sprintf "++%s"       e.paren
        | Dec       e           -> sprintf "--%s"       e.paren
        | PostInc   e           -> sprintf "%s++"       e.paren
        | PostDec   e           -> sprintf "%s--"       e.paren

type Var
    = { varName :   string
        varTy   :   string
        varVal  :   Expr option }
with
    member x.asString =
        match x.varVal with
        | None      -> sprintf "%s %s" x.varTy x.varName
        | Some e    -> sprintf "%s %s = %s" x.varTy x.varName e.asString

type ForHeader
    = { start   : Var option
        stopCond: Expr option
        step    : Expr option }
with
    member x.asString =

        let start =
            match x.start with
            | Some s -> s.asString
            | _ -> ""

        let stop =
            match x.stopCond with
            | Some s -> s.asString
            | _ -> ""

        let step =
            match x.step with
            | Some s -> s.asString
            | _ -> ""
        sprintf "%s; %s; %s" start stop step

type Stmt
    = Assign    of {| varName : string; exp: Expr |}
    | If        of {| cond: Expr; t: Stmt[]; e: Stmt[] |}
    | Return    of Expr
    | Break
    | For       of ForHeader * code: Stmt[]
    | While     of cond: Expr * code: Stmt[]
    | Do        of code: Stmt[] * cond: Expr
    | StmtExp   of Expr     // yes! expressions are allowed, call/inc/dec make sense, the rest is just stupid
    | Switch    of Expr * {| case: string; code: Stmt[] |}[]
    | Var       of Var

type Ty
    = TyName        of string
    | TyFn          of {| name: string; retTy: Ty; args: Ty[] |}
    | TyRef         of RefTy
    | TyConstRef    of RefTy
    | TyVoid
    | TyBool
    | TyChar
    | TyUInt8
    | TyUInt16
    | TyUInt32
    | TyUInt64
    | TySInt8
    | TySInt16
    | TySInt32
    | TySInt64
    | TyReal32
    | TyReal64
    | TyString

and RefTy
    = TyPtr     of string
    | TyFixed   of string * int

type RefTy
with
    member x.asString =
        match x with
        | TyPtr     s -> sprintf "%s*" s
        | TyFixed   (s, i) -> sprintf "%s[%d]" s i

type Ty
with
    member x.asString =
        match x with
        | TyName        s -> s
        | TyFn          fn ->
            let sb = StringBuilder()
            let args =
                fn.args
                |> Array.iteri(fun i x ->
                    if i = 0
                    then x.asString |> append sb
                    else sprintf ", %s" (x.asString) |> append sb)
            sprintf "%s(*%s)(%s)" fn.retTy.asString fn.name (args.ToString())
        | TyRef         rt -> rt.asString
        | TyConstRef    rt -> sprintf "const %s" rt.asString
        | TyVoid        -> "Void"
        | TyBool        -> "Bool"
        | TyChar        -> "Char"
        | TyUInt8       -> "UInt8"
        | TyUInt16      -> "UInt16"
        | TyUInt32      -> "UInt32"
        | TyUInt64      -> "UInt64"
        | TySInt8       -> "SInt8"
        | TySInt16      -> "SInt16"
        | TySInt32      -> "SInt32"
        | TySInt64      -> "SInt64"
        | TyReal32      -> "Real32"
        | TyReal64      -> "Real64"
        | TyString      -> "String"

let dumpUS (ty: string) (name: string, fields: {| fieldName: string; ty: Ty |} []) =
    let sb = StringBuilder()
    sprintf "%s %s {\n" ty name |> append sb
    for f in fields do
        sprintf "    %s %s;\n" f.ty.asString f.fieldName |> append sb
    sprintf "};\n" |> append sb
    sb.ToString()

let tab (s: string) =
    s.Split '\n'
    |> Array.map(sprintf "    %s\n")
    |> Array.map(fun s -> s.ToCharArray())
    |> Array.concat
    |> System.String

type Stmt
with
    static member writeBlock(block: Stmt[]) =
        let sb = StringBuilder()
        block
        |> Array.iter(fun s -> sprintf "%s\n" s.asString |> sb.Append |> ignore)
        sb.ToString()

    member x.asString =
        match x with
        | Assign    ve  -> sprintf "%s = %s;" ve.varName ve.exp.asString
        | If        ite ->
            match ite.e with
            | [||] ->
                let t = ite.t |> Stmt.writeBlock |> tab
                sprintf "if( %s ) {\n%s}" ite.cond.asString t
            | _ ->
                let t = ite.t |> Stmt.writeBlock  |> tab
                let e = ite.e |> Stmt.writeBlock  |> tab

                sprintf "if( %s ) {\n%s\n} else {\n%s\n}" ite.cond.asString t e

        | Return    r   -> sprintf "return %s;" r.asString
        | Break         -> "break;"
        | For       (f, c) ->
            let code = c |> Stmt.writeBlock |> tab
            sprintf "for( %s ) {\n%s}" f.asString code

        | While     (cond, code) ->
            let cb = code |> Stmt.writeBlock |> tab
            sprintf "while( %s ) {\n%s\n}" cond.asString cb

        | Do        (code, cond) ->
            let cb = code |> Stmt.writeBlock |> tab
            sprintf "do {\n%s\n} while(%s);" cb cond.asString

        | StmtExp   e   -> sprintf "%s;" e.asString     // yes! expressions are allowed, call/inc/dec make sense, the rest is just stupid
        | Switch    (e, cases) ->
            let sb = StringBuilder()
            let cases
                = cases
                |> Array.iter(fun cc ->
                    let cb = cc.code |> Stmt.writeBlock |> tab
                    sprintf "case %s: {\n%s\n}" cc.case cb |> sb.Append |> ignore)
            sprintf "switch( %s ) {\n%s\n}" e.asString (sb.ToString() |> tab)
        | Var       v -> sprintf "%s;" v.asString

type Decl
    = Struct    of name: string * {| fieldName: string; ty: Ty |} []
    | Union     of name: string * {| fieldName: string; ty: Ty |} []
    | Enum      of name: string * {| caseName: string; value: int option |} []
    | Alias     of name: string * alias: string
    | FuncDecl  of name: string * args: (string * Ty)[] * ret: Ty
    | FuncImpl  of name: string * args: (string * Ty)[] * ret: Ty * code: Stmt[]
with
    member x.asString =
        match x with
        | Struct    (s, fields)     -> dumpUS "struct" (s, fields)
        | Union     (u, cases)      -> dumpUS "union" (u, cases)
        | Enum      (e, cases)      ->
            let sb = StringBuilder()
            sprintf "enum %s {\n" e |> append sb
            for c in cases do
                match c.value with
                | Some v -> sprintf "    %s     = %d,\n" c.caseName v |> append sb
                | None   -> sprintf "    %s,\n" c.caseName |> append sb
            sprintf "};\n" |> append sb
            sb.ToString()

        | Alias     (orig, alias)   -> sprintf "typedef %s %s;" orig alias
        | FuncDecl  (s, args, ret)  ->
            let sb = StringBuilder()
            let args =
                args
                |> Array.iteri(fun i (s, ty) ->
                    if i = 0
                    then sprintf "%s %s" ty.asString s |> append sb
                    else sprintf ", %s %s" ty.asString s |> append sb)
            sprintf "%s %s(%s);" ret.asString s (args.ToString())
        | FuncImpl  (s, args, ret, code) -> failwith "TODO"

