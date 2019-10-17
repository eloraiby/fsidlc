//
// IDLC - Interface Definition Language Compiler
//
// Copyright (C) 2018  Wael El Oraiby
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

// TODO: object should be able to refer to themselves
// TODO: add reference types
// TODO: for data types, only allow usage of type in a preceeding declaration if the type is a reference type

module TypeValidator
open Ast
open System

type AccessTy =
    | ValueType
    | RefType

type TypeEntry
    = { fullName    : string * string
        access      : AccessTy
        pos         : Position
        referredTypes : Set<string * string> }
with
    member x.orig   = x.fullName |> fst

type Context
    = { qTypeMap    : Map<string * string, TypeEntry> }
with
    member x.add (modName: string, tyName: string, access: AccessTy, pos: Position, referred: Set<string * string>) =
        { x with
            qTypeMap    = x.qTypeMap.Add((modName, tyName), { fullName = modName, tyName; access = access; pos = pos; referredTypes = referred}) }

    member x.tryFindQ (ty: string * string) = x.qTypeMap.TryFind ty

    member x.import (other: Context) =
        { x with
            qTypeMap =
                other.qTypeMap
                |> Map.fold(fun (state: Map<_, _>) k v ->
                    state.Add (k, v)) x.qTypeMap }

    static member create() =
        { Context.qTypeMap = Map.empty }

let rec checkDuplicateMembers (pathEnv: Map<string, string>) modName (decl: Ast.Decl) =
    let checkInMemberList (modName, tyName) (ml: Member list) =
        ml
        |> List.fold(fun (id2Member: Map<string, Member>, errList: string list) m ->
            match id2Member.TryFind m.name with
            | Some s ->
                let err = sprintf "in module %s, @%A: in type %s, %s was defined before @%A" pathEnv.[modName] m.pos tyName m.name s.pos
                id2Member, err :: errList
            | None -> id2Member.Add(m.name, m), errList) (Map.empty, [])
        |> snd

    let checkInUnionCaseList (modName, tyName) (ml: UnionCase list) =
        ml
        |> List.fold(fun (id2Member: Map<string, string * Ty * Position>, errList: string list) uc ->
            let name = uc.name
            let t = uc.ty
            let p = uc.pos
            match id2Member.TryFind name with
            | Some (_, _, sPos) ->
                let err = sprintf "in module %s, @%A: in type %s, %s was defined before @%A" pathEnv.[modName] p tyName name sPos
                id2Member, err :: errList
            | None -> id2Member.Add(name, (name, t, p)), errList) (Map.empty, [])
        |> snd

    let checkInEnumCaseList (modName, tyName) (ml: (string * Position * int64) list) =
        ml
        |> List.fold(fun (id2Member: Map<string, string * Position * int64>, errList: string list) (name, t, p) ->
            match id2Member.TryFind name with
            | Some (_, _, sPos) ->
                let err = sprintf "in module %s, @%A: in type %s, %s was defined before @%A" pathEnv.[modName] p tyName name sPos
                id2Member, err :: errList
            | None -> id2Member.Add(name, (name, t, p)), errList) (Map.empty, [])
        |> snd

    match decl with
    | Decl.DeclInterface { Interface.name = name; members = members }
    | Decl.DeclStruct   { Struct.name = name; members = members }
    | Decl.DeclObj      { Obj.name = name; members = members }      -> checkInMemberList (modName, name) members
    | Decl.DeclUnion    { Union.name = name; cases = cases }        -> checkInUnionCaseList (modName, name) cases
    | Decl.DeclEnum     { Enum.name = name; cases = cases}          -> checkInEnumCaseList (modName, name) cases
    | Decl.DeclImport   (Mod m) ->
        m.decls
        |> List.map (checkDuplicateMembers pathEnv modName)
        |> List.concat
    | Decl.DeclImport   (Read (m, p)) ->
        let err = sprintf "in module %s, @%A: module %s is not read!" modName p m
        [err]
    | Decl.DeclFunc     _ -> []

let rec checkDuplicateTypes (pathEnv: Map<string, string>) (m: Module) : string list =
    // direct duplicate types errors
    let directErr
        = m.decls
        |> List.filter(fun decl ->
            match decl with
            | Decl.DeclFunc _
            | Decl.DeclImport _         -> false
            | _ -> true )
        |> List.fold(fun (typeMap: Map<string, Position>, errList: string list) (decl: Decl) ->
            let name, pos =
                match decl with
                | Decl.DeclEnum e       -> e.name, e.pos
                | Decl.DeclInterface i  -> i.name, i.pos
                | Decl.DeclObj o        -> o.name, o.pos
                | Decl.DeclStruct s     -> s.name, s.pos
                | Decl.DeclUnion u      -> u.name, u.pos
                | _                     -> failwith "impossible case"
            match typeMap.TryFind name with
            | Some p   -> typeMap, (sprintf "in module %s, type %s was already defined @%A" pathEnv.[m.name] name p) :: errList
            | _        -> (typeMap.Add(name, pos)), errList) (Map.empty, [])
        |> snd

    // check for nested modules errors
    let nestedErrors
        = m.decls
        |> List.map(fun decl ->
            match decl with
            | Decl.DeclImport (Mod m) -> checkDuplicateTypes pathEnv m
            | Decl.DeclImport (Read (f, p)) ->
                let err = sprintf "in module %s, @%A: module %s is not read!" pathEnv.[m.name] p f
                [err]
            | _ -> [])
        |> List.filter(fun l ->
            match l with
            | [] -> false
            | _ -> true)
        |> List.concat

    directErr :: nestedErrors :: []
    |> List.concat

let checkDuplicateFunctions (pathEnv: Map<string, string>) (m: Module) : string list =
    let funcs
        = m.decls
        |> List.filter(fun decl ->
            match decl with
            | Decl.DeclFunc _ -> true
            | _ -> false)
        |> List.map(fun decl ->
            match decl with
            | Decl.DeclFunc (f, a) -> f, a
            | _ -> failwith "impossible")

    // no overloads are supported!!!!
    let _, errList
        = funcs
        |> List.fold(fun (fMap: Map<string, Position * Ty>, errList: string list) f ->
            let ((name, pos), args) = f
            match fMap.TryFind name with
            | Some (pos, _) -> fMap, (sprintf "in module %s, function %s was already defined @%A" pathEnv.[m.name] name pos) :: errList
            | _ -> fMap.Add (name, (pos, args)), errList) (Map.empty, [])
    errList

let rec extractTy (ty: Ty) =
    match ty with
    | TyArray (x, _)
    | TyVector x
    | TyList x ->
        match x with
        | TyArray _
        | TyList _ -> extractTy x
        | _ -> x
    | _ -> ty

let rec validateType (pathEnv: Map<string, string>) (modName: string) (ctx: Context) (ty: Ty) : string list =
    match extractTy ty with
    | TyUnit
    | TyBool
    | TyChar
    | TyU8
    | TyU16
    | TyU32
    | TyU64
    | TyS8
    | TyS16
    | TyS32
    | TyS64
    | TyF32
    | TyF64 -> []
    | TyName (n, p) ->
        match ctx.tryFindQ (modName, n) with
        | Some x ->
            match x.access, x.referredTypes.Contains (modName, n) with
            | ValueType, true -> (sprintf "in module %s, type %s @%A refers to itself" pathEnv.[modName] n p) :: []
            | _ -> []
        | None   -> (sprintf "in module %s, undeclared type %s @%A" pathEnv.[modName] n p) :: []
    | TyQName (m, n, p) ->
        match ctx.tryFindQ (m, n) with
        | Some x ->
            match x.access, x.referredTypes.Contains (m, n) with
            | ValueType, true -> (sprintf "in module %s, type %s @%A refers to itself" pathEnv.[modName] n p) :: []
            | _ -> []
        | None   -> (sprintf "in module %s, undeclared type %s @%A" pathEnv.[modName] n p) :: []
    | TyTuple tys ->
        tys
        |> List.map (fun ty -> ty |> extractTy |> validateType pathEnv modName ctx)
        |> List.concat
    | TyFnSig (arg, ret) ->
        let argRes
            = arg
            |> validateType pathEnv modName ctx
        let retRes
            = ret
            |> validateType pathEnv modName ctx
        argRes :: retRes :: []
        |> List.concat
    | _ -> failwith "impossible type validation condition"

let rec extractTypeList (modName: string) (ty: Ty) =
    match extractTy ty with
    | TyUnit
    | TyBool
    | TyChar
    | TyU8
    | TyU16
    | TyU32
    | TyU64
    | TyS8
    | TyS16
    | TyS32
    | TyS64
    | TyF32
    | TyF64 -> []
    | Ty.TyName (x, p) -> [modName, x]
    | Ty.TyQName (x, y, p) -> [x, y]
    | Ty.TyTuple tys ->
        tys
        |> List.map (extractTypeList modName)
        |> List.concat
    | Ty.TyFnSig (arg, ret) ->
        let argRes
            = arg
            |> extractTypeList modName
        let retRes
            = ret
            |> extractTypeList modName
        argRes :: retRes :: []
        |> List.concat
    | _ -> failwith "impossible type extraction condition"

let referredTypes (modName: string) (decl: Decl) : Set<string * string> =
    match decl with
    | DeclInterface i ->
        i.members
        |> List.map(fun m -> extractTypeList modName m.ty)
        |> List.concat
        |> Set.ofList

    | DeclObj       o ->
        let ms
            = o.members
            |> List.map(fun m -> extractTypeList modName m.ty)
            |> List.concat
        let ifs
            = o.ifaces
            |> List.map (fun t ->
                match t with
                | Simple (s, _) -> modName, s
                | Qualified (m, s, _) -> m, s)
        ms :: ifs :: []
        |> List.concat
        |> Set.ofList

    | DeclStruct    s ->
        s.members
        |> List.map(fun m -> extractTypeList modName m.ty)
        |> List.concat
        |> Set.ofList
    | DeclImport    _ -> failwith "imports cannot refer types"

    | DeclUnion     u ->
        u.cases
        |> List.map(fun c -> extractTypeList modName c.ty)
        |> List.concat
        |> Set.ofList

    | DeclEnum      _ -> Set.empty
    | DeclFunc      (_, f) ->
        extractTypeList modName f
        |> Set.ofList

let validateModule (pathEnv: Map<string, string>) (ctx: Context, m: Module) : string list =
    m.decls
    |> List.map(fun decl ->
        match decl with
        | Decl.DeclEnum e -> []
        | Decl.DeclFunc ((name, p), t) ->
            let tRes
                = t
                |> validateType pathEnv m.name ctx
            tRes :: []
            |> List.concat
        | Decl.DeclImport _ -> []
        | Decl.DeclInterface i ->
            i.members
            |> List.map (fun me -> validateType pathEnv m.name ctx me.ty)
            |> List.concat
        | Decl.DeclObj o ->
            let oIfaceList
                = o.ifaces
                |> List.map(fun t ->
                    match t with
                    | TypeName.Qualified (n0, n1, p) -> Ty.TyQName (n0, n1, p)
                    | TypeName.Simple (n, p) -> Ty.TyName (n, p))
                |> List.map(validateType pathEnv m.name ctx)
                |> List.concat
            let ms
                = o.members
                |> List.map (fun me -> validateType pathEnv m.name ctx me.ty)
                |> List.concat
            oIfaceList :: ms :: []
            |> List.concat
        | Decl.DeclStruct s ->
            s.members
            |> List.map (fun me -> validateType pathEnv m.name ctx me.ty)
            |> List.concat
        | DeclUnion u ->
            u.cases
            |> List.map(fun uc -> validateType pathEnv m.name ctx uc.ty)
            |> List.concat)
    |> List.concat

let rec buildModuleContext (pathEnv: Map<string, string>) (m: Module) : Context * string list =
    // 1. build the nested context (imported modules)
    let nestedCtx, errList =
        m.decls
        |> List.filter(fun x ->
            match x with
            | Decl.DeclImport m -> true
            | _ -> false)
        |> List.map(fun x ->
            match x with
            | Decl.DeclImport (Mod m) -> buildModuleContext pathEnv m
            | Decl.DeclImport (Read (f, p)) ->
                let err = sprintf "in module %s, @%A: module %s is not read!" pathEnv.[m.name] p f
                failwith err
            | _ -> failwith "getModuleContext: impossible case")
        |> List.fold(fun (state: Context, errList: string list) (ctx, prevList) ->
            let newErrList
                = prevList :: errList :: []
                |> List.concat
            state.import ctx, newErrList) (Context.create(), [])

    // 2. add the current module's type declarations to the nested context
    let newContext
        = m.decls
        |> List.fold (fun (ctx: Context) decl ->
            match decl with
            | Decl.DeclEnum { name = n; pos = p }
            | Decl.DeclStruct { name = n; pos = p }
            | Decl.DeclUnion { name = n; pos = p } -> ctx.add(m.name, n, ValueType, p, referredTypes m.name decl)
            | Decl.DeclInterface { name = n; pos = p }
            | Decl.DeclObj { name = n; pos = p } -> ctx.add(m.name, n, RefType, p, referredTypes m.name decl)
            | _ -> ctx) nestedCtx

    // 3. validate the current module types/declaration
    let modErrs = validateModule pathEnv (newContext, m)

    let errors
        = errList :: modErrs :: []
        |> List.concat
    newContext, errors

let validate (pathEnv: Map<string, string>) (m: Module) =
    // check duplicates
    let dupMembers
        = m.decls
        |> List.map (checkDuplicateMembers pathEnv m.name)
        |> List.concat

    let dupTypes = checkDuplicateTypes pathEnv m

    let dupFuncs = checkDuplicateFunctions pathEnv m

    // sum them
    let dupErrors
        = dupFuncs :: dupTypes :: dupMembers :: []
        |> List.concat

    let ctx, errors = buildModuleContext pathEnv m
    //printfn "final: %A" ctx
    dupErrors :: errors :: []
    |> List.concat

let rec openImports (parseInternal: string -> string -> Decl list) (fileEnv: Map<string, string>) (env: Map<string, Module>) (scope: Set<string>) (decls: Decl list) =
    decls
    |> List.fold (fun (decls: Decl list, env: Map<string, Module>, s: Set<string>) d ->
        match d with
        | DeclImport (Read (x, p)) ->
            try (
                if scope.Contains x
                then failwith (sprintf "recusive module inclusion!!!: included from one of these %A" s)
                let stream = IO.File.ReadAllText (fileEnv.[x])
                let s = s.Add x
                let ds, e, s
                    = parseInternal x stream
                    |> openImports parseInternal fileEnv env scope
                let m
                    = { Module.name   = x
                        decls         = ds }
                let errs = validate fileEnv m
                match errs with
                | [] -> (DeclImport (Mod m)) :: decls, e.Add(x, m), s
                | _ ->
                    printfn "%A" errs
                    failwith "terminal erros while importing")
            with e ->
                failwith (sprintf "@%A: trying to import '%s': %s" p x e.Message)
        | _ -> (d :: decls), env, s
    ) ([], env, scope)