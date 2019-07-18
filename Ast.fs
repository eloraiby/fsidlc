module Ast

open PAst

type TypeName
    = Simple    of string * Position
    | Qualified of string * string * Position

type Ty
    = TyName    of string * Position
    | TyQName   of string * string * Position
    | TyTuple   of Ty list
    | TyArray   of Ty * uint32
    | TyVector  of Ty
    | TyList    of Ty
    | TyFnSig   of Ty * Ty

type Member
    = { name    : string
        pos     : Position
        ty      : Ty }

type Interface
    = { name    : string
        pos     : Position
        members : Member list }

type Obj
    = { name    : string
        pos     : Position
        ifaces  : TypeName list
        members : Member list }

type Struct
    = { name    : string
        pos     : Position
        members : Member list }

type Union
    = { name    : string
        pos     : Position
        cases   : (string * Ty * Position) list}

type Enum
    = { name    : string
        pos     : Position
        cases   : (string * int64 * Position) list }

type Decl
    = DeclInterface of Interface
    | DeclObj       of Obj
    | DeclStruct    of Struct
    | DeclImport    of Module
    | DeclUnion     of Union
    | DeclEnum      of Enum
    | DeclFunc      of Member

and Module
    = { name    : string
        decls   : Decl list }
