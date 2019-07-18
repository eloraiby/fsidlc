// Parser AST
module PAst

type Position
    = { streamName  : string
        line        : int
        column      : int
        cursor      : int }


type Decl
    = DeclImport        of string * Position
    | DeclInterface     of (string * Position) * members: Member list
    | DeclObject        of (string * Position) * ifaces: (string * Position) list * members: Member list
    | DeclStruct        of (string * Position) * members: Member list
    | DeclUnion         of (string * Position) * UnionCase list
    | DeclEnum          of (string * Position) * (string * Position * int) list
    | DeclFunc          of (string * Position) * Ty list * Ty

and UnionCase
    = { name    : string
        pos     : Position
        ty      : Ty }

and Member
    = { name    : string
        pos     : Position
        ty      : Ty }

and Ty
    = TyName    of string * Position
    | TyQName   of string * string * Position
    | TyTuple   of Ty list
    | TyArray   of Ty * int
    | TyVector  of Ty
    | TyFn      of Ty list * Ty

