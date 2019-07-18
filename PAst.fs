// Parser AST
module PAst

type Decl
    = DeclImport        of string
    | DeclInterface     of string * members: Member list
    | DeclObject        of string * ifaces: string list * members: Member list
    | DeclStruct        of string * members: Member list
    | DeclUnion         of string * UnionCase list
    | DeclEnum          of string * (string * int) list
    | FuncDecl          of Ty * Ty

and UnionCase
    = { name    : string
        ty      : Ty }

and Member
    = { name    : string
        ty      : Ty }

and Ty
    = TyName    of string
    | TyTuple   of Ty list
    | TyArray   of Ty * int
    | TyVector  of Ty
    | TyFn      of Ty * Ty

