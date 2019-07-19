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
module Ast

type Position
    = { streamName  : string
        line        : int
        column      : int
        cursor      : int }

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
    | TyFnSig   of Ty list * Ty

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

type UnionCase
    = { name    : string
        pos     : Position
        ty      : Ty }

type Union
    = { name    : string
        pos     : Position
        cases   : UnionCase list}

type Enum
    = { name    : string
        pos     : Position
        cases   : (string * Position * int64) list }

type Import
    = Read          of string * Position
    | Mod           of Module

and Decl
    = DeclInterface of Interface
    | DeclObj       of Obj
    | DeclStruct    of Struct
    | DeclImport    of Import
    | DeclUnion     of Union
    | DeclEnum      of Enum
    | DeclFunc      of (string * Position) * Ty list * Ty

and Module
    = { name    : string
        decls   : Decl list }
