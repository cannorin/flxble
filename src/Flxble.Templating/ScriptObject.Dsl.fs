module Flxble.Templating.ScriptObjectHelper

open Flxble.Templating.SyntaxTree
open DataTypeExtra

let inline bool' b = Bool b
let inline int' i = Int i
let inline float' f = Float f
let inline string' s = String s
let inline date' d = Date d
let inline timespan' s = TimeSpan s
let inline array' xs = Array xs
let inline record' mappings = mappings |> Record
let inline function' argCount f = Function (argCount, StructuralFunction f)
let null' = Null ENull
let inline nullWithMsg' msg = Null (EValue msg)