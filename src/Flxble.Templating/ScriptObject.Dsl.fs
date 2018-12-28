module Flxble.Templating.ScriptObject.Dsl

open Flxble.Templating
open SyntaxTree
open DataTypeExtra

let inline bool' b = Bool b
let inline int' i = Int i
let inline float' f = Float f
let inline string' s = String s
let inline date' d = Date d
let inline timespan' s = TimeSpan s
let inline array' xs = Array xs
let inline record' mappings = mappings |> Map.ofSeq |> Record
let inline function' argCount f = Function (argCount, StructuralFunction f)