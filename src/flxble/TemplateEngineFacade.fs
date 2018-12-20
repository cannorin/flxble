module Flxble.Templating.Facades
open System

[<RequireQualifiedAccess>]
type TEValue<'FunctionRepresentation> =
  | String of string
  | Integer of int
  | Float   of float
  | Boolean of bool
  | Date of DateTime
  | Function of 'FunctionRepresentation
  | Array of TEValue<'FunctionRepresentation> seq
  | Dictionary of dict<string, TEValue<'FunctionRepresentation>>

type TEContext<'f> = TEContext of dict<string, TEValue<'f>>
