module Flxble.Toml

open System

[<RequireQualifiedAccess; Struct>]
type TomlType = Bool | Int | Float | String | Date | Array

[<RequireQualifiedAccess>]
type NodeArray =
  | Bools   of bool   list
  | Ints    of int    list
  | Floats  of float  list
  | Strings of string list
  | Dates   of DateTime list
  | Arrays  of NodeArray list 
  override __.ToString () =
    let inline f xs = List.map string xs |> String.concat ", "
    match __ with
      | Bools   bs -> f bs 
      | Ints    is -> f is 
      | Floats  fs -> f fs
      | Strings ss -> f ss
      | Dates   ds -> f ds
      | Arrays ars -> f ars
  member this.EraceType() =
    match this with
      | Bools   xs -> xs |> List.map box |> List.toArray |> box
      | Ints    xs -> xs |> List.map box |> List.toArray |> box
      | Floats  xs -> xs |> List.map box |> List.toArray |> box
      | Strings xs -> xs |> List.map box |> List.toArray |> box
      | Dates   xs -> xs |> List.map box |> List.toArray |> box
      | Arrays  xss ->
        xss |> List.map (fun x -> x.EraceType()) |> List.toArray |> box
  member this.ItemType =
    match this with
      | Bools   _ -> TomlType.Bool
      | Ints    _ -> TomlType.Int
      | Floats  _ -> TomlType.Float
      | Strings _ -> TomlType.String
      | Dates   _ -> TomlType.Date
      | Arrays  _ -> TomlType.Array

[<RequireQualifiedAccess>]
type TomlValue =
  | Bool   of b:bool
  | Int    of i:int
  | Float  of f:float
  | String of s:string
  | Date   of d:DateTime
  | Array  of xs:NodeArray
  override __.ToString () =
    match __ with
      | Bool   b -> sprintf "%b"   b
      | Int    i -> sprintf "%i"   i
      | Float  f -> sprintf "%f"   f
      | String s -> sprintf "%s"   s
      | Date   d -> sprintf "%A"   d
      | Array  a -> sprintf "[%O]" a
  member this.EraceType() =
    match this with
      | Bool   x -> box x
      | Int    x -> box x
      | Float  x -> box x
      | String x -> box x
      | Date   x -> box x
      | Array  xs -> xs.EraceType()
  member this.Type =
    match this with
      | Bool   _ -> TomlType.Bool
      | Int    _ -> TomlType.Int
      | Float  _ -> TomlType.Float
      | String _ -> TomlType.String
      | Date   _ -> TomlType.Date
      | Array  _ -> TomlType.Array

type Token = KeyGroup of string list | KeyValue of string * TomlValue

open FParsec

module Parser =
  open FParsecUtils

  let spc = many (anyOf [' '; '\t']) |>> ignore
  let lexeme p = p .>> spc
  let comment = pchar '#' .>>. restOfLine false |>> ignore
  let line p = p .>> lexeme newline
  let blanks = lexeme (skipMany ((comment <|> spc) .>>? lexeme newline))

  let ls s = lexeme <| pstring s
  let quote   = ls "\""
  let quote3  = ls "\"\"\""
  let lquote  = ls "'"
  let lquote3 = ls "'''"
  let lbrace  = pstring "[" .>> spaces
  let rbrace  = pstring "]" .>> spaces
  let comma   = pstring "," .>> spaces
  let period  = ls "."
  let equal   = ls "="
  let ptrue   = ls "true"  >>% true
  let pfalse  = ls "false" >>% false

  let normalizeString isLiteral str =
    let lines = str |> String.splitSeq ["\r\n"; "\r"; "\n"]
    if lines.Length < 2 then str
    else
      let skipFirstLine = lines.[0] |> String.IsNullOrWhiteSpace 
      if isLiteral then
        if skipFirstLine then
          let firstNewline =
            max (str |> String.findIndex '\n') (str |> String.findIndex '\r')
          str |> String.skip (firstNewline + 1)
        else str
      else
        let lines =
          if skipFirstLine then
            lines |> Array.skip 1
          else lines
        let mutable skip = false
        seq {
          for line in lines do
            if line |> String.IsNullOrWhiteSpace then
              if not skip then yield line
            else
              let line =
                if skip then line |> String.trimStart else line
              if line |> String.trim |> String.endsWith "\\" then
                skip <- true
                let line = line |> String.removeAfter(line.Length - 1)
                if line |> String.IsNullOrWhiteSpace |> not then
                  yield line
              else
                skip <- false
                yield line
        } |> String.concat Environment.NewLine

  let pbool  = ptrue <|> pfalse <?> "pbool"
  let pstr1  = between quote   quote   (escapedString "\"") <?> "pstr" |>> normalizeString false
  let pstr3  = between quote3  quote3  (escapedString "\"") <?> "pstr" |>> normalizeString false
  let pstr1L = between lquote  lquote  (manySatisfy (fun c -> "'\n\r" |> String.contains c |> not)) <?> "pstr" |>> normalizeString true
  let pstr3L = lquote3 >>? manyCharsTill anyChar lquote3 <?> "pstr"    |>> normalizeString true
  let pstr   = choice [ pstr3; pstr1; pstr3L; pstr1L ]
  let pint   = attempt pint32 <?> "pint"
  let pfloat = attempt pfloat <?> "pfloat"

  
  let pdate  = attempt (spc >>. ISO8601DateTime.pdatetime .>> spc) <?> "pdate" |>> fun d -> d.DateTime

  let parray elem = attempt (between lbrace rbrace (sepBy (elem .>> spaces) comma))
  let pboolarray  = parray pbool  |>> NodeArray.Bools   <?> "pboolarray"
  let pdatearray  = parray pdate  |>> NodeArray.Dates   <?> "pdatearray"
  let pintarray   = parray pint   |>> NodeArray.Ints    <?> "pintarray"
  let pstrarray   = parray pstr   |>> NodeArray.Strings <?> "pstrarray"
  let pfloatarray = parray pfloat |>> NodeArray.Floats  <?> "pfloatarray"
  let rec parrayarray = 
    parray (pboolarray <|> pdatearray <|> pintarray <|> pstrarray <|> pfloatarray) 
    |>> NodeArray.Arrays <?> "parrayarray"

  let value = 
    (pbool       |>> TomlValue.Bool ) <|> 
    (pdate       |>> TomlValue.Date ) <|> 
    (pstr        |>> TomlValue.String)<|> 
    (pint        |>> TomlValue.Int  ) <|> 
    (pfloat      |>> TomlValue.Float) <|> 
    (pboolarray  |>> TomlValue.Array) <|>
    (pdatearray  |>> TomlValue.Array) <|>
    (pintarray   |>> TomlValue.Array) <|>
    (pstrarray   |>> TomlValue.Array) <|>
    (pfloatarray |>> TomlValue.Array) <|>
    (parrayarray |>> TomlValue.Array)
    
  let keyvalue = 
    let key = many1Chars (noneOf " \t\n=")
    lexeme key .>>.? (equal >>? value) |>> KeyValue

  let keygroup = 
    let key = lexeme (many1Chars (noneOf " \t\n]."))
    blanks >>. between lbrace rbrace (sepBy key period) |>> KeyGroup

  let document = blanks >>? many (keygroup <|> keyvalue .>>? blanks)

open Parser
open System.Collections.Generic

exception TomlParseError of msg:string with
  override this.Message = this.msg

/// Represents a TOML document of specific type.
type TomlDocumentOf<'t> = { tomlOfT: Map<string, 't> } with
  member this.Item(key) = this.tomlOfT |> Map.tryFind key
  static member inline (@?) (this: TomlDocumentOf<_>, key: string) =
    this.tomlOfT |> Map.tryFind key 
  static member inline (@.) (this: TomlDocumentOf<_>, key: string) =
    this.tomlOfT
      |> Map.tryFind' key 
      |> ValueOption.defaultWith (fun () ->
        KeyNotFoundException(sprintf "The key '%s' not found." key) |> raise)
  interface IDictionary<string, 't> with
    member this.Item
      with get x = this.tomlOfT.[x]
      and  set _ _ = failwith "TomlDocumentOf<_> is immutable"
    member this.Keys   = (this.tomlOfT :> dict<_, _>).Keys
    member this.Values = (this.tomlOfT :> dict<_, _>).Values
    member this.TryGetValue(l, r) = (this.tomlOfT :> dict<_, _>).TryGetValue(l, &r)
    member this.Contains (x: kvp<_, _>) = (this.tomlOfT :> dict<_, _>).Contains x
    member this.CopyTo (arr, i) = (this.tomlOfT :> dict<_, _>).CopyTo(arr, i)
    member this.ContainsKey k = this.tomlOfT |> Map.containsKey k
    member this.Count = this.tomlOfT.Count
    member this.IsReadOnly = true
    member this.Add _     = failwith "TomlDocumentOf<_> is immutable"
    member this.Add (_,_) = failwith "TomlDocumentOf<_> is immutable"
    member this.Remove (_: string) : bool = failwith "TomlDocumentOf<_> is immutable"
    member this.Remove (_: kvp<string, 't>) : bool = failwith "TomlDocumentOf<_> is immutable"
    member this.Clear ()  = failwith "TomlDocumentOf<_> is immutable"
    member this.GetEnumerator() = (this.tomlOfT :> seq<_>).GetEnumerator()
    member this.GetEnumerator() = (this.tomlOfT :> Collections.IEnumerable).GetEnumerator()

/// Represents a TOML document.
type TomlDocument(toml: Map<string, TomlValue>) = 
  member val toml = toml
  member val bools   = lazy { tomlOfT = toml |> Map.choose (fun _ -> function TomlValue.Bool b -> Some b | _ -> None) }
  member val ints    = lazy { tomlOfT = toml |> Map.choose (fun _ -> function TomlValue.Int i -> Some i | _ -> None) }
  member val floats  = lazy { tomlOfT = toml |> Map.choose (fun _ -> function TomlValue.Float f -> Some f | _ -> None) }
  member val strings = lazy { tomlOfT = toml |> Map.choose (fun _ -> function TomlValue.String s -> Some s | _ -> None) }
  member val dates   = lazy { tomlOfT = toml |> Map.choose (fun _ -> function TomlValue.Date d -> Some d | _ -> None) }
  member val arrays  = lazy { tomlOfT = toml |> Map.choose (fun _ -> function TomlValue.Array xs -> Some xs | _ -> None) }
  member inline this.Item(key) = this.toml |> Map.tryFind key
  static member inline (@?) (this: TomlDocument, key: string) =
    this.toml |> Map.tryFind key 
  static member inline (@.) (this: TomlDocument, key: string) =
    this.toml
      |> Map.tryFind' key 
      |> ValueOption.defaultWith (fun () ->
        KeyNotFoundException(sprintf "The key '%s' not found." key) |> raise)
  interface IDictionary<string, TomlValue> with
    member this.Item
      with get x = this.toml.[x]
      and  set _ _ = failwith "TomlDocument is immutable"
    member this.Keys   = (this.toml :> dict<_, _>).Keys
    member this.Values = (this.toml :> dict<_, _>).Values
    member this.TryGetValue(l, r) = (this.toml :> dict<_, _>).TryGetValue(l, &r)
    member this.Contains (x: kvp<_, _>) = (this.toml :> dict<_, _>).Contains x
    member this.CopyTo (arr, i) = (this.toml :> dict<_, _>).CopyTo(arr, i)
    member this.ContainsKey k = this.toml |> Map.containsKey k
    member this.Count = this.toml.Count
    member this.IsReadOnly = true
    member this.Add _     = failwith "TomlDocument is immutable"
    member this.Add (_,_) = failwith "TomlDocument is immutable"
    member this.Remove (_: string) : bool = failwith "TomlDocument is immutable"
    member this.Remove (_: kvp<string, TomlValue>) : bool = failwith "TomlDocument is immutable"
    member this.Clear ()  = failwith "TomlDocument is immutable"
    member this.GetEnumerator() = (this.toml :> seq<_>).GetEnumerator()
    member this.GetEnumerator() = (this.toml :> Collections.IEnumerable).GetEnumerator()

module TomlDocument =
  let fromTokens tokens =
    let mutable toml = Map.empty
    let mutable currentKeyGroup = ValueNone
    for token in tokens do
      match token with
      | KeyGroup kg -> currentKeyGroup <- ValueSome kg
      | KeyValue (key,value) -> 
        let key = 
          seq {
            if currentKeyGroup.IsSome then
              yield! currentKeyGroup.Value
            yield key
          } |> String.concat "."
        toml <- toml |> Map.add key value
    TomlDocument(toml)
  
  /// Parses a TOML string `text` to a `TomlDocument`.
  let parse text =
    match run document text with
      | Success(tokens,_,_) ->
        fromTokens tokens
      | Failure(msg, _, _) ->
        TomlParseError msg |> raise

#nowarn "0064" // This construct causes code to be less generic than indicated by the type annotations.

[<RequireQualifiedAccess>]
/// Internal utility module.
module TomlUnwrapper =
  [<Struct>]
  type NormalValue = NormalValue

  let inline unwrap (x: ^X) =
    let inline u (_: ^NormalValue) =
      ((^NormalValue or ^X): (static member Unwrap: ^X -> ^X') x)
    u NormalValue
  
  type NormalValue with
    static member inline Unwrap (x: bool) = x
    static member inline Unwrap (x: int) = x
    static member inline Unwrap (x: float) = x
    static member inline Unwrap (x: string) = x
    static member inline Unwrap (x: DateTimeOffset) = x
    static member inline Unwrap (x: _ list) = x |> List.map unwrap

/// Enables lens-like type filtering on a `TomlDocument`.
/// If you have `doc: TomlDocument`, `doc%.ofArray%.ofArray%.ofString` will return
/// a value of `TomlDocumentOf<string list list>`, which contains keys and values
/// of type `string list list`.
module LensLike =
  [<RequireQualifiedAccess>]
  module TomlTypes =
    [<Struct>] type Bool = Bool
    [<Struct>] type Int = Int
    [<Struct>] type Float = Float
    [<Struct>] type String = String
    [<Struct>] type Date = Date
    [<Struct>] type Array = Array
  let ofBool   = TomlTypes.Bool
  let ofInt    = TomlTypes.Int
  let ofFloat  = TomlTypes.Float
  let ofString = TomlTypes.String
  let ofDate   = TomlTypes.Date
  let ofArray  = TomlTypes.Array

  /// Static type filtering operator.
  /// If you have `doc: TomlDocument`, `doc%.ofArray%.ofArray%.ofString` will return
  /// a value of `TomlDocumentOf<string list list>`, which contains keys and values
  /// of type `string list list`.
  let inline (%.) (obj: ^Object) (ty: ^TomlType) =
    (^Object: (static member Of: ^Object * ^TomlType -> ^Filtered) obj,ty)

open LensLike

type TomlArray<'t> = { items: 't list } with
  member inline this.Item i = this.items.[i]
  static member inline Of (arr: TomlArray< ^Item >, ty: ^TomlType) =
    Some { items = arr.items |> List.choose (fun x -> x%.ty) }
  static member inline Unwrap (xs: TomlArray< ^Item >) =
    xs.items |> List.map TomlUnwrapper.unwrap
  interface IEnumerable<'t> with
    member this.GetEnumerator() = (this.items :> seq<_>).GetEnumerator()
    member this.GetEnumerator() = (this.items :> Collections.IEnumerable).GetEnumerator()

type TomlDocument with
  static member inline Of (doc: TomlDocument, TomlTypes.Bool) = doc.bools.Value
  static member inline Of (doc: TomlDocument, TomlTypes.Int) = doc.ints.Value
  static member inline Of (doc: TomlDocument, TomlTypes.Float) = doc.floats.Value
  static member inline Of (doc: TomlDocument, TomlTypes.String) = doc.strings.Value
  static member inline Of (doc: TomlDocument, TomlTypes.Date) = doc.dates.Value
  static member inline Of (doc: TomlDocument, TomlTypes.Array) = doc.arrays.Value

type NodeArray with
  static member inline Unwrap x = x
  static member inline Of (xs: NodeArray, TomlTypes.Bool)   = match xs with Bools xs -> Some xs | _ -> None
  static member inline Of (xs: NodeArray, TomlTypes.Int)    = match xs with Ints xs -> Some xs | _ -> None
  static member inline Of (xs: NodeArray, TomlTypes.Float)  = match xs with Floats xs -> Some xs | _ -> None
  static member inline Of (xs: NodeArray, TomlTypes.String) = match xs with Strings xs -> Some xs | _ -> None
  static member inline Of (xs: NodeArray, TomlTypes.Date)   = match xs with Dates xs -> Some xs | _ -> None
  static member inline Of (xs: NodeArray, TomlTypes.Array)  = match xs with Arrays xs -> Some { items = xs } | _ -> None

type TomlDocumentOf<'a> with
  static member inline Of (doc: TomlDocumentOf< ^Item >, ty: TomlTypes.Bool)   = { tomlOfT = doc.tomlOfT |> Map.choose (fun _ v -> v%.ty |> Option.map TomlUnwrapper.unwrap) }
  static member inline Of (doc: TomlDocumentOf< ^Item >, ty: TomlTypes.Int)    = { tomlOfT = doc.tomlOfT |> Map.choose (fun _ v -> v%.ty |> Option.map TomlUnwrapper.unwrap) }
  static member inline Of (doc: TomlDocumentOf< ^Item >, ty: TomlTypes.Float)  = { tomlOfT = doc.tomlOfT |> Map.choose (fun _ v -> v%.ty |> Option.map TomlUnwrapper.unwrap) }
  static member inline Of (doc: TomlDocumentOf< ^Item >, ty: TomlTypes.String) = { tomlOfT = doc.tomlOfT |> Map.choose (fun _ v -> v%.ty |> Option.map TomlUnwrapper.unwrap) }
  static member inline Of (doc: TomlDocumentOf< ^Item >, ty: TomlTypes.Date)   = { tomlOfT = doc.tomlOfT |> Map.choose (fun _ v -> v%.ty |> Option.map TomlUnwrapper.unwrap) }
  static member inline Of (doc: TomlDocumentOf< ^Item >, ty: TomlTypes.Array)  = { tomlOfT = doc.tomlOfT |> Map.choose (fun _ v -> v%.ty) }
