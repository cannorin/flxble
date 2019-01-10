module Flxble.Templating.SyntaxTree
open System
open DataTypeExtra
open FParsecUtils

[<RequireQualifiedAccess; StructuredFormatDisplay("{AsString}"); Struct>]
type ScriptObjectType =
  | Bool | Int | Float | String | Date | TimeSpan
  | Array | Record | Function | Null
  with
    member this.AsString =
      match this with
        | Bool -> "bool" | Int -> "int" | Float -> "float" | String -> "string" | Date -> "date"
        | TimeSpan -> "timespan" | Array -> "array" | Record -> "record" | Function -> "function" | Null -> "null"
    override this.ToString() = this.AsString

[<StructuralEquality; NoComparison>]
type ScriptObject =
  | Bool    of bool
  | Int     of int
  | Float   of float
  | String  of string
  | Date    of DateTime
  | TimeSpan of TimeSpan
  | Array   of ScriptObject array
  | Record  of Map<string, ScriptObject>
  | Function of argLength: int * StructuralFunction<ScriptObject list, ScriptObject>
  /// Null value. Can also contain an error message as `EValue errorMessage`.
  | Null of errorMessage:EqualityNull<Lazy<string>>
  with
    /// Converts the script object to a string with the given `culture`.
    /// Null value with an error message will be quoted using `commentize`.
    /// An array, a record, or a function cannot be printed with this function.
    member this.AsCulturalString (culture: System.Globalization.CultureInfo) commentize =
      match this with
        | Bool b -> sprintf "%b" b
        | Int  i -> sprintf "%i" i
        | Float f -> sprintf "%g" f
        | String s -> s
        | Date d -> d.ToString(culture)
        | TimeSpan t -> t.ToString("g", culture)
        | Array _
        | Record _
        | Function _ ->
          commentize <|
            sprintf "this object cannot be directly printed: %s" (to_s this)
        | Null (EValue msg) -> commentize msg.Value
        | Null ENull -> ""

    /// Gets the type of the script object.
    member this.ObjectType =
      match this with
        | Bool _ -> ScriptObjectType.Bool
        | Int  _ -> ScriptObjectType.Int
        | Float _ -> ScriptObjectType.Float
        | String _ -> ScriptObjectType.String
        | Date _ -> ScriptObjectType.Date
        | TimeSpan _ -> ScriptObjectType.TimeSpan
        | Array _ -> ScriptObjectType.Array
        | Record _ -> ScriptObjectType.Record
        | Function _ -> ScriptObjectType.Function
        | Null _ -> ScriptObjectType.Null
    
    /// Determines if the script object is `Null`.
    member this.IsScriptNull = this.ObjectType = ScriptObjectType.Null
    
    /// Gets a value of the given field `name` of the script object.
    /// If it is not a record or does not have the filed,
    /// `Null` with the corresponding error message will be returned.
    member this.Item name =
      match this with
        | Record d ->
          d
          |> Map.tryFind' name
          |> defaultValueArg <|
            Null (EqualityNull <| lazy (sprintf "key not found: '%s'" name))
        | Date d ->
          match name with
            | "year" -> Int d.Year
            | "month" -> Int d.Month
            | "day" -> Int d.Day
            | "hour"-> Int d.Hour
            | "minute" -> Int d.Minute
            | "second" -> Int d.Second
            | "millisecond" -> Int d.Millisecond
            | _ ->
              Null (EqualityNull <| lazy (sprintf "date does not support the key '%s'" name))
        | TimeSpan t ->
          match name with
            | "day" -> Int t.Days
            | "hour" -> Int t.Hours
            | "minute" -> Int t.Minutes
            | "second" -> Int t.Seconds
            | "millisecond" -> Int t.Milliseconds
            | _ ->
              Null (EqualityNull <| lazy (sprintf "timespan does not support the key '%s'" name))
        | Null _ -> this
        | _ -> Null (EqualityNull (lazy "not a record"))

    /// Gets a value at the given `index` of the script object.
    /// If it is not an array or the index is out of range,
    /// `Null` with the corresponding error message will be returned.
    member this.Item index =  
      match this with
        | Array xs ->
          let xs' =
            if index >= 0 then
              xs |> Array.tryItem' index
            else
              xs |> Array.tryItem' (xs.Length + index)
          xs' |> defaultValueArg <| Null (EqualityNull (lazy "index out of range"))
        | String s ->
          s |> Seq.tryItem' index |> ValueOption.map (string >> String)
          |> defaultValueArg <| Null (EqualityNull (lazy "index out of range"))
        | Null _ -> this
        | _ -> Null (EqualityNull (lazy "not an array"))

    /// Applies the script object to the given `args`.
    /// If it is not a function or raised an error,
    /// `Null` with the corresponding error message will be returned.
    member this.Invoke args =
      let args = List.ofSeq args
      let rec apply args func =
        let arglen = List.length args
        match func with
          | v when arglen = 0 -> v
          | Function (i, f) ->
            if i <= arglen then
              let args' = args |> List.take i
              f.Invoke args'
              |> apply (args |> List.skip i)
            else
              let i' = i - arglen
              let f xs =
                f.Invoke (List.append args xs)
              Function (i', StructuralFunction f)
          | Null (EValue _) -> func
          | _ -> Null (EqualityNull (lazy "not a function"))
      apply args this

and [<StructuredFormatDisplay("{AsString}")>] ScriptInfo = {
  location: SourceLocation
} with
  member this.AsString = to_s this.location
  override this.ToString() = this.AsString

and ScriptExprWithInfo = With<ScriptInfo, ScriptExpr>

and ScriptExpr =
  | Literal of ScriptObject
  | Variable of string
  | Let of string * ScriptExprWithInfo * ScriptExprWithInfo
  | Lambda of string list * ScriptExprWithInfo
  | ArrayNew of ScriptExprWithInfo list
  | RecordNew of (string * ScriptExprWithInfo) list
  | RecordWith of orig:ScriptExprWithInfo * (string * ScriptExprWithInfo) list
  | MemberAccess of string * ScriptExprWithInfo
  | IndexerAccess of index:ScriptExprWithInfo * ScriptExprWithInfo
  | If of cond:ScriptExprWithInfo * ScriptExprWithInfo * ScriptExprWithInfo
  | Application of func:ScriptExprWithInfo * args:ScriptExprWithInfo list

type ScriptStatement =
  /// %%open EXPR
  | Open of ScriptExprWithInfo
  /// %%def NAME = EXPR
  | Define of string * ScriptExprWithInfo
  /// %%partial NAME EXPR
  | Partial of string * ScriptExprWithInfo
  /// %%begin
  /// ...
  /// %%end
  | Block of Template
  /// %%when CONDITION do
  /// ...
  /// \[%%otherwise
  /// ...\]
  /// %%end
  | When of cond:ScriptExprWithInfo * exec:Template * otherwise:Template voption
  /// %%for NAME in EXPR do
  /// ...
  /// %%end
  | For of variable:string * ScriptExprWithInfo * Template
  /// {{ EXPR }}
  | YieldObject of ScriptExprWithInfo
  /// ANY_OTHER_STRING
  | YieldText of string

and Template = ScriptStatement list

module ScriptExpr =
  let inline private errorFmt info format =
    Printf.kprintf (fun s ->
      match info with
        | ValueSome loc -> sprintf "%s (at %s)" s (to_s loc)
        | ValueNone -> s
    ) format

  let inline private err msg = Null (EValue msg)

  /// Evaluates the given `expr` to a `ScripeObject` with the `context`.
  /// If there was an error, `Null (EValue errorMessage)` will be returned.
  let rec eval context expr =
    match expr.item with
      | Literal l -> l
      | Variable v ->
        context
        |> Map.tryFind' v
        |> ValueOption.defaultValue (
          err (lazy (errorFmt expr.info "the variable '%s' does not exist" v)))
      | Lambda (vars, body) ->
        let f xs =
          let ctx =
            List.zip vars xs
            |> List.fold (fun state (k, v) -> state |> Map.add k v) context
          body |> eval ctx
        Function (vars.Length, StructuralFunction f)
      | Let (var, value, body) ->
        body |> eval (context |> Map.add var (eval context value))
      | ArrayNew xs ->
        Array (xs |> Seq.map (eval context) |> Array.ofSeq)
      | RecordNew xs ->
        xs |> List.map (Tuple.map2 id (eval context))
           |> Map.ofList
           |> Record
      | RecordWith (orig, xs) ->
        match eval context orig with
          | Record m ->
            xs |> List.fold (fun m (k, v) -> m |> Map.add k (eval context v)) m
               |> Record  
          | Null (EValue _)  as x -> x
          | _ -> err <| lazy (errorFmt expr.info "not a record")
      | MemberAccess (name, e) ->
        match (eval context e).[name] with
          | Null (EValue msg) -> msg |> Lazy.map (errorFmt expr.info "%s") |> err
          | x -> x
      | IndexerAccess (index, e) ->
        let value =
          match eval context index with
            | Int i -> (eval context e).[i]
            | String s -> (eval context e).[s]
            | Null (EValue _) as e -> e 
            | _ -> Null (EValue (lazy "not an index(int) or a field(string)"))
        match value with
          | Null (EValue msg) -> msg |> Lazy.map (errorFmt expr.info "%s") |> err
          | x -> x

      | If (cond, a, b) ->
        match eval context cond with
          | Null _ | Bool false -> eval context b
          | _ -> eval context a
      
      // operator optimizations
      | Application (Item (Variable "|>"), [x; f])
      | Application (Item (Variable "<|"), [f; x]) ->
        Application (f, [x]) |> With.sameInfoOf expr |> eval context
      | Application (Item (Variable "||>"), [Item (ArrayNew xs); f])
      | Application (Item (Variable "<||"), [f; Item (ArrayNew xs)]) ->
        Application (f, xs) |> With.sameInfoOf expr |> eval context
      | Application (Item (Variable "?|"), [x; y]) ->
        match eval context x with
          | Null _ -> eval context y
          | x -> x
      
      | Application (Item (Application (f, xs)), ys) ->
        Application (f, xs @ ys) |> With.sameInfoOf expr |> eval context
     
      | Application (func, args) ->
        let func = eval context func
        match func.Invoke (args |> List.map (eval context)) with
          | Null (EValue msg) -> msg |> Lazy.map (errorFmt expr.info "%s") |> err
          | x -> x