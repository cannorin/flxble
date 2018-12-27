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
  | Array   of ScriptObject seq
  | Record  of Map<string, ScriptObject>
  | Function of argLength: int * StructuralFunction<ScriptObject seq, ScriptObject>
  /// Null value. Can also contain an error message as `EValue errorMessage`.
  | Null of errorMessage:EqualityNull<string>
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
        | Null (EValue msg) -> commentize msg
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
          d |> Map.tryFind name
          ?| Null (EqualityNull <| sprintf "key not found: '%s'" name)
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
              Null (EqualityNull <| sprintf "date does not support the key '%s'" name)
        | TimeSpan t ->
          match name with
            | "day" -> Int t.Days
            | "hour" -> Int t.Hours
            | "minute" -> Int t.Minutes
            | "second" -> Int t.Seconds
            | "millisecond" -> Int t.Milliseconds
            | _ ->
              Null (EqualityNull <| sprintf "timespan does not support the key '%s'" name)
        | Null _ -> this
        | _ -> Null (EqualityNull "not a record")

    /// Gets a value at the given `index` of the script object.
    /// If it is not an array or the index is out of range,
    /// `Null` with the corresponding error message will be returned.
    member this.Item index =  
      match this with
        | Array xs ->
          let xs' =
            if index >= 0 then
              xs |> Seq.tryItem index
            else
              xs |> Seq.rev |> Seq.tryItem (-index-1)
          xs' ?| Null (EqualityNull "index out of range")
        | String s ->
          s |> Seq.tryItem index |> Option.map (string >> String)
          ?| Null (EqualityNull "index out of range")
        | Null _ -> this
        | _ -> Null (EqualityNull "not an array")

    /// Applies the script object to the given `args`.
    /// If it is not a function or raised an error,
    /// `Null` with the corresponding error message will be returned.
    member this.Invoke args =
      let rec apply args func =
        let arglen = Seq.length args
        match func with
          | v when arglen = 0 -> v
          | Function (i, f) ->
            if i <= arglen then
              let args' = args |> Seq.take i
              f.invoke args'
              |> apply (args |> Seq.skip i)
            else
              let i' = i - arglen
              let f xs =
                f.invoke (Seq.append args xs)
              Function (i', StructuralFunction f)
          | Null (EValue _) -> func
          | _ -> Null (EqualityNull "not a function")
      apply args this

and [<Struct; StructuredFormatDisplay("{AsString}")>] ScriptInfo = {
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
  /// %%begin
  /// ...
  /// %%end
  | Block of Script
  /// %%when CONDITION do
  /// ...
  /// \[%%otherwise
  /// ...\]
  /// %%end
  | When of cond:ScriptExprWithInfo * exec:Script * otherwise:Script option
  /// %%for NAME in EXPR do
  /// ...
  /// %%end
  | For of variable:string * ScriptExprWithInfo * Script
  /// {{ EXPR }}
  | YieldObject of ScriptExprWithInfo
  /// ANY_OTHER_STRING
  | YieldText of string

and Script = ScriptStatement list

module ScriptExpr =
  let inline private error info format =
    Printf.kprintf (fun s ->
      let msg =
        match info with
          | Some loc -> sprintf "%s (at %s)" s (to_s loc)
          | None -> s
      Null (EqualityNull msg)
    ) format

  /// Evaluates the given `expr` to a `ScripeObject` with the `context`.
  /// If there was an error, `Null (EValue errorMessage)` will be returned.
  let rec eval context expr =
    match expr.item with
      | Literal l -> l
      | Variable v ->
        context |> Map.tryFind v
        ?| error expr.info "the variable '%s' does not exist" v
      | Lambda (vars, body) ->
        let f xs =
          let ctx =
            Seq.zip vars xs
            |> Seq.fold (fun state (k, v) -> state |> Map.add k v) context
          body |> eval ctx
        Function (vars.Length, StructuralFunction f)
      | Let (var, value, body) ->
        body |> eval (context |> Map.add var (eval context value))
      | ArrayNew xs ->
        Array (xs |> Seq.map (eval context) |> Seq.cache)
      | RecordNew xs ->
        let mutable m = Map.empty
        for k, v in xs do
          m <- m |> Map.add k (eval context v)
        Record m
      | RecordWith (orig, xs) ->
        match eval context orig with
          | Record m ->
            let mutable m = m
            for k, v in xs do
              m <- m |> Map.add k (eval context v)
            Record m
          | Null (EValue _)  as x -> x
          | _ -> error expr.info "not a record"
      | MemberAccess (name, e) ->
        match (eval context e).[name] with
          | Null (EValue msg) -> error expr.info "%s" msg
          | x -> x
      | IndexerAccess (index, e) ->
        let value =
          match eval context index with
            | Int i -> (eval context e).[i]
            | String s -> (eval context e).[s]
            | Null (EValue _) as e -> e 
            | _ -> Null (EValue "not an index(int) or a field(string)")
        match value with
          | Null (EValue msg) -> error expr.info "%s" msg
          | x -> x

      | If (cond, a, b) ->
        match eval context cond with
          | Null _ | Bool false -> eval context b
          | _ -> eval context a
      
      // pipeline optimizations
      | Application (Item (Variable "|>"), [x; f])
      | Application (Item (Variable "<|"), [f; x]) ->
        Application (f, [x]) |> With.sameInfoOf expr |> eval context
      | Application (Item (Variable "||>"), [Item (ArrayNew xs); f])
      | Application (Item (Variable "<||"), [f; Item (ArrayNew xs)]) ->
        Application (f, xs) |> With.sameInfoOf expr |> eval context

      | Application (Item (Application (f, xs)), ys) ->
        Application (f, xs @ ys) |> With.sameInfoOf expr |> eval context
      
      | Application (func, args) ->
        let func = eval context func
        match func.Invoke (args |> Seq.map (eval context) |> Seq.cache) with
          | Null (EValue msg) -> error expr.info "%s" msg
          | x -> x