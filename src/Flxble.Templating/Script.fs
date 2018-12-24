module Flxble.Templating.Script

open System
open System.Globalization

open DataTypeExtra
open FParsecUtils

open System.Text

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
  | Null of errorMessage:EqualityNull<string>
  with
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
    
    member this.IsScriptNull = this.ObjectType = ScriptObjectType.Null
    
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

    member this.Invoke args =
      let rec apply args func =
        let arglen = Seq.length args
        match func with
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
          | Null (EValue _) as e -> e
          | v when arglen = 0 -> v
          | _ -> Null (EqualityNull "not a function")
      apply args this

and [<Struct>] ScriptInfo = {
  location: SourceLocation
}

and ScriptExprWithInfo = With<ScriptInfo, ScriptExpr>

and ScriptExpr =
  | Literal of ScriptObject
  | Variable of string
  | Lambda of string list * ScriptExprWithInfo
  | ArrayNew of ScriptExprWithInfo list
  | RecordNew of (string * ScriptExprWithInfo) list
  | RecordWith of orig:ScriptExprWithInfo * (string * ScriptExprWithInfo) list
  | MemberAccess of string * ScriptExprWithInfo
  | IndexerAccess of int * ScriptExprWithInfo
  | Application of func:ScriptExprWithInfo * args:ScriptExprWithInfo list

type ScriptStatement =
  | Open of ScriptExprWithInfo
  | Define of string * ScriptExprWithInfo
  | Block of Script
  | If of cond:ScriptExprWithInfo * Script * Script
  | For of variable:string * ScriptExprWithInfo * Script
  | YieldObject of ScriptExprWithInfo
  | YieldText of string

and Script = ScriptStatement list

type ScriptContext = {
  bindings: Map<string, ScriptObject>
  /// will be used to format datetimes
  culture: CultureInfo
  /// function to print an error message as a comment
  /// in the target language.
  /// for HTML, this should be something like
  /// `sprintf "<!-- %s -->"`.
  commentize: string -> string
}

module ScriptContext =
  let inline tryFind key ctx = ctx.bindings |> Map.tryFind key
  let inline add key value ctx = { ctx with bindings = ctx.bindings |> Map.add key value }

module ScriptExpr =
  module Ctx = ScriptContext

  let inline private error info format =
    Printf.kprintf (fun s ->
      let msg =
        match info with
          | Some loc -> sprintf "%s (at %s)" s (to_s loc)
          | None -> s
      Null (EqualityNull msg)
    ) format

  let rec eval ctx expr =
    match expr.item with
      | Literal l -> l
      | Variable v ->
        ctx |> Ctx.tryFind v
        ?| error expr.info "the variable '%s' does not exist" v
      | Lambda (vars, body) ->
        let f xs =
          let bindings =
            Seq.zip vars xs
            |> Seq.fold (fun state (k, v) -> state |> Map.add k v) ctx.bindings
          body |> eval { ctx with bindings = bindings }
        Function (vars.Length, StructuralFunction f)
      | ArrayNew xs ->
        Array (xs |> Seq.map (eval ctx) |> Seq.cache)
      | RecordNew xs ->
        let mutable m = Map.empty
        for k, v in xs do
          m <- m |> Map.add k (eval ctx v)
        Record m
      | RecordWith (orig, xs) ->
        match eval ctx orig with
          | Record m ->
            let mutable m = m
            for k, v in xs do
              m <- m |> Map.add k (eval ctx v)
            Record m
          | Null (EValue _)  as x -> x
          | _ -> error expr.info "not a record"
      | MemberAccess (name, e) ->
        match (eval ctx e).[name] with
          | Null (EValue msg) -> error expr.info "%s" msg
          | x -> x
      | IndexerAccess (index, e) ->
        match (eval ctx e).[index] with
          | Null (EValue msg) -> error expr.info "%s" msg
          | x -> x
      
      // pipeline optimizations
      | Application (Item (Literal (String "|>")), [x; f])
      | Application (Item (Literal (String "<|")), [f; x]) ->
        Application (f, [x]) |> With.sameInfoOf expr |> eval ctx
      | Application (Item (Literal (String "||>")), [Item (ArrayNew xs); f])
      | Application (Item (Literal (String "<||")), [f; Item (ArrayNew xs)]) ->
        Application (f, xs) |> With.sameInfoOf expr |> eval ctx
      
      | Application (func, args) ->
        let func = eval ctx func
        match func.Invoke (args |> Seq.map (eval ctx) |> Seq.cache) with
          | Null (EValue msg) -> error expr.info "%s" msg
          | x -> x

module Script =
  open ScriptExpr
  open System.IO
  module Ctx = ScriptContext
  
  let execute ctx (writer: TextWriter) (script: Script) =
    let rec exec ctx = function
      | [] -> ()
      | statement :: rest ->
        match statement with
          | Define (var, value) ->
            exec (ctx |> Ctx.add var (eval ctx value)) rest
          | Open record ->
            match eval ctx record with
              | Record m ->
                exec { ctx with bindings = Map.append ctx.bindings m } rest
              | _ ->
                let loc =
                  match record.info with
                    | Some i -> sprintf " (at %s)" (to_s i.location)
                    | None -> ""
                loc |> sprintf "open failed, not a record%s"
                    |> ctx.commentize
                    |> writer.Write
                exec ctx rest
          | Block scr ->
            exec ctx scr
            exec ctx rest
          | If (cond, a, b) ->
            let next =
              match eval ctx cond with
                | Null _
                | Bool false -> b
                | _ -> a
            exec ctx next
            exec ctx rest
          | For (var, xs, next) ->
            match eval ctx xs with
              | Array xs ->
                for x in xs do
                  exec (ctx |> Ctx.add var x) next
              | Record m ->
                for KVP(k, v) in m do
                  let kvp = Map.ofSeq ["key", String k; "value", v] |> Record
                  exec (ctx |> Ctx.add var kvp) next
              | _ -> ()
            exec ctx rest
          | YieldObject obj ->
            (eval ctx obj).AsCulturalString ctx.culture ctx.commentize
            |> writer.Write
          | YieldText str ->
            writer.Write str
    exec ctx script

  let executeToString ctx script =
    let sb = new StringBuilder()
    do
      use writer = new StringWriter(sb)
      execute ctx writer script
    sb.ToString()