module internal Flxble.Templating.Builtin
open System
open System.Globalization

open DataTypeExtra
open SyntaxTree

let inline fn i f =
  Function (i, StructuralFunction f)

let inline record m =
  Record (m |> Map.ofSeq)

let inline err i name (objs: ScriptObject list) =
  if i = objs.Length then
    lazy (
      sprintf "the function '%s' does not support type(s) %s"
        name (objs |> List.map (fun x -> x.ObjectType |> sprintf "'%A'") |> String.concat ", ")
    ) |> EValue |> Null
  else
    lazy (
    sprintf "the function '%s' takes %i argument(s) but given %i"
      name i objs.Length
    ) |> EValue |> Null

let inline toBool obj =
  match obj with
    | Bool false | Null _ -> false | _ -> true

let inline toInt obj =
  match obj with
    | Int i -> i
    | Float i -> int i
    | String s -> int s
    | _ -> 0

let inline toFloat obj =
  match obj with
    | Int i -> float i
    | Float i -> i
    | String s -> float s
    | _ -> 0.0

let inline toArr obj =
  match obj with
    | Array xs -> xs | _ -> Array.empty

let inline toStr culture (obj: ScriptObject) =
  obj.AsCulturalString culture id

let inline toFmtBox culture obj =
  match obj with
    | Bool b -> box b
    | Int i -> box i
    | Float i -> box i
    | String s -> box s
    | Date d -> box d
    | TimeSpan s -> box s
    | x -> toStr culture x |> box

[<Struct; StructuralEquality; StructuralComparison>]
type ScriptObjectCompare =
  | CmpBool of b:bool
  | CmpInt of i:int
  | CmpFloat of f:float
  | CmpSpan of p:TimeSpan
  | CmpDate of d:DateTime
  | CmpString of s:string
  | CmpNull

let inline toCompare obj =
  match obj with
    | Int i -> CmpInt i
    | Float f -> CmpFloat f
    | Bool b -> CmpBool b
    | Date d -> CmpDate d
    | TimeSpan s -> CmpSpan s
    | String s -> CmpString s
    | _ -> CmpNull

let inline math2 i f name =
  name, fn 2 (function
    | [Int a; Int b] -> i a b |> Int
    | [Float a; Float b] -> f a b |> Float
    | xs -> err 2 name xs
  )

let inline cmp g name =
  name, fn 2 (function [a;b] -> g (toCompare a) (toCompare b) |> Bool | xs -> err 2 name xs)
let inline s1 g name ty =
  name, fn 1 (function [String s] -> g s |> ty | xs -> err 1 name xs)
let inline s2 g name ty =      
  name, fn 2 (function [String a; String b] -> g a b |> ty | xs -> err 2 name xs)

let inline cast f name ty =
  name, fn 1 (function [x] -> f x |> ty | xs -> err 1 name xs)

let inline removelike f name =
  name, fn 3 (function
    | [Int s; Int e; String t] -> f s e t |> String
    | xs -> err 3 name xs
      )
let inline padby f name =
  name, fn 3 (function
    | [Int count; String chars; String s] ->
      chars |> String.toChars |> Array.fold (fun state c -> f count c state) s |> String
    | xs -> err 3 name xs
      )
let inline takelike f name =
  name, fn 2 (function [Int len; String s] -> f len s |> String | xs -> err 2 name xs)

let inline defaultBindings (culture: CultureInfo) =
  Map.ofArray [|
    // comparison
    yield "=",  fn 2 (function [a;b] -> a = b |> Bool | xs -> err 2 "=" xs)
    yield "<>", fn 2 (function [a;b] -> a <> b |> Bool | xs -> err 2 "=" xs)
    yield cmp (>) ">"
    yield cmp (<) "<"
    yield cmp (>=) ">="
    yield cmp (<=) "<="
    
    // null check
    yield "is_null", fn 1 (function [Null _] -> Bool true | _ -> Bool false)
    yield "is_not_null", fn 1 (function [Null _] -> Bool false | _ -> Bool true)
    yield "?|", fn 2 (function [Null _; x] | [x; _] -> x | xs -> err 2 "?|" xs)

    // logical
    yield "||", fn 2 (fun xs ->
      match xs |> List.tryItem' 0 with
        | ValueSome (Bool false | Null _) -> List.tryItem' 1 xs |> defaultValueArg <| err 2 "||" xs
        | ValueSome _ -> Bool true
        | ValueNone -> err 2 "||" xs
    )
    yield "&&", fn 2 (fun xs ->
      match xs |> List.tryItem' 0 with
        | ValueSome (Bool false | Null _) -> Bool false
        | ValueSome _ -> List.tryItem' 1 xs |> defaultValueArg <| err 2 "&&" xs
        | ValueNone -> err 2 "&&" xs
    )
    yield "not", fn 1 (function
      | [Bool false] | [Null _] -> Bool true
      | [_] -> Bool false
      | xs -> err 1 "not" xs
    )

    // arithmetic
    yield "+", fn 2 (function
      | [Int i; Int j]       -> i+j |> Int
      | [Float i; Float j]   -> i+j |> Float
      | [String i; String j] -> i+j |> String
      | [Array xs; Array ys] -> Array.append xs ys |> Array
      | [Record a; Record b] -> Map.append a b |> Record
      | [TimeSpan a; TimeSpan b] -> a+b |> TimeSpan
      | [TimeSpan s; Date d]
      | [Date d; TimeSpan s] -> d+s |> Date
      | xs -> err 2 "+" xs
    )
    yield "-", fn 2 (function
      | [Int i; Int j]     -> i-j |> Int
      | [Float i; Float j] -> i-j |> Float
      | [Date i; Date j]   -> i-j |> TimeSpan
      | [TimeSpan i; TimeSpan j] -> i-j |> TimeSpan
      | [Date i; TimeSpan j] -> i-j |> Date
      | xs -> err 2 "-" xs
    )
    yield math2 (*) (*) "*"
    yield math2 (/) (/) "/"
    yield math2 pown ( ** ) "^"
    yield math2 (%) (%) "%"
    yield "abs", fn 1 (function
      | [Int i]   -> abs i |> Int
      | [Float i] -> abs i |> Float
      | xs -> err 1 "abs" xs
    )
    yield "neg", fn 1 (function
      | [Int i]   -> -i |> Int
      | [Float i] -> -i |> Float
      | [Bool b]  -> not b |> Bool
      | [TimeSpan t] -> -t |> TimeSpan
      | xs -> err 1 "neg" xs
    )
    yield "ceil", fn 1 (function
      | [Int i] -> Int i
      | [Float i] -> ceil i |> Float
      | xs -> err 1 "ceil" xs
    )
    yield "floor", fn 1 (function
      | [Int i] -> Int i
      | [Float i] -> floor i |> Float
      | xs -> err 1 "floor" xs
    )
    yield "round", fn 1 (function
      | [Int i] -> Int i
      | [Float i] -> round i |> Float
      | xs -> err 1 "round" xs
    )
    yield "sqrt", fn 1 (function
      | [Float i]   -> sqrt i |> Float
      | [Int i] -> sqrt (float i) |> int |> Int
      | xs -> err 1 "sqrt" xs
    )
    yield math2
      (fun i j -> float j ** (1.0/float i) |> int)
      (fun i j -> j ** (1.0/i))
      "root"

    // type & casting
    yield cast (toStr culture) "string" String
    yield cast (toStr CultureInfo.InvariantCulture) "string_invariant" String
    yield cast toInt "int" Int
    yield cast toFloat "float" Float
    yield cast (fun x -> to_s x.ObjectType) "typeof" String

    // function
    yield "id", fn 1 (function [x] -> x | xs -> err 1 "id" xs)
    yield "|>", fn 2 (function [a;b] -> b.Invoke [a] | xs -> err 2 "|>" xs)
    yield "<|", fn 2 (function [a;b] -> a.Invoke [b] | xs -> err 2 "<|" xs)
    yield "||>", fn 2 (function [a; Array xs] -> a.Invoke xs | xs -> err 2 "||>" xs)
    yield "<||", fn 2 (function [Array xs; a] -> a.Invoke xs | xs -> err 2 "<||" xs)
    yield ">>", fn 2 (function
      | [f;g] ->
        Function (1, StructuralFunction(fun x -> g.Invoke [f.Invoke x]))
      | xs -> err 2 ">>" xs
    )
    yield "<<", fn 2 (function
      | [f;g] ->
        Function (1, StructuralFunction(fun x -> f.Invoke [g.Invoke x]))
      | xs -> err 2 "<<" xs
    )

    // timespan module
    yield "timespan", record <| seq {
      yield "new", fn 1 (function
        | [Record r] ->
          new TimeSpan(
            toInt r.["day"],
            toInt r.["hour"],
            toInt r.["minute"],
            toInt r.["second"],
            toInt r.["millisecond"]
          ) |> TimeSpan
        | xs -> err 1 "timespan" xs
      )
      yield "from_ticks", fn 1 (function
        | [Int i] ->
          TimeSpan.FromTicks(int64 i) |> TimeSpan
        | xs -> err 1 "from_ticks" xs
      )
      yield "parse", fn 1 (function
        | [String s] ->
          TimeSpan.Parse(s, culture.DateTimeFormat) |> TimeSpan
        | xs -> err 1 "parse" xs
      )
      yield "parse_invariant", fn 1 (function
        | [String s] ->
          TimeSpan.Parse(s, CultureInfo.InvariantCulture.DateTimeFormat) |> TimeSpan
        | xs -> err 1 "parse_invariant" xs
      )
    }

    // date module
    yield "date", record <| seq {
      yield "new", fn 1 (function
        | [Record r] ->
          new DateTime(
            toInt r.["year"],
            toInt r.["month"],
            toInt r.["day"],
            toInt r.["hour"],
            toInt r.["minute"],
            toInt r.["second"],
            toInt r.["millisecond"]
          ) |> Date
        | xs -> err 1 "new" xs
      )
      yield "now", fn 1 (function
        | [_] -> DateTime.Now |> Date
        | xs -> err 1 "now" xs
      )
      yield "utc_now", fn 1 (function
        | [_] -> DateTime.UtcNow |> Date
        | xs -> err 1 "utc_now" xs
      )
      yield "today", fn 1 (function
        | [_] -> DateTime.Today |> Date
        | xs -> err 1 "today" xs
      )
      // https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings
      yield "to_string", fn 2 (function
        | [String fmt; Date d] ->
          d.ToString(fmt, culture.DateTimeFormat) |> String
        | xs -> err 2 "to_string" xs
      )
      // https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings
      yield "to_string_invariant", fn 2 (function
        | [String fmt; Date d] ->
          d.ToString(fmt, CultureInfo.InvariantCulture.DateTimeFormat) |> String
        | xs -> err 2 "to_string_invariant" xs
      )
      yield "parse", fn 1 (function
        | [String s] -> DateTime.Parse(s, culture.DateTimeFormat) |> Date
        | xs -> err 1 "parse" xs
      )
      yield "parse_invariant", fn 1 (function
        | [String s] -> DateTime.Parse(s, CultureInfo.InvariantCulture.DateTimeFormat) |> Date
        | xs -> err 1 "parse_invariant" xs
      )
    }

    // array module
    yield "array", record <| seq {
      yield "map", fn 2 (function
        | [f; Array xs] ->
          Array (xs |> Array.map (List.singleton >> f.Invoke))
        | xs -> err 2 "map" xs
      )
      yield "filter", fn 2 (function
        | [p; Array xs] ->
          Array (xs |> Array.filter (List.singleton >> p.Invoke >> toBool))
        | xs -> err 2 "filter" xs
      )
      yield "take", fn 2 (function
        | [Int n; Array xs] ->
          match xs |> Array.tryTake' n with
            | ValueSome xs' -> Array xs'
            | ValueNone -> Array xs
        | xs -> err 2 "take" xs
      )
      yield "skip", fn 2 (function
        | [Int n; Array xs] -> Array (xs |> Array.skipSafe n)
        | xs -> err 2 "skip" xs
      )
      yield "fold", fn 3 (function
        | [f; s; Array xs] ->
          xs |> Array.fold (fun s x -> f.Invoke [s; x]) s
        | xs -> err 3 "fold" xs
      )
      yield "forall", fn 2 (function
        | [p; Array xs] ->
          xs |> Array.forall (List.singleton >> p.Invoke >> toBool) |> Bool
        | xs -> err 2 "forall" xs
      )
      yield "exists", fn 2 (function
        | [p; Array xs] ->
          xs |> Array.exists (List.singleton >> p.Invoke >> toBool) |> Bool
        | xs -> err 2 "exists" xs
      )
      yield "is_empty", fn 1 (function
        | [Array xs] ->
          xs |> Array.isEmpty |> Bool
        | xs -> err 1 "is_empty" xs
      )
      yield "indexed", fn 1 (function
        | [Array xs] ->
          xs |> Array.mapi (fun i x -> Record (Map.ofList ["index", Int i; "item", x]))
             |> Array
        | xs -> err 1 "indexed" xs
      )
      yield "find", fn 2 (function
        | [p; Array xs] ->
           xs |> Array.tryFind (List.singleton >> p.Invoke >> toBool)
           ?| Null (EValue (lazy "element not found"))
        | xs -> err 2 "find" xs
      )
      yield "length", fn 1 (function
        | [Array xs] -> Array.length xs |> Int
        | xs -> err 1 "length" xs
      )
      yield "rev", fn 1 (function
        | [Array xs] -> Array (Array.rev xs)
        | xs -> err 1 "rev" xs
      )
      yield "sort", fn 1 (function
        | [Array xs] ->
          Array (xs |> Array.sortBy toCompare)
        | xs -> err 1 "sort" xs
      )
      yield "sort_desc", fn 1 (function
        | [Array xs] ->
          Array (xs |> Array.sortByDescending toCompare)
        | xs -> err 1 "sort_desc" xs
      )
      yield "sort_by", fn 2 (function
        | [f; Array xs] ->
          Array (xs |> Array.sortBy (List.singleton >> f.Invoke >> toCompare))
        | xs -> err 2 "sort_by" xs
      )
      yield "sort_desc_by", fn 2 (function
        | [f; Array xs] ->
          Array (xs |> Array.sortByDescending (List.singleton >> f.Invoke >> toCompare))
        | xs -> err 2 "sort_desc_by" xs
      )
      yield "concat", fn 1 (function
        | [Array xss] ->
          Array (xss |> Array.map toArr |> Array.concat)
        | xs -> err 1 "concat" xs
      )
    }

    // string module
    yield "string", record <| seq {
      yield "concat", fn 2 (function
        | [String s; Array xs] ->
          xs |> Array.map (toStr culture) |> String.concat s |> String
        | xs -> err 2 "concat" xs
      )
      yield "concat_invariant", fn 2 (function
        | [String s; Array xs] ->
          xs |> Array.map (toStr CultureInfo.InvariantCulture) |> String.concat s |> String
        | xs -> err 2 "concat_invariant" xs
      )
      yield s2 String.contains "contains" Bool
      yield s2 String.endsWith "ends_with" Bool
      
      yield s2 String.findIndex "find_index" Int
      yield s2 String.findLastIndex "find_last_index" Int

      yield "insert_at", fn 3 (function
        | [String a; Int index;  String s] ->
          String.insertAt a index s |> String
        | xs -> err 2 "insert_at" xs
      )
      yield "length", fn 1 (function [String s] -> String.length s |> Int | xs -> err 1 "length" xs)

#if !NETSTANDARD1_6
      yield s1 String.normalize "normalize" String
#else
      yield s1 (fun (s: string) -> s.Normalize()) "normalize" String
#endif

      yield padby String.padLeftBy "pad_left"
      yield padby String.padRightBy "pad_right"
      
      yield removelike String.remove "remove"
      yield "replace", fn 3 (function
        | [String q; String r; String s] -> String.replace q r s |> String
        | xs -> err 3 "replace" xs
      )
      yield "rev", fn 1 (function [String s] -> String.rev s |> String | xs -> err 1 "rev" xs)
      
      yield takelike String.skip "skip"
      yield "split", fn 2 (function
        | [String sep; String s] ->
          String.split sep s |> Array.map String |> Array
        | [Array seps; String s] ->
          String.splitSeq (seps |> Array.map (toStr culture)) s
          |> Array.map String |> Array
        | xs -> err 2 "split" xs
      )
      yield s2 String.startsWith "starts_with" Bool
      yield removelike String.substring "substring"
      yield takelike String.take "take"

      yield s1 (String.toLower culture) "to_lower" String
      yield s1 String.toLowerInvariant "to_lower_invariant" String
      yield s1 (String.toUpper culture) "to_upper" String
      yield s1 String.toUpperInvariant "to_upper_invariant" String
      yield s1 String.trim "trim" String
      yield s1 String.trimStart "trim_start" String
      yield s1 String.trimEnd "trim_end" String

      yield s2 String.trimBySeq "trim_by" String
      yield s2 String.trimStartBySeq "trim_start_by" String
      yield s2 String.trimEndBySeq "trim_end_by" String

      yield "truncate", fn 3 (function
        | [Int length; String ellipsis; String str] ->
          let str = str |> String.take (length - String.length ellipsis)
          str + ellipsis |> String
        | xs -> err 3 "truncate" xs
      )
      
      yield s1 String.IsNullOrEmpty "is_empty" Bool
      yield s1 String.IsNullOrWhiteSpace "is_whitespace" Bool

      // https://docs.microsoft.com/en-us/dotnet/standard/base-types/composite-formatting?view=netframework-4.7.2#composite-format-string
      yield "format", fn 2 (function
        | [String fmt; Array xs] ->
          String.Format(fmt, xs |> Array.map (toFmtBox culture)) |> String
        | xs -> err 2 "format" xs
      )
      yield "format_invariant", fn 2 (function
        | [String fmt; Array xs] ->
          String.Format(fmt, xs |> Array.map (toFmtBox CultureInfo.InvariantCulture)) |> String
        | xs -> err 2 "format_invariant" xs
      )

      yield s1 Uri.EscapeUriString "url_escape" String
      yield s1 Uri.EscapeDataString "url_encode" String
      yield s1 Net.WebUtility.HtmlEncode "html_escape" String
    }

    // path module
    yield "path", record <| seq {
      yield "combine", fn 1 (function
        | [Array xs] ->
          xs |> Array.map (toStr culture) |> Path.combineMany |> String
        | xs -> err 1 "combine" xs
      )
      yield "make_relative_to", fn 2 (function
        | [String root; String path] ->
          Path.makeRelativeTo root path |> String
        | xs -> err 2 "make_relative_to" xs
      )
    }

    // record module
    yield "record", record <| seq {
      yield "keys", fn 1 (function
        | [Record m] -> (m :> dict<_, _>).Keys |> Seq.map String |> Array.ofSeq |> Array
        | xs -> err 1 "keys" xs
      )

      yield "values", fn 1 (function
        | [Record m] -> (m :> dict<_, _>).Values :> seq<_> |> Array.ofSeq |> Array
        | xs -> err 1 "values" xs
      )

      yield "has_key", fn 2 (function
        | [String k; Record m] -> m |> Map.containsKey k |> Bool
        | xs -> err 2 "has_key" xs
      )
    }

    // debug module
    yield "debug", record <| seq {
      yield "print", fn 1 (function
        | [x] -> printf "%A" x; Null ENull
        | xs -> err 1 "print" xs
      )
    }
  |]
