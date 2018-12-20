(*
The MIT License
FParsecUtils.fs - Useful extensions for FParsec
Copyright(c) 2018 cannorin
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*)

/// Useful extensions for FParsec
module FParsecUtils
open System
open FParsec

/// Parsec computation expression builder with backtrack
[<Struct>]
type BacktrackParsecBuilder =
  member inline __.Bind (m, f) = m >>=? f
  member inline __.Return x = preturn x
  member inline __.ReturnFrom x = x
  member inline __.Yield x = preturn x
  member inline __.YieldFrom x = x
  member inline __.Combine (x, y) = attempt x <|> y
  member inline __.For (xs, f) = xs |> Seq.map (f >> attempt) |> choice
  member inline __.Zero() = pzero

/// Parsec computation expression builder
[<Struct>]
type ParsecBuilder =
  member inline __.Bind (m, f) = m >>= f
  member inline __.Return x = preturn x
  member inline __.ReturnFrom x = x
  member inline __.Yield x = preturn x
  member inline __.YieldFrom x = x
  member inline __.Combine (x, y) = x <|> y
  member inline __.For (xs, f) = xs |> Seq.map f |> choice
  member inline __.Zero() = pzero
  member inline __.backtrack = BacktrackParsecBuilder()

/// Parsec computation expression
let parsec = ParsecBuilder()

/// Variant of `<|>` but accept different types of parsers and returns `Choice<'a, 'b>`.
let inline (<||>) a b = (a |>> Choice1Of2) <|> (b |>> Choice2Of2)

/// short hand for `skipString s`
let inline syn s = skipString s

/// short hand for `skipChar c `
let inline cyn c = skipChar c

/// short hand for `x .>>? spaces`
let inline ws x = x .>>? spaces

/// Applies the `parser` for `n` times.
let rec times n parser =
  if n <= 0 then invalidArg "n" "n must be positive"
  else if n = 1 then parser |>> List.singleton
  else
    parser .>>.? (times (n-1) parser) |>> fun (h, t) -> h :: t

/// Given a sequence of `(key, value)`, parses the string `key`
/// and returns the corresponding `value`.
let inline pdict (d: #seq<_*_>) =
  dict d |> Seq.map (fun kv -> pstring kv.Key >>% kv.Value)
         |> choice

/// Optimized version of `pdict d <?> descr`.
let inline pdictL (d: #seq<_*_>) descr =
  dict d |> Seq.map (fun kv -> pstring kv.Key >>% kv.Value)
         |> choiceL <| descr

/// String with escaped characters. Should be used along with `between`.
let inline escapedString (escapedChars: #seq<char>) =
  let controls =
    pdictL [
      "\\b", '\b'; "\\t", '\t'; "\\n", '\n';
      "\\v", '\u000B'; "\\f", '\u000C'; "\\r", '\r'; "\\\\", '\\'
    ] "control characters"
  let unicode16bit =
    syn "\\u" >>? times 4 hex |>> (Convert.hexsToInt >> char)
  let unicode32bit =
    syn "\\U" >>? times 8 hex |>> (Convert.hexsToInt >> char)
  let customEscapedChars =
    let d = escapedChars |> Seq.map (fun c -> sprintf "\\%c" c, c)
    pdict d
  
  let escape = choice [controls; unicode16bit; unicode32bit; customEscapedChars]
  let nonEscape = noneOf (sprintf "\\\b\t\n\u000B\u000C\r%s" (String.ofChars escapedChars))
  let character = nonEscape <|> escape
  many character |>> String.ofChars

/// Defines a recursive rule.
let inline recursive (definition: (Parser<'a, _> -> Parser<'a, _>)) =
  let p, pr = createParserForwardedToRef<'a, _>()
  pr := definition p
  p

/// Succeeds when the condition `cond` returns `Result.Ok ()`.
/// Fails with a `messageError msg` when it returns `Result.Error msg`.
/// THIS WILL DECREASE THE PERFORMANCE
let inline pwhenL (cond: 'a -> Result<unit, string>) (parser: Parser<'a, _>) =
  parser >>= fun x ->
    match cond x with
      | Result.Ok _ -> preturn x
      | Result.Error msg -> fail msg

/// Succeeds when the condition `cond` holds.
/// Otherwise, fails with an empty error message list.
/// THIS WILL DECREASE THE PERFORMANCE
let inline pwhen (cond: 'a -> bool) (parser: Parser<'a, _>) =
  parser >>= fun x ->
    if cond x then preturn x else pzero

/// Succeeds when the condition `cond` returns `Result.Ok ()`.
/// Fails fatally with a `messageError msg` when it returns `Result.Error msg`.
/// THIS WILL DECREASE THE PERFORMANCE
let inline passertL (cond: 'a -> Result<unit, string>) (parser: Parser<'a, _>) =
  parser >>= fun x ->
    match cond x with
      | Result.Ok _ -> preturn x
      | Result.Error msg -> failFatally msg

/// Succeeds when the condition `cond` holds.
/// Otherwise, fails fatally with an empty error message list.
/// THIS WILL DECREASE THE PERFORMANCE
let inline passert (cond: 'a -> bool) (parser: Parser<'a, _>) =
  parser >>= fun x ->
    if cond x then preturn x else fun _ -> Reply(FatalError, NoErrorMessages)


/// ISO8601-compliant Date/Time Parser.
/// See https://tools.ietf.org/html/iso8601#section-5.6 for details.
module ISO8601DateTime =
  open Convert

  // date-fullyear   = 4DIGIT
  // date-month      = 2DIGIT
  // date-mday       = 2DIGIT
  // full-date       = date-fullyear "-" date-month "-" date-mday
  let private iso8601_full_date =
    times 4 digit .>>.? times 2 (cyn '-' >>? times 2 digit)
    <?> "ISO8601 Full Date"
    |>> function 
      | (year, [month; day]) ->
        digitsToInt year, digitsToInt month, digitsToInt day
      | _ -> failwith "impossible"

  // time-hour       = 2DIGIT  ; 00-23
  // time-minute     = 2DIGIT  ; 00-59
  // time-second     = 2DIGIT  ; 00-5
  // time-secfrac    = "." 1*DIGIT
  // partial-time    = time-hour ":" time-minute ":" time-second [time-secfrac]
  let private iso8601_partial_time =
    times 2 digit .>>.? times 2 (cyn ':' >>? times 2 digit)
    .>>.? opt (cyn '.' >>? many1 digit)
    <?> "ISO8601 Partial Time"
    |>> function
      | ((hour, [minute; second]), secfrac) ->
        digitsToInt hour, digitsToInt minute, digitsToInt second,
        secfrac
        |> Option.map (fun xs ->
          digitsToInt <|
            // we only respect up to the top 3 bits (milliseconds)
            if List.length xs > 3 then List.take 3 xs else xs)
        |> Option.defaultValue 0
      | _ -> failwith "impossible"

  // time-numoffset  = ("+" / "-") time-hour ":" time-minute
  // time-offset     = "Z" / time-numoffset
  // NOTE: Per [ABNF] and ISO8601, the "T" and "Z" characters in this
  //  syntax may alternatively be lower case "t" or "z" respectively.
  let private iso8601_offset =
    let sign = (cyn '+' >>% true) <|> (cyn '-' >>% false)
    let numoffset =
      sign .>>.? times 2 digit .>>? cyn ':' .>>.? times 2 digit
      |>> fun ((sign, minute), second) -> sign, digitsToInt minute, digitsToInt second
    ((anyOf "zZ" >>% ()) <||> numoffset) <?> "ISO8601 Time Offset"

  // full-time       = partial-time time-offset
  let private iso8601_full_time =
    iso8601_partial_time .>>.? iso8601_offset
    <?> "ISO8601 Full Time"
    |>> fun ((h,m,s,f),o) -> h,m,s,f,o

  // date-time       = full-date "T" full-time
  // NOTE: Per [ABNF] and ISO8601, the "T" and "Z" characters in this
  //  syntax may alternatively be lower case "t" or "z" respectively.
  // NOTE: ISO 8601 defines date and time separated by "T".
  //  Applications using this syntax may choose, for the sake of
  //  readability, to specify a full-date and full-time separated by
  //  (say) a space character.
  let private iso8601_date_time =
    iso8601_full_date .>>? (anyOf "tT " >>% ()) .>>.? iso8601_full_time
    <?> "ISO8601 Date Time"
    |>> fun ((Y,M,D), (h,m,s,f,o)) -> Y,M,D,h,m,s,f,o
  
  /// ISO8601-compliant partial-time parser.
  let ppartialtime : Parser<_, unit> =
    iso8601_partial_time |>> fun (h,m,s,f) -> DateTime(0,0,0,h,m,s,f,DateTimeKind.Local)
  
  /// ISO8601-compliant full-time parser.
  let pfulltime : Parser<_, unit> =
    iso8601_full_time |>> function
      | (h,m,s,f,Choice1Of2()) ->
        DateTimeOffset(0,0,0,h,m,s,f,TimeSpan.Zero)
      | (h,m,s,f,Choice2Of2(sign, oh, om)) ->
        let inline sign x = if sign then x else -x
        DateTimeOffset(0,0,0,h,m,s,f,TimeSpan(sign oh, sign om, 0))

  /// ISO8601-compliant full-date parser.
  let pfulldate : Parser<_, unit> =
    iso8601_full_date |>> fun (y,m,d) -> DateTime(y,m,d,0,0,0,DateTimeKind.Local)

  /// ISO8601-compliant datetime parser.
  let pdatetime : Parser<_, unit> =
    iso8601_date_time |>> function
      | (Y,M,D,h,m,s,f,Choice1Of2()) ->
        DateTimeOffset(Y,M,D,h,m,s,f,TimeSpan.Zero)
      | (Y,M,D,h,m,s,f,Choice2Of2(sign, oh, om)) ->
        let inline sign x = if sign then x else -x
        DateTimeOffset(Y,M,D,h,m,s,f,TimeSpan(sign oh, sign om, 0))