[<AutoOpen>]
module Flxble.Logger

open System
open System.IO

[<Struct>]
type LogLevel = Trace | Info | Warning | Error with
  member this.AsInt =
    match this with
      | Trace -> 0
      | Info  -> 1
      | Warning -> 2
      | Error -> 3
  override this.ToString() =
    match this with
      | Trace -> "trace" | Info -> "info"
      | Warning -> "warning" | Error -> "error"

type Logger = {
  mutable writer: TextWriter option
  level: LogLevel
} with
  static member create(?writer) = { writer = writer; level = Warning }
  interface IDisposable with
    member this.Dispose() =
      this.writer |> Option.iter (fun x -> x.Dispose())
      this.writer <- None

let private format (date: DateTimeOffset) (lvl: LogLevel) text =
  let mutable isFirstLine = true
  seq {
    for line in text |> String.splitSeq ["\r\n"; "\r"; "\n"] do
      if isFirstLine then
        yield sprintf "[ %s ] %s: %s" (date.ToString()) (to_s lvl) line
        isFirstLine <- false
      else
        yield sprintf "           %s" line
  } |> String.concat Environment.NewLine

let private print lvl text =
  let color =
    match lvl with
      | Trace -> ConsoleColor.Gray
      | Info  -> ConsoleColor.Blue
      | Warning -> ConsoleColor.Yellow
      | Error  -> ConsoleColor.Red
  cprintfn color "%s: %s" (to_s lvl) text

let private tryWriteLine logger (text: string) =
  logger.writer |> Option.iter (fun x -> x.WriteLineAsync(text).Start())

let private log logger (lvl: LogLevel) text =
  if lvl.AsInt >= logger.level.AsInt then
    print lvl text
  format DateTimeOffset.UtcNow lvl text |> tryWriteLine logger
  sprintf "%s: %s" (to_s lvl) text

type Logger with
  member this.trace pf =
    Printf.kprintf (log this Trace >> ignore) pf

  member this.info pf =
    Printf.kprintf (log this Info >> ignore) pf

  member this.warning pf =
    Printf.kprintf (log this Warning >> ignore) pf

  member this.error pf =
    Printf.kprintf (log this Error >> ignore) pf

  member this.fail pf =
    Printf.kprintf (log this Error >> failwith) pf

  member this.failExn exnCont pf =
    Printf.kprintf (
      log this Error
      >> exnCont
      >> (fun exn ->
        log this Error (to_s exn) |> ignore
        raise exn
      )) pf