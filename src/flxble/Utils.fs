[<AutoOpen>]
module internal Flxble.Utils
open System
open System.IO
open System.IO.Compression
open System.Net
open System.Reflection
open FSharp.CommandLine

let genLogFileName() =
  let fn = Path.GetRandomFileName()
  let now = DateTime.Now
  Path.combine
    (Path.GetTempPath())
    (sprintf "flxble_%i%02i%02i_%02i%02i_%s"
      now.Year now.Month now.Day now.Hour now.Minute
      (Path.ChangeExtension(fn, ".log")))

let inline copt name argName = commandOption {
  names name
  takes (format("%s").withNames[argName])
}

let getLogger args =
  if args |> List.exists (fun s -> String.startsWith "--l" s || String.startsWith "-l" s) |> not then
    None, args
  else
    let loglvlOpt = copt ["log-level"; "l"] "trace|info|warn|error"
    let logfileOpt =
      commandOption {
        names ["log-to-file"]
        takes (format("%s").withNames["file"])
        defaultValue ""
      } |> CommandOption.map (
        Option.map (fun s -> if String.IsNullOrWhiteSpace s then genLogFileName() else s))
    let loglvl, args = CommandOption.parse loglvlOpt args
    let logfile, args = CommandOption.parse logfileOpt args
    let logger =
      match loglvl, logfile with
        | None, None -> None
        | None, Some file -> Logger.create(new StreamWriter(file)) |> Some
        | Some "trace", _ ->
          Some { 
            level = LogLevel.Trace;
            writer = logfile |> Option.map (fun s -> new StreamWriter(s) :> TextWriter) }
        | Some "info", _ ->
          Some { 
            level = LogLevel.Info;
            writer = logfile |> Option.map (fun s -> new StreamWriter(s) :> TextWriter) }
        | Some "warning", _ ->
          Some { 
            level = LogLevel.Warning;
            writer = logfile |> Option.map (fun s -> new StreamWriter(s) :> TextWriter) }
        | Some "error", _ ->
          Some { 
            level = LogLevel.Error;
            writer = logfile |> Option.map (fun s -> new StreamWriter(s) :> TextWriter) }
        | Some invalid, _ -> failwithf "invalid log level: %s" invalid
    logger, args

[<Literal>]
let templateZipUrl = "https://github.com/cannorin/flxble/raw/master/src/blog_template.zip"

let extractTemplateZip outDir =
  async {
    use wc = new WebClient()
    use zipStream =
      try
        wc.OpenRead(templateZipUrl)
      with
        | _ ->
          cprintfn ConsoleColor.Yellow "warning: your computer seems to be offline and cannot download the latest blog template."
          printf "would you like to use the bundled version (which might not be the latest)? [Y/n] "
          match Console.ReadLine() with
            | "n" | "N" -> failwith "network unreachable"
            | _ ->
              let asm = Assembly.GetEntryAssembly()
              let bundledPath =
                Path.combine (Path.GetDirectoryName asm.Location) "blog_template.zip"
              File.OpenRead bundledPath :> _
    use zip = new ZipArchive(zipStream)
    do zip.ExtractToDirectory outDir
    return ()
  } |> Async.RunSynchronously
