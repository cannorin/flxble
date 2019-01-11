module Flxble.Main
open Flxble
open Flxble.Context
open Flxble.Templating
open System
open System.IO
open System.Globalization
open Markdig
open FSharp.CommandLine

[<Literal>]
let helpText = """
usage: flxble <command> [options]

commands:
  init-blog [options]       .. initializes a new blog.
    --output, -o <dir       .. specify the root dir of the blog. [default: ./]
  
  new-page <path> [options] .. generates a new empty markdown page.
    --type,   -t <name>     .. specify the page type. if the page will belong to a blog,
                               it also determines which template to apply. (templates/<name>.html)
                               [default: page]

  render-blog [options]     .. renders an entire blog.
    --config, -c <file>     .. specify the blog config file. [default: ./flxble.toml]

  render-file <file>+       .. renders markdown or html files.
    --output, -o <dir>      .. specify the directory to output result files. [default: ./]
    --template, -t <file>   .. specify a template file to apply.
    --define, -d <name>=<value>
                            .. define a script variable to be used while rendering.

global options:
  --help, -h                .. show this.
  --log-level, -l {trace|info|warn|error} 
                            .. specify logging level.
  --log-to-file [file]      .. output log to [file] or, if not specified, a randomly generated file in the temp directory.
"""

let initBlog args =
  let outputOpt = copt ["output"; "o"] "dir"
  let outDir, _ = CommandOption.parse outputOpt args
  let outDir = outDir ?| Environment.CurrentDirectory
  extractTemplateZip outDir

let newPage args =
  let typeOpt = copt ["type"; "t"] "name"
  let typ, args = CommandOption.parse typeOpt args
  let _, args = getLogger args

  match args with
    | [path] ->
      let now = DateTimeOffset.Now.ToString("o")
      let metadata = sprintf "---
type = '%s'
title = ''
description = ''
post_date = %s
tags = []
---

"
      File.WriteAllText(path, metadata (typ ?| "post") now)
    | _ ->
      failwithf "'flxble new-page' takes 1 argument but given %i." args.Length
  
let renderBlog args =
  if args |> List.isEmpty then
    let ctx = Context.load None None
    Render.everything ctx
  else
    let configOpt = copt ["config"; "c"] "file"
    let config, args = CommandOption.parse configOpt args
    let logger, _ = getLogger args
    let ctx = Context.load logger config
    Render.everything ctx

let renderFile args =
  let templateOpt = copt ["template"; "t"] "file"
  let outputOpt   = copt ["output"; "o"] "dir"
  let defineOpt   = Command.option (["define"; "d"], "%s=%s") |> CommandOption.zeroOrMore
  let logger, args = getLogger args
  let template, args = CommandOption.parse templateOpt args
  let outDir, args = CommandOption.parse outputOpt args
  let defs, args = CommandOption.parse defineOpt args

  let curDir = Environment.CurrentDirectory
  let outDir = outDir ?| curDir
  let files = args |> List.map (Models.PageInfo.load curDir)
  let template = template |> Option.map (Models.TemplateInfo.load)

  let tmpCtx =
    let tmpCtx = TemplateContext.create CultureInfo.InvariantCulture (sprintf "<!-- %s -->")
    tmpCtx |> TemplateContext.addMany
      (defs
        |> List.map (Tuple.map2 id (ScriptExpr.parse >> (ScriptExpr.eval tmpCtx)))
        |> Map.ofList)

  let pipeline = MarkdownPipelineBuilder().UseAdvancedExtensions().Build()

  for file in files do
    let outPath = Path.combine outDir file.relativeLocation
    logger |> Option.iter (fun l -> l.trace "generating '%s'..." outPath)
    let pageVariables = file.ToScriptObjectMap()
    let tmpCtx = tmpCtx |> TemplateContext.addMany pageVariables
    let content =
      let content = Render.renderPage tmpCtx pipeline file
      match template with
        | Some (_, info) ->
          let tmpCtx = tmpCtx |> TemplateContext.add "content" (ScriptObject.String content)
          Template.renderToString tmpCtx info.template
        | None -> content
    File.WriteAllText (outPath, content)

[<EntryPoint>]
let main argv =
  let args = List.ofArray argv
  match args with
    | "render-blog" :: args -> renderBlog args; 0
    | "render-file" :: args -> renderFile args; 0
    | "new-page"    :: args -> newPage args; 0
    | "init-blog"   :: args -> initBlog args; 0
    | unknown :: _ -> cprintfn ConsoleColor.Red "error: unknown command '%s'." unknown; -1
    | [] -> printfn "%s" helpText; 0
