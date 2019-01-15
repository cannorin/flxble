module Flxble.Render
open System.IO
open System.Threading.Tasks
open FSharp.Collections
open Flxble.Configuration
open Flxble.Models
open Flxble.Context
open Flxble.Templating
open Flxble.Templating.SyntaxTree
open Flxble.Templating.ScriptObjectHelper
open Markdig
open System.Net

[<Literal>]
let synchronousRenderCount = 50

let rec private applyTemplateToHtml ctx renderCtx localVariables templateName html fileName =
  tryOperation ctx "applyTemplateToHtml" fileName <| fun () ->
    let tmpInfo =
      ctx.templates
        |> Map.tryFind' templateName
        |> ValueOption.defaultWith (
          fun () ->
            ctx.logger.fail
              "the specified template '%s' does not exist"
              templateName)

    let localVariables =
      Map.append localVariables (tmpInfo.ToScriptObjectMap())
  
    let html' =
      let rctx =
        renderCtx
        |> TemplateContext.addMany localVariables
        |> TemplateContext.add "content" (string' html)
      tmpInfo.template |> Template.renderToString rctx
    
    match tmpInfo.dependsOn with
      | ValueSome tmpName ->
        applyTemplateToHtml ctx renderCtx localVariables tmpName html' fileName
      | ValueNone -> html'

type mpb = MarkdownPipelineBuilder

let rec private useCustomExtension names : _ * (mpb -> mpb) =
  match names with
    | [] -> [], id
    // TODO: append flexble's custom extensions here
    // | "name", xs ->
    //   let xs', f = useCustomExtension xs
    //   xs', f >> fun x -> x.UseXxx()
    | x :: xs ->
      let xs', f = useCustomExtension xs
      x :: xs', f

let private generatePipeline ctx =
  tryOperation ctx "generatePipeline" ctx.config.MarkdownExtensions <| fun () ->
    let exts =
      ctx.config.MarkdownExtensions
      |> String.split '+' |> String.removeEmptyEntries |> Array.toList
    let p = new MarkdownPipelineBuilder()
    let exts, customize = useCustomExtension exts
    p.Configure(exts |> String.concat "+")
      |> customize
      |> fun x -> x.Build()

let inline internal renderPage renderCtx pipeline (page: PageInfo) =
  match page.format with
    | PageFormat.Markdown ->
      Markdown.ToHtml(page.content, pipeline)
    | PageFormat.Html ->
      Template.loadString page.absoluteLocation page.content
      |> Template.renderToString renderCtx
    | _ -> failwith "impossible"

let private renderPageToOutput ctx templateCtx pipeline page =
  tryOperation ctx "renderPageToOutput" page.relativeLocation <| fun () ->
    match page.format with
      | PageFormat.Html
      | PageFormat.Markdown ->
        let location = Path.ChangeExtension(page.relativeLocation, "html")
        let path = Path.Combine(ctx.OutputDir, location)

        ctx.logger.trace "generating '%s'..." path

        let pageVariables = page.ToScriptObjectMap()

        let prevnextObjs = [|
          match page.metadata with
            | ValueNone -> ()
            | ValueSome metadata ->
              let prev, next = ctx.FindPrevNextPost page
              match prev with
                | ValueSome ({ metadata = ValueSome x } as m) when x.PageType = metadata.PageType ->
                  yield "prev_date_page", ScriptObject.from m
                | _ -> ()
              match next with
                | ValueSome ({ metadata = ValueSome x } as m) when x.PageType = metadata.PageType ->
                  yield "next_date_page", ScriptObject.from m
                | _ -> ()
        |]

        let renderCtx = 
          templateCtx |> TemplateContext.addMany pageVariables
                      |> TemplateContext.addMany (Map.ofArray prevnextObjs)

        let content = renderPage renderCtx pipeline page

        let html =
          match page.metadata with
            | ValueSome mt when mt.PageType <> "none" ->
              applyTemplateToHtml ctx renderCtx Map.empty mt.PageType content page.absoluteLocation
            | _ -> content

        let outDir = path |> Path.GetDirectoryName
        if Directory.Exists outDir |> not then
          Directory.CreateDirectory outDir |> ignore
        File.WriteAllText(path, html)

      | _ ->
        let srcpath  = page.absoluteLocation
        let destpath = Path.Combine(ctx.OutputDir, page.relativeLocation)
        let outDir = destpath |> Path.GetDirectoryName
        if Directory.Exists outDir |> not then
          Directory.CreateDirectory outDir |> ignore
        File.Copy(srcpath, destpath, true)

let private renderArchive archiveType tempName elements predicate title printer outDir ctx renderCtx =
  tryOperation ctx (sprintf "renderArchive: %s" archiveType) "current context" <| fun () ->
    let tagTmp =
      ctx.templates
        |> Map.tryFind' tempName
        |> ValueOption.defaultWith (fun () ->
            ctx.logger.fail "the theme '%s' does not have the '%s' template."
              ctx.config.Theme tempName)
    let pages =
      ctx.pages
      |> Array.choose (fun x ->
          if x.metadata.IsSome && (x.format = PageFormat.Html || x.format = PageFormat.Markdown) then
            Some (x.metadata.Value, x |> ScriptObject.from)
          else None)
    
    let outputDir = Path.Combine(ctx.OutputDir, outDir)
    if Directory.Exists outputDir |> not then
      Directory.CreateDirectory(outputDir) |> ignore
    
    seq {
      for elementChunk in elements |> Array.chunkBySize synchronousRenderCount do
        yield fun () ->
          for element in elementChunk do
            let pagesObj = 
              pages
                |> Array.filter (fun (x, _) -> predicate element x)
                |> Array.map    snd
                |> ScriptObject.Array

            let archiveObj =
              [|
                "pages", pagesObj
                "title", ScriptObject.String (title element)
                "archive_type",  ScriptObject.String archiveType
              |] |> Map.ofArray

            let renderCtx = renderCtx |> TemplateContext.addMany archiveObj
          
            let outPath = Path.Combine(outputDir, sprintf "%s.html" <| printer element)
            ctx.logger.trace "generating '%s'..." outPath
            
            let html =
              let h = tagTmp.template |> Template.renderToString renderCtx
              match tagTmp.dependsOn with
                | ValueSome x -> applyTemplateToHtml ctx renderCtx Map.empty x h (sprintf "template: %s" x)
                | ValueNone -> h
            
            File.WriteAllText(outPath, html)
    }

let private renderTagArchive (ctx: Context) sobj =
  ctx.config.TagDir
  |> Option.map (fun dir ->
    ctx.logger.info "processing tag archive..."
    renderArchive "tag" "archive" ctx.Tags
      (fun tag x -> x.Tags |> List.contains tag)
      (sprintf "Tag: %s")
      id dir ctx sobj)
  |> Option.defaultValue Seq.empty

let private renderMonthlyArchive (ctx: Context) sobj =
  let inline getMonthName i =
    ctx.Culture.DateTimeFormat.GetMonthName(i)
  ctx.config.ArchiveDir
  |> Option.map (fun dir ->
    ctx.logger.info "processing monthly archive..."
    renderArchive "archive" "archive" ctx.Months
      (fun d x -> x.Date.IsSome && x.Date.Value.Year = d.Year && x.Date.Value.Month = d.Month)
      (fun d -> sprintf "Archive: %s %i" (getMonthName d.Month) d.Year)
      (fun d -> sprintf "%i-%i" d.Year d.Month)
      dir ctx sobj)
  |> Option.defaultValue Seq.empty

let private copyContent (ctx: Context) =
  let srcContent =
    Path.combine ctx.ConfigDir ctx.config.ContentDir
  let themeContent =
    Path.combineMany [|
      ctx.ConfigDir;
      ctx.config.ThemesDir;
      ctx.config.Theme;
      "content"
    |]
  let inline doCopy content =
    if Directory.Exists content then
      for file in Directory.enumerateFilesRecursively false content do
        let outPath =
          file |> Path.makeRelativeTo content |> Path.combine ctx.OutputDir
        let outDir = outPath |> Path.GetDirectoryName
        if Directory.Exists outDir |> not then
          Directory.CreateDirectory outDir |> ignore
        File.Copy(file, outPath, true)
  doCopy srcContent
  doCopy themeContent

open System.Xml
let private generateRssFeed (ctx: Context) =
  let inline writeElementString name value (writer: XmlWriter) =
    writer.WriteElementString("", name, "", value)
  let inline writeStartElement name (writer: XmlWriter) =
    writer.WriteStartElement("", name, "")
  
  let rssLocation =
    Path.combine ctx.OutputDir "rss.xml"
  let settings = new XmlWriterSettings()
  use writer = XmlWriter.Create(rssLocation, settings)
  do writer.WriteStartDocument()
  do writer |> writeStartElement "rss"
  do writer |> writeElementString "version" "2.0"
  do writer |> writeStartElement "channel"
  do writer |> writeElementString "title" ctx.config.Title
  do writer |> writeElementString "link"  ctx.config.BaseUrl
  do writer |> writeElementString "description" ctx.config.Description
  if ctx.config.Copyright.IsSome then
    do writer |> writeElementString "copyright" ctx.config.Copyright.Value
  for page in ctx.pages do
    if page.PageType = PageType.Post then
      do writer |> writeStartElement "item"
      do 
        writer |> writeElementString "link"
          (Path.ChangeExtension(page.relativeLocation, "html")
           |> Path.combine ctx.config.BaseUrl)
      match page.metadata with
        | ValueSome meta ->
          do writer |> writeElementString "title" meta.Title
          do writer |> writeElementString "description" meta.Description
          match meta.Date with
            | Some date ->
              do writer |> writeElementString "pubDate" (date.ToString("r"))
            | None -> ()
        | ValueNone -> ()
      do writer.WriteEndElement()
  do writer.WriteEndElement()
  do writer.WriteEndElement()
  do writer.WriteEndDocument()
  do writer.Flush()

/// Render everything in the current context.
let everything (ctx: Context) =
  tryOperation ctx "Render.everything" "current context" <| fun () ->
    if Directory.Exists ctx.OutputDir |> not then
      Directory.CreateDirectory(ctx.OutputDir) |> ignore

    let pipeline = generatePipeline ctx
    let renderCtx =
      let rctx = TemplateContext.create ctx.Culture (sprintf "<!-- %s -->")
      rctx |> TemplateContext.addMany (ctx.ToScriptObjectMap())
           |> TemplateContext.addPartials (ctx.templates |> Map.map (fun _ v -> v.template))

    let tasks =
      seq {
        yield fun () -> generateRssFeed ctx
        yield fun () -> copyContent ctx

        yield! renderTagArchive ctx renderCtx
        yield! renderMonthlyArchive ctx renderCtx

        for pageChunk in ctx.pages |> Array.chunkBySize synchronousRenderCount do
        // for page in ctx.pages do
          yield fun () ->
            for page in pageChunk do
              ctx.logger.trace "processing file '%s'..." page.absoluteLocation
              renderPageToOutput ctx renderCtx pipeline page
      }

    Parallel.ForEach(tasks, fun f -> f()) |> ignore

