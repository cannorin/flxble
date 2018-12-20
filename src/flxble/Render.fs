module Flxble.Render
open System
open System.IO
open System.Threading
open System.Threading.Tasks
open FSharp.Collections
open Flxble.Configuration
open Flxble.Models
open Flxble.Context
open Scriban
open Scriban.Runtime
open Markdig
open System.Net

let rec private applyTemplateToHtml ctx sobj templateName html fileName =
  tryOperation ctx "applyTemplateToHtml" fileName <| fun () ->
    let tmpInfo =
      ctx.templates
        |> Map.tryFind templateName
        |> Option.defaultWith (
          fun () ->
            ctx.logger.fail
              "the specified template '%s' does not exist"
              templateName)
  
    let sobj' =
      let x = new ScriptObject()
      x.Import(sobj)
      let tobj = tmpInfo |> ScriptObject.ofExportable
      x.Add("template", tobj)
      x.Add("content", html)
      x

    let html' =
      tmpInfo.template.Render(sobj')
    
    match tmpInfo.dependsOn with
      | Some tmpName ->
        applyTemplateToHtml ctx sobj' tmpName html' fileName
      | None -> html'

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

let private convertIfMarkdown pipeline page =
  if page.format = PageFormat.Markdown then
    { page with
        format = PageFormat.Html
        content = Markdown.ToHtml(page.content, pipeline)
    }
  else page

let private renderPageToOutput ctx pipeline prevnextfinder sobj page =
  tryOperation ctx "renderPageToOutput" page.relativeLocation <| fun () ->
    if Directory.Exists ctx.OutputDir |> not then
      ctx.logger.warning "the output directory '%s' does not exist, creating." ctx.config.OutputDir
      Directory.CreateDirectory(ctx.OutputDir) |> ignore

    match page.format with
      | PageFormat.Html
      | PageFormat.Markdown ->
        let path =
          Path.Combine(
            ctx.OutputDir,
            Path.ChangeExtension(page.relativeLocation, "html"))

        ctx.logger.trace "generating '%s'..." path

        let page = convertIfMarkdown pipeline page
        assert (page.format = PageFormat.Html)

        let sobj' = new ScriptObject()
        let pageObj = ScriptObject.ofExportable page
        sobj'.Import(sobj)
        sobj'.Add("page", pageObj)

        page.metadata |> Option.iter (fun mt ->
          let prev, next = prevnextfinder mt.PageType page
          prev |> Option.iter (fun x -> sobj'.Add("prev_date_page", ScriptObject.ofExportable x))
          next |> Option.iter (fun x -> sobj'.Add("next_date_page", ScriptObject.ofExportable x))
        )

        let content =
          Template.Parse(page.content).Render(sobj')

        let html =
          match page.metadata with
            | Some mt when mt.PageType <> "none" ->
              applyTemplateToHtml ctx sobj' mt.PageType content page.absoluteLocation
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

let private renderArchive archiveType tempName elements predicate title printer outDir ctx sobj =
  tryOperation ctx (sprintf "renderArchive: %s" archiveType) "current context" <| fun () ->
    let sobj' = new ScriptObject()
    sobj'.Import(sobj)
    
    let tagTmp =
      ctx.templates
        |> Map.tryFind tempName
        |> Option.defaultWith (fun () ->
            ctx.logger.fail "the theme '%s' does not have the '%s' template."
              ctx.config.Theme tempName)
    let pages =
      ctx.pages
      |> Seq.choose (function
        | { relativeLocation = l; metadata = Some meta;
            format = PageFormat.Html | PageFormat.Markdown } as page ->
          Some (Path.ChangeExtension(l, ".html"), meta, page)
        | _ -> None)

    for element in elements do
      let pagesObj = new ScriptArray()

      pagesObj.AddRange(
        pages
          |> Seq.filter (fun (_, x, _) -> predicate element x)
          |> Seq.map    (fun (_, _, page) -> page |> ScriptObject.ofExportable |> box)
      )

      let arcObj = new ScriptObject()
      arcObj.Add("pages", pagesObj)
      arcObj.Add("title", title element)
      arcObj.Add("type",  archiveType)

      sobj'.Add("archive", arcObj)
    
      let outputDir = Path.Combine(ctx.OutputDir, outDir)

      if Directory.Exists outputDir |> not then
        ctx.logger.warning "the output directory '%s' does not exist, creating." outputDir
        Directory.CreateDirectory(outputDir) |> ignore
    
      let outPath = Path.Combine(outputDir, sprintf "%s.html" <| printer element)
      ctx.logger.trace "generating '%s'..." outPath
      
      let html =
        let h = tagTmp.template.Render(sobj')
        match tagTmp.dependsOn with
          | Some x -> applyTemplateToHtml ctx sobj' x h (sprintf "template: %s" x)
          | None -> h
      
      File.WriteAllText(outPath, html)

let private renderTagArchive (ctx: Context) sobj =
  ctx.config.TagDir
  |> Option.iter (fun dir ->
    ctx.logger.info "processing tag archive..."
    renderArchive "tag" "archive" ctx.Tags
      (fun tag x -> x.Tags |> List.contains tag)
      (sprintf "Tag: %s")
      WebUtility.UrlEncode dir ctx sobj)

let private renderMonthlyArchive (ctx: Context) sobj =
  let inline getMonthName i =
    ctx.Culture.DateTimeFormat.GetMonthName(i)
  ctx.config.ArchiveDir
  |> Option.iter (fun dir ->
    ctx.logger.info "processing monthly archive..."
    renderArchive "archive" "archive" ctx.Months
      (fun d x -> x.Date.IsSome && x.Date.Value.Year = d.Year && x.Date.Value.Month = d.Month)
      (fun d -> sprintf "Archive: %s %i" (getMonthName d.Month) d.Year)
      (fun d -> sprintf "%i-%i" d.Year d.Month)
      dir ctx sobj)

/// Render everything in the current context.
let everything (ctx: Context) =
  tryOperation ctx "Render.everything" "current context" <| fun () ->
    let pipeline = generatePipeline ctx
    let sobj = ctx |> ScriptObject.ofExportable

    // scriban's null check is broken, so empty dicts are needed beforehand:
    sobj.Add("page", Dict.empty<string, obj>)
    sobj.Add("archive", Dict.empty<string, obj>)

    let prevnextfinder = ctx.PrevNextPostFinder

    let result =
      seq {
        yield async { renderTagArchive ctx sobj }
        yield async { renderMonthlyArchive ctx sobj }

        for pageChunk in ctx.pages |> Seq.chunkBySize 50 do
        // for page in ctx.pages do
          yield async {
            for page in pageChunk do
              ctx.logger.trace "processing file '%s'..." page.absoluteLocation
              renderPageToOutput ctx pipeline prevnextfinder sobj page
          }
      } |> Async.Parallel
        |> Async.Catch
        |> Async.RunSynchronously

    match result with
      | Choice2Of2 exn -> reraise' exn
      | _ -> ()
    ()
    

