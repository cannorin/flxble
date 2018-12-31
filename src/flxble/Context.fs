module Flxble.Context
open Flxble.Configuration
open Flxble.Models
open Flxble.Templating
open Flxble.Templating.ScriptObjectHelper
open System
open System.IO
open System.Runtime.ExceptionServices
open System.Globalization
open DataTypeExtra

type ContextHelperCache(this: Context) =
  member val Culture =
    lazy (
      try
        new CultureInfo(this.config.Language)
      with
        | :? CultureNotFoundException ->
          this.logger.fail "the language '%s' is not supported." this.config.Language
    )
  member val ConfigDir = lazy (Path.GetDirectoryName this.configLocation)
  member val SourceDir = lazy (Path.Combine(this.ConfigDir, this.config.SourceDir))
  member val OutputDir = lazy (Path.Combine(this.ConfigDir, this.config.OutputDir))
  member val Tags =
    lazy (
      this.pages
        |> Array.collect (function { metadata = ValueSome md } -> md.Tags |> Array.ofList | _ -> [||])
        |> Array.distinct
    )
  member val Months =
    lazy (
      this.pages  
        |> Array.choose (function
          | { metadata = ValueSome md } ->
            md.Date |> Option.map (fun x -> new DateTime(x.Year, x.Month, 1))
          | _ -> None)
        |> Array.distinct
    )
  member val AsScriptObjectMap =
    lazy (
      let pagesOfType x =
        match x with
          | [ScriptObject.String name] ->
            this.PagesOfType name |> Array.map ScriptObject.from |> ScriptObject.Array
          | _ -> ScriptObject.Null DataTypeExtra.ENull

      let pagesOfTag x =
        match x with
          | [ScriptObject.String name] ->
            this.PagesOfTag name |> Array.map ScriptObject.from |> ScriptObject.Array
          | _ -> ScriptObject.Null DataTypeExtra.ENull

      let pagesOfMonth x =
        match x with
          | [ScriptObject.Int year; ScriptObject.Int month] ->
            this.PagesOfMonth year month |> Array.map ScriptObject.from |> ScriptObject.Array
          | _ -> ScriptObject.Null ENull

      Map.ofArray [|
        yield "config", this.config |> ScriptObject.from
        yield "pages", array' (this.pages |> Array.map ScriptObject.from)
        yield "templates", ScriptObject.Record (this.templates |> Map.map (fun _ -> ScriptObject.from))
        yield "tags",  array' (this.Tags  |> Array.map ScriptObject.String)
        yield "months", array' (this.Months |> Array.map ScriptObject.Date)
        yield "pages_of_month", function' 2 pagesOfMonth
        yield "pages_of_tag", function' 1 pagesOfTag
        yield "pages_of_type", function' 1 pagesOfType
      |]
    )

and Context = {
  /// absolute location of the config file.
  configLocation: string
  config: BlogConfig
  pages: PageInfo array
  templates: Map<string, TemplateInfo>
  logger: Logger
  mutable cache: ContextHelperCache voption
} with
  member this.Cache =
    match this.cache with
      | ValueNone ->
        let cache = ContextHelperCache(this)
        this.cache <- ValueSome cache
        cache
      | ValueSome x -> x
  
  member this.Culture = this.Cache.Culture.Value
  member this.ConfigDir = this.Cache.ConfigDir.Value
  member this.SourceDir = this.Cache.SourceDir.Value
  member this.OutputDir = this.Cache.OutputDir.Value
  member this.Tags = this.Cache.Tags.Value
  member this.Months = this.Cache.Months.Value

  member this.PagesOfType ty =
    this.pages
      |> Array.filter (function
        | { metadata = ValueSome md } when md.PageType = ty -> true
        | _ -> false)
  
  member this.PagesOfTag tag =
    this.pages
      |> Array.filter (function
        | { metadata = ValueSome md } when md.Tags |> List.contains tag -> true
        | _ -> false)

  member this.PagesOfMonth year month =
    this.pages
      |> Array.filter (function
        | { metadata = ValueSome md } ->
          md.Date
          |> Option.map (fun d -> d.Year = year && d.Month = month)
          |> Option.defaultValue false
        | _ -> false)

  member this.FindPrevNextPost page =
    match page with
      | { metadata = ValueNone; relativeLocation = _ } -> ValueNone, ValueNone
      | { metadata = ValueSome md; index = ValueSome i } ->
        if md.Date.IsSome then
          this.pages |> Array.tryItem' (i+1), this.pages |> Array.tryItem' (i-1)
        else ValueNone, ValueNone
      | { metadata = ValueSome md } ->
        let date = md.Date
        match this.pages |> Array.tryFindIndex' (fun x -> x.metadata.IsSome && x.metadata.Value.Date = date) with
          | ValueNone -> ValueNone, ValueNone
          | ValueSome i -> this.pages |> Array.tryItem' (i+1), this.pages |> Array.tryItem' (i-1)

  member this.ToScriptObjectMap() = this.Cache.AsScriptObjectMap.Value

type FlxbleException
  (ctx: Context, operationName: string, target: string, innerExn: ExceptionDispatchInfo) =
  inherit Exception(
    sprintf "the operation '%s' on target '%s' failed with '%s'"
      operationName
      target
      (innerExn.SourceException.GetType().Name)
    , innerExn.SourceException)
  member val Context = ctx
  member val OperationName = operationName
  member val Target = target

let inline tryOperation ctx name target body =
  try body ()
  with
    | :? FlxbleException as ex -> reraise' ex
    | ex ->
      let inner = System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(ex)
      FlxbleException(ctx, name, target, inner) |> raise

module Context =
  // get a PageInfo from the path.
  let private getPageInfo sourceDir (absolutePath: string) (ctx: Context) =
    tryOperation ctx "addFile" absolutePath <| fun () ->
      let absPath = Path.GetFullPath absolutePath
      let content = File.ReadAllText absPath
      let (metadata, content) = PageMetaData.tryExtract content
      let relativePath =
        absPath |> Path.makeRelativeTo sourceDir
      let pageFormat =
        match Path.GetExtension absPath with
          | ".md" | ".markdown" -> PageFormat.Markdown
          | ".htm" | ".html" -> PageFormat.Html
          | _ -> PageFormat.Other
      {
        relativeLocation = relativePath
        absoluteLocation = absPath
        format   = pageFormat
        metadata = metadata
        content  = content
        scriptObjectMap = ValueNone
        index = ValueNone
      }

  let inline private sortPagesAndCache ctx =
    let pages =
      ctx.pages 
        |> Array.sortByDescending (
          fun x ->
            match x.metadata with
              | ValueSome md -> x.PageType, md.Date
              | ValueNone -> x.PageType, None
          )
        |> Array.mapi (fun i page -> { page with index = ValueSome i })
    { ctx with pages = pages }

  let inline private clearCache ctx = { ctx with cache = ValueNone }
  
  /// adds all files found in the source directory to the current context.
  let private addAllSourceFiles (ctx: Context) =
    tryOperation ctx "addAllSourceFiles" ctx.SourceDir <| fun () ->
      if Directory.Exists ctx.SourceDir |> not then
        ctx.logger.warning "the specified source directory '%s' does not exist in '%s'"
          ctx.config.SourceDir
          ctx.SourceDir
        ctx
      else
        let pages =
          Directory.enumerateFilesRecursively false ctx.SourceDir
          |> Seq.map (fun file -> getPageInfo ctx.SourceDir file ctx)
          |> Seq.toArray
        { ctx with pages = Array.append ctx.pages pages }
  
  /// get a TemplateInfo from the path.
  let private getTemplateInfo (path: string) ctx =
    tryOperation ctx "addTemplate" path <| fun () ->
      let absPath = Path.GetFullPath path
      let toml, template =
        let encoding = Text.Encoding.UTF8
        Template.loadFileWithTomlMetadata encoding absPath
      let metadata = toml |> Option.map PageMetaData
      let dependency =
        metadata |> Option.map (fun md -> md.PageType)
      let name = Path.GetFileNameWithoutExtension absPath
      name, {
        name = name
        dependsOn = dependency |> ValueOption.ofOption
        metadata = metadata |> ValueOption.ofOption
        template = template
        scriptObjectMap = ValueNone
      }

  /// loads the theme to the current context.
  let private loadTheme (ctx: Context) =
    let themeFolder =
      Path.combineMany [|
        ctx.ConfigDir;
        ctx.config.ThemesDir; ctx.config.Theme |]
    
    let ctx =
      tryOperation ctx "loadThemeTemplates" themeFolder <| fun () ->
        if Directory.Exists themeFolder |> not then
          ctx.logger.fail "the specified theme '%s' does not exist in '%s'"
            ctx.config.Theme ctx.config.ThemesDir
        let templatesDir =
          Path.Combine(themeFolder, "templates")
        if Directory.Exists templatesDir |> not then
          ctx.logger.warning "the 'templates' directory of theme '%s' is empty"
            ctx.config.Theme
          ctx
        else
          let mp =
            Directory.enumerateFilesRecursively false templatesDir
            |> Seq.map (fun file -> getTemplateInfo file ctx)
          let newMap =
            Map.append (Map.ofSeq mp) ctx.templates
          { ctx with templates = newMap }

    tryOperation ctx "loadThemeSource" themeFolder <| fun () ->
      let sourceDir = Path.Combine(themeFolder, "src")
      if Directory.Exists sourceDir then
        let pages =
          Directory.enumerateFilesRecursively false sourceDir
          |> Seq.map (fun file -> getPageInfo sourceDir file ctx)
          |> Seq.toArray
        { ctx with pages = Array.append ctx.pages pages }
      else ctx
  
  /// creates a new context from the given configuration file and (optionally) a logger.
  ///   if not given, the one in the current directory will be used (if exists).
  /// also loads the theme and the source files.
  let load (logger: Logger option) configLocation =
    let logger = logger ?| Logger.create()
    try
      let location =
        match configLocation with
          | Some l -> Path.GetFullPath l
          | None ->
            Environment.CurrentDirectory
            |> Directory.GetFiles
            |> Array.tryFind (fun file ->
                match
                  Path.GetFileNameWithoutExtension file,
                  Path.GetExtension file with
                  | "flex", ".toml" -> true
                  | _ -> false
              )
            |> Option.map Path.GetFullPath
            |> Option.defaultWith
                (fun () ->
                  logger.failExn
                    (fun msg ->
                      FileNotFoundException(
                        msg,
                        Path.Combine(Environment.CurrentDirectory, "flex.toml")))
                    "flex config file not found in the current directory.")
      let content = File.ReadAllText location
      let blogConf = BlogConfig(content)
      {
        configLocation = location
        config = blogConf
        pages = Array.empty
        templates = Map.empty
        logger = logger
        cache = ValueNone
      } |> loadTheme |> addAllSourceFiles |> sortPagesAndCache |> clearCache
    with
      | ( :? DirectoryNotFoundException
        | :? FileLoadException
        | :? FileNotFoundException) & ex ->
        logger.error "problem loading flex.toml: %s" ex.Message
        reraise' ex
