module Flxble.Models
open Flxble.Configuration
open Flxble.Templating
open System.IO
open System.Text

/// Page type. Also specifies which template to apply.
[<RequireQualifiedAccess; Struct>]
type PageType =
  /// blog post
  | Post
  /// does not apply any template
  | None
  /// any other page type
  | Other of string

/// File type.
[<RequireQualifiedAccess; Struct>]
type PageFormat =
  /// .md, .markdown
  | Markdown
  /// .html
  | Html
  /// .html found in ${config.theme-dir}/templates/
  | ScribanTemplate of scribanTemplateName: string
  // /// .liquid found in ${config.theme-dir}/templates/
  // | LiquidTemplate  of liquidTemplateName:  string
  /// will be copied as-is.
  | Other

[<Struct>]
type PageInfo = {
  /// location of the file relative to ${source}.
  relativeLocation: string 
  absoluteLocation: string
  format: PageFormat
  metadata: PageMetaData voption
  content: string
  index: int voption
  mutable scriptObjectMap: Map<string, ScriptObject> voption
} with
  member this.PageType =
    match this.metadata |> ValueOption.map (fun x -> x.PageType) with
      | ValueSome "post" -> PageType.Post
      | ValueSome "none"
      | ValueNone -> PageType.None
      | ValueSome str -> PageType.Other str
  member this.ToScriptObjectMap() =
    match this.scriptObjectMap with
      | ValueSome x -> x
      | ValueNone ->
        let location =
          match this.format with
            | PageFormat.Markdown -> Path.ChangeExtension(this.relativeLocation, "html")
            | _ -> this.relativeLocation
        let map =
          this.metadata
          |> ValueOption.map (fun x -> x.ToScriptObjectMap())
          |> ValueOption.defaultValue Map.empty
          |> Map.add "location" (SyntaxTree.ScriptObject.String location)
        this.scriptObjectMap <- ValueSome map
        map
  /// loads a PageInfo from the path.
  static member load sourceDir (absolutePath: string) =
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

[<Struct>]
type TemplateInfo = {
  name: string
  dependsOn: string voption
  metadata: PageMetaData voption
  template: Template
  mutable scriptObjectMap: Map<string, ScriptObject> voption
} with
  member this.ToScriptObjectMap() = 
    match this.scriptObjectMap with
      | ValueSome x -> x
      | ValueNone ->
        let map =
          this.metadata
          |> ValueOption.map (fun x -> x.ToScriptObjectMap()) 
          |> ValueOption.defaultValue Map.empty
          |> Map.add "template_name" (SyntaxTree.ScriptObject.String this.name)
        this.scriptObjectMap <- ValueSome map
        map
  /// loads a TemplateInfo from the path.
  static member load (path: string) =
    let absPath = Path.GetFullPath path
    let toml, template =
      let encoding = Encoding.UTF8
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


