module Flxble.Models
open Flxble.Configuration
open Flxble.Templating
open System.IO

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
