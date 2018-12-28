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

type PageInfo = {
  /// location of the file relative to ${source}.
  relativeLocation: string 
  absoluteLocation: string
  format: PageFormat
  metadata: PageMetaData option
  content: string
  mutable scriptObjectMap: Map<string, ScriptObject> option
} with
  member this.PageType =
    match this.metadata |> Option.map (fun x -> x.PageType) with
      | Some "post" -> PageType.Post
      | Some "none"
      | None -> PageType.None
      | Some str -> PageType.Other str
  member this.ToScriptObjectMap() =
    match this.scriptObjectMap with
      | Some x -> x
      | None ->
        let location =
          match this.format with
            | PageFormat.Markdown -> Path.ChangeExtension(this.relativeLocation, "html")
            | _ -> this.relativeLocation
        let map =
          this.metadata
          |> Option.map (fun x -> x.ToScriptObjectMap()) ?| Map.empty
          |> Map.add "location" (SyntaxTree.ScriptObject.String location)
        this.scriptObjectMap <- Some map
        map

type TemplateInfo = {
  name: string
  dependsOn: string option
  metadata: PageMetaData option
  template: Template
  mutable scriptObjectMap: Map<string, ScriptObject> option
} with
  member this.ToScriptObjectMap() = 
    match this.scriptObjectMap with
      | Some x -> x
      | None ->
        let map =
          this.metadata
          |> Option.map (fun x -> x.ToScriptObjectMap()) ?| Map.empty
          |> Map.add "template_name" (SyntaxTree.ScriptObject.String this.name)
        this.scriptObjectMap <- Some map
        map
