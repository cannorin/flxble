module Flxble.Models
open Flxble.Configuration
open Scriban
open Scriban.Runtime
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
  metadata: PageMetaData option
  content: string
} with
  member this.PageType =
    match this.metadata |> Option.map (fun x -> x.PageType) with
      | Some "post" -> PageType.Post
      | Some "none"
      | None -> PageType.None
      | Some str -> PageType.Other str
  interface IScribanExportable with
    member this.WriteTo(sobj) =
      let location =
        match this.format with
          | PageFormat.Markdown -> Path.ChangeExtension(this.relativeLocation, "html")
          | _ -> this.relativeLocation
      sobj.Add("location", location)
      this.metadata |> Option.iter (ScriptObject.ofExportable >> sobj.Import)

[<Struct>]
type TemplateInfo = {
  name: string
  dependsOn: string option
  metadata: PageMetaData option
  template: Template
} with
  interface IScribanExportable with
    member this.WriteTo(sobj) =
      sobj.Add("name", this.name)
      this.metadata |> Option.iter (ScriptObject.ofExportable >> sobj.Import)

