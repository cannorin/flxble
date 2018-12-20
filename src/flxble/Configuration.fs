module Flxble.Configuration
open Flxble.Toml
open Flxble.Toml.LensLike
open Scriban.Runtime
open System

type IScribanExportable =
  abstract member WriteTo : ScriptObject -> unit

type TomlDocument with
  member this.WriteTo(sobj: ScriptObject) =
    for KVP(k, v) in this do
      sobj.Add(
        k,
        v.EraceType())

module ScriptObject =
  let ofExportable (x: #IScribanExportable) =
    let sobj = new ScriptObject()
    x.WriteTo(sobj)
    sobj

  let inline importExportable (x: #IScribanExportable) sobj =
    x.WriteTo(sobj)
    sobj

type BlogConfig(str) =
  let toml = TomlDocument.parse str
  member val BaseUrl = toml%.ofString@."base_url"
  member val Title   = toml%.ofString@."title"
  member val Theme   = toml%.ofString@."theme"

  member val Subtitle = toml%.ofString@?"subtitle" ?| ""
  member val Description = toml%.ofString@?"description" ?| ""
  
  member val Language = toml%.ofString@?"lang" ?| "en-us"
  member val SourceDir = toml%.ofString@?"source" ?| "src/"
  member val OutputDir = toml%.ofString@?"output" ?| "output/"
  member val ContentDir = toml%.ofString@?"content" ?| "content/"
  member val ThemesDir = toml%.ofString@?"themes" ?| "themes/"

  member val TagDir = toml%.ofString@?"tag"
  member val ArchiveDir = toml%.ofString@?"archive"
  member val CacheDir = toml%.ofString@?"cache"

  member val MarkdownExtensions = toml%.ofString@?"markdown" ?| "common"
  member val MarkdownExtraRulesDir = toml%.ofString@?"markdown_extra_rules" ?| "extra_rules/"
  member val RawTomlDocument = toml
  interface IScribanExportable with member __.WriteTo(sobj) = toml.WriteTo(sobj)

[<Literal>]
let ExampleBlogConfig = @"# Required
base_url = 'https://example.com'
title = ""flxble""
theme = ""minimal""

# Optional, will be empty by default
subtitle = ""Fast Lightweight eXtensible BLog Engine""
description = '''
""flxble"" stands for Fast Lightweight eXtensible BLog Engine.
It's written in F#, and powered by Markdig markdown processor
and scriban template engine.'''

# Optional, will be culture-insensitive English by default.
lang    = ""en-us""    # can affect month names etc.

# Optional, will be this value by default
source  = ""src/""     # directory to find source files
output  = ""output/""  # directory to output processed files
content = ""content/"" # files in this directory will be copied directly to ./${output}/
themes  = ""themes/""  # directory to find themes

# Optional, disabled by default
tag     = ""tag/""     # tag archives will be generated to
                       # ./${output}/${tag}/${tag-name}.html
archive = ""archive/"" # monthly archives will be generated to
                       # ./${output}/${archive}/${yyyy}-${mm}.html
cache   = "".cache/""  # directory to store caches for skipping unchanged documents

## Markdown config (optional) 
markdown = ""common""  # set this ""advanced"" to enable popular extensions
markdown_extra_rules = ""extra_rules/""
                       # put your custom rule definitions here
"

type PageMetaData(str) =
  let toml = TomlDocument.parse str
  member val PageType = toml%.ofString@?"type" ?| "none"
  member val Title = toml%.ofString@?"title" ?| ""
  member val Date = toml%.ofDate@?"date"
  member val Tags = toml%.ofArray%.ofString@?"tags" ?| []
  member val Description = toml%.ofString@?"description" ?| ""
  member val RawTomlDocument = toml
  interface IScribanExportable with member __.WriteTo(sobj) = toml.WriteTo(sobj)

[<Literal>]
let ExamplePageMetadata = @"# Optional, will be 'none' by default
# | 'post': blog post | 'none': no template applied
# | other string: custom page type
# the corresponding template (${type}.html) will be applied except for 'none'.
type   = 'post'

# Optional, will be empty by default
title  = ""Test Blog Post!""
description = ""this is an example of a page metadata section.""

# Required for blog posts (type = ""post"")
date   = 1979-05-27T07:32:00-08:00 # offset date-time is only allowed!
tags   = [ 'test', 'blog', 'example' ]
"

module PageMetaData =
  open System.Globalization

  let empty =
    let emptyMetadata = sprintf @"
type = 'none'
title = 'no title'
description = ''
tags = []
" 
    PageMetaData(emptyMetadata)

  /// a metadata block must be on top on the file
  /// enclosed by lines consisting of only three hyphens `---` or more.
  let tryExtract str =
    let lines =
      str |> String.splitSeq ["\r\n"; "\r"; "\n"]
          |> Seq.cache
    let inline isMetadataBlockSeparator str =
         str |> String.forall (fun c -> c = '-' || Char.IsWhiteSpace c)
      && str |> String.contains "---"
    let sections =
      lines |> Seq.splitWith isMetadataBlockSeparator
            |> Seq.filter (Seq.isEmpty >> not)
            |> Seq.filter (Seq.forall String.IsNullOrWhiteSpace >> not)
    if Seq.length sections >= 2 then
      let metaBlock = sections |> Seq.item 0
      let length = Seq.length metaBlock
      let metadata =
        metaBlock |> String.concat Environment.NewLine |> PageMetaData
      let remainingText =
        lines |> Seq.skip (length + 2) |> String.concat Environment.NewLine
      Some metadata, remainingText
    else
      None, str
