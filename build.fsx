#r "paket:
source https://api.nuget.org/v3/index.json
nuget FSharp.Core
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fake.IO.Zip
nuget Fake.DotNet.MSBuild
nuget Fake.Core.Target
nuget Fake.DotNet.AssemblyInfoFile
nuget Fake.Core.ReleaseNotes //"

#load ".fake/build.fsx/intellisense.fsx"
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open System.Text.RegularExpressions

type Date = System.DateTimeOffset

let (|Regex|_|) pattern input =
  let m = Regex.Match(input, pattern)
  if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
  else None

let loadReleaseNoteWithAutoIncrement releaseNote =
  let y2k = Date.Parse "1/1/2010+0000"
  let today = Date.UtcNow
  let dayssincey2k = (today - y2k).Days
  let secs = today.TimeOfDay.TotalSeconds |> int |> (/) <| 2
  let lines = File.read releaseNote
  let topLine =
    match (lines |> Seq.tryHead) with
      | Some (Regex @"^(\*|\#\# New in) ([0-9]+)\.([0-9]+)\.([0-9]+)(?:\.\*) (.+)$" [head; maj; min; rev; tail]) ->
        sprintf "%s %s.%s.%s.%i %s" head maj min rev (today.DayOfYear * 10000 + secs) tail
      | Some (Regex @"^(\*|\#\# New in) ([0-9]+)\.([0-9]+)(?:\.\*) (.+)$" [head; maj; min; tail]) ->
        sprintf "%s %s.%s.%i.%i %s" head maj min dayssincey2k secs tail
      | Some x -> x
      | None ->
        sprintf "0.%i.%i.%i" (today.Year - 2000) today.DayOfYear secs
  lines |> Seq.mapi (fun i x -> if i = 0 then topLine else x)
        |> ReleaseNotes.parse

// Read additional information from the release notes document
let release = loadReleaseNoteWithAutoIncrement "RELEASE_NOTES.md"

release.AssemblyVersion |> Trace.tracefn "Auto version: %s"

Target.create "ZipBlogTemplate" (fun _ ->
  let target = "./src/blog_template.zip"
  if File.exists target then File.delete target
  Trace.tracefn "Updating '%s'.." target
  Zip.zip "./src/blog_template" target <|
    !!"./src/blog_template/**/*"
)

Target.create "WriteVersion" (fun _ ->
  AssemblyInfoFile.createFSharp "./src/common/Version.fs" [
    AssemblyInfo.Version release.AssemblyVersion
    AssemblyInfo.FileVersion release.AssemblyVersion
  ]
)

Target.create "Clean" (fun _ ->
  !! "src/**/bin"
  ++ "src/**/obj"
  |> Shell.cleanDirs 
)

Target.create "Build" (fun _ ->
  !! "src/**/*.*proj"
  |> Seq.iter (DotNet.build (fun opt ->
    { opt with
        Configuration = DotNet.BuildConfiguration.Release
    }))
)

Target.create "Publish" (fun _ ->
  let publish runtime =
    DotNet.publish (fun opt ->
      { opt with
          Common = opt.Common |> DotNet.Options.withAdditionalArgs ["--self-contained=true"]
          Runtime = Some runtime
          Configuration = DotNet.BuildConfiguration.Release
          MSBuildParams = {
            opt.MSBuildParams with
              Properties =
                ("PackAsTool", "false") :: opt.MSBuildParams.Properties
          }
          OutputPath = Some <| sprintf "../../bin/publish/%s" runtime
      }
    ) "src/flxble/flxble.fsproj"
  let runtimes = [
    "win-x64"
    "linux-x64"
    "linux-arm"
    "osx-x64"
  ]
  for runtime in runtimes do
    publish runtime
    let deployPath = sprintf "./bin/publish/%s" runtime
    Trace.tracefn "---> %s" (sprintf "%s.zip" deployPath)
    Zip.zip deployPath (sprintf "%s.zip" deployPath) <|
      !!(sprintf "%s/**/*" deployPath)
)

Target.create "Pack" (fun _ ->
  !! "src/**/*.*proj"
  |> Seq.iter (fun proj -> proj |> DotNet.pack (fun opt ->
    { opt with
        Configuration = DotNet.BuildConfiguration.Release
        OutputPath = Some "../../bin/packages/"
        MSBuildParams = {
          opt.MSBuildParams with
            Properties =
              ["PackageVersion", release.NugetVersion]
              @ opt.MSBuildParams.Properties
        }
    }
  ))
)

Target.create "All" ignore

"Clean"
  ==> "ZipBlogTemplate"
  ==> "Pack"
  ==> "Publish"
  ==> "All"

Target.runOrDefault "All"
