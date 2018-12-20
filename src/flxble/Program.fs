module Flexble.Main
open Flxble
open Flxble.Toml
open Flxble.Toml.LensLike
open Flxble.Configuration
open Flxble.Context
open System
open System.Reflection
open Scriban
open Scriban.Parsing
open Scriban.Runtime

[<EntryPoint>]
let main argv =
  if argv.Length > 0 then
    let f = argv.[0]

    let ctx = Context.load None (Some f)
    Render.everything ctx
  0