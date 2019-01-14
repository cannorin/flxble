open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

open Flxble.Templating

[<Literal>]
let tmpFlxble = """
<ul id='products'>
  %% for product in products do
    <li>
      <h2>{{ product.name }}</h2>
           Only {{ product.price }}
           {{ product.description |> string.truncate 15 "..." }}
    </li>
  %% end
</ul>
"""

[<Literal>]
let tmpLiquid = @"
<ul id='products'>
  {% for product in products %}
    <li>
      <h2>{{ product.name }}</h2>
           Only {{ product.price }}
           {{ product.description | truncate: 15 }}
    </li>
  {% endfor %}
</ul>
"

[<Literal>]
let tmpScriban = @"
<ul id='products'>
  {{ for product in products }}
    <li>
      <h2>{{ product.name }}</h2>
           Only {{ product.price }}
           {{ product.description | string.truncate 15 }}
    </li>
  {{ end }}
</ul>
"

[<Literal>]
let tmpMustache = @"
<ul id='products'>
  {{#products}}
    <li>
      <h2>{{ name }}</h2>
           Only {{ price }}
           {{#truncate}}{{description}}{{/truncate}}
    </li>
  {{/products}}
</ul>
"

[<Literal>]
let tmpCottle = @"
<ul id='products'>
  { for product in products:
    <li>
      <h2>{ product.Name }</h2>
           Only { product.Price }
           { string.truncate(product.Description, 15) }
    </li>
  }
</ul>
"

[<Literal>]
let lorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum"

[<MemoryDiagnoser>]
type Parsing() =
  [<Benchmark(Description="parsing: Flxble")>]
  member __.Flxble() =
    Template.loadString "test" tmpFlxble

  [<Benchmark(Description="parsing: Scriban")>]
  member __.Scriban() =
    Scriban.Template.Parse(tmpScriban)

  [<Benchmark(Description="parsing: DotLiquid")>]
  member __.DotLiquid() =
    DotLiquid.Template.Parse(tmpLiquid)

  [<Benchmark(Description="parsing: Stubble")>]
  member __.Stubble() =
    Stubble.Core.Settings.RendererSettingsBuilder().BuildSettings().Parser.Parse(tmpMustache)

  [<Benchmark(Description="parsing: Cottle")>]
  member __.Cottle() =
    Cottle.Documents.SimpleDocument(tmpCottle)

type Product(name: string, price: float, description: string) =
  member val Name = name
  member val Price = price
  member val Description = description
  member __.ToScriptObject() =
    Map.ofList [
      "name", ScriptObject.String name
      "price", ScriptObject.Float price
      "description", ScriptObject.String description
    ] |> ScriptObject.Record

let inline implicit (x:^a) : ^b = ((^a or ^b) : (static member op_Implicit : ^a -> ^b) x)

[<MemoryDiagnoser>]
type Rendering() =
  let parsers = new Parsing()
  let flxble = parsers.Flxble()
  let scriban = parsers.Scriban()
  let dotLiquid = parsers.DotLiquid()
  let stubble = parsers.Stubble()
  let cottle = parsers.Cottle()
  let products = [|
    for i = 1 to 500 do
      yield new Product(sprintf "Product %i" i, float i, lorem)
  |]
  let dotLiquidProducts = ResizeArray()
  let cottleProducts = ResizeArray()
  do
    for product in products do
      dotLiquidProducts.Add <|
        DotLiquid.Hash.FromAnonymousObject product
      cottleProducts.Add <|
        new Cottle.Values.ReflectionValue(product)
  let flxbleProducts =
    products |> Array.map (fun x -> x.ToScriptObject()) |> ScriptObject.Array

  let cottleStringStore =
    dict [
      Cottle.Value.op_Implicit("string"),
      Cottle.Functions.NativeFunction((fun values ->
        if values.Count <> 2 then
          failwith "truncate expects 2 arguments"
        Scriban.Functions.StringFunctions.Truncate(
          values.[0].AsString,
          values.[1].AsNumber |> int
        ) |> Cottle.Value.op_Implicit),2)
        |> Cottle.Value.op_Implicit
    ] |> System.Collections.Generic.Dictionary

  let invariantCulture = System.Globalization.CultureInfo.InvariantCulture

  let flxbleCtx =
    TemplateContext.create
      invariantCulture 
      (sprintf "<!-- %s -->")

  let scribanCtx =
    let ctx = Scriban.TemplateContext()
    ctx.PushCulture invariantCulture
    ctx

  [<Benchmark(Description="rendering: Flxble")>]
  member __.Flxble() =
    let ctx = flxbleCtx |> TemplateContext.add "products" flxbleProducts
    Template.renderToString ctx flxble

  [<Benchmark(Description="rendering: Scriban")>]
  member __.Scriban() =
    let obj = Scriban.Runtime.ScriptObject()
    obj.Add("products", products)
    scribanCtx.PushGlobal(obj)
    let result = scriban.Render(scribanCtx)
    scribanCtx.PopGlobal() |> ignore
    result

  [<Benchmark(Description="rendering: DotLiquid")>]
  member __.DotLiquid() =
    dotLiquid.Render(DotLiquid.Hash.FromDictionary <| dict ["products", box dotLiquidProducts])
  
  [<Benchmark(Description="rendering: Stubble")>]
  member __.Stubble() =
    let renderer = Stubble.Core.StubbleVisitorRenderer()
    let props = new System.Collections.Generic.Dictionary<string, obj>()
    props.["products"] <- box dotLiquidProducts
    let mutable i = 0
    props.["truncate"] <-
      box <|
        System.Func<string, obj>(fun str ->
          let j = i
          i <- i+1
          box <|
            Scriban.Functions.StringFunctions.Truncate(
              renderer.Render(str, dotLiquidProducts.[j]), 15))
    renderer.Render(tmpMustache, props)
  
  [<Benchmark(Description="rendering: Cottle")>]
  member __.Cottle() =
    let cottleStore = Cottle.Stores.BuiltinStore()
    cottleStore.[implicit "string"] <- implicit cottleStringStore
    cottleStore.[implicit "products"] <- Cottle.Values.ReflectionValue products
    cottle.Render(cottleStore)

[<MemoryDiagnoser>]
type All() =
  let parsers = new Parsing()
  let flxble = parsers.Flxble()
  let scriban = parsers.Scriban()
  let dotLiquid = parsers.DotLiquid()
  let stubble = parsers.Stubble()
  let cottle = parsers.Cottle()
  let products = [|
    for i = 1 to 500 do
      yield new Product(sprintf "Product %i" i, float i, lorem)
  |]
  let dotLiquidProducts = ResizeArray()
  let cottleProducts = ResizeArray()
  do
    for product in products do
      dotLiquidProducts.Add <|
        DotLiquid.Hash.FromAnonymousObject product
      cottleProducts.Add <|
        new Cottle.Values.ReflectionValue(product)
  let flxbleProducts =
    products |> Array.map (fun x -> x.ToScriptObject()) |> ScriptObject.Array

  let cottleStringStore =
    dict [
      Cottle.Value.op_Implicit("string"),
      Cottle.Functions.NativeFunction((fun values ->
        if values.Count <> 2 then
          failwith "truncate expects 2 arguments"
        Scriban.Functions.StringFunctions.Truncate(
          values.[0].AsString,
          values.[1].AsNumber |> int
        ) |> Cottle.Value.op_Implicit),2)
        |> Cottle.Value.op_Implicit
    ] |> System.Collections.Generic.Dictionary

  let invariantCulture = System.Globalization.CultureInfo.InvariantCulture

  let flxbleCtx =
    TemplateContext.create
      invariantCulture 
      (sprintf "<!-- %s -->")

  let scribanCtx =
    let ctx = Scriban.TemplateContext()
    ctx.PushCulture invariantCulture
    ctx

  [<Benchmark(Description="parsing+rendering: Flxble")>]
  member __.Flxble() =
    let flxble = parsers.Flxble()
    let ctx = flxbleCtx |> TemplateContext.add "products" flxbleProducts
    Template.renderToString ctx flxble

  [<Benchmark(Description="parsing+rendering: Scriban")>]
  member __.Scriban() =
    let scriban = parsers.Scriban()
    let obj = Scriban.Runtime.ScriptObject()
    obj.Add("products", products)
    scribanCtx.PushGlobal(obj)
    let result = scriban.Render(scribanCtx)
    scribanCtx.PopGlobal() |> ignore
    result

  [<Benchmark(Description="parsing+rendering: DotLiquid")>]
  member __.DotLiquid() =
    let dotLiquid = parsers.DotLiquid()
    dotLiquid.Render(DotLiquid.Hash.FromDictionary <| dict ["products", box dotLiquidProducts])
  
  [<Benchmark(Description="parsing+rendering: Stubble")>]
  member __.Stubble() =
    let renderer = Stubble.Core.StubbleVisitorRenderer()
    let props = new System.Collections.Generic.Dictionary<string, obj>()
    props.["products"] <- box dotLiquidProducts
    let mutable i = 0
    props.["truncate"] <-
      box <|
        System.Func<string, obj>(fun str ->
          let j = i
          i <- i+1
          box <|
            Scriban.Functions.StringFunctions.Truncate(
              renderer.Render(str, dotLiquidProducts.[j]), 15))
    renderer.Render(tmpMustache, props)
  
  [<Benchmark(Description="parsing+rendering: Cottle")>]
  member __.Cottle() =
    let cottle = parsers.Cottle()
    let cottleStore = Cottle.Stores.BuiltinStore()
    cottleStore.[implicit "string"] <- implicit cottleStringStore
    cottleStore.[implicit "products"] <- Cottle.Values.ReflectionValue products
    cottle.Render(cottleStore)

[<EntryPoint>]
let main argv =
  let switcher =
    BenchmarkSwitcher.FromTypes [| typeof<Parsing>; typeof<Rendering>; typeof<All> |]
  switcher.RunAllJoined() |> ignore
  0

