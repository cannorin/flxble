module Tests.Templating

open System
open System.Globalization
open Xunit
open FsUnit.Xunit
open Flxble.Templating
open Flxble.Templating.SyntaxTree
open FParsec

let ctx =
  TemplateContext.create
    CultureInfo.InvariantCulture
    (sprintf "<!-- %s -->")

let inline private parse parser text fileName =
  let inline runStringWithFileName fileName parser text =
    runParserOnString parser () fileName text

  match runStringWithFileName fileName (spaces >>. parser .>> spaces .>> eof) text with
    | Success (r, _, _) -> r
    | Failure (msg, err, _) ->
        TemplateParseFailed msg |> raise

let inline parseExprWithFileName fn text = parse Parser.exprBlock text fn

let inline parseInlineExprWithFileName fn text = parse Parser.exprInline text fn

let evalTemplateWithFileName fn text =
  match runParserOnString (Parser.template) () fn text with
    | Success (r, _, _) -> 
      Template.renderToString ctx r
    | Failure (msg, _, _) -> TemplateParseFailed msg |> raise

let inline evalCode code =
  let expr = parseExprWithFileName "test" code
  let result = ScriptExpr.eval ctx.bindings expr
  result

let inline evalInlineCode code =
  let expr = parseInlineExprWithFileName "test" code
  let result = ScriptExpr.eval ctx.bindings expr
  result

[<Fact>]
let ``Can parse and render script expression`` () =
  let code = """
let a = {
  field1 = 42,
  field2 = "foo",
  field3 = 42.0,
  field4 = true,
  field5 = null
} in

let b = [0,1,2,3,4] in

let c = {
  a with
    field3 = 2,
    field4 = 3
} in

if 2 * (c.["field3"] + (c).field4) = (b |> array.fold `+` 0) then
  "ok"
else
  "error"
"""
  let result = evalCode code
  result = ScriptObject.String "ok"
  |> should be True

[<Fact>]
let ``Can parse and render script expression 2`` () =
  let code = """
let now = date.now() in
let fmt = "MMMM dd, yyyy" in

let normal = date.to_string fmt now in
let piped = now |> date.to_string fmt in
let applied = `|>` now (date.to_string fmt) in

if (normal = piped) && (piped = applied) then
  true
else
  [normal, piped, applied] |> string.format "{0}, {1}, {2}"
"""
  let result = evalCode code
  result |> should equal (Bool true)

[<Fact>]
let ``Can parse and render script expression 3`` () =
  let code = """
let add2 x = x + 2 in
let mul3 = fun x -> x * 3 in

let x0 = 12 in
let x1 = (add2 >> mul3) 2 in
let x2 = 2 |> add2 |> mul3 in
let x3 = mul3 << add2 <| 2 in

if [x1,x2,x3] |> array.forall (`=` x0) then
  true
else
  [x1,x2,x3] |> string.format "{0}, {1}, {2}"
"""
  let result = evalCode code
  result |> should equal (Bool true)

[<Fact>]
let ``Can parse and render inline script expression`` () =
  let code = """
let a = { \
  field1 = 42, \
  field2 = "foo", \
  field3 = 42.0, \
  field4 = true, \
  field5 = null \
} in \
 \
let b = [0,1,2,3,4] in \
 \
let c = { \
  a with \
    field3 = 2, \
    field4 = 3 \
} in \
 \
if 2 * (c.field3 + (c).field4) = (b |> array.fold `+` 0) then \
  "ok" \
else \
  "error" \
"""
  let result = evalInlineCode code
  result = ScriptObject.String "ok"
  |> should be True

[<Fact>]
let ``Can fail on ilegal inline script expression`` () =
  let code = """
let a = { \
  field1 = 42, \
  field2 = "foo", \
  field3 = 42.0, \
  field4 = true, \
  field5 = null \
} in \
 \
let b = [0,1,2,3,4] in \
 \
let c = { \
  a with \
    field3 = 2,
    field4 = 3 \
} in \
 \
if 2 * (c.field3 + (c).field4) = (b |> array.fold `+` 0) then \
  "ok" \
else \
  "error" \
"""
  shouldFail (fun () -> evalInlineCode code |> ignore)

[<Fact>]
let ``Can parse and render a template`` () =
  let document = """
Hello, World!
%% def pages = [ \
  { title = "page 1", showTag = true,  tags = ["tag1", "tag2", "tag3"] }, \
  { title = "page 2", showTag = false, tags = ["tag2", "tag3", "tag4"] }, \
  { title = "page 3", showTag = true,  tags = ["tag3", "tag4", "tag5"] } \
]
-- Pages --
%% for page in pages do
Title: {{page.title}}
  %% when page.showTag \
      && (array.length page.tags) > 0 do
  Tags: {{page.tags.[0]}}{% for tag in page.tags |> array.skip 1 do %}, {{tag}}{% end %}
  %% otherwise
  No tags.
  %% end
%% end
"""
  let expected = """
Hello, World!
-- Pages --
Title: page 1
  Tags: tag1, tag2, tag3
Title: page 2
  No tags.
Title: page 3
  Tags: tag3, tag4, tag5
"""
  let result = evalTemplateWithFileName "test" document
  result |> should equal expected

[<Fact>]
let ``Lambda captures the current context`` () =
  let document = """
%%def x = 42
%%def f _ = x
%%def x = 0
{{ f() }}
 """
  let result = evalTemplateWithFileName "test" document
  result |> should haveSubstring "42"

