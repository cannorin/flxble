namespace Flxble.Templating

open System.Text
open SyntaxTree

type TemplateContext = Context.TemplateContext
type Template = SyntaxTree.Template
type ScriptObject = SyntaxTree.ScriptObject
module ScriptExpr = SyntaxTree.ScriptExpr

module TemplateContext =
  /// Creates a new `TemplateContext` with the `culture` and a function
  /// `commentize` to comment out error messages.
  let create culture commentize =
    {
      bindings = Builtin.defaultBindings culture
      partials = Map.empty
      culture = culture
      commentize = commentize
    }
  
  let inline tryFind key ctx = ctx.bindings |> Map.tryFind key

  /// Adds a new binding (`key`, `value`) to the context.
  let inline add key value ctx = { ctx with bindings = ctx.bindings |> Map.add key value }

  /// Appends a `binding` to the context.
  let inline addMany bindings ctx =
    { ctx with bindings = Map.append ctx.bindings bindings }

  /// Adds a partial template to the context.
  let inline addPartial name template ctx = { ctx with partials = ctx.partials |> Map.add name template }

  /// Adds partial templates to the context.
  let inline addPartials partials ctx =
    { ctx with partials = Map.append ctx.partials partials }


exception TemplateParseFailed of msg:string
  with
    override this.Message = this.msg

module Template =
  open ScriptExpr
  open System.IO
  open FParsec

  let inline private unwrapParseResult x =
    match x with
      | Success (r, _, _) -> r
      | Failure (msg, _, _) -> TemplateParseFailed msg |> raise

  /// Loads a template from the specified `path` with `encoding`.
  let loadFile encoding path =
    runParserOnFile Parser.template () path encoding
    |> unwrapParseResult

  /// Loads a template from the input `stream` of `name` with `encoding`.
  let loadStream name encoding stream =
    runParserOnStream Parser.template () name stream encoding
    |> unwrapParseResult

  /// Loads a template from the string `str` of `name`.
  let loadString name str =
    runParserOnString Parser.template () name str
    |> unwrapParseResult

  /// Loads a template with optionally TOML metadata from the specified `path` with `encoding`.
  let loadFileWithTomlMetadata encoding path =
    runParserOnFile Parser.templateWithTomlMetadata () path encoding
    |> unwrapParseResult

  /// Loads a template with optionally TOML metadata from the input `stream` of `name` with `encoding`.
  let loadStreamWithTomlMetadata name encoding stream =
    runParserOnStream Parser.templateWithTomlMetadata () name stream encoding
    |> unwrapParseResult

  /// Loads a template with optionally TOML metadata from the string `str` of `name`.
  let loadStringWithTomlMetadata name str =
    runParserOnString Parser.templateWithTomlMetadata () name str
    |> unwrapParseResult

  /// Renders the `script` to the specified `writer` with the `context`.
  let render context (writer: TextWriter) (script: Template) =
    let rec exec ctx = function
      | [] -> ()
      | statement :: rest ->
        match statement with
          | YieldText str ->
            writer.Write str
            exec ctx rest
          | When (cond, a, b) ->
            match eval ctx.bindings cond with
              | Null _
              | Bool false ->
                if b.IsSome then exec ctx b.Value
              | _ -> exec ctx a
            exec ctx rest
          | For (var, xs, next) ->
            match eval ctx.bindings xs with
              | Array xs ->
                for x in xs do
                  exec (ctx |> TemplateContext.add var x) next
              | Record m ->
                for KVP(k, v) in m do
                  let kvp = Map.ofList ["key", String k; "value", v] |> Record
                  exec (ctx |> TemplateContext.add var kvp) next
              | _ -> ()
            exec ctx rest
          | YieldObject obj ->
            (eval ctx.bindings obj).AsCulturalString ctx.culture ctx.commentize
            |> writer.Write
            exec ctx rest
          | Define (var, value) ->
            exec (ctx |> TemplateContext.add var (eval ctx.bindings value)) rest
          | Open record ->
            match eval ctx.bindings record with
              | Record m ->
                exec { ctx with bindings = Map.append ctx.bindings m } rest
              | _ ->
                let loc =
                  match record.info with
                    | ValueSome i -> sprintf " (at %s)" (to_s i.location)
                    | ValueNone -> ""
                loc |> sprintf "open failed, not a record%s"
                    |> ctx.commentize
                    |> writer.Write
                exec ctx rest
          | Partial (name, record) ->
            match ctx.partials |> Map.tryFind' name with
              | ValueSome template ->
                match eval ctx.bindings record with
                  | Record m ->
                    exec (ctx |> TemplateContext.addMany m) template
                  | _ ->
                    let loc =
                      match record.info with
                        | ValueSome i -> sprintf " (at %s)" (to_s i.location)
                        | ValueNone -> ""
                    loc |> sprintf "partial failed, the argument is not a record%s"
                        |> ctx.commentize
                        |> writer.Write
              | ValueNone ->
                sprintf "partial failed, the partial template '%s' is not registered" name
                |> ctx.commentize |> writer.Write
            exec ctx rest
          | Block scr ->
            exec ctx scr
            exec ctx rest

    exec context script

  /// Renders the `script` to a string with the `context`.
  let renderToString context script =
    let sb = new StringBuilder()
    use writer = new StringWriter(sb)
    render context writer script
    sb.ToString()
