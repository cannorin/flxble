module Flxble.Templating.Parser

open Flxble.Templating.SyntaxTree
open FParsec
open FParsecUtils
open DataTypeExtra

type parser<'a> = Parser<'a, unit>
type sc<'a> = ISpaceCombinatorsProvider<'a>

let keywords =
  ["true"; "false"; "let"; "for"; "in"; "do"; "done"; "null"
   "if"; "then"; "else"; "fun"; "open"; "begin"; "end"; "with";
   "def"; "when"; "otherwise"; ]

let op_chars = "+-*/<>%|&^~=!:".ToCharArray() |> Set.ofArray
let op_reserved = ["||"; "&&"; "->"]

let name : parser<_> =
  let identifierString =
    many1Satisfy (List.concat [['a'..'z']; ['A'..'Z']; ['_']; ['0'..'9']] |> isAnyOf)
  let reserved =
    Set.ofList keywords
  let expectedIdentifier = expected "identifier"
  fun stream ->
    let state = stream.State
    let reply = identifierString stream
    if reply.Status = Ok && not (reserved |> Set.contains reply.Result || System.Char.IsDigit (reply.Result.[0])) then reply
    else
      stream.BacktrackTo(state)
      Reply(Error, expectedIdentifier)

let opvar = cyn '`' >>. many1Satisfy ((<>) '`') .>> cyn '`'

let inline addinfo parser =
  parser |> location |>> fun (x, loc) -> x |> With.info { location = loc }

let inline prepareOpp (sc: #sc<_>) op_chars op_reserved op2Handler =
  let opp = new OperatorPrecedenceParser<'t, string, unit>()
  opp.OperatorConflictErrorFormatter <-
    fun (pos1, op1, afterString1) (pos2, op2, afterString2) ->
      let msg = sprintf "The operator '%s' conflicts with the previous operator '%s' at %A."
                        (op2.String + afterString2)
                        (op1.String + afterString1) pos1
      in messageError msg
  let addOp2Ext prefix precedence associativity =
    let op =
      InfixOperator (
        prefix, 
        manySatisfy (isAnyOf op_chars) .>> sc.spaces,
        precedence, 
        associativity, 
        (),
        op2Handler prefix
      )
    in
    opp.AddOperator(op)
  let addOp2Res x pcd asy =
    for i in op_chars |> Set.map (fun c -> sprintf "%s%c" x c)
                      |> Set.filter (fun s -> op_reserved |> List.contains s |> not) do
      addOp2Ext i pcd asy
  
  (opp, addOp2Ext, addOp2Res)

let inline expr (sc: #sc<_>) = recursive <| fun expr ->
  let inline ws x = sc.ws x
  let inline spaces x = sc.spaces x
  let inline spaces1 x = sc.spaces1 x

  let quotedString =
    let quote = cyn '"'
    escapedString ['"']
    |> between quote quote
  
  let stringLiteral =
    quotedString
    |>> (String >> Literal)
    |> addinfo

  let inline bracketed l r parser =
    parser |> between (ws <| cyn l) (cyn r)

  let boolLiteral =
    pdictL [ "true", Literal <| Bool true; "false", Literal <| Bool false ] "boolean"
    |> addinfo

  let intLiteral = pint32 .>> notFollowedByString "." |>> (Int >> Literal) |> addinfo

  let floatLiteral = pfloat |>> (Float >> Literal) |> addinfo

  let nullLiteral = (syn "()" <|> syn "null") >>% Literal (Null ENull) |> addinfo

  let arrayExpr =
    sepBy1 (ws expr) (ws <| cyn ',')
    |> bracketed '[' ']'
    |>> ArrayNew |> addinfo

  let recordExpr =
    let binding = ws name .>> ws (cyn '=') .>>. expr
    let bindings = sepBy1 (ws binding) (ws <| cyn ',')
    let withClause =
      ws expr .>> ws (syn "with")
    let body =
      attempt (opt withClause) <|> preturn None
      .>>. bindings
      |>> function
        | Some e, bs -> RecordWith (e, bs)
        | None,   bs -> RecordNew bs
    body |> bracketed '{' '}' |> addinfo

  
  let variableExpr =
    (name <|> opvar) |>> Variable |> addinfo

  let ifExpr =
    tuple3
      (ws (syn "if") >>. ws expr)
      (ws (syn "then") >>. ws expr)
      (ws (syn "else") >>. expr)
    |>> If |> addinfo

  let lambdaExpr =
    tuple2
      (ws (syn "fun") >>. sepEndBy1 name spaces1)
      (ws (syn "->") >>. expr)
    |>> Lambda |> addinfo

  let letExpr =
    tuple3
      (ws (syn "let") >>. ws name .>>. sepEndBy name spaces1)
      (ws (cyn '=')   >>. ws expr)
      (ws (syn "in")  >>. expr)
    |>> (fun ((name, args), value, body) ->
      if args |> List.isEmpty then Let(name, value, body)
      else Let(name, Lambda(args, value) |> With.noInfo, body))
    |> addinfo

  let nonLeftRecursive = choice [
    stringLiteral
    boolLiteral
    (attempt intLiteral <|> floatLiteral)
    arrayExpr
    recordExpr
    ifExpr
    lambdaExpr
    letExpr
    variableExpr
    nullLiteral
    bracketed '(' ')' expr
  ]

  let dotExpr =
    let indexer =
      expr |> bracketed '[' ']' 

    let field = name

    let access =
      ws (cyn '.') >>. (field <||> indexer)
      |> addinfo

    nonLeftRecursive 
    .>>. many access
    |>> fun (e, xs) ->
      xs |> List.fold (fun state x ->
        match x.item with
          | Choice1Of2 m ->
            MemberAccess(m, state) |> With.sameInfoOf x
          | Choice2Of2 i ->
            IndexerAccess(i, state) |> With.sameInfoOf x
      ) e

  let application =
    ws dotExpr
    .>>.
    sepEndBy dotExpr spaces
    |>> function
      | x, [] -> x.item
      | x, xs -> Application(x, xs)
    |> addinfo

  let (opp, addOp2Ext, addOp2Res) =
    prepareOpp
      sc
      op_chars
      op_reserved
      (fun prefix remOpChars expr1 expr2 ->
        let info =
          match expr1.info, expr2.info with
            | ValueSome i, ValueSome j ->
              { location = i.location + j.location } |> ValueSome
            | _, _ -> ValueNone
        {
          item = Application(Variable (prefix + remOpChars) |> With.noInfo, [expr1; expr2])
          info = info
        })

  do opp.TermParser <- ws application
    // the operator definitions:
  do
    addOp2Ext "|>" 15 Associativity.Left
    addOp2Ext "&&" 20 Associativity.Left
    addOp2Ext "||" 20 Associativity.Left
    addOp2Ext "<"  30 Associativity.Left
    addOp2Ext ">"  30 Associativity.Left
    addOp2Ext "="  30 Associativity.Left
    addOp2Ext "?"  35 Associativity.Left
    addOp2Ext "::" 35 Associativity.Right
    addOp2Ext "+"  40 Associativity.Left
    addOp2Ext "-"  40 Associativity.Left
    addOp2Ext "%"  50 Associativity.Left
    addOp2Ext "*"  50 Associativity.Left
    addOp2Ext "/"  50 Associativity.Left
    addOp2Ext "^"  60 Associativity.Right

  opp.ExpressionParser

let exprBlock = expr (DefaultSpaceCombinators())

let exprInline = expr (LineSplicingSpaceCombinators())

let template : parser<Template> = recursive <| fun statements ->
  let inline openClause (sc: #sc<_>) =
    sc.ws (syn "open") >>. expr sc
    |>> Open

  let inline defineClause (sc: #sc<_>) =
    tuple2
      (sc.ws (syn "def") >>. sc.ws name .>>. sepEndBy name sc.spaces1)
      (sc.ws (cyn '=')   >>. expr sc)
    |> addinfo
    |>> function
      | Item ((name, []), expr) -> Define (name, expr)
      | Item ((name, args), expr) as x ->
        Define (name, Lambda (args, expr) |> With.sameInfoOf x)

  let inline whenClause (sc: #sc<_>) =
    sc.ws (syn "when") >>. sc.ws (expr sc) .>> syn "do"

  let inline forClause (sc: #sc<_>) =
    tuple2
      (sc.ws (syn "for") >>. sc.ws name)
      (sc.ws (syn "in")  >>. sc.ws (expr sc) .>> syn "do")

  let inline otherwiseClause _ = syn "otherwise"

  let inline beginClause _ = syn "begin"
  let inline endClause _ = syn "end"

  /// %% CLAUSE
  let inline asLine clause =
    whitespaces
    >>? syn "%%"
    >>. whitespaces
    >>? clause (LineSplicingSpaceCombinators() :> sc<_>)
    .>> whitespaces
    .>> skipRestOfLine true

  /// {% CLAUSE %}
  let inline asBlock clause =
    (
      syn "{%"
      >>. spaces
      >>? clause (DefaultSpaceCombinators() :> sc<_>)
      .>> spaces
      .>> syn "%}"
    ) <?> "{% ... %}"

  let inline clause name clauseParser =
        (asLine clauseParser <?> sprintf "%%%% %s" name)
    <|> (asBlock clauseParser <?> sprintf "{%% %s %%}" name)

  let whenStatement =
    clause "when <condition> do" whenClause
    .>>. statements
    .>>. opt (clause "otherwise" otherwiseClause >>. statements)
    .>>  clause "end" endClause
    |>> fun ((cond, exec), otherwise) ->
      When(cond, exec, otherwise)

  let forStatement =
    clause "for <variable> in <expr> do" forClause
    .>>. statements
    .>> clause "end" endClause
    |>> fun ((cond, xs), exec) ->
      For (cond, xs, exec)

  let beginStatement =
    clause "begin" beginClause
    >>. statements
    .>> clause "end" endClause
    |>> Block

  /// {{ EXPR }}
  let yieldObject =
    embeddedBlock "{{" "}}" (spaces >>. expr (DefaultSpaceCombinators()) .>> spaces)
    |>> YieldObject

  /// ANY_OTHER_STRING
  let yieldText =
    many1Chars (
      notFollowedBy (syn "{{" <|> syn "{%") >>. noneOf "\n"
    ) |>> YieldText
    <?> "raw text"

  let stmts =
    notFollowedBy (clause "end" endClause <|> clause "otherwise" otherwiseClause)
    >>. choice [
      clause "open <record>" openClause
      clause "def <variable> = <expr>" defineClause
      whenStatement
      forStatement
      beginStatement
      yieldObject
      newlineReturn <| YieldText "\n"
      yieldText
    ] <?> "%% <statement> | {% <statement> %} | {{ <expr> }} | <raw text>"
  
  many stmts

open Flxble.Toml

let templateWithTomlMetadata =
  let tomlMetadataBlock =
    embeddedBlock "---" "---" (skipNewline >>. Parser.document .>> spaces)
    |>> TomlDocument.fromTokens

  opt (spaces >>. tomlMetadataBlock .>> skipNewline) .>>. template