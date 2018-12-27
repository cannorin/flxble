namespace Flxble.Templating
open SyntaxTree
open System.Globalization

[<AutoOpen>]
module Context =
  type TemplateContext = {
    bindings: Map<string, ScriptObject>
    /// will be used to format datetimes
    culture: CultureInfo
    /// function to print an error message as a comment
    /// in the target language.
    /// for HTML, this should be something like
    /// `sprintf "<!-- %s -->"`.
    commentize: string -> string
  }

