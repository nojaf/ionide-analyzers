module Ionide.Analyzers.Performance.NoArgFormatStringAnalyzer

open System.Collections.Generic
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.CodeAnalysis
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open Ionide.Analyzers.TypedOperations

[<Literal>]
let message =
    "Format string has no format specifiers, the printf-style function adds parsing overhead for nothing."

type private Replacement =
    /// The string literal itself is the fix.
    | Literal
    /// A function that takes the string literal.
    | Function of name: string
    /// A method on the first argument (writer or builder), followed by an optional suffix.
    | Method of name: string * suffix: string

/// Maps the printf-style function to the non-format alternative.
let private replacements =
    Map.ofList
        [
            "sprintf", Literal
            "failwithf", Function "failwith"
            "printf", Function "stdout.Write"
            "printfn", Function "stdout.WriteLine"
            "eprintf", Function "stderr.Write"
            "eprintfn", Function "stderr.WriteLine"
            "fprintf", Method("Write", "")
            "fprintfn", Method("WriteLine", "")
            "bprintf", Method("Append", " |> ignore")
        ]

[<Struct>]
type private FormatCall =
    | FormatCall of functionIdent: Ident * receiver: SynExpr option * formatRange: range * range: range

[<return: Struct>]
let rec private (|ConstString|_|) =
    function
    | SynExpr.Paren(expr = ConstString(text, m)) -> ValueSome(text, m)
    | SynExpr.Const(SynConst.String(text,
                                    (SynStringKind.Regular | SynStringKind.Verbatim | SynStringKind.TripleQuote),
                                    _),
                    m) -> ValueSome(text, m)
    | _ -> ValueNone

[<return: Struct>]
let private (|PrintfFunction|_|) =
    function
    | SynExpr.Ident ident -> ValueSome ident
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) ->
        match List.tryLast ids with
        | None -> ValueNone
        | Some ident -> ValueSome ident
    | _ -> ValueNone

/// True when the format string only contains literal text. `%%` is an escaped percent sign and does not count.
let private hasNoFormatSpecifiers (text: string) =
    not (text.Replace("%%", "").Contains '%')

let private analyze
    (sourceText: ISourceText)
    (parsedInput: ParsedInput)
    (checkResults: FSharpCheckFileResults)
    : Message list
    =
    let calls = HashSet<FormatCall>()

    let collector =
        { new SyntaxCollectorBase() with
            override x.WalkExpr(path, synExpr) =
                match synExpr with
                | SynExpr.App(ExprAtomicFlag.NonAtomic, false, PrintfFunction ident, ConstString(text, mFormat), m) when
                    hasNoFormatSpecifiers text
                    ->
                    match Map.tryFind ident.idText replacements with
                    | Some(Literal | Function _) -> calls.Add(FormatCall(ident, None, mFormat, m)) |> ignore
                    | _ -> ()
                | SynExpr.App(ExprAtomicFlag.NonAtomic,
                              false,
                              SynExpr.App(ExprAtomicFlag.NonAtomic, false, PrintfFunction ident, receiver, _),
                              ConstString(text, mFormat),
                              m) when hasNoFormatSpecifiers text ->
                    match Map.tryFind ident.idText replacements with
                    | Some(Method _) -> calls.Add(FormatCall(ident, Some receiver, mFormat, m)) |> ignore
                    | _ -> ()
                | _ -> ()
        }

    walkAst collector parsedInput

    calls
    |> Seq.choose (fun (FormatCall(ident, receiver, mFormat, m)) ->
        tryFSharpMemberOrFunctionOrValueFromIdent sourceText checkResults ident
        |> Option.bind (fun mfv ->
            if mfv.Assembly.SimpleName <> "FSharp.Core" || not mfv.IsFunction then
                None
            else

            let literal = (sourceText.GetSubTextFromRange mFormat).Replace("%%", "%")

            let toText =
                match replacements[ident.idText], receiver with
                | Literal, _ -> literal
                | Function name, _ -> $"%s{name} %s{literal}"
                | Method(name, suffix), Some receiver ->
                    let receiverText = sourceText.GetSubTextFromRange receiver.Range

                    let receiverText =
                        match receiver with
                        | SynExpr.Ident _
                        | SynExpr.LongIdent _
                        | SynExpr.Paren _ -> receiverText
                        | _ -> $"(%s{receiverText})"

                    $"%s{receiverText}.%s{name} %s{literal}%s{suffix}"
                | Method _, None -> literal

            Some
                {
                    Type = "noArgFormatString"
                    Message = message
                    Code = "IONIDE-014"
                    Severity = Severity.Hint
                    Range = m
                    Fixes =
                        [
                            {
                                FromText = ""
                                FromRange = m
                                ToText = toText
                            }
                        ]
                }
        )
    )
    |> Seq.toList

[<Literal>]
let name = "NoArgFormatStringAnalyzer"

[<Literal>]
let shortDescription =
    "Detects printf-style functions applied to a format string without format specifiers."

[<Literal>]
let helpUri = "https://ionide.io/ionide-analyzers/performance/014.html"

[<CliAnalyzer(name, shortDescription, helpUri)>]
let noArgFormatStringCliAnalyzer: Analyzer<CliContext> =
    fun (context: CliContext) ->
        async { return analyze context.SourceText context.ParseFileResults.ParseTree context.CheckFileResults }

[<EditorAnalyzer(name, shortDescription, helpUri)>]
let noArgFormatStringEditorAnalyzer: Analyzer<EditorContext> =
    fun (context: EditorContext) ->
        async {
            match context.CheckFileResults with
            | None -> return []
            | Some checkFileResults ->
                return analyze context.SourceText context.ParseFileResults.ParseTree checkFileResults
        }
