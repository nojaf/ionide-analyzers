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

/// Maps the printf-style function to the non-format alternative. `None` means the argument itself is the fix.
let private replacements =
    Map.ofList
        [
            "sprintf", None
            "failwithf", Some "failwith"
            "printf", Some "stdout.Write"
            "printfn", Some "stdout.WriteLine"
            "eprintf", Some "stderr.Write"
            "eprintfn", Some "stderr.WriteLine"
        ]

[<Struct>]
type private FormatCall = | FormatCall of functionIdent: Ident * formatText: string * formatRange: range * range: range

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
                    replacements.ContainsKey ident.idText && hasNoFormatSpecifiers text
                    ->
                    calls.Add(FormatCall(ident, text, mFormat, m)) |> ignore
                | _ -> ()
        }

    walkAst collector parsedInput

    calls
    |> Seq.choose (fun (FormatCall(ident, _, mFormat, m)) ->
        tryFSharpMemberOrFunctionOrValueFromIdent sourceText checkResults ident
        |> Option.bind (fun mfv ->
            if mfv.Assembly.SimpleName <> "FSharp.Core" || not mfv.IsFunction then
                None
            else

            let literal = (sourceText.GetSubTextFromRange mFormat).Replace("%%", "%")

            let toText =
                match replacements[ident.idText] with
                | None -> literal
                | Some replacement -> $"%s{replacement} %s{literal}"

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
