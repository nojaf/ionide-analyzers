module Ionide.Analyzers.Tests.Performance.NoArgFormatStringAnalyzerTests

open NUnit.Framework
open FSharp.Compiler.CodeAnalysis
open FSharp.Analyzers.SDK.Testing
open Ionide.Analyzers.Performance.NoArgFormatStringAnalyzer

let mutable projectOptions: FSharpProjectOptions = FSharpProjectOptions.zero

[<SetUp>]
let Setup () =
    task {
        let! opts = mkOptionsFromProject "net8.0" []
        projectOptions <- opts
    }

let private assertSingleFix (expectedFix: string) (source: string) =
    async {
        let ctx = getContext projectOptions source
        let! msgs = noArgFormatStringCliAnalyzer ctx
        Assert.That(msgs, Has.Length.EqualTo 1)
        let msg = msgs[0]
        Assert.That(Assert.messageContains message msg, Is.True)
        Assert.That(msg.Fixes[0].ToText, Is.EqualTo expectedFix)
    }

let private assertNoMessages (source: string) =
    async {
        let ctx = getContext projectOptions source
        let! msgs = noArgFormatStringCliAnalyzer ctx
        Assert.That(msgs, Is.Empty)
    }

/// Runs the analyzer on `source`, expects exactly one message with one fix, and returns the source with that fix applied.
let private codeFixFor (source: string) : Async<string> =
    async {
        let ctx = getContext projectOptions source
        let! msgs = noArgFormatStringCliAnalyzer ctx
        Assert.That(msgs, Has.Length.EqualTo 1)
        let fix = msgs[0].Fixes[0]
        let lines = source.Split '\n'

        let offset line column =
            (lines |> Array.take (line - 1) |> Array.sumBy (fun l -> l.Length + 1)) + column

        let start = offset fix.FromRange.StartLine fix.FromRange.StartColumn
        let finish = offset fix.FromRange.EndLine fix.FromRange.EndColumn
        return source.Substring(0, start) + fix.ToText + source.Substring finish
    }

/// Asserts the fixed source equals `expected`, and that `expected` compiles without triggering the analyzer again.
let private becomes (expected: string) (fixedSource: Async<string>) =
    async {
        let! fixedSource = fixedSource
        Assert.That(fixedSource, Is.EqualTo expected)
        do! assertNoMessages expected
    }

[<Test>]
let ``sprintf with plain string`` () =
    assertSingleFix
        "\"<pre><code>\""
        """module Lib
let a = sprintf "<pre><code>"
"""

[<Test>]
let ``sprintf with parenthesized string`` () =
    assertSingleFix
        "\"<pre><code>\""
        """module Lib
let a = sprintf ("<pre><code>")
"""

[<Test>]
let ``sprintf with triple quoted string keeps the quotes`` () =
    assertSingleFix
        "\"\"\"<pre class=\"x\">\"\"\""
        "module Lib
let a = sprintf \"\"\"<pre class=\"x\">\"\"\"
"

[<Test>]
let ``sprintf with verbatim string keeps the prefix`` () =
    assertSingleFix
        "@\"C:\\temp\""
        """module Lib
let a = sprintf @"C:\temp"
"""

[<Test>]
let ``escaped percent is unescaped in the fix`` () =
    assertSingleFix
        "\"100%\""
        """module Lib
let a = sprintf "100%%"
"""

[<Test>]
let ``qualified sprintf`` () =
    assertSingleFix
        "\"hello\""
        """module Lib
let a = Printf.sprintf "hello"
"""

[<Test>]
let ``failwithf becomes failwith`` () =
    assertSingleFix
        "failwith \"boom\""
        """module Lib
let a () : int = failwithf "boom"
"""

[<Test>]
let ``printfn becomes stdout.WriteLine`` () =
    assertSingleFix
        "stdout.WriteLine \"hello\""
        """module Lib
printfn "hello"
"""

[<Test>]
let ``printf becomes stdout.Write`` () =
    assertSingleFix
        "stdout.Write \"hello\""
        """module Lib
printf "hello"
"""

[<Test>]
let ``eprintfn becomes stderr.WriteLine`` () =
    assertSingleFix
        "stderr.WriteLine \"hello\""
        """module Lib
eprintfn "hello"
"""

[<Test>]
let ``eprintf becomes stderr.Write`` () =
    assertSingleFix
        "stderr.Write \"hello\""
        """module Lib
eprintf "hello"
"""

[<Test>]
let ``fprintf becomes Write on the writer`` () =
    assertSingleFix
        "tw.Write \"hello\""
        """module Lib
let tw = System.IO.TextWriter.Null
fprintf tw "hello"
"""

[<Test>]
let ``fprintfn becomes WriteLine on the writer`` () =
    assertSingleFix
        "tw.WriteLine \"hello\""
        """module Lib
let tw = System.IO.TextWriter.Null
fprintfn tw "hello"
"""

[<Test>]
let ``bprintf becomes Append on the builder`` () =
    assertSingleFix
        "sb.Append \"hello\" |> ignore"
        """module Lib
let sb = System.Text.StringBuilder()
Printf.bprintf sb "hello"
"""

[<Test>]
let ``qualified bprintf with long ident receiver`` () =
    assertSingleFix
        "state.Builder.Append \"hello\" |> ignore"
        """module Lib
type State = { Builder: System.Text.StringBuilder }
let state = { Builder = System.Text.StringBuilder() }
Printf.bprintf state.Builder "hello"
"""

[<Test>]
let ``bprintf with complex receiver is wrapped in parentheses`` () =
    assertSingleFix
        "(mk ()).Append \"hello\" |> ignore"
        """module Lib
open Printf
let mk () = System.Text.StringBuilder()
bprintf (mk ()) "hello"
"""

[<Test>]
let ``fprintf before and after`` () =
    codeFixFor
        """module Lib

let write (tw: System.IO.TextWriter) =
    fprintf tw "hello"
    fprintf tw "%s" "world"
"""
    |> becomes
        """module Lib

let write (tw: System.IO.TextWriter) =
    tw.Write "hello"
    fprintf tw "%s" "world"
"""

[<Test>]
let ``fprintfn before and after`` () =
    codeFixFor
        """module Lib

let write (tw: System.IO.TextWriter) =
    fprintfn tw "hello"
    fprintfn tw "%s" "world"
"""
    |> becomes
        """module Lib

let write (tw: System.IO.TextWriter) =
    tw.WriteLine "hello"
    fprintfn tw "%s" "world"
"""

[<Test>]
let ``bprintf before and after`` () =
    codeFixFor
        """module Lib

let build (sb: System.Text.StringBuilder) =
    Printf.bprintf sb "hello"
    Printf.bprintf sb "%s" "world"
"""
    |> becomes
        """module Lib

let build (sb: System.Text.StringBuilder) =
    sb.Append "hello" |> ignore
    Printf.bprintf sb "%s" "world"
"""

[<Test>]
let ``bprintf as last expression before and after`` () =
    codeFixFor
        """module Lib

let build (sb: System.Text.StringBuilder) : unit = Printf.bprintf sb "hello"
"""
    |> becomes
        """module Lib

let build (sb: System.Text.StringBuilder) : unit = sb.Append "hello" |> ignore
"""

[<Test>]
let ``fprintf with format specifier does not trigger`` () =
    assertNoMessages
        """module Lib
let tw = System.IO.TextWriter.Null
fprintf tw "hello %s" "world"
"""

[<Test>]
let ``user defined bprintf does not trigger`` () =
    assertNoMessages
        """module Lib
let bprintf (sb: System.Text.StringBuilder) (s: string) = ()
let sb = System.Text.StringBuilder()
bprintf sb "hello"
"""

[<Test>]
let ``format specifier does not trigger`` () =
    assertNoMessages
        """module Lib
let a = sprintf "language-%s" "fsharp"
"""

[<Test>]
let ``partially applied format specifier does not trigger`` () =
    assertNoMessages
        """module Lib
let a = sprintf "language-%s"
"""

[<Test>]
let ``interpolated string does not trigger`` () =
    assertNoMessages
        """module Lib
let x = 1
let a = sprintf $"value {x}"
"""

[<Test>]
let ``user defined sprintf does not trigger`` () =
    assertNoMessages
        """module Lib
let sprintf (s: string) = s
let a = sprintf "hello"
"""

[<Test>]
let ``non literal format does not trigger`` () =
    assertNoMessages
        """module Lib
let fmt = Printf.StringFormat<string> "hello"
let a = sprintf fmt
"""

[<Test>]
let ``sprintf with double parenthesized string`` () =
    assertSingleFix
        "\"<pre><code>\""
        """module Lib
let a = sprintf (("<pre><code>"))
"""
