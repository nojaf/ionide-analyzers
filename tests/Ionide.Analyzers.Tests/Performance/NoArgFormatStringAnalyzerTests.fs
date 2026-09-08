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
