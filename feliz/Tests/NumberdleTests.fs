module NumberdleTests

open Xunit
open FsUnit.Xunit
open Numberdle.Rules

let private enter keys state =
    let typed = (state, keys) ||> List.fold (fun s k -> apply k s |> fst)
    apply Enter typed

let private digits (text: string) = [ for c in text -> Digit(int c - int '0') ]

[<Fact>]
let ``Every number from 1 to 100 is a puzzle exactly once`` () =
    puzzles |> Array.sort |> should equal [| 1..100 |]

[<Fact>]
let ``Guesses say higher or lower and how close`` () =
    feedback 50 47 |> should equal { Value = 47; Direction = Higher; Warmth = Hot }
    feedback 50 62 |> should equal { Value = 62; Direction = Lower; Warmth = Warm }
    feedback 50 10 |> should equal { Value = 10; Direction = Higher; Warmth = Cold }
    (feedback 50 50).Direction |> should equal Correct

[<Fact>]
let ``Entries are at most three digits with no leading zero`` () =
    let s = (start 50, [ Digit 0; Digit 1; Digit 2; Digit 3; Digit 4 ]) ||> List.fold (fun s k -> apply k s |> fst)
    s.Entry |> should equal "123"

[<Fact>]
let ``Delete removes the last digit`` () =
    let s = (start 50, digits "42" @ [ Delete ]) ||> List.fold (fun s k -> apply k s |> fst)
    s.Entry |> should equal "4"

[<Fact>]
let ``Out of range, repeated and empty guesses are rejected with a message`` () =
    start 50 |> apply Enter |> snd |> should equal (Some "Type a number")
    start 50 |> enter (digits "150") |> snd |> should equal (Some "Pick a number from 1 to 100")
    let once = start 50 |> enter (digits "30") |> fst
    once |> enter (digits "30") |> snd |> should equal (Some "You've tried 30 already")
    (once |> enter (digits "30") |> fst).Guesses.Length |> should equal 1

[<Fact>]
let ``Seven wrong guesses is a loss`` () =
    let s =
        ([ 1..7 ], start 50)
        ||> List.foldBack (fun n s -> s |> enter (digits (string n)) |> fst)
    outcome s |> should equal (Some Engine.Failed)
