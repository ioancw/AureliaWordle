module SilentLettersTests

open Xunit
open FsUnit.Xunit
open Engine
open SilentLetters.Rules

let private silent (w: Word) = string w.Text.[w.Gap]

let private spellNow state =
    apply (Pick(silent (currentWord state).Value)) state

let private missNow state =
    let w = (currentWord state).Value
    let wrong = w.Choices |> List.find (fun c -> c <> silent w && not (List.contains c state.Wrong))
    apply (Pick wrong) state

/// Plays a day, with one wrong letter before the words at these positions.
let private playDay (missAt: int list) state =
    (state, [ 0 .. perDay - 1 ])
    ||> List.fold (fun s i ->
        let s = if List.contains i missAt && (outcome s).IsNone then missNow s |> fst else s
        if (outcome s).IsNone then spellNow s |> fst else s)

[<Fact>]
let ``Every word has three different letters to pick, one of them its silent letter`` () =
    for w in bank do
        w.Choices |> List.length |> should equal 3
        w.Choices |> List.distinct |> List.length |> should equal 3
        w.Choices |> should contain (silent w)
        w.Choices |> List.forall (fun c -> c.Length = 1 && c = c.ToLowerInvariant()) |> should equal true

[<Fact>]
let ``Every word has a picture, a clue that doesn't give it away, and a rule`` () =
    for w in bank do
        w.Emoji |> should not' (equal "")
        w.Tip |> should not' (equal "")
        w.Clue.ToLowerInvariant() |> should not' (haveSubstring (w.Text.ToLowerInvariant()))

[<Fact>]
let ``Words are in the bank once`` () =
    bank |> Array.map (fun w -> w.Text) |> Array.distinct |> Array.length |> should equal bank.Length

[<Fact>]
let ``Each day has six different words getting harder, from different families`` () =
    for day in puzzles do
        day |> List.length |> should equal perDay
        day |> List.distinct |> List.length |> should equal perDay
        day |> List.map (fun i -> bank.[i].Level) |> should equal [ 1; 1; 2; 2; 3; 3 ]
        day |> List.map (fun i -> bank.[i].Family) |> List.distinct |> List.length |> should equal perDay

[<Fact>]
let ``Every word comes up during the year`` () =
    let used = puzzles |> Seq.concat |> Set.ofSeq
    used.Count |> should equal bank.Length

[<Fact>]
let ``The right letter moves on; a wrong one costs a heart and you try again`` () =
    let s0 = start puzzles.[0]
    let s1, msg = missNow s0
    s1.Current |> should equal 0
    s1.Mistakes |> should equal 1
    s1.Missed |> should equal [ 0 ]
    msg |> should equal (Some "Not quite, try again")
    // the same wrong letter again costs nothing
    apply (Pick s1.Wrong.Head) s1 |> fst |> should equal s1
    // a letter that isn't one of the choices does nothing
    apply (Pick "z") s0 |> fst |> should equal s0

    let s2, _ = spellNow s1
    s2.Current |> should equal 1
    s2.Wrong |> List.isEmpty |> should equal true

[<Fact>]
let ``Letters can be picked by position, and capitals count`` () =
    let s0 = start puzzles.[0]
    let w = (currentWord s0).Value
    let i = w.Choices |> List.findIndex ((=) (silent w))
    (apply (Choose i) s0 |> fst).Current |> should equal 1
    (apply (Pick((silent w).ToUpperInvariant())) s0 |> fst).Current |> should equal 1

[<Fact>]
let ``Spelling every word wins, scored by mistakes; three wrong is out of hearts`` () =
    let perfect = start puzzles.[0] |> playDay []
    outcome perfect |> should equal (Some(Solved 1))
    scoreText (outcome perfect) |> should equal "no mistakes"
    shareGrid false perfect |> should equal "🟩🟩🟩🟩🟩🟩 ❤️❤️❤️"

    let two = start puzzles.[0] |> playDay [ 1; 4 ]
    outcome two |> should equal (Some(Solved 3))
    shareGrid false two |> should equal "🟩🟨🟩🟩🟨🟩 ❤️🤍🤍"

    let lost = start puzzles.[0] |> playDay [ 0; 1; 2 ]
    outcome lost |> should equal (Some Failed)
    shareGrid true lost |> should equal "🟦🟦⬜⬜⬜⬜ 🤍🤍🤍"

[<Fact>]
let ``Missed words come back the next day at the same level, until spelt right first time`` () =
    let day0 = start puzzles.[0] |> playDay [ 4 ]
    let missed = puzzles.[0].[4]
    let day1 = carryOver day0 (start puzzles.[1])
    day1.Practice |> should equal [ missed ]
    day1.Words |> should contain missed
    day1.Words |> List.map (fun i -> bank.[i].Level) |> should equal [ 1; 1; 2; 2; 3; 3 ]

    let at = day1.Words |> List.findIndex ((=) missed)
    isPractice { day1 with Current = at } |> should equal true

    let day2 = carryOver (day1 |> playDay []) (start puzzles.[2])
    day2.Practice |> List.isEmpty |> should equal true

[<Fact>]
let ``Saves round trip, and a broken game starts afresh`` () =
    let saved = resume game 0 None |> play game (Choose 0) |> fst
    saved |> toJson game |> fromJson game |> should equal (Some saved)

    let json = (toJson game saved).Replace("\"words\":[", "\"words\":[999,")
    (fromJson game json).Value.State |> should equal (start puzzles.[0])
