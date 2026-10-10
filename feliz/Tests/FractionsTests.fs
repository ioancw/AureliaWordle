module FractionsTests

open Xunit
open FsUnit.Xunit
open Engine
open Fractions.Rules

let private f = fraction

/// The right guess for the next card (Higher for a pair: it doesn't matter).
let private rightGuess state =
    let i = state.Revealed - 1
    if compareFractions state.Cards.[i + 1] state.Cards.[i] >= 0 then Higher else Lower

let private wrongGuess state = if rightGuess state = Higher then Lower else Higher

let private guess g state = apply (Guess g) state |> fst

let private playRight state = guess (rightGuess state) state

/// Plays a row from the start with a wrong guess at the given flips (unless they're pairs).
let private playDay wrongAt state =
    (state, [ 0 .. cardsPerDay - 2 ])
    ||> List.fold (fun s j ->
        if (outcome s).IsNone then
            if List.contains j wrongAt then guess (wrongGuess s) s else playRight s
        else
            s)

[<Fact>]
let ``Fractions compare by value`` () =
    compareFractions (f 3 4) (f 2 3) |> should be (greaterThan 0)
    compareFractions (f 3 5) (f 2 3) |> should be (lessThan 0)
    sameValue (f 1 2) (f 4 8) |> should equal true
    sameValue (f 2 3) (f 8 12) |> should equal true

[<Fact>]
let ``Explanations show why, the way a child would work it out`` () =
    explain (f 2 3) (f 3 4) |> should equal "3/4 is higher than 2/3. Make the bottoms the same: 2/3 = 8/12 and 3/4 = 9/12."
    explain (f 3 5) (f 2 5) |> should equal "2/5 is lower than 3/5. Same bottom number, so compare the tops: 2 is less than 3."
    explain (f 1 3) (f 1 4) |> should equal "1/4 is lower than 1/3. Same top number: the bigger the bottom number, the smaller the pieces, so 1/4 is less than 1/3."

    explain (f 1 2) (f 4 8) |> should equal "4/8 is the same as 1/2. Times the top and bottom of 1/2 by 4 and you get 4/8."
    explain (f 1 2) (f 3 4) |> should equal "3/4 is higher than 1/2. Make the bottoms the same: 1/2 = 2/4, so compare 2/4 and 3/4."
    explain (f 4 5) (f 7 10) |> should equal "7/10 is lower than 4/5. Make the bottoms the same: 4/5 = 8/10, so compare 8/10 and 7/10."
    explain (f 4 6) (f 6 9) |> should equal "6/9 is the same as 4/6. Make the bottoms the same: both are 12/18."

[<Fact>]
let ``Every day is a row of eight proper fractions, getting harder, with pairs only in the middle`` () =
    for row in puzzles do
        row |> List.length |> should equal cardsPerDay

        for c in row do
            (c.N > 0 && c.N < c.D) |> should equal true

        for j, level in List.indexed comparisonLevels do
            let a, b = row.[j], row.[j + 1]

            if sameValue a b then
                (j >= 2 && j <= 4) |> should equal true
            else
                let gap = abs (float a.N / float a.D - float b.N / float b.D)

                match level with
                | 1 -> (gap >= 0.25 && a.D <= 4 && b.D <= 4) |> should equal true
                | 3 -> gap |> should be (lessThanOrEqualTo (1.0 / 6.0 + 1e-9))
                | _ -> ()

        row |> List.pairwise |> List.filter (fun (a, b) -> sameValue a b) |> List.length |> should be (lessThanOrEqualTo 1)

[<Fact>]
let ``Most days have a pair, and there's a mix of higher and lower`` () =
    let withPair = puzzles |> Array.filter (List.pairwise >> List.exists (fun (a, b) -> sameValue a b)) |> Array.length
    withPair |> should be (greaterThan 150)
    withPair |> should be (lessThan 300)

    let ups =
        puzzles |> Seq.collect List.pairwise |> Seq.filter (fun (a, b) -> compareFractions b a > 0) |> Seq.length

    let downs =
        puzzles |> Seq.collect List.pairwise |> Seq.filter (fun (a, b) -> compareFractions b a < 0) |> Seq.length

    (float ups / float (ups + downs)) |> should be (inRange 0.35 0.65)

[<Fact>]
let ``A right guess turns the card over and keeps the hearts`` () =
    let s, msg = apply (Guess(rightGuess (start puzzles.[0]))) (start puzzles.[0])
    s.Revealed |> should equal 2
    s.Mistakes |> should equal 0
    msg.IsSome |> should equal true

[<Fact>]
let ``A wrong guess costs a heart but still turns the card over`` () =
    let s0 = start puzzles.[0]
    let s, msg = apply (Guess(wrongGuess s0)) s0
    s.Revealed |> should equal 2
    s.Results |> should equal [ Wrong ]
    s.Mistakes |> should equal 1
    msg.Value |> should startWith "Oh no"

[<Fact>]
let ``You get nothing for a pair, and it costs nothing`` () =
    let s0 = start [ f 1 2; f 2 4; f 3 4 ]

    for g in [ Higher; Lower ] do
        let s, msg = apply (Guess g) s0
        s.Results |> should equal [ Pair ]
        s.Mistakes |> should equal 0
        msg |> should equal (Some "You get nothing for a pair!")

[<Fact>]
let ``Turning every card is a win, scored by mistakes; three wrong is out of hearts`` () =
    let perfect = start puzzles.[0] |> playDay []
    outcome perfect |> should equal (Some(Solved 1))
    scoreText (outcome perfect) |> should equal "no mistakes"

    let twoWrong = start puzzles.[0] |> playDay [ 0; 6 ]
    outcome twoWrong |> should equal (Some(Solved 3))

    let lost = start puzzles.[0] |> playDay [ 0; 1; 5 ]
    outcome lost |> should equal (Some Failed)
    lost.Revealed |> should equal 7
    // no more guesses once it's over
    apply (Guess Higher) lost |> fst |> should equal lost

[<Fact>]
let ``The share grid shows each flip and the hearts left`` () =
    let s = { start [ f 1 2; f 3 4; f 6 8; f 1 4 ] with Revealed = 4; Results = [ Right; Pair; Wrong ]; Mistakes = 1 }
    shareGrid false s |> should equal "🟩🟰🟥 ❤️❤️🤍"
    shareGrid true s |> should equal "🟧🟰🟥 ❤️❤️🤍"

[<Fact>]
let ``Missed comparisons come back the next day, marked as practice, until got right`` () =
    let day0 = start puzzles.[0] |> playDay [ 1 ]
    let missed = puzzles.[0].[1], puzzles.[0].[2]
    let day1 = carryOver day0 (start puzzles.[1])
    day1.Practice |> should equal [ missed ]

    let j = day1.Cards |> List.pairwise |> List.findIndex ((=) missed)
    isPractice day1 j |> should equal true
    day1.Cards |> List.length |> should equal cardsPerDay

    // still no accidental pairs either side of it
    if j > 0 then sameValue day1.Cards.[j - 1] (fst missed) |> should equal false
    if j + 2 < cardsPerDay then sameValue day1.Cards.[j + 2] (snd missed) |> should equal false

    // got right: it's done
    let day1Played = day1 |> playDay []
    let day2 = carryOver day1Played (start puzzles.[2])
    day2.Practice |> List.isEmpty |> should equal true

[<Fact>]
let ``Practice is put in on every day without breaking the row`` () =
    let practice = [ f 2 3, f 3 4; f 1 2, f 1 4 ]

    for row in puzzles do
        let cards = withPractice practice row
        cards |> List.length |> should equal cardsPerDay
        cards |> List.pairwise |> List.contains (practice.[0]) |> should equal true
        // no pairs made by accident where a practice comparison was put in
        for j, (a, b) in List.indexed (List.pairwise cards) do
            if List.contains (a, b) practice |> not && sameValue a b then
                List.contains (a, b) (List.pairwise row) |> should equal true

[<Fact>]
let ``Saves round trip, and older saves without practice still load`` () =
    let saved = resume game 0 None |> play game (Guess Higher) |> fst
    saved |> toJson game |> fromJson game |> should equal (Some saved)

    let s = { resume game 0 None with State = { start puzzles.[0] with Practice = [ f 2 3, f 3 4 ] } }
    (s |> toJson game |> fromJson game).Value.State.Practice |> should equal [ f 2 3, f 3 4 ]
    let old = (toJson game s).Replace(",\"practice\":[[[2,3],[3,4]]]", "")
    old |> should not' (haveSubstring "practice")
    (fromJson game old).Value.State.Practice |> List.isEmpty |> should equal true

[<Fact>]
let ``A broken game is started afresh, keeping the stats`` () =
    let saved = resume game 0 None |> play game (Guess Higher) |> fst
    let json = (toJson game saved).Replace("\"revealed\":2", "\"revealed\":9")
    let loaded = (fromJson game json).Value
    loaded.State |> should equal (start puzzles.[0])
    loaded.Stats |> should equal saved.Stats
