module WhichWitchTests

open Xunit
open FsUnit.Xunit
open Engine
open WhichWitch.Rules

let private indexOf word (s: Sentence) = List.findIndex ((=) word) s.Choices

let private answerNow state =
    let s = (currentSentence state).Value
    apply (Choose(indexOf s.Answer s)) state

let private missNow state =
    let s = (currentSentence state).Value
    let wrong = s.Choices |> List.find (fun c -> c <> s.Answer && not (List.contains c state.Wrong))
    apply (Choose(indexOf wrong s)) state

let private today = start puzzles.[0]

[<Fact>]
let ``Every sentence has one blank, and its answer is one of its choices`` () =
    for s in bank do
        s.Text.Split("___").Length |> should equal 2
        s.Choices |> should contain s.Answer

[<Fact>]
let ``Every word that can be picked has an explanation`` () =
    for s in bank do
        for c in s.Choices do
            meanings |> Map.containsKey c |> should equal true

[<Fact>]
let ``Each day has six different sentences from different word families, including their/there/they're`` () =
    for day in puzzles do
        day |> List.length |> should equal perDay
        day |> List.distinct |> List.length |> should equal perDay
        day |> List.map (fun i -> bank.[i].Family) |> List.distinct |> List.length |> should equal perDay
        day |> List.exists (fun i -> bank.[i].Family = theirFamily) |> should equal true

[<Fact>]
let ``Each day gets harder: two easy, two medium, two apostrophe sentences`` () =
    for day in puzzles do
        day |> List.map (fun i -> bank.[i].Level) |> should equal [ 1; 1; 2; 2; 3; 3 ]

[<Fact>]
let ``Every level has enough word families to fill its places`` () =
    let familiesAt level = bank |> Array.filter (fun s -> s.Level = level) |> Array.distinctBy (fun s -> s.Family) |> Array.length
    familiesAt 1 |> should be (greaterThanOrEqualTo 2)
    familiesAt 2 |> should be (greaterThanOrEqualTo 2)
    familiesAt 3 |> should be (greaterThanOrEqualTo 2)

[<Fact>]
let ``The apostrophe challenges include could've and apostrophe placement`` () =
    let answers = bank |> Array.filter (fun s -> s.Level = 3) |> Array.map (fun s -> s.Answer)
    answers |> should contain "could've"
    answers |> should contain "didn't"
    answers |> should contain "dog's"
    // and a wrong-placement option is never the answer
    bank |> Array.exists (fun s -> s.Answer.Contains "'nt") |> should equal false

[<Fact>]
let ``A right answer moves on with some praise`` () =
    let next, message = answerNow today
    next.Current |> should equal 1
    next.Mistakes |> should equal 0
    message.IsSome |> should equal true

[<Fact>]
let ``A wrong answer costs a heart and you try the same sentence again`` () =
    let next, message = missNow today
    next.Current |> should equal 0
    next.Mistakes |> should equal 1
    next.Missed |> should equal [ 0 ]
    next.Wrong.Length |> should equal 1
    message |> should equal (Some "Not quite, try again")
    // picking the same wrong word again costs nothing
    let s = (currentSentence next).Value
    apply (Choose(indexOf next.Wrong.Head s)) next |> fst |> should equal next

[<Fact>]
let ``All six done is solved, counted by mistakes`` () =
    let s1 = today |> missNow |> fst
    let finished = (s1, [ 1..perDay ]) ||> List.fold (fun s _ -> answerNow s |> fst)
    outcome finished |> should equal (Some(Solved 2))
    scoreText (outcome finished) |> should equal "1 mistake"
    shareGrid false finished |> should equal "🟨🟩🟩🟩🟩🟩 ❤️❤️🤍"

[<Fact>]
let ``Three mistakes is out of hearts`` () =
    // two wrong on the first sentence (it has three choices), then one more on the next
    let s = puzzles.[0] |> start
    let s = { s with Questions = [ 0; 1; 2; 3; 4; 5 ] } // their/there/they're sentences
    let s = s |> missNow |> fst |> missNow |> fst |> answerNow |> fst |> missNow |> fst
    outcome s |> should equal (Some Failed)
    scoreText (outcome s) |> should equal "out of hearts"
    // nothing changes after that
    apply (Choose 0) s |> fst |> should equal s

[<Fact>]
let ``A game in progress survives saving and loading`` () =
    let saved = resume game 0 None |> play game (Choose 0) |> fst
    saved |> toJson game |> fromJson game |> should equal (Some saved)

[<Fact>]
let ``A whole day played through the engine updates the stats once`` () =
    let finished =
        (resume game 0 None, [ 1..perDay ])
        ||> List.fold (fun saved _ ->
            let s = (currentSentence saved.State).Value
            play game (Choose(indexOf s.Answer s)) saved |> fst)

    game.Outcome finished.State |> should equal (Some(Solved 1))
    finished.Stats.Won |> should equal 1
    finished.Stats.Distribution |> should equal [ 1; 0; 0 ]
    shareText game false finished |> should equal "Which Witch? 0 no mistakes\n\n🟩🟩🟩🟩🟩🟩 ❤️❤️❤️"

// Practising sentences got wrong

/// Plays a whole day: a wrong pick first on each listed position, then the right answer everywhere.
let private playDay missAt (state: State) =
    (state, [ 0 .. state.Questions.Length - 1 ])
    ||> List.fold (fun s i ->
        let s = if List.contains i missAt then missNow s |> fst else s
        answerNow s |> fst)

let private levelsOf (s: State) = s.Questions |> List.map (fun i -> bank.[i].Level)

[<Fact>]
let ``A sentence got wrong comes back the next day, at the same level`` () =
    let day1 = start puzzles.[10] |> playDay [ 0 ]
    let missed = day1.Questions.[0]
    let day2 = carryOver day1 (start puzzles.[11])

    day2.Practice |> should equal [ missed ]
    day2.Questions |> should contain missed
    bank.[missed].Level |> should equal 1
    levelsOf day2 |> should equal [ 1; 1; 2; 2; 3; 3 ]
    day2.Questions |> List.exists (fun i -> bank.[i].Family = theirFamily) |> should equal true

[<Fact>]
let ``The practice sentence is marked when it comes up`` () =
    let day1 = start puzzles.[10] |> playDay [ 0 ]
    let day2 = carryOver day1 (start puzzles.[11])
    let position = day2.Questions |> List.findIndex ((=) day1.Questions.[0])
    let atPractice = (day2, [ 1..position ]) ||> List.fold (fun s _ -> answerNow s |> fst)
    isPractice atPractice |> should equal true

[<Fact>]
let ``Answered right first time, it leaves the practice list`` () =
    let day1 = start puzzles.[10] |> playDay [ 0 ]
    let day2 = carryOver day1 (start puzzles.[11]) |> playDay []
    let day3 = carryOver day2 (start puzzles.[12])
    day3.Practice |> List.isEmpty |> should equal true

[<Fact>]
let ``Got wrong again, it stays on the list`` () =
    let day1 = start puzzles.[10] |> playDay [ 0 ]
    let missed = day1.Questions.[0]
    let day2 = carryOver day1 (start puzzles.[11])
    let position = day2.Questions |> List.findIndex ((=) missed)
    let day3 = carryOver (day2 |> playDay [ position ]) (start puzzles.[12])
    day3.Practice |> should equal [ missed ]
    day3.Questions |> should contain missed

[<Fact>]
let ``Skipped days don't lose the practice list`` () =
    let day1 = start puzzles.[10] |> playDay [ 2; 4 ]
    let later = carryOver day1 (start puzzles.[20])
    later.Practice |> should equal [ day1.Questions.[2]; day1.Questions.[4] ]
    later.Questions |> should contain day1.Questions.[2]
    later.Questions |> should contain day1.Questions.[4]

[<Fact>]
let ``At most two practice sentences a day; the rest wait for later days`` () =
    let today = start puzzles.[11]
    // four sentences waiting to be practised, none of them already in today's six
    let waiting =
        [ 1; 2; 3; 3 ]
        |> List.mapi (fun n level ->
            bank
            |> Array.indexed
            |> Array.filter (fun (i, s) -> s.Level = level && s.Family <> theirFamily && not (List.contains i today.Questions))
            |> Array.item n
            |> fst)

    let last = { start puzzles.[10] with Practice = waiting }
    let next = carryOver last today
    next.Practice |> should equal waiting
    next.Questions |> List.filter (fun q -> List.contains q waiting) |> should equal (List.take 2 waiting)
    levelsOf next |> should equal [ 1; 1; 2; 2; 3; 3 ]

[<Fact>]
let ``The engine carries the practice list into a new day`` () =
    let day0 = { resume game 0 None with State = start puzzles.[0] |> playDay [ 1 ] }
    let next = resume game 1 (Some day0)
    next.State.Practice |> should equal [ day0.State.Questions.[1] ]
    next.State.Questions |> should contain day0.State.Questions.[1]

[<Fact>]
let ``The practice list is saved, and older saves without it still load`` () =
    let s = { resume game 0 None with State = { start puzzles.[0] with Practice = [ 3; 40 ] } }
    (s |> toJson game |> fromJson game).Value.State.Practice |> should equal [ 3; 40 ]
    let old = (toJson game s).Replace(",\"practice\":[3,40]", "")
    (fromJson game old).Value.State.Practice |> List.isEmpty |> should equal true
