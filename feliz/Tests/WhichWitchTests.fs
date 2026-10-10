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
let ``Each day has six different sentences from different groups, including their/there/they're`` () =
    for day in puzzles do
        day |> List.length |> should equal perDay
        day |> List.distinct |> List.length |> should equal perDay
        day |> List.map (fun i -> bank.[i].Group) |> List.distinct |> List.length |> should equal perDay
        day |> List.exists (fun i -> bank.[i].Group = 0) |> should equal true

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
