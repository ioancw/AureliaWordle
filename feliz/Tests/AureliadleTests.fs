module AureliadleTests

open Xunit
open FsUnit.Xunit
open Domain
open Engine
open Aureliadle.Rules

let game = Aureliadle.Rules.game

/// A day whose puzzle is CHEAP.
let cheapDay = Words.wordles |> List.findIndex (fun (w, _, _) -> w = "CHEAP")

/// A save written by the live (Lit) version: one scored row and a row being typed.
let liveSave =
    """{"Wordle":"CHEAP","Guesses":[[5,[["C","Green"],["R","Grey"],["A","Yellow"],["N","Grey"],["E","Yellow"]]],[2,[["s","Black"],["l","Black"],["","Black"],["","Black"],["","Black"]]],[0,[["","Black"],["","Black"],["","Black"],["","Black"],["","Black"]]],[0,[["","Black"],["","Black"],["","Black"],["","Black"],["","Black"]]],[0,[["","Black"],["","Black"],["","Black"],["","Black"],["","Black"]]],[0,[["","Black"],["","Black"],["","Black"],["","Black"],["","Black"]]]],"State":"Started","Round":1,"GamesWon":4,"GamesLost":1,"WinDistribution":[0,1,2,1,0,0]}"""

let private typeWord (word: string) saved =
    let inputs = [ for c in word.ToLower() -> Letter(string c) ] @ [ Enter ]
    (saved, inputs) ||> List.fold (fun s i -> play game i s |> fst)

let private migrate json day =
    let _, convert = game.Legacy.Value
    convert json day |> Option.get

[<Fact>]
let ``Uses the same puzzle each day as the live version`` () =
    puzzleFor game (today game) |> should equal (Daily.todaysPuzzle ())

[<Fact>]
let ``A live-version save carries over: the game in progress and the stats`` () =
    let s = migrate liveSave cheapDay
    s.State.Round |> should equal 1
    s.State.Guesses.[1] |> GameRules.Guess.getLetter |> List.map fst |> should equal [ "S"; "L"; ""; ""; "" ]
    s.Stats.Played |> should equal 5
    s.Stats.Won |> should equal 4
    s.Stats.Distribution |> should equal [ 0; 1; 2; 1; 0; 0 ]

[<Fact>]
let ``A live-version save from an earlier day keeps just the stats`` () =
    let s = migrate liveSave (cheapDay + 1)
    s.State.State |> should equal NotStarted
    s.State.Wordle |> should not' (equal "CHEAP")
    s.Stats.Won |> should equal 4

[<Fact>]
let ``Messages for short and unknown words`` () =
    let s = resume game cheapDay None
    play game (Letter "c") s |> fst |> play game Enter |> snd |> should equal (Some "Not enough letters")
    let s5 = (s, [ "x"; "q"; "z"; "z"; "z" ]) ||> List.fold (fun s l -> play game (Letter l) s |> fst)
    play game Enter s5 |> snd |> should equal (Some "Not in word list")

[<Fact>]
let ``Winning through the engine counts the win once`` () =
    let s = resume game cheapDay None |> typeWord "crane" |> typeWord "cheap"
    game.Outcome s.State |> should equal (Some(Solved 2))
    s.Stats.Won |> should equal 1
    s.Stats.Distribution |> should equal [ 0; 1; 0; 0; 0; 0 ]
    s |> typeWord "crane" |> should equal s

[<Fact>]
let ``A game in progress survives saving and loading`` () =
    let s = resume game cheapDay None |> typeWord "crane" |> play game (Letter "s") |> fst
    s |> toJson game |> fromJson game |> should equal (Some s)

[<Fact>]
let ``Share grid matches the live version's`` () =
    let s = resume game cheapDay None |> typeWord "crane" |> typeWord "cheap"
    shareText game false s |> should endWith "2/6\n\n🟩⬛🟨⬛🟨\n🟩🟩🟩🟩🟩"
    shareText game true s |> should endWith "🟧⬛🟦⬛🟦\n🟧🟧🟧🟧🟧"
