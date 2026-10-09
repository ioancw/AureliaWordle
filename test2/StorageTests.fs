module StorageTests

open Xunit
open FsUnit.Xunit
open Domain
open GameRules
open Storage

let private puzzle = "CHEAP", "/ee/", "ea"

/// A save written by the app before saving moved to Thoth.Json: one scored row and a row being typed.
let private existingSave =
    """{"Wordle":"CHEAP","Guesses":[[5,[["C","Green"],["R","Grey"],["A","Yellow"],["N","Grey"],["E","Yellow"]]],[2,[["s","Black"],["l","Black"],["","Black"],["","Black"],["","Black"]]],[0,[["","Black"],["","Black"],["","Black"],["","Black"],["","Black"]]],[0,[["","Black"],["","Black"],["","Black"],["","Black"],["","Black"]]],[0,[["","Black"],["","Black"],["","Black"],["","Black"],["","Black"]]],[0,[["","Black"],["","Black"],["","Black"],["","Black"],["","Black"]]]],"State":"Started","Round":1,"GamesWon":4,"GamesLost":1,"WinDistribution":[0,1,2,1,0,0]}"""

let private play keys state =
    keys
    |> List.fold
        (fun s key ->
            match key with
            | "Ent" -> Play.submitEnter Game.numberOfRounds Game.numberOfLetters s
            | letter -> Play.submitLetter Game.numberOfRounds Game.numberOfLetters letter s)
        state

let private typeWord (word: string) = [ for c in word.ToLower() -> string c ] @ [ "Ent" ]

let private roundTrip state =
    state |> Game.toSaved |> toJson |> fromJson |> Game.fromSaved puzzle

[<Fact>]
let ``Saves made by the previous version still load`` () =
    let state = existingSave |> fromJson |> Game.fromSaved puzzle

    state.State |> should equal Started
    state.Round |> should equal 1
    state.GamesWon |> should equal 4
    state.GamesLost |> should equal 1
    state.WinDistribution |> should equal [ 0; 1; 2; 1; 0; 0 ]
    state.Guesses.[0] |> Guess.getLetter |> List.map snd |> should equal [ Green; Grey; Yellow; Grey; Yellow ]
    state.Guesses.[1] |> Guess.getLetter |> List.map fst |> should equal [ "S"; "L"; ""; ""; "" ]
    // the keyboard colours are rebuilt from the guesses
    state.UsedLetters |> Map.find "C" |> should equal Green
    state.UsedLetters |> Map.find "A" |> should equal Yellow

[<Fact>]
let ``A game in progress survives saving and loading unchanged`` () =
    let state = Game.fromSaved puzzle None |> play (typeWord "crane" @ [ "s"; "l" ])
    roundTrip state |> should equal state

[<Fact>]
let ``A finished game survives saving and loading unchanged`` () =
    let state = Game.fromSaved puzzle None |> play (typeWord "crane" @ typeWord "cheap")
    state.State |> should equal Won
    roundTrip state |> should equal state

[<Fact>]
let ``Saved data is the same JSON shape as before`` () =
    existingSave |> fromJson |> Option.map toJson |> should equal (Some existingSave)

[<Fact>]
let ``Winning counts the win and the round it was won in`` () =
    let state = Game.fromSaved puzzle None |> play (typeWord "crane" @ typeWord "cheap")
    state.GamesWon |> should equal 1
    state.GamesLost |> should equal 0
    state.WinDistribution |> should equal [ 0; 1; 0; 0; 0; 0 ]

[<Fact>]
let ``Losing counts a loss and nothing else`` () =
    let state =
        Game.fromSaved puzzle None
        |> play (List.collect typeWord [ "crane"; "slate"; "pious"; "dumpy"; "fight"; "globe" ])

    state.State |> should equal Lost
    state.GamesLost |> should equal 1
    state.GamesWon |> should equal 0
    state.WinDistribution |> should equal [ 0; 0; 0; 0; 0; 0 ]

[<Fact>]
let ``Typing after the game is over changes nothing`` () =
    let won = Game.fromSaved puzzle None |> play (typeWord "cheap")
    won |> play (typeWord "crane") |> should equal won

[<Fact>]
let ``A new day starts a fresh game and keeps the stats`` () =
    let yesterday = existingSave |> fromJson |> Option.map (fun s -> { s with State = Won })
    let today = Game.fromSaved ("BREAK", "/ae/", "ea") yesterday

    today.Wordle |> should equal "BREAK"
    today.State |> should equal NotStarted
    today.Round |> should equal 0
    today.UsedLetters |> Map.isEmpty |> should equal true
    today.Guesses |> List.forall (fun g -> Guess.guessToWord (snd g) = "") |> should equal true
    today.GamesWon |> should equal 4
    today.WinDistribution |> should equal [ 0; 1; 2; 1; 0; 0 ]

[<Fact>]
let ``A partly damaged save keeps whatever stats it has`` () =
    let state = """{"Wordle":"CHEAP","GamesWon":3}""" |> fromJson |> Game.fromSaved puzzle
    state.GamesWon |> should equal 3
    state.State |> should equal NotStarted
    state.Guesses |> List.length |> should equal Game.numberOfRounds
    state.WinDistribution |> should equal [ 0; 0; 0; 0; 0; 0 ]

[<Fact>]
let ``Out of range values are brought back into range`` () =
    let state =
        """{"Wordle":"CHEAP","Round":42,"GamesWon":-1,"WinDistribution":[1,2]}"""
        |> fromJson
        |> Game.fromSaved puzzle

    state.Round |> should equal (Game.numberOfRounds - 1)
    state.GamesWon |> should equal 0
    state.WinDistribution |> should equal [ 1; 2; 0; 0; 0; 0 ]

[<Theory>]
[<InlineData("not json at all")>]
[<InlineData("")>]
[<InlineData("null")>]
[<InlineData("""{"GamesWon":3}""")>]
[<InlineData("""{"Wordle":"CHEAP","Guesses":"oops"}""")>]
let ``Unreadable saves are ignored rather than crashing`` (json: string) =
    json |> fromJson |> should equal None
    // and the game falls back to a fresh one
    json |> fromJson |> Game.fromSaved puzzle |> (fun s -> s.State) |> should equal NotStarted

[<Fact>]
let ``Each day has one puzzle, and the day changes at midnight`` () =
    let evening = System.DateTime(2026, 10, 9, 23, 59, 0)
    let morning = System.DateTime(2026, 10, 10, 0, 1, 0)
    Daily.dayNumberOn morning - Daily.dayNumberOn evening |> should equal 1
    // clocks going back in autumn doesn't skip or repeat a day
    Daily.dayNumberOn (System.DateTime(2026, 10, 26)) - Daily.dayNumberOn (System.DateTime(2026, 10, 25)) |> should equal 1

[<Fact>]
let ``Refreshing picks up a newer save from another tab`` () =
    let here = Game.today None
    let otherTab = here |> play (typeWord "crane")
    Game.refresh (Some(Game.toSaved otherTab)) here |> should equal otherTab

[<Fact>]
let ``Refreshing without a readable save keeps the game in memory`` () =
    let here = Game.today None |> play [ "c"; "r" ]
    Game.refresh None here |> should equal here
