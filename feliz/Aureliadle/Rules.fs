/// Aureliadle as a DailyGame. The rules themselves are the existing, unchanged modules in
/// ../../src (Domain, Words, GameRules); this file just plugs them into the engine.
module Aureliadle.Rules

open System
open Domain
open GameRules
open Engine
open Thoth.Json.Core

let rounds = 6
let letters = 5

type Input =
    | Letter of string
    | Enter
    | Delete

/// (word, phonic hint, grapheme)
type Puzzle = string * string * string

let start (puzzle: Puzzle) : State = Game.fromSaved puzzle None

let apply input (state: State) =
    match input with
    | Letter l -> Play.submitLetter rounds letters l state, None
    | Delete -> Play.submitDelete rounds letters state, None
    | Enter ->
        let message =
            if not (Validate.round rounds state) then None
            elif not (Validate.allLetters state) then Some "Not enough letters"
            elif not (Validate.word state) then Some "Not in word list"
            else None

        Play.submitEnter rounds letters state, message

let outcome (state: State) =
    match state.State with
    | Won -> Some(Solved(state.Round + 1))
    | Lost -> Some Failed
    | NotStarted
    | Started -> None

let private puzzleOf wordle =
    Words.wordles |> List.tryFind (fun (w, _, _) -> w = wordle)

// The game state is saved in its existing JSON shape (see src/Storage.fs). Stats live in the
// engine now, so the old stats fields in it are left at zero.
let encode (state: State) =
    Storage.encodeGame (Game.toSaved { state with GamesWon = 0; GamesLost = 0; WinDistribution = [] })

let decoder: Decoder<State> =
    Storage.gameDecoder
    |> Decode.andThen (fun saved ->
        match puzzleOf saved.Wordle with
        | Some puzzle -> Decode.succeed (Game.fromSaved puzzle (Some saved))
        | None -> Decode.fail $"Unknown wordle {saved.Wordle}")

let shareGrid highContrast (state: State) =
    let square status =
        match status with
        | Green -> if highContrast then "🟧" else "🟩"
        | Yellow -> if highContrast then "🟦" else "🟨"
        | _ -> "⬛"

    state.Guesses
    |> List.take (state.Round + 1)
    |> List.map (fun (_, guess) -> guess.Letters |> List.map (fun l -> square l.Status) |> String.concat "")
    |> String.concat "\n"

/// Reads a save from the current live version, so players keep their game and stats.
let private fromLiveVersion (puzzles: Puzzle array) (json: string) (day: int) =
    Storage.fromJson json
    |> Option.map (fun old ->
        let (todaysWordle, _, _) as todaysPuzzle = puzzles.[day % puzzles.Length]
        let stillToday = old.Wordle = todaysWordle

        { Day = day
          State = Game.fromSaved todaysPuzzle (if stillToday then Some old else None)
          Stats =
            { Played = old.GamesWon + old.GamesLost
              Won = old.GamesWon
              Distribution = List.init rounds (fun i -> old.WinDistribution |> List.tryItem i |> Option.defaultValue 0)
              // the old version didn't track streaks
              CurrentStreak = 0
              MaxStreak = 0
              LastWonDay = None } })

let game: DailyGame<Puzzle, State, Input> =
    let puzzles = Array.ofList Words.wordles

    { Id = "aureliadle"
      Title = "Aureliadle"
      // the same day numbering and puzzle order as the live version (src/Daily.fs)
      FirstDay = DateTime(2022, 6, 4)
      Puzzles = puzzles
      MaxAttempts = rounds
      Start = start
      Apply = apply
      Outcome = outcome
      Encode = encode
      Decoder = decoder
      ScoreText = attemptsScore rounds
      ShareGrid = shareGrid
      Legacy = Some(Storage.gameKey, fromLiveVersion puzzles) }
