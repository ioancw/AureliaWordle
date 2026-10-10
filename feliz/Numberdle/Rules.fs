/// Numberdle: guess today's number from 1 to 100 in 7 tries. Each guess says whether the
/// number is higher or lower, and how close you are.
module Numberdle.Rules

open System
open Engine
open Thoth.Json.Core

let highest = 100
let attempts = 7

type Direction =
    | Higher
    | Lower
    | Correct

type Warmth =
    | Hot
    | Warm
    | Cold

type Guess =
    { Value: int
      Direction: Direction
      Warmth: Warmth }

type State =
    { Target: int
      Guesses: Guess list
      /// The number being typed.
      Entry: string }

type Input =
    | Digit of int
    | Enter
    | Delete

let feedback target value =
    let distance = abs (target - value)

    { Value = value
      Direction =
        if value = target then Correct
        elif target > value then Higher
        else Lower
      Warmth =
        if distance <= 5 then Hot
        elif distance <= 15 then Warm
        else Cold }

let start target =
    { Target = target
      Guesses = []
      Entry = "" }

let apply input state =
    match input with
    | Digit d when state.Entry.Length < 3 && not (state.Entry = "" && d = 0) ->
        { state with Entry = state.Entry + string d }, None
    | Digit _ -> state, None
    | Delete when state.Entry <> "" -> { state with Entry = state.Entry.[.. state.Entry.Length - 2] }, None
    | Delete -> state, None
    | Enter ->
        match Int32.TryParse state.Entry with
        | false, _ -> state, Some "Type a number"
        | true, n when n < 1 || n > highest -> { state with Entry = "" }, Some $"Pick a number from 1 to {highest}"
        | true, n when state.Guesses |> List.exists (fun g -> g.Value = n) -> { state with Entry = "" }, Some $"You've tried {n} already"
        | true, n ->
            { state with
                Guesses = state.Guesses @ [ feedback state.Target n ]
                Entry = "" },
            None

let outcome state =
    match List.tryLast state.Guesses with
    | Some g when g.Direction = Correct -> Some(Solved state.Guesses.Length)
    | _ when state.Guesses.Length >= attempts -> Some Failed
    | _ -> None

// Only the numbers are saved; the feedback is worked out again when loading.
let encode state =
    Encode.object
        [ "target", Encode.int state.Target
          "guesses", state.Guesses |> List.map (fun g -> Encode.int g.Value) |> Encode.list
          "entry", Encode.string state.Entry ]

// The fields are read first and processed afterwards: Thoth's object builder keeps running
// (with placeholders) after a field fails, so it shouldn't compute anything itself.
let decoder: Decoder<State> =
    Decode.object (fun get ->
        get.Required.Field "target" Decode.int,
        get.Required.Field "guesses" (Decode.list Decode.int),
        get.Optional.Field "entry" Decode.string)
    |> Decode.andThen (fun (target, guesses, entry) ->
        if target >= 1 && target <= highest then
            Decode.succeed
                { Target = target
                  Guesses = guesses |> List.map (feedback target)
                  Entry = entry |> Option.defaultValue "" }
        else
            Decode.fail "Target out of range")

let shareGrid (_highContrast: bool) state =
    state.Guesses
    |> List.map (fun g ->
        match g.Direction, g.Warmth with
        | Correct, _ -> "✅"
        | dir, warmth ->
            (if dir = Higher then "⬆️" else "⬇️")
            + (match warmth with
               | Hot -> "🔥"
               | Warm -> "🙂"
               | Cold -> "🧊"))
    |> String.concat " "

/// 1 to 100 in a fixed shuffled order, so every player gets the same number each day.
/// (Park-Miller generator: its products stay below 2^53, so JavaScript and .NET agree exactly.)
let puzzles =
    let seed = ref 2026L

    let next () =
        seed.Value <- seed.Value * 16807L % 2147483647L
        seed.Value

    let numbers = Array.init highest (fun i -> i + 1)

    for i in numbers.Length - 1 .. -1 .. 1 do
        let j = int (next () % int64 (i + 1))
        let t = numbers.[i]
        numbers.[i] <- numbers.[j]
        numbers.[j] <- t

    numbers

let game: DailyGame<int, State, Input> =
    { Id = "numberdle"
      Title = "Numberdle"
      FirstDay = DateTime(2026, 10, 10)
      Puzzles = puzzles
      MaxAttempts = attempts
      Start = start
      Apply = apply
      Outcome = outcome
      Encode = encode
      Decoder = decoder
      ShareGrid = shareGrid
      Legacy = None }
