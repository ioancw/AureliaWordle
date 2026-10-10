/// Fractions: Higher or Lower. A row of eight fraction cards each day; guess whether the next
/// card is higher or lower. The comparisons get harder along the row, and an equal pair
/// ("you get nothing for a pair!") costs nothing. Three hearts.
module Fractions.Rules

open System
open Engine
open Thoth.Json.Core

type Fraction = { N: int; D: int }

let fraction n d = { N = n; D = d }

let private value f = float f.N / float f.D

/// Compares by value: 2/4 and 1/2 are equal.
let compareFractions a b = compare (a.N * b.D) (b.N * a.D)

let sameValue a b = compareFractions a b = 0

let rec private gcd a b = if b = 0 then a else gcd b (a % b)

let private lcm a b = a / gcd a b * b

let show f = $"{f.N}/{f.D}"

/// Why the second card is higher, lower or the same as the first, for a child.
let explain a b =
    let c = compareFractions b a
    let word = if c > 0 then "higher than" elif c < 0 then "lower than" else "the same as"

    let reason =
        if c = 0 then
            let small, big = if a.D <= b.D then a, b else b, a
            let times = big.D / small.D

            if big.D % small.D = 0 && small.N * times = big.N then
                $"Times the top and bottom of {show small} by {times} and you get {show big}."
            else
                let common = lcm a.D b.D
                $"Make the bottoms the same: both are {a.N * (common / a.D)}/{common}."
        elif a.D = b.D then
            let more = if c > 0 then "more" else "less"
            $"Same bottom number, so compare the tops: {b.N} is {more} than {a.N}."
        elif a.N = b.N then
            let small, big = if a.D > b.D then a, b else b, a
            $"Same top number: the bigger the bottom number, the smaller the pieces, so {show small} is less than {show big}."
        else
            let common = lcm a.D b.D
            let a' = fraction (a.N * (common / a.D)) common
            let b' = fraction (b.N * (common / b.D)) common

            if common = b.D then $"Make the bottoms the same: {show a} = {show a'}, so compare {show a'} and {show b}."
            elif common = a.D then $"Make the bottoms the same: {show b} = {show b'}, so compare {show a} and {show b'}."
            else $"Make the bottoms the same: {show a} = {show a'} and {show b} = {show b'}."

    $"{show b} is {word} {show a}. {reason}"

// The cards

let cardsPerDay = 8
let hearts = 3

/// The level of each comparison along the row: easy, then medium, then close calls.
let comparisonLevels = [ 1; 1; 2; 2; 2; 3; 3 ]

let private easy = [ fraction 1 2; fraction 1 4; fraction 3 4 ]

let private medium =
    easy
    @ [ for d, ns in [ 3, [ 1; 2 ]; 5, [ 1; 2; 3; 4 ]; 6, [ 1; 5 ]; 8, [ 1; 3; 5; 7 ] ] do
            for n in ns -> fraction n d ]

let private hard =
    medium
    @ [ for d, ns in [ 10, [ 3; 7; 9 ]; 12, [ 5; 7; 11 ] ] do
            for n in ns -> fraction n d ]

/// Other ways of writing a fraction, for equal pairs (2/4 for 1/2, 6/8 for 3/4 ...).
let private equivalents f =
    [ for d in 2..12 do
          if (f.N * d) % f.D = 0 then
              let n = f.N * d / f.D
              if n > 0 && n < d && (n, d) <> (f.N, f.D) then fraction n d ]

/// Whether a next card suits a comparison at this level: easy ones are far apart, close
/// calls are near each other.
let private suits level a b =
    let gap = abs (value a - value b)

    not (sameValue a b)
    && (match level with
        | 1 -> gap >= 0.25
        | 2 -> gap >= 1.0 / 12.0 - 1e-9 && gap <= 0.5
        | _ -> gap <= 1.0 / 6.0 + 1e-9)

/// Each day's row, the same for everyone. (Park-Miller generator: JavaScript and .NET agree.)
let puzzles: Fraction list array =
    Array.init 366 (fun day ->
        let seed = ref (int64 day * 104729L + 31L)

        let next () =
            seed.Value <- seed.Value * 16807L % 2147483647L
            seed.Value

        let shuffled (items: 'a list) =
            let a = Array.ofList items

            for i in a.Length - 1 .. -1 .. 1 do
                let j = int (next () % int64 (i + 1))
                let t = a.[i]
                a.[i] <- a.[j]
                a.[j] <- t

            List.ofArray a

        // two days in three have an equal pair, somewhere in the middle of the row
        let pairAt = if next () % 3L = 0L then None else Some(2 + int (next () % 3L))
        let start = (shuffled easy).Head

        let row =
            (([ start ], 0), comparisonLevels)
            ||> List.fold (fun (cards, j) level ->
                let a = List.last cards
                let used = cards |> List.map (fun c -> c.N, c.D)
                let pool = match level with 1 -> easy | 2 -> medium | _ -> hard

                let pairCard =
                    if pairAt = Some j then
                        equivalents a |> List.filter (fun e -> not (List.contains (e.N, e.D) used)) |> shuffled |> List.tryHead
                    else
                        None

                let wantHigher = next () % 2L = 0L

                let candidates =
                    shuffled pool
                    |> List.filter (fun b -> suits level a b && not (List.contains (b.N, b.D) used))

                let chosen =
                    pairCard
                    |> Option.orElse (candidates |> List.tryFind (fun b -> (compareFractions b a > 0) = wantHigher))
                    |> Option.orElse (List.tryHead candidates)
                    // nothing unused fits: the nearest different card from the level's pool
                    |> Option.defaultWith (fun () ->
                        pool |> List.filter (fun b -> not (sameValue a b)) |> List.minBy (fun b -> abs (value a - value b)))

                cards @ [ chosen ], j + 1)
            |> fst

        row)

// Playing

type Guess =
    | Higher
    | Lower

type Result =
    | Right
    | Wrong
    | Pair

type State =
    { Cards: Fraction list
      /// How many cards are face up (the first one always is).
      Revealed: int
      Results: Result list
      Mistakes: int
      /// Comparisons got wrong on earlier days, oldest first; up to two come back each day.
      Practice: (Fraction * Fraction) list }

type Input = Guess of Guess

let start cards =
    { Cards = cards
      Revealed = 1
      Results = []
      Mistakes = 0
      Practice = [] }

let outcome state =
    if state.Mistakes >= hearts then Some Failed
    elif state.Revealed >= state.Cards.Length then Some(Solved(state.Mistakes + 1))
    else None

let private cheers = [| "Yes!"; "Spot on!"; "Brilliant!"; "Correct!"; "Super!"; "Well done!"; "Yes!" |]

let apply (Guess guess) state =
    match outcome state with
    | Some _ -> state, None
    | None ->
        let i = state.Revealed - 1
        let a, b = state.Cards.[i], state.Cards.[i + 1]
        let c = compareFractions b a

        let result =
            if c = 0 then Pair
            elif (c > 0) = (guess = Higher) then Right
            else Wrong

        let message =
            match result with
            | Pair -> "You get nothing for a pair!"
            | Right -> cheers.[i % cheers.Length]
            | Wrong -> if c > 0 then "Oh no, it was higher" else "Oh no, it was lower"

        { state with
            Revealed = state.Revealed + 1
            Results = state.Results @ [ result ]
            Mistakes = state.Mistakes + (if result = Wrong then 1 else 0) },
        Some message

/// The comparison just revealed: (previous card, new card, result).
let lastFlip state =
    match List.tryLast state.Results with
    | Some r -> Some(state.Cards.[state.Revealed - 2], state.Cards.[state.Revealed - 1], r)
    | None -> None

// Practice

let practicePerDay = 2
let practiceLimit = 10

/// Whether the comparison of card j with card j+1 is one being practised.
let isPractice state j =
    match List.tryItem j state.Cards, List.tryItem (j + 1) state.Cards with
    | Some a, Some b -> List.contains (a, b) state.Practice
    | _ -> false

/// Puts up to two practice comparisons into today's row as neighbouring cards, at places
/// whose level matches if possible, without making an accidental pair with the cards around them.
let withPractice (practice: (Fraction * Fraction) list) (cards: Fraction list) =
    let levelOf (a, b) =
        let gap = abs (value a - value b)
        if gap >= 0.25 && a.D <= 4 && b.D <= 4 then 1 elif gap > 1.0 / 6.0 then 2 else 3

    let fitsAt (cs: Fraction list) j (a, b) =
        let before = if j > 0 then Some cs.[j - 1] else None
        let after = List.tryItem (j + 2) cs

        (before |> Option.forall (fun x -> not (sameValue x a)))
        && (after |> Option.forall (fun x -> not (sameValue x b)))

    let place (cs: Fraction list, taken: int list) pair =
        let free j = taken |> List.forall (fun t -> abs (t - j) >= 2)
        let places = [ 0 .. cs.Length - 2 ] |> List.filter (fun j -> free j && fitsAt cs j pair)

        let chosen =
            places
            |> List.tryFind (fun j -> comparisonLevels.[j] = levelOf pair)
            |> Option.orElse (List.tryLast places)

        match chosen with
        | Some j -> cs |> List.updateAt j (fst pair) |> List.updateAt (j + 1) (snd pair), j :: taken
        | None -> cs, taken

    practice |> List.truncate practicePerDay |> List.fold place (cards, []) |> fst

/// A new day: comparisons got wrong last time join the practice list, practised ones got right
/// leave it, and up to two are put into today's row.
let carryOver (last: State) (today: State) =
    let flips =
        last.Results |> List.mapi (fun j r -> (last.Cards.[j], last.Cards.[j + 1]), r)

    let gotRight = flips |> List.filter (fun (_, r) -> r = Right) |> List.map fst
    let missed = flips |> List.filter (fun (_, r) -> r = Wrong) |> List.map fst

    let practice =
        (last.Practice |> List.filter (fun p -> not (List.contains p gotRight))) @ missed
        |> List.distinct
        |> List.rev
        |> List.truncate practiceLimit
        |> List.rev

    { today with
        Cards = withPractice practice today.Cards
        Practice = practice }

// Saving

let private encodeFraction f = Encode.list [ Encode.int f.N; Encode.int f.D ]

let private fractionDecoder =
    Decode.list Decode.int
    |> Decode.andThen (fun ns ->
        match ns with
        | [ n; d ] when d > 0 && n > 0 && n < d -> Decode.succeed (fraction n d)
        | _ -> Decode.fail "Invalid fraction")

let private resultCode r =
    match r with
    | Right -> "right"
    | Wrong -> "wrong"
    | Pair -> "pair"

let encode state =
    Encode.object
        [ "cards", state.Cards |> List.map encodeFraction |> Encode.list
          "revealed", Encode.int state.Revealed
          "results", state.Results |> List.map (resultCode >> Encode.string) |> Encode.list
          "mistakes", Encode.int state.Mistakes
          "practice", state.Practice |> List.map (fun (a, b) -> Encode.list [ encodeFraction a; encodeFraction b ]) |> Encode.list ]

// Fields are read first and checked afterwards (Thoth's object builder keeps running after a failure).
let decoder: Decoder<State> =
    let result =
        Decode.string
        |> Decode.andThen (fun s ->
            match s with
            | "right" -> Decode.succeed Right
            | "wrong" -> Decode.succeed Wrong
            | "pair" -> Decode.succeed Pair
            | _ -> Decode.fail "Invalid result")

    let practicePair =
        Decode.list fractionDecoder
        |> Decode.andThen (fun fs ->
            match fs with
            | [ a; b ] -> Decode.succeed (a, b)
            | _ -> Decode.fail "Invalid practice pair")

    Decode.object (fun get ->
        get.Required.Field "cards" (Decode.list fractionDecoder),
        get.Required.Field "revealed" Decode.int,
        get.Required.Field "results" (Decode.list result),
        get.Required.Field "mistakes" Decode.int,
        get.Optional.Field "practice" (Decode.list practicePair))
    |> Decode.andThen (fun (cards, revealed, results, mistakes, practice) ->
        if cards.Length >= 2 && revealed >= 1 && revealed <= cards.Length && results.Length = revealed - 1 && mistakes >= 0 then
            Decode.succeed
                { Cards = cards
                  Revealed = revealed
                  Results = results
                  Mistakes = mistakes
                  Practice = practice |> Option.defaultValue [] }
        else
            Decode.fail "Invalid fractions game")

let scoreText outcome =
    match outcome with
    | Some(Solved 1) -> "no mistakes"
    | Some(Solved 2) -> "1 mistake"
    | Some(Solved n) -> $"{n - 1} mistakes"
    | Some Failed -> "out of hearts"
    | None -> "unfinished"

/// One square per flip: right, wrong, or a pair; then the hearts left.
let shareGrid (highContrast: bool) state =
    let squares =
        state.Results
        |> List.map (fun r ->
            match r with
            | Right -> if highContrast then "🟧" else "🟩"
            | Wrong -> "🟥"
            | Pair -> "🟰")
        |> String.concat ""

    let left = max 0 (hearts - state.Mistakes)
    squares + " " + String.replicate left "❤️" + String.replicate (hearts - left) "🤍"

let game: DailyGame<Fraction list, State, Input> =
    { Id = "fractions"
      Title = "Fractions"
      FirstDay = DateTime(2026, 10, 10)
      Puzzles = puzzles
      // the stats chart counts mistakes: finished with 0, 1 or 2
      MaxAttempts = hearts
      Start = start
      Apply = apply
      Outcome = outcome
      Encode = encode
      Decoder = decoder
      ScoreText = scoreText
      ShareGrid = shareGrid
      Legacy = None
      CarryOver = Some carryOver }
