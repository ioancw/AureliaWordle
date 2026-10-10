/// A generic engine for "puzzle of the day" games. A game supplies its rules as a DailyGame;
/// the engine handles which puzzle is today's, resuming and saving, new days, and stats.
/// Pure F#: no browser or UI code, so it's tested on .NET.
module Engine

open System
open Thoth.Json.Core

type Outcome =
    | Solved of attempts: int
    | Failed

type Stats =
    { Played: int
      Won: int
      /// Wins by number of attempts: index 0 is "solved in 1".
      Distribution: int list
      CurrentStreak: int
      MaxStreak: int
      LastWonDay: int option }

/// The usual score for games won in a number of attempts: "3/6", or "X/6" if lost.
let attemptsScore maxAttempts outcome =
    match outcome with
    | Some(Solved n) -> $"{n}/{maxAttempts}"
    | Some Failed -> $"X/{maxAttempts}"
    | None -> $"-/{maxAttempts}"

/// What's saved: today's game, and the player's stats across all days.
type Saved<'State> = { Day: int; State: 'State; Stats: Stats }

/// Everything a daily game supplies.
type DailyGame<'Puzzle, 'State, 'Input> =
    { /// Storage key prefix, e.g. "aureliadle".
      Id: string
      /// Shown in share text.
      Title: string
      /// Day 0; also numbers the puzzles when sharing.
      FirstDay: DateTime
      /// One per day, cycling.
      Puzzles: 'Puzzle array
      MaxAttempts: int
      /// A fresh game for a puzzle.
      Start: 'Puzzle -> 'State
      /// The rules: the next state, plus a message to show (e.g. "Not in word list").
      Apply: 'Input -> 'State -> 'State * string option
      /// None while still playing.
      Outcome: 'State -> Outcome option
      Encode: 'State -> IEncodable
      Decoder: Decoder<'State>
      /// The score in the share text, e.g. "3/6" (see attemptsScore).
      ScoreText: Outcome option -> string
      /// The emoji grid in the share text, given the player's high contrast setting.
      ShareGrid: bool -> 'State -> string
      /// Optional: a storage key and converter for saves in an older format, read when
      /// there's no save in the current one. The converter gets the JSON and today's day.
      Legacy: (string * (string -> int -> Saved<'State> option)) option }

// Days and puzzles

let dayOn game (date: DateTime) =
    // round rather than truncate, so a 23 or 25 hour day (clocks changing) still counts as one
    round ((date.Date - game.FirstDay.Date).TotalHours / 24.) |> int

let today game = dayOn game DateTime.Now

let puzzleFor game day =
    let n = game.Puzzles.Length
    game.Puzzles.[((day % n) + n) % n]

// Stats

let emptyStats game =
    { Played = 0
      Won = 0
      Distribution = List.replicate game.MaxAttempts 0
      CurrentStreak = 0
      MaxStreak = 0
      LastWonDay = None }

/// Counts a finished game.
let record game day outcome (stats: Stats) =
    match outcome with
    | Solved attempts ->
        let streak = if stats.LastWonDay = Some(day - 1) then stats.CurrentStreak + 1 else 1

        { stats with
            Played = stats.Played + 1
            Won = stats.Won + 1
            Distribution =
                stats.Distribution
                |> List.mapi (fun i n -> if i = attempts - 1 then n + 1 else n)
            CurrentStreak = streak
            MaxStreak = max streak stats.MaxStreak
            LastWonDay = Some day }
    | Failed ->
        { stats with
            Played = stats.Played + 1
            CurrentStreak = 0 }

/// The streak to show today: it's broken if yesterday's puzzle wasn't won.
let currentStreak day (stats: Stats) =
    match stats.LastWonDay with
    | Some won when won >= day - 1 -> stats.CurrentStreak
    | _ -> 0

// Playing

/// Today's game: the saved one if it's from today, otherwise a fresh one keeping the stats.
let resume game day (saved: Saved<'State> option) =
    match saved with
    | Some s when s.Day = day -> s
    | Some s ->
        { Day = day
          State = game.Start(puzzleFor game day)
          Stats = s.Stats }
    | None ->
        { Day = day
          State = game.Start(puzzleFor game day)
          Stats = emptyStats game }

/// Applies an input. Stats are updated once, at the moment the game finishes;
/// input after that is ignored.
let play game input (saved: Saved<'State>) =
    match game.Outcome saved.State with
    | Some _ -> saved, None
    | None ->
        let next, message = game.Apply input saved.State

        let stats =
            match game.Outcome next with
            | Some outcome -> record game saved.Day outcome saved.Stats
            | None -> saved.Stats

        { saved with State = next; Stats = stats }, message

/// The game as it should be now, given the latest save: picks up progress made in another
/// tab, or moves on to a new day. With no readable save, the in-memory game is kept.
let refresh game day (latestSave: Saved<'State> option) (current: Saved<'State>) =
    resume game day (Some(latestSave |> Option.defaultValue current))

let shareText game highContrast (saved: Saved<'State>) =
    $"{game.Title} {saved.Day} {game.ScoreText(game.Outcome saved.State)}\n\n{game.ShareGrid highContrast saved.State}"

// Saving

let private encodeStats (stats: Stats) =
    Encode.object
        [ "played", Encode.int stats.Played
          "won", Encode.int stats.Won
          "distribution", stats.Distribution |> List.map Encode.int |> Encode.list
          "currentStreak", Encode.int stats.CurrentStreak
          "maxStreak", Encode.int stats.MaxStreak
          "lastWonDay", (match stats.LastWonDay with Some d -> Encode.int d | None -> Encode.nil) ]

let private statsDecoder game : Decoder<Stats> =
    Decode.object (fun get ->
        let field name decoder = get.Optional.Field name decoder

        field "played" Decode.int,
        field "won" Decode.int,
        field "distribution" (Decode.list Decode.int),
        (field "currentStreak" Decode.int, field "maxStreak" Decode.int, field "lastWonDay" Decode.int))
    |> Decode.map (fun (played, won, distribution, (current, best, lastWon)) ->
        let count = Option.defaultValue 0 >> max 0
        let distribution = distribution |> Option.defaultValue []

        { Played = count played
          Won = count won
          // always exactly MaxAttempts entries
          Distribution = List.init game.MaxAttempts (fun i -> distribution |> List.tryItem i |> Option.defaultValue 0 |> max 0)
          CurrentStreak = count current
          MaxStreak = count best
          LastWonDay = lastWon })

let encodeSaved game (saved: Saved<'State>) =
    Encode.object
        [ "version", Encode.int 1
          "day", Encode.int saved.Day
          "state", game.Encode saved.State
          "stats", encodeStats saved.Stats ]

/// A game's decoder, made safe: a decoder that throws counts as a failed decode,
/// so one game's bug can't stop its save (and stats) from loading.
let private safely (decoder: Decoder<'T>) =
    { new Decoder<'T> with
        member _.Decode(helpers, value) =
            try
                decoder.Decode(helpers, value)
            with e ->
                Error("", FailMessage e.Message) }

/// Damaged parts are dropped rather than failing the whole save: if the game state can't be
/// read, the day starts afresh but the stats are kept.
// (Fields are read first and processed afterwards: Thoth's object builder keeps running, with
// placeholders, after a field fails.)
let savedDecoder game : Decoder<Saved<'State>> =
    let orNone decoder =
        Decode.oneOf [ safely decoder |> Decode.map Some; Decode.succeed None ]

    Decode.object (fun get ->
        get.Required.Field "day" Decode.int,
        get.Optional.Field "state" (orNone game.Decoder) |> Option.flatten,
        get.Optional.Field "stats" (orNone (statsDecoder game)) |> Option.flatten)
    |> Decode.map (fun (day, state, stats) ->
        { Day = day
          State = state |> Option.defaultWith (fun () -> game.Start(puzzleFor game day))
          Stats = stats |> Option.defaultValue (emptyStats game) })

#if FABLE_COMPILER
let toJson game saved =
    Thoth.Json.JavaScript.Encode.toString 0 (encodeSaved game saved)

let fromJson game (json: string) =
    Thoth.Json.JavaScript.Decode.fromString (savedDecoder game) json |> Result.toOption
#else
let toJson game saved =
    Thoth.Json.Newtonsoft.Encode.toString 0 (encodeSaved game saved)

let fromJson game (json: string) =
    Thoth.Json.Newtonsoft.Decode.fromString (savedDecoder game) json |> Result.toOption
#endif

let saveKey game = game.Id + ".save"

let highContrastKey game = game.Id + ".highContrast"
