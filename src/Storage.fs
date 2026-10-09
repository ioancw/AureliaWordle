/// Saving and loading the game and settings in the browser's local storage.
module Storage

open Domain
open GameRules
open Thoth.Json.Core

/// A saved game. It's stored in the same JSON shape the app has always used, so existing saves still load.
type SavedGame =
    { Wordle: string
      Guesses: (Position * Guess) list
      State: GameState
      Round: int
      GamesWon: int
      GamesLost: int
      WinDistribution: int list }

let private statusToString status =
    match status with
    | Green -> "Green"
    | Yellow -> "Yellow"
    | Grey -> "Grey"
    | Black -> "Black"
    | Invalid -> "Invalid"

let private statusFromString status =
    match status with
    | "Green" -> Green
    | "Yellow" -> Yellow
    | "Grey" -> Grey
    | "Black" -> Black
    | _ -> Invalid

let private gameStateToString state =
    match state with
    | NotStarted -> "Not Started"
    | Started -> "Started"
    | Won -> "Won"
    | Lost -> "Lost"

let private gameStateFromString state =
    match state with
    | "Started" -> Started
    | "Won" -> Won
    | "Lost" -> Lost
    | _ -> NotStarted

let private encodeGuess (position: Position, guess: Guess) =
    let encodeLetter (letter: GuessLetter) =
        Encode.tuple2 Encode.string Encode.string (Letter.toString letter, statusToString letter.Status)

    Encode.tuple2 Encode.int Encode.list (position, guess.Letters |> List.map encodeLetter)

let encodeGame (game: SavedGame) : IEncodable =
    Encode.object
        [ "Wordle", Encode.string game.Wordle
          "Guesses", game.Guesses |> List.map encodeGuess |> Encode.list
          "State", Encode.string (gameStateToString game.State)
          "Round", Encode.int game.Round
          "GamesWon", Encode.int game.GamesWon
          "GamesLost", Encode.int game.GamesLost
          "WinDistribution", game.WinDistribution |> List.map Encode.int |> Encode.list ]

let private guessDecoder: Decoder<Position * Guess> =
    let letterDecoder =
        Decode.tuple2 Decode.string Decode.string
        |> Decode.map (fun (letter, status) ->
            { Letter = Letter.toOption letter
              Status = statusFromString status })

    Decode.tuple2 Decode.int (Decode.list letterDecoder)
    |> Decode.map (fun (position, letters) -> position, { Letters = letters })

/// Only the word is required: anything else missing falls back to a default,
/// so a partly damaged save still keeps whatever stats it has.
let gameDecoder: Decoder<SavedGame> =
    Decode.object (fun get ->
        let optional name decoder defaultValue =
            get.Optional.Field name decoder |> Option.defaultValue defaultValue

        { Wordle = get.Required.Field "Wordle" Decode.string
          Guesses = optional "Guesses" (Decode.list guessDecoder) []
          State = optional "State" (Decode.string |> Decode.map gameStateFromString) NotStarted
          Round = optional "Round" Decode.int 0
          GamesWon = optional "GamesWon" Decode.int 0
          GamesLost = optional "GamesLost" Decode.int 0
          WinDistribution = optional "WinDistribution" (Decode.list Decode.int) [] })

#if FABLE_COMPILER
let toJson (game: SavedGame) =
    Thoth.Json.JavaScript.Encode.toString 0 (encodeGame game)

let fromJson (json: string) =
    Thoth.Json.JavaScript.Decode.fromString gameDecoder json |> Result.toOption
#else
let toJson (game: SavedGame) =
    Thoth.Json.Newtonsoft.Encode.toString 0 (encodeGame game)

let fromJson (json: string) =
    Thoth.Json.Newtonsoft.Decode.fromString gameDecoder json |> Result.toOption
#endif

/// The local storage key holding the saved game. Other tabs watch it for changes.
let gameKey = "gameStateAureliav3"

let private highContrastKey = "aureliaHighContrast"

// Storage can be unavailable or full (e.g. some private browsing modes); the game still works without it.
let private tryGetItem key =
    try
        Browser.WebStorage.localStorage.getItem key |> Option.ofObj
    with _ ->
        None

let private trySetItem key value =
    try
        Browser.WebStorage.localStorage.setItem (key, value)
    with _ ->
        ()

/// The saved game, or None if there isn't one or it can't be read.
let loadGame () = tryGetItem gameKey |> Option.bind fromJson

let saveGame (game: SavedGame) = trySetItem gameKey (toJson game)

let loadHighContrast () = tryGetItem highContrastKey = Some "true"

let saveHighContrast (enabled: bool) =
    trySetItem highContrastKey (if enabled then "true" else "false")
