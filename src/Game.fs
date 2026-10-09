/// Starting today's game: resuming a saved one or beginning a fresh one.
module Game

open Domain
open GameRules
open Storage

let numberOfRounds = 6

let numberOfLetters = 5

let private emptyLetter = { Letter = None; Status = Black }

let private emptyGuess: Position * Guess =
    0, { Letters = List.init numberOfLetters (fun _ -> emptyLetter) }

/// Pads or trims a list to exactly n items.
let private fitTo n filler items =
    List.init n (fun i -> items |> List.tryItem i |> Option.defaultValue filler)

/// Today's game state. A saved game for today's word is resumed; otherwise a fresh game
/// starts for today's word, keeping the stats from the save.
let fromSaved (wordle, hint, grapheme) (saved: SavedGame option) : State =
    let fresh =
        { Wordle = wordle
          Phonics = { Hint = hint; Grapheme = grapheme }
          Guesses = List.init numberOfRounds (fun _ -> emptyGuess)
          UsedLetters = Map.empty
          State = NotStarted
          Round = 0
          GamesWon = 0
          GamesLost = 0
          WinDistribution = List.init numberOfRounds (fun _ -> 0) }

    match saved with
    | None -> fresh
    | Some saved ->
        let withStats =
            { fresh with
                GamesWon = max 0 saved.GamesWon
                GamesLost = max 0 saved.GamesLost
                WinDistribution = saved.WinDistribution |> fitTo numberOfRounds 0 }

        if saved.Wordle <> wordle then
            withStats
        else
            let guesses =
                saved.Guesses
                |> List.map (fun (position, guess) ->
                    position, { Letters = guess.Letters |> fitTo numberOfLetters emptyLetter })
                |> fitTo numberOfRounds emptyGuess

            // Colour the keyboard keys from the guesses already made.
            let usedLetters =
                guesses
                |> List.fold (fun used (_, guess) -> Play.updateKeyboardState guess.Letters used) Map.empty

            { withStats with
                Guesses = guesses
                UsedLetters = usedLetters
                State = saved.State
                Round = saved.Round |> max 0 |> min (numberOfRounds - 1) }

let toSaved (state: State) : SavedGame =
    { Wordle = state.Wordle
      Guesses = state.Guesses
      State = state.State
      Round = state.Round
      GamesWon = state.GamesWon
      GamesLost = state.GamesLost
      WinDistribution = state.WinDistribution }

/// Today's game, resumed from the latest save where possible.
let today (latestSave: SavedGame option) = fromSaved (Daily.todaysPuzzle ()) latestSave

/// The game as it should be now, given the latest save: picks up a save made in another
/// tab, or moves on to the new day's puzzle if the date has changed. With no readable
/// save (e.g. storage unavailable), the in-memory game is kept.
let refresh (latestSave: SavedGame option) (current: State) =
    let saved = latestSave |> Option.defaultValue (toSaved current)
    fromSaved (Daily.todaysPuzzle ()) (Some saved)

/// The on-screen keyboard. "Ent" and "Del" are the Enter and Delete keys.
type KeyBoard =
    { Top: string list
      Middle: string list
      Bottom: string list }

let keyBoard =
    { Top =
        [ "q"; "w"; "e"; "r"; "t"; "y"; "u"; "i"; "o"; "p" ]
      Middle =
        [ "a"; "s"; "d"; "f"; "g"; "h"; "j"; "k"; "l" ]
      Bottom =
        [ "Ent"; "z"; "x"; "c"; "v"; "b"; "n"; "m"; "Del" ] }

/// Shown after a win, indexed by the round the word was guessed in.
let winMessages = [| "Genius!"; "Magnificent!"; "Impressive!"; "Splendid!"; "Great!"; "Phew!" |]

/// Today's result as an emoji grid, like Wordle's share text.
let shareText highContrast (state: State) =
    let square status =
        match status with
        | Green -> if highContrast then "🟧" else "🟩"
        | Yellow -> if highContrast then "🟦" else "🟨"
        | _ -> "⬛"

    let score = if state.State = Won then string (state.Round + 1) else "X"

    let grid =
        state.Guesses
        |> List.take (state.Round + 1)
        |> List.map (fun (_, guess) -> guess.Letters |> List.map (fun l -> square l.Status) |> String.concat "")
        |> String.concat "\n"

    $"Aureliadle {Daily.dayNumber ()} {score}/{numberOfRounds}\n\n{grid}"
