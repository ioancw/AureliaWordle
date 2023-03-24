module Game

open System
open Wordles
open Domain
open Common
open Persistence

// starts a new game and loads the persisted state if it exists
let startNewGame numberOfLetters numberOfRounds =
    // set up all the world tiles depending on the number of rounds.
    let emptyGuess = 0, { Letters = List.init numberOfLetters (fun _ -> { Letter = None; Status = Black }) }
    let emptyGuesses = List.init numberOfRounds (fun _ -> emptyGuess)
    
    let guesses = emptyGuesses

    // Function to select Today's wordle from the array of daily words.
    let wordle () : string * string * string =
        let today = DateTime.Now
        let startDate = DateTime(2022, 6, 4)
        let todayDate = DateTime(today.Year, today.Month, today.Day)
        let differenceMilli = (todayDate - startDate).TotalMilliseconds
        let millisInDay = 60 * 60 * 24 * 1000 |> double
        let differenceDays = round (differenceMilli / millisInDay) |> int
        let index = differenceDays % wordles.Length
        wordles.[index]

    // Sets the wordle, the phonic hint and the corresponding grapheme
    let wordle, hint, grapheme = wordle ()

    // Load persisted game state from storage and act accordingly.
    match loadGameStateLocalStorage () with
    | Some stored ->
        let localGuesses =
            stored.Guesses
            |> Array.map (fun (position, letters) ->
                position,
                { Letters =
                    letters
                    |> Array.toList
                    |> List.map (fun (guessLetter, guessStatus) ->
                        { Letter = Letter.toOption guessLetter
                          Status = StateHelpers.statusFromString guessStatus }) })
            |> Array.toList

        // To handle additional rows.
        let extendedLocalGuesses =
            localGuesses
            |> List.mapi (fun i guess -> guess, i)
            |> List.fold (fun state (guess, i) -> List.set state guess i) (List.init numberOfRounds (fun _ -> emptyGuess))

        // Colour the keyboard keys based on existing guesses and their status.
        let storageUsedLetters =
            extendedLocalGuesses
            |> List.map snd
            |> List.fold (fun state guess -> State.updateKeyboardState guess.Letters state) Map.empty

        let loadedDistribution =
            stored.WinDistribution
            |> List.ofArray
            |> List.mapi (fun i dist -> dist, i)
            |> List.fold (fun state (dist, i) -> List.set state dist i) (List.init numberOfRounds (fun _ -> 0))

        let loadedGame =
            { Wordle = wordle
              Phonics = {Hint = hint; Grapheme = grapheme}
              Guesses = extendedLocalGuesses
              Round = stored.Round
              State = StateHelpers.stateFromString stored
              UsedLetters = storageUsedLetters
              GamesLost = stored.GamesLost
              GamesWon = stored.GamesWon
              WinDistribution = loadedDistribution }

        let newGameWithStats =
            { loadedGame with
                Guesses = guesses
                Round = 0
                State = NotStarted
                UsedLetters = Map.empty }

        // handle whether we loaded an in progress game or it's a fresh game for today.
        if wordle <> stored.Wordle then
            newGameWithStats
        else
            loadedGame
    | _ ->
        //the case where we haven't played before and there is no local storage, so new game state.
        { Wordle = wordle
          Phonics = {Hint = hint; Grapheme = grapheme}
          Guesses = guesses
          Round = 0
          State = NotStarted
          UsedLetters = Map.empty
          GamesLost = 0
          GamesWon = 0
          WinDistribution = List.init numberOfRounds (fun _ -> 0) }
