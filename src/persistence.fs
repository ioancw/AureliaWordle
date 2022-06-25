module Persistence

open Domain
open Common
open Fable.Import
open Fable.Core

let gameStateKey = "gameStateAureliav2"

module StateHelpers =
    let statusToString status =
        match status with
        | Yellow -> "Yellow"
        | Grey -> "Grey"
        | Black -> "Black"
        | Green -> "Green"
        | Invalid -> "Invalid"

    let stateToString state =
        match state with
        | NotStarted -> "Not Started"
        | Won -> "Won"
        | Lost -> "Lost"
        | Started -> "Started"

    let stateFromString (stored: LocalStorageGameState) =
        match stored.State with
        | "Not Started" -> NotStarted
        | "Won" -> Won
        | "Lost" -> Lost
        | "Started" -> Started
        | _ -> NotStarted

    let statusFromString guessStatus =
        match guessStatus with
        | "Yellow" -> Yellow
        | "Grey" -> Grey
        | "Black" -> Black
        | "Green" -> Green
        | _ -> Invalid

let saveGameStateLocalStorage (state: State) =
    let guessedWords =
        state.Guesses
        |> List.map (fun (position, guess) ->
            position,
            guess.Letters
            |> List.map (fun gl -> Letter.toString gl, StateHelpers.statusToString gl.Status)
            |> List.toArray)
        |> List.toArray

    // create a local state which doesn't use record types as they don't round trip.
    Browser.WebStorage.localStorage.setItem (
        gameStateKey,
        JS.JSON.stringify
            { Guesses = guessedWords
              Solution = state.Wordle
              Round = state.Round
              State = StateHelpers.stateToString state.State
              GamesWon = state.GamesWon
              GamesLost = state.GamesLost
              WinDistribution = state.WinDistribution |> List.toArray }
    )

let loadGameStateLocalStorage () =
    let localState = Browser.WebStorage.localStorage.getItem gameStateKey

    match localState with
    | null -> None
    | _ -> Some(localState |> JS.JSON.parse :?> LocalStorageGameState)
