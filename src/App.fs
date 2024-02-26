module Lit.Wordle

open System
open Lit
open Game
open Domain
open Common
open Persistence
open Display
open Modals
open Fable.Core

JsInterop.importSideEffects "./index.css"

let numberOfRounds = 6

let numberOfLetters = 5
    
/// The main part of the application, responsible for updating state and returning HTML. 
[<LitElement("wordle-app")>]
let MatchComponent () =
    let _ = LitElement.init (fun cfg -> cfg.useShadowDom <- false)
    
    // Starts a new game and gets the current state and a function to update the state.
    let newGame = startNewGame numberOfLetters numberOfRounds
    let gameState, setGameState = Hook.useState newGame
        
    /// Controls how the state is represented as HTML.
    let writeState state =
        // modal states
        let showHelpModal, setShowHelpModal = Hook.useState false
        let showInfoModal, setShowInfoModal = Hook.useState false
        let showStatsModal, setShowStatsModal = Hook.useState false
        
        let submitEnter = State.submitEnter numberOfRounds numberOfLetters
        let submitDelete = State.submitDelete numberOfRounds numberOfLetters
        let submitLetter = State.submitLetter numberOfRounds numberOfLetters
        
        saveGameStateLocalStorage state

        let letterToDisplayBox (letters: (Position * Guess)) =
            letters
            |> Guess.getLetter
            |> List.mapi gameTile

        let onKeyClick (c: string) =
            Ev (fun ev ->
                ev.preventDefault ()
                
                let submitEntry =
                    match c with
                    | "Ent" -> submitEnter
                    | "Del" -> submitDelete
                    | _ -> submitLetter c

                state |> submitEntry |> setGameState)
            
        let onModalClick modalType =
            Ev (fun ev ->
                ev.preventDefault ()

                match modalType with
                | Info -> setShowInfoModal (showInfoModal <> true)
                | Stats -> setShowStatsModal (showStatsModal <> true)
                | Help -> setShowHelpModal (showHelpModal <> true))            
                        
        let keyboardKey = keyboardChar state.UsedLetters onKeyClick

        html
            $"""
            <div class="min-h-screen space-y-3 bg-stone-900">
                <div class="mb-1">
                    <div class="flex items-center justify-between h-10 px-5">
                        <svg @click={onModalClick Info} xmlns="http://www.w3.org/2000/svg" class="h-7 w-7 text-white" fill="none" viewBox="0 0 24 24" stroke="currentColor" stroke-width="2">
                            <path stroke-linecap="round" stroke-linejoin="round" d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
                        </svg>
                        <div class="aurelia-header">Aureliadle</div>
                        <div class="flex">
                            <div class="mr-3">
                                <svg @click={onModalClick Stats} xmlns="http://www.w3.org/2000/svg" class="h-7 w-7 text-white" viewBox="0 0 20 20" fill="currentColor"  >
                                    <path d="M2 11a1 1 0 011-1h2a1 1 0 011 1v5a1 1 0 01-1 1H3a1 1 0 01-1-1v-5zM8 7a1 1 0 011-1h2a1 1 0 011 1v9a1 1 0 01-1 1H9a1 1 0 01-1-1V7zM14 4a1 1 0 011-1h2a1 1 0 011 1v12a1 1 0 01-1 1h-2a1 1 0 01-1-1V4z" />
                                </svg>
                            </div>
                            <div>
                                <svg @click={onModalClick Help} xmlns="http://www.w3.org/2000/svg" class="h-7 w-7 text-white" fill="none" viewBox="0 0 24 24" stroke="currentColor" stroke-width="2">
                                    <path stroke-linecap="round" stroke-linejoin="round" d="M8.228 9c.549-1.165 2.03-2 3.772-2 2.21 0 4 1.343 4 3 0 1.4-1.278 2.575-3.006 2.907-.542.104-.994.54-.994 1.093m0 3h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
                                </svg>
                            </div>
                        </div>
                    </div>
                    <hr></hr>
                </div>

                {modal "About" infoText showInfoModal (onModalClick Info)}
                {modal "Game Statistics" (statsText state) showStatsModal (onModalClick Stats)}
                {modal "Grapheme Phoneme Correspondence" (helpText state) showHelpModal (onModalClick Help)}
                {LostModal state}

                <div>
                    <div class="flex justify-center mb-1">
                        {List.item 0 state.Guesses |> letterToDisplayBox}
                    </div>
                    <div class="flex justify-center mb-1">
                        {List.item 1 state.Guesses |> letterToDisplayBox}
                    </div>
                    <div class="flex justify-center mb-1">
                        {List.item 2 state.Guesses |> letterToDisplayBox}
                    </div>
                    <div class="flex justify-center mb-1">
                        {List.item 3 state.Guesses |> letterToDisplayBox}
                    </div>
                    <div class="flex justify-center mb-1">
                        {List.item 4 state.Guesses |> letterToDisplayBox}
                    </div>
                    <div class="flex justify-center mb-1">
                        {List.item 5 state.Guesses |> letterToDisplayBox}
                    </div>
                </div>

                <div class="absolute inset-x-0 bottom-0">
                    <div class="flex justify-center mb-1.5">
                        {keyBoard.Top |> List.map keyboardKey}
                    </div>
                    <div class="flex justify-center mb-1.5">
                        {keyBoard.Middle |> List.map keyboardKey}
                    </div>
                    <div class="flex justify-center mb-1.5">
                        {keyBoard.Bottom |> List.map keyboardKey}
                    </div>
                </div>
            </div>
        """

    // do we always do the same thing irrespective of state?
    match gameState.State with
    | NotStarted | Started | Won | Lost -> gameState |> writeState
