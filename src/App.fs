module Lit.Wordle

open System
open Lit
open Wordles
open Domain
open Common
open Persistence
open Fable.Import
open Fable.Core

JsInterop.importSideEffects "./index.css"

let rounds = 5

let letters = 5

module Validate =

    let letterPosition n = n >= 0 && n < letters

    let round state =
        if state.State = Lost || state.State = Won then
            false
        else
            (state.Round >= 0 && state.Round < rounds)

    let allLetters n (guesses: (Position * Guess) list) =
        let _, guess = List.item n guesses

        let fiveLetterWords =
            words
            |> Seq.filter (fun (l: string) -> l.Length = 5)
            |> Seq.map Letter.toUpper
            |> Seq.toList

        guess.Letters
        |> List.forall (fun gl -> gl.Letter <> None)
        && List.contains (Guess.guessToWord guess) fiveLetterWords

let emptyGuesses =
    let emptyGuess =
        0, { Letters = List.init letters (fun _ -> { Letter = None; Status = Black }) }

    List.init rounds (fun _ -> emptyGuess)

// fold over each letter in the guess to determine keyboard colours
let updateKeyboardState guesses (keyBoardState: Map<string, Status>) =
    (keyBoardState, guesses)
    ||> List.fold (fun (state: Map<string, Status>) gl ->
        let l = Letter.toString gl
        let s = gl.Status

        if s = Green
           || (s = Grey && not <| state.ContainsKey l)
           || (s = Yellow && not <| state.ContainsKey l) then
            state.Add(l, s)
        else
            state)

let startNewGame =
    let loadedStorage = loadGameStateLocalStorage ()
    let guesses = emptyGuesses

    let wordle () : string * string =
        let today = DateTime.Now
        let startDate = DateTime(2022, 6, 4)
        let todayDate = DateTime(today.Year, today.Month, today.Day)
        let differenceMilli = (todayDate - startDate).TotalMilliseconds
        let millisInDay = 60 * 60 * 24 * 1000 |> double
        let differenceDays = round (differenceMilli / millisInDay) |> int
        let index = differenceDays % wordles.Length
        wordles.[index]

    let wordle, hint = wordle ()

    match loadedStorage with
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

        //fold local guesses in order to colour the keyboard.
        let storageUsedLetters =
            localGuesses
            |> List.map snd
            |> List.fold (fun state guess -> updateKeyboardState guess.Letters state) Map.empty

        let loadedGame =
            { Wordle = wordle
              Hint = hint
              ShowInfo = false
              ShowStats = false
              ShowHelp = false
              Guesses = localGuesses
              Round = stored.Round
              State = StateHelpers.stateFromString stored
              UsedLetters = storageUsedLetters
              GamesLost = stored.GamesLost
              GamesWon = stored.GamesWon
              WinDistribution = stored.WinDistribution |> List.ofArray }

        let newGameWithStats =
            { loadedGame with
                Guesses = guesses
                Round = 0
                State = NotStarted
                UsedLetters = Map.empty }

        if wordle <> stored.Solution then
            newGameWithStats
        else
            loadedGame
    | _ ->
        //the case where we haven't played before and there is no local storage
        { Wordle = wordle
          Hint = hint
          ShowInfo = false
          ShowStats = false
          ShowHelp = false
          Guesses = guesses
          Round = 0
          State = NotStarted
          UsedLetters = Map.empty
          GamesLost = 0
          GamesWon = 0
          WinDistribution = List.init rounds (fun _ -> 0) }

let getAnswerMask actualWord guessWord =
    let letters = Seq.zip actualWord guessWord

    let folder ((count, mask): Map<'a, int> * Status list) (actualLetter, guessLetter) =
        if actualLetter = guessLetter then
            count, Green :: mask
        elif Seq.contains guessLetter actualWord
             && Counter.countOf count guessLetter > 0 then
            Counter.updateCount count guessLetter, Yellow :: mask
        else
            count, Grey :: mask

    letters
    |> Seq.fold folder (Counter.createCounter letters, [])
    |> snd
    |> Seq.rev

let scoreGuess actualWord guessWord =
    { Letters =
        getAnswerMask actualWord guessWord
        |> Seq.zip guessWord
        |> Seq.toList
        |> List.map (fun (a, m) -> { Letter = Some(string a); Status = m }) }

let updateState state updateFunction =
    if Validate.round state then
        let word = List.item state.Round state.Guesses
        let updatedWord = updateFunction word
        { state with Guesses = List.set state.Guesses updatedWord state.Round }
    else
        state

let submitLetter letter state =
    let add letter (position, guessLetters) =
        if Validate.letterPosition position then
            position + 1, { Letters = List.set guessLetters.Letters { Letter = Some letter; Status = Black } position }
        else
            position, { Letters = guessLetters.Letters }

    letter |> add |> updateState state

let submitDelete state =
    let deleteLetter (position, guessLetters) =
        let deletePosition = position - 1

        if Validate.letterPosition deletePosition then
            deletePosition, { Letters = List.set guessLetters.Letters { Letter = None; Status = Black } deletePosition }
        else
            position, { Letters = guessLetters.Letters }

    deleteLetter |> updateState state

// This function is called when enter on the keyboard is clicked.
// If the round is valid and all letters are valid (i.e. the word formed by the letters exists in the dictionary)
// If these conditions are met, then the word for that row is submitted as a guess.
let submitEnter state =
    let submitGuess ((position, guess): Position * Guess) =
        let guessWord = Guess.guessToWord guess
        let scoredGuess = guessWord |> scoreGuess state.Wordle

        let updatedUsedLetters = updateKeyboardState scoredGuess.Letters state.UsedLetters

        let updatedGuess = (position, scoredGuess)

        let updatedState =
            if guessWord = state.Wordle then
                Won
            elif state.Round = (rounds - 1) then
                Lost
            else
                Started

        updatedGuess, updatedState, updatedUsedLetters

    if Validate.round state then
        if Validate.allLetters state.Round state.Guesses then
            let scoredGuess, updatedGameState, updatedUsedLetters =
                submitGuess (List.item state.Round state.Guesses)

            let winDistribution = List.item state.Round state.WinDistribution

            // update the game state based on the results of submitting the guess
            { state with
                Guesses = List.set state.Guesses scoredGuess state.Round
                UsedLetters = updatedUsedLetters
                State = updatedGameState
                Round =
                    if updatedGameState = Won || updatedGameState = Lost then
                        state.Round
                    else
                        state.Round + 1
                GamesWon =
                    if updatedGameState = Won then
                        state.GamesWon + 1
                    else
                        state.GamesWon
                GamesLost =
                    if updatedGameState = Lost then
                        state.GamesLost + 1
                    else
                        state.GamesLost
                WinDistribution =
                    if updatedGameState = Won then
                        List.set state.WinDistribution (winDistribution + 1) state.Round
                    else
                        state.WinDistribution }
        else
            let invalidGuess = state.Guesses |> List.item state.Round |> snd

            // set all letters are invalid word.
            // this required the user to delete them from final position
            let updated =
                { invalidGuess with
                    Letters =
                        invalidGuess.Letters
                        |> List.map (fun ls -> { ls with Status = Invalid }) }

            { state with Guesses = List.set state.Guesses (letters, updated) state.Round }
    else
        state

let boxedChar (c, status) =
    // https://tailwindcss.com/docs/border-style
    let colour, border =
        match status with
        | Black -> "bg-stone-900", "border-neutral-500"
        | Grey -> "bg-neutral-700", "border-neutral-700"
        | Green -> "bg-green-700", "border-green-700"
        | Yellow -> "bg-yellow-500", "border-yellow-500"
        | Invalid -> "bg-red-500", "border-red-500"

    html
        $"""
        <div class="border-solid border-transparent flex border-2 items-center rounded">
            <button class="w-14 h-14 {colour} text-center leading-none text-3xl font-bold text-white border-2 {border}">{c}</button>
        </div>
    """

let littleBoxedChar (c, status) =
    // https://tailwindcss.com/docs/border-style
    let colour, border =
        match status with
        | Black -> "bg-stone-900", "border-neutral-500"
        | Grey -> "bg-red-800", "border-red-800"
        | Green -> "bg-green-700", "border-green-700"
        | Yellow -> "bg-yellow-600", "border-yellow-600"
        | Invalid -> "bg-neutral-400", "border-neutral-400"

    html
        $"""
        <div class="border-solid border-transparent flex border-0 items-center rounded">
            <button class="w-6 h-8 {colour} text-center leading-none text-2xl font-bold font-sans text-white border-0 {border}">{c}</button>
        </div>
    """

let keyboardChar usedLetters handler (c: string) =
    let colour =
        let letterStatus =
            match Map.tryFind (c.ToUpper()) usedLetters with
            | Some x -> x
            | None -> Black

        match letterStatus with
        | Black -> "bg-neutral-500"
        | Yellow -> "bg-yellow-500"
        | Grey -> "bg-neutral-700"
        | Green -> "bg-green-700"
        | Invalid -> "bg-gray-400"

    let width =
        match c with
        | "Del"
        | "Ent" -> "w-12"
        | _ -> "w-9"

    html
        $"""
        <button
            @click={handler c}
                class="flex items-center justify-center rounded mx-0.5 {width} h-14 {colour} uppercase text-white"
        >{c}</button>
    """

let modal customHead bodyText modalDisplayState handler =
    let hidden =
        match modalDisplayState with
        | true -> ""
        | false -> "hidden"

    html
        $"""
        <div class="modal fade fixed inset-0 flex justify-center {hidden} outline-none overflow-x-hidden overflow-y-auto" id="exampleModal" tabindex="-1" aria-labelledby="exampleModalLabel" aria-hidden="true">
            <div class="modal-dialog pointer-events-none">
                <div class="modal-content border-none shadow-lg relative flex flex-col w-full pointer-events-auto bg-neutral-400 bg-clip-padding rounded-md outline-none text-current">
                    <div class="modal-header flex flex-shrink-0 items-center justify-between p-2 border-b border-stone-600 rounded-t-md">
                        <h5 class="text-lg text-left font-medium leading-normal text-stone-800" id="exampleModalLabel">
                            {customHead}
                        </h5>
                        <button type="button" @click={handler} class="px-2
                            py-1
                            bg-stone-800
                            text-white
                            font-bold
                            text-xs
                            leading-tight
                            uppercase
                            rounded
                            shadow-md
                            transition
                            duration-150
                            ease-in-out" data-bs-dismiss="modal">
                            X
                        </button>
                    </div>
                    {bodyText}
                </div>
            </div>
        </div>
    """

let infoText =
    html
        $"""
        <div class="modal-body p-2 text-slate-800">
            <p>This is a wordle type game to help children with their phonics.</p>
            </br>
            <p>For each wordle, a phonic hint is given as a phoneme (i.e. the sound).</p>
            </br>
            <p>For example, if the word to be guessed is <span class="text-green-700 font-bold">SHACK</span>, then the phoneme hint given is <span class="text-red-800 font-bold">/sh/</span>.</p>
            <p>Not all phonemes in the word are provided, instead one of the phonemes is given in the hint.</p>
            </br>
            <p>Children can use their grapheme-phoneme correspondence knowledge in order to determine the appropriate</p>
            <p>grapheme (spelling) for the phoneme in question.</p>
            </br>
            <p>GPC examples for the phoneme hint can be seen by clicking the ? button.</p>
            </br>
            <p>This application was developed using the <a href="https://fsharp.org" class="text-blue-600">F#</a> language using <a href="https://fable.io/Fable.Lit/" class="text-blue-600">Fable.Lit</a></p>
        </div>
    """

let helpText hint =
    //go get the graphemes from the phonemes.
    let hintedGraphemes =
        defaultArg (Map.tryFind hint phonemeGraphemeCorresspondances) []

    let graphemes =
        let maxLenGrapheme =
            hintedGraphemes
            |> List.map (snd >> String.length)
            |> List.max

        [ for grapheme, exampleWord in hintedGraphemes do
              let pad = maxLenGrapheme - (String.length grapheme)

              let padded =
                  Seq.concat [ grapheme |> Seq.map (fun g -> g, Green)
                               Seq.init (pad + 1) (fun _ -> ' ', Invalid) ]

              let letters =
                  exampleWord
                  |> Seq.map (fun l ->
                      l,
                      if (Seq.contains l grapheme) then
                          Green
                      else
                          Yellow)

              html
                  $"""
                    <div class="flex justify-left mb-1">
                        {padded |> Seq.map littleBoxedChar}
                        {letters |> Seq.map littleBoxedChar}
                    </div>
                """ ]

    html
        $"""
        <div class="modal-body p-2 text-slate-800 text-center">
            <p>Graphemes corresponding to today's phoneme.
                <div class="flex justify-center mb-1">
                    {hint
                     |> Seq.map (fun l -> (l, Grey) |> littleBoxedChar)}
                </div>
            </p>
            </br>
            <p>{graphemes}</p>
        </div>
    """

[<LitElement("wordle-app")>]
let MatchComponent () =
    let _ = LitElement.init (fun cfg -> cfg.useShadowDom <- false)
    let startedGame = startNewGame
    let gameState, setGameState = Hook.useState startedGame

    let writeState state =
        saveGameStateLocalStorage state

        let letterToDisplayBox = Guess.getLetter >> List.map boxedChar

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
                | Info -> { state with ShowInfo = state.ShowInfo <> true }
                | Stats -> { state with ShowStats = state.ShowStats <> true }
                | Help -> { state with ShowHelp = state.ShowHelp <> true }
                |> setGameState)

        let keyboardKey = keyboardChar state.UsedLetters onKeyClick
        let stats = $"Won: %d{state.GamesWon}, Lost: %d{state.GamesLost}"

        let message =
            match state.State with
            | NotStarted
            | Started -> $"Today's phonic hint is: %s{state.Hint}"
            | Won -> $"Congratulations! %s{stats}"
            | Lost -> $"It was %s{state.Wordle}. %s{stats}"
        // <svg xmlns="http://www.w3.org/2000/svg" class="h-7 w-7 text-white" viewBox="0 0 20 20" fill="currentColor"  >
        //     <path d="M2 11a1 1 0 011-1h2a1 1 0 011 1v5a1 1 0 01-1 1H3a1 1 0 01-1-1v-5zM8 7a1 1 0 011-1h2a1 1 0 011 1v9a1 1 0 01-1 1H9a1 1 0 01-1-1V7zM14 4a1 1 0 011-1h2a1 1 0 011 1v12a1 1 0 01-1 1h-2a1 1 0 01-1-1V4z" />
        // </svg>

        html
            $"""
            <div class="min-h-screen space-y-3 bg-stone-900">
                <div class="mb-2">
                    <div class="flex items-center justify-between h-12 px-5">
                        <svg @click={onModalClick Info} xmlns="http://www.w3.org/2000/svg" class="h-7 w-7 text-white" fill="none" viewBox="0 0 24 24" stroke="currentColor" stroke-width="2">
                        <path stroke-linecap="round" stroke-linejoin="round" d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
                        </svg>
                        <p class="ml-2.5 justify-center font-mono text-3xl text-white">Aurelia-dle</p>
                        <div class="flex">
                            <svg @click={onModalClick Help} xmlns="http://www.w3.org/2000/svg" class="h-7 w-7 text-white" fill="none" viewBox="0 0 24 24" stroke="currentColor" stroke-width="2">
                                <path stroke-linecap="round" stroke-linejoin="round" d="M8.228 9c.549-1.165 2.03-2 3.772-2 2.21 0 4 1.343 4 3 0 1.4-1.278 2.575-3.006 2.907-.542.104-.994.54-.994 1.093m0 3h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
                            </svg>
                        </div>
                    </div>
                    <hr></hr>
                </div>

                {modal "About" infoText state.ShowInfo (onModalClick Info)}
                {modal "Grapheme Phoneme Correspondence" (helpText state.Hint) state.ShowHelp (onModalClick Help)}

                <div class="flex justify-center text-lg font-mono text-white">
                    {message}
                </div>
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
                </div>
                <div class="flex justify-center mb-1">
                    {keyBoard.Top |> List.map keyboardKey}
                </div>
                <div class="flex justify-center mb-1">
                    {keyBoard.Middle |> List.map keyboardKey}
                </div>
                <div class="flex justify-center ">
                    {keyBoard.Bottom |> List.map keyboardKey}
                </div>
            </div>
        """

    // do we always do the same thing irrespective of state?
    match gameState.State with
    | NotStarted -> gameState |> writeState
    | Started -> gameState |> writeState
    | Won -> gameState |> writeState
    | Lost -> gameState |> writeState
