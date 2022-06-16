module Lit.Wordle

open System
open Lit
open Wordles
open Fable.Import
open Fable.Core
open System.Text.RegularExpressions

JsInterop.importSideEffects "./index.css"

type KeyBoard =
    { Top: string list
      Middle: string list
      Bottom: string list }

type Modal =
    | Info
    | Help
    | Stats

type Status =
    | Green
    | Yellow
    | Grey
    | Black
    | Invalid

type GuessLetter =
    { Letter: string option
      Status: Status }

type LocalStorageGameState =
    { Guesses: (int * (string * string) []) [] //guess contains a phonic hint.
      Solution: string
      Round: int
      State: string
      GamesWon: int
      GamesLost: int
      WinDistribution: int [] }

module Letter =
    let letterToString guessLetter = defaultArg guessLetter.Letter ""

type Guess =
    { Letters: GuessLetter list }

    member t.AsWord() =
        let guess =
            t.Letters
            |> List.map Letter.letterToString
            |> Seq.fold (+) ""

        guess.ToUpper()

type Position = int

type GameState =
    | NotStarted
    | Won
    | Lost
    | Started

type State =
    { Wordle: string
      Hint: string
      Guesses: (Position * Guess) list
      ShowInfo: bool
      ShowStats: bool
      ShowHelp: bool
      UsedLetters: Map<string, Status>
      State: GameState
      Round: int
      GamesWon: int
      GamesLost: int
      WinDistribution: int list }

let rounds = 5

let letters = 5

let validLetterPosition n = n >= 0 && n < letters

let validRound state =
    if state.State = Lost || state.State = Won then
        false
    else
        (state.Round >= 0 && state.Round < rounds)

let emptyGuesses =
    let emptyGuess =
        0, { Letters = List.init letters (fun _ -> { Letter = None; Status = Black }) }

    List.init rounds (fun _ -> emptyGuess)

let allValidLetters n (guesses: (Position * Guess) list) =
    let (_, guess) = List.item n guesses

    let fiveLetterWords =
        words
        |> Seq.filter (fun (l: string) -> l.Length = 5)
        |> Seq.map (fun s -> s.ToUpper())
        |> Seq.toList

    let guessWord: string = guess.AsWord()

    guess.Letters
    |> List.forall (fun gl -> gl.Letter <> None)
    && List.contains guessWord fiveLetterWords

let keyBoard =
    { Top =
        [ "q"
          "w"
          "e"
          "r"
          "t"
          "y"
          "u"
          "i"
          "o"
          "p" ]
      Middle =
        [ "a"
          "s"
          "d"
          "f"
          "g"
          "h"
          "j"
          "k"
          "l" ]
      Bottom =
        [ "Ent"
          "z"
          "x"
          "c"
          "v"
          "b"
          "n"
          "m"
          "Del" ] }

// persisted game state stuff.
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
            |> List.map (fun gl -> Letter.letterToString gl, StateHelpers.statusToString gl.Status)
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
    let localState = Browser.WebStorage.localStorage.getItem (gameStateKey)

    match localState with
    | null -> None
    | _ -> Some(localState |> JS.JSON.parse :?> LocalStorageGameState)

// fold over each letter in the guess to determine keyboard colours
let getUsedLetters letterGuesses (state: Map<string, Status>) =
    List.fold
        (fun (state: Map<string, Status>) gl ->
            let l = Letter.letterToString gl
            let s = gl.Status

            if s = Green
               || (s = Grey && not <| state.ContainsKey l)
               || (s = Yellow && not <| state.ContainsKey l) then
                state.Add(l, s)
            else
                state)
        state
        letterGuesses

let startNewGame =
    Console.WriteLine("Starting a new game")
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

    let todaysWordle = wordle ()

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
                        let letterOption =
                            if guessLetter = "" then
                                None
                            else
                                Some guessLetter

                        { Letter = letterOption
                          Status = StateHelpers.statusFromString guessStatus }) })
            |> Array.toList

        //fold local guesses in order to colour the keyboard.
        let storageUsedLetters =
            localGuesses
            |> List.map snd
            |> List.fold (fun state guess -> getUsedLetters guess.Letters state) Map.empty

        let loadedGame =
            { Wordle = fst todaysWordle
              Hint = snd todaysWordle
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

        if fst todaysWordle <> stored.Solution then
            newGameWithStats
        else
            loadedGame
    | _ ->
        //the case where we haven't played before and there is no local storage
        { Wordle = fst todaysWordle
          Hint = snd todaysWordle
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

module Counter =
    let createCounter items =
        items
        |> List.filter (fun (a, g) -> a <> g)
        |> List.map fst
        |> List.countBy id
        |> Map.ofList

    let countOf counter item =
        match Map.tryFind item counter with
        | Some c -> c
        | None -> 0

    let updateCount counter item =
        match Map.tryFind item counter with
        | Some c -> Map.add item (c - 1) counter
        | None -> counter

let getAnswerMask actual guess =
    let letters = Seq.zip actual guess |> Seq.toList

    let folder ((count, mask): Map<'a, int> * Status list) (a, g) =
        if a = g then
            count, Green :: mask
        elif Seq.contains g actual
             && Counter.countOf count g > 0 then
            Counter.updateCount count g, Yellow :: mask
        else
            count, Grey :: mask

    { Letters =
        Seq.fold folder (Counter.createCounter letters, []) letters
        |> snd
        |> Seq.rev
        |> Seq.zip guess
        |> Seq.toList
        |> List.map (fun (a, m) -> { Letter = Some(string a); Status = m }) }

let listSet list value pos =
    list
    |> List.mapi (fun i v -> if i = pos then value else v)

let applyLetterUpdate updateFunction state =
    if validRound state then
        let item = List.item state.Round state.Guesses
        let updated = updateFunction item
        { state with Guesses = listSet state.Guesses updated state.Round }
    else
        state

let submitLetter input state =
    let getNewWordStateAdd (position, guessLetters) =
        if validLetterPosition position then
            position + 1, { Letters = listSet guessLetters.Letters { Letter = Some input; Status = Black } position }
        else
            position, { Letters = guessLetters.Letters }

    applyLetterUpdate getNewWordStateAdd state

let submitDelete state =
    let getNewWordStateDelete (position, guessLetters) =
        let deletePosition = position - 1

        if validLetterPosition deletePosition then
            deletePosition, { Letters = listSet guessLetters.Letters { Letter = None; Status = Black } deletePosition }
        else
            position, { Letters = guessLetters.Letters }

    applyLetterUpdate getNewWordStateDelete state

let submitEnter state =
    let updateRoundStatus ((position, guess): Position * Guess) =
        let guessWord = guess.AsWord()
        let guessMask = guessWord |> getAnswerMask state.Wordle

        let updatedUsedLetters = getUsedLetters guessMask.Letters state.UsedLetters

        let updatedGuess = (position, guessMask)

        let updatedState =
            if guessWord = state.Wordle then
                Won
            elif state.Round = (rounds - 1) then
                Lost
            else
                Started

        updatedGuess, updatedState, updatedUsedLetters

    if validRound state then
        if allValidLetters state.Round state.Guesses then
            let updatedGuess, updatedState, updatedUsedLetters =
                updateRoundStatus (List.item state.Round state.Guesses)

            let winDistribution = List.item state.Round state.WinDistribution

            { state with
                Guesses = listSet state.Guesses updatedGuess state.Round
                UsedLetters = updatedUsedLetters
                State = updatedState
                Round =
                    if updatedState = Won || updatedState = Lost then
                        state.Round
                    else
                        state.Round + 1
                GamesWon =
                    if updatedState = Won then
                        state.GamesWon + 1
                    else
                        state.GamesWon
                GamesLost =
                    if updatedState = Lost then
                        state.GamesLost + 1
                    else
                        state.GamesLost
                WinDistribution =
                    if updatedState = Won then
                        listSet state.WinDistribution (winDistribution + 1) state.Round
                    else
                        state.WinDistribution }
        else
            let (position, letters) = state.Guesses |> List.item state.Round

            let updated =
                { letters with
                    Letters =
                        letters.Letters
                        |> List.map (fun ls -> { ls with Status = Invalid }) }

            { state with Guesses = listSet state.Guesses (position, updated) state.Round }
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
        <div class="modal-body relative p-2 text-slate-800">
            <p>This is a wordle type game to help children with their phonics.</p>
            </br>
            <p>For each wordle, a phonic hint is given as a phoneme (i.e. the sound).</p>
            </br>
            <p>For example, if the word to be guessed is <span class="text-green-700 font-bold">SHACK</span>, then the phoneme hint given is <span class="text-red-800 font-bold">/sh/</span>.
            Not all phonemes in the word are provided, rather the more complex phoneme is given in the hint.</p>
            </br>
            <p>Children can use their grapheme-phoneme correspondence knowledge in order to determine the appropriate grapheme (spelling) for the phoneme in question.</p>
            </br>
            <p>GPC examples for the phoneme hint can be seen by clicking the ? button.</p>
            </br>
            <p>This application was developed using the <a href="https://fsharp.org" class="text-blue-600">F#</a> language using <a href="https://fable.io/Fable.Lit/" class="text-blue-600">Fable.Lit</a></p>
        </div>
    """

let helpText hint =
    //go get the graphemes from the phonemes.
    let hintedGraphemes =
        match Map.tryFind hint phonemeGraphemeCorresspondances with
        | Some g -> g
        | None -> []

    let graphemes =
        let maxLenGrapheme =
            hintedGraphemes
            |> List.map (fun (g, w) -> String.length g)
            |> List.max

        [ for (grapheme, exampleWord) in hintedGraphemes do
              let pad = maxLenGrapheme - (String.length grapheme)

              let padded =
                  Seq.concat [ grapheme |> Seq.map (fun g -> g, Green)
                               Seq.init (pad + 1) (fun _ -> ' ', Invalid) ]

              let ls =
                  exampleWord
                  |> Seq.map (fun l ->
                      l,
                      if (Seq.contains l grapheme)
                      then Green
                      else Yellow)

              html
                  $"""
                <div class="flex justify-left mb-1">
                    {padded |> Seq.map littleBoxedChar}
                    {ls |> Seq.map littleBoxedChar}
                </div>
            """ ]

    html
        $"""
        <div class="modal-body p-2 text-slate-800 text-center">
            <p>Graphemes corresponding to today's phoneme.
                <div class="flex justify-center mb-1">
                    {hint |> Seq.map (fun l -> (l, Grey) |> littleBoxedChar)}
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
        saveGameStateLocalStorage state |> ignore

        let letterToDisplayBox =
            let getLetter (_, word) =
                let letter l =
                    let letterString = defaultArg l.Letter ""
                    letterString.ToUpper(), l.Status

                word.Letters |> List.map letter

            getLetter >> List.map boxedChar

        let onKeyClick (c: string) =
            Ev (fun ev ->
                ev.preventDefault ()

                let submitEntry =
                    match c with
                    | "Ent" -> submitEnter
                    | "Del" -> submitDelete
                    | _ -> submitLetter c

                state |> submitEntry |> setGameState)

        let onModalClick modalTyoe =
            Ev (fun ev ->
                ev.preventDefault ()
                Console.WriteLine("Info Clicked")

                match modalTyoe with
                | Info ->
                    { state with
                        ShowInfo =
                            if state.ShowInfo = true
                            then false
                            else true }
                | Stats ->
                    { state with
                        ShowStats =
                            if state.ShowStats = true
                            then false
                            else true }
                | Help ->
                    { state with
                        ShowHelp =
                            if state.ShowHelp = true
                            then false
                            else true }
                |> setGameState)

        let keyboardKey = keyboardChar state.UsedLetters onKeyClick
        let stats = sprintf "Won: %d, Lost: %d" state.GamesWon state.GamesLost

        let message =
            match state.State with
            | NotStarted
            | Started -> sprintf "Today's phonic hint is: %s" state.Hint
            | Won -> sprintf "Congratulations! %s" stats
            | Lost -> sprintf "It was %s. %s" state.Wordle stats
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
