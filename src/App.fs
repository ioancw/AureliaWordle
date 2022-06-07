module Lit.Wordle

open System
open Lit
open Wordles
open Fable.Import
open Fable.Core

JsInterop.importSideEffects "./index.css"


type KeyBoard =
    { Top: string list
      Middle: string list
      Bottom: string list }

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
    { Guesses: (int * (string * string) []) []
      Solution: string
      Round: int
      State: string
      GamesWon: int
      GamesLost: int
      WinDistribution: int [] }

type LocalStorageGameStats =
    { GamesWon: int
      GamesLost: int
      WinDistribution: int list
    }

type Guess =
    { Letters: GuessLetter list }

    member t.AsWord() =
        let guess =
            t.Letters
            |> List.map (fun gl -> defaultArg gl.Letter "")
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
    if state.State = Lost || state.State = Won
    then false
    else (state.Round >= 0 && state.Round < rounds)

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
    { Top = [ "q"; "w"; "e"; "r"; "t"; "y"; "u"; "i"; "o"; "p" ]
      Middle = [ "a"; "s"; "d"; "f"; "g"; "h"; "j"; "k"; "l" ]
      Bottom = [ "Ent"; "z"; "x"; "c"; "v"; "b"; "n"; "m"; "Del" ] }

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

    let statusFromString guessStatus=
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
            |> List.map (fun gl -> defaultArg gl.Letter "", StateHelpers.statusToString gl.Status)
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
              WinDistribution = state.WinDistribution |> List.toArray}
    )

let loadGameStateLocalStorage () =
    Console.WriteLine("Now reading from local storoage")
    let localState = Browser.WebStorage.localStorage.getItem (gameStateKey)
    Console.WriteLine(sprintf "Local %A" localState)
    match localState with
    | null -> None
    | _ -> Some(localState |> JS.JSON.parse :?> LocalStorageGameState)

let join p q =
    Map(
        Seq.concat [ (Map.toSeq p)
                     (Map.toSeq q) ]
    )

let getUsedLetters letterGuesses (state: Map<string, Status>) =
    letterGuesses
    |> List.map (fun gl -> defaultArg gl.Letter "", gl.Status)
    |> List.filter (fun (l, s) ->
        s = Green
        || (s = Grey && not <| state.ContainsKey l)
        || (s = Yellow && not <| state.ContainsKey l))
    |> Map.ofList
    |> join state

let startNewGame =
    Console.WriteLine("Starting a new game")
    let loadedStorage = loadGameStateLocalStorage ()
    // load the statistics at the start of a game.
    let guesses = emptyGuesses

    let wordle () : string * string=
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
    | Some stored when
        fst todaysWordle = stored.Solution //we know that it's a previously saved game for today
        && StateHelpers.stateFromString stored <> NotStarted
        ->
        let localGuesses =
            stored.Guesses
            |> Array.map (fun (position, letters) ->
                position,
                { Letters =
                    letters
                    |> Array.toList
                    |> List.map (fun (guessLetter, guessStatus) ->
                        let letterOption =
                            if guessLetter = ""
                            then None
                            else Some guessLetter

                        { Letter = letterOption
                          Status = StateHelpers.statusFromString guessStatus}) })
            |> Array.toList

        //fold local guesses in order to colour the keyboard.
        let storageUsedLetters =
            localGuesses
            |> List.map snd
            |> List.fold (fun state guess -> getUsedLetters guess.Letters state) Map.empty

        { Wordle = fst todaysWordle
          Hint = snd todaysWordle
          Guesses = localGuesses
          Round = stored.Round
          State = StateHelpers.stateFromString stored
          UsedLetters = storageUsedLetters
          GamesLost = stored.GamesLost
          GamesWon = stored.GamesWon
          WinDistribution = stored.WinDistribution |> List.ofArray }
    | _ ->
        //the case where we haven't played before
        { Wordle = fst todaysWordle
          Hint = snd todaysWordle
          Guesses = guesses
          Round = 0
          State = NotStarted
          UsedLetters = Map.empty
          GamesLost = 0
          GamesWon = 0
          WinDistribution = List.init rounds (fun _ -> 0) }

// this can be improved quite a bit - need tests though
let getAnswerMask (actual: string) (guess: string) =
    let removeFirstInstance remove fromList =
        let rec removeFirst predicate =
            function
            | [] -> []
            | h :: t when predicate h -> t //terminates
            | h :: t -> h :: removeFirst predicate t

        removeFirst (fun i -> i = remove) fromList

    let getCounts letters matchOn =
        letters
        |> List.filter (fun i -> i = matchOn)
        |> List.length

    let rec masker ls count mask =
        match (ls, count) with
        | [], _ -> mask
        | (a, g) :: t, cs ->
            if a = g then
                masker t cs (Green :: mask)
            else if Seq.contains g actual && getCounts cs g > 0 then
                masker t (removeFirstInstance g cs) (Yellow :: mask)
            else
                masker t cs (Grey :: mask)

    let notMatched zipped =
        zipped
        |> List.filter (fun (a, g) -> a <> g)
        |> List.map fst

    let letters = Seq.zip actual guess |> Seq.toList
    let masked = masker letters (notMatched letters) [] |> List.rev

    { Letters =
        Seq.zip guess masked
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
                    if updatedState = Won || updatedState = Lost
                    then state.Round
                    else state.Round + 1
                GamesWon =
                    if updatedState = Won
                    then state.GamesWon + 1
                    else state.GamesWon
                GamesLost =
                    if updatedState = Lost
                    then state.GamesLost + 1
                    else state.GamesLost
                WinDistribution =
                    if updatedState = Won
                    then listSet state.WinDistribution (winDistribution + 1) state.Round
                    else state.WinDistribution            }
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
        | "Del"| "Ent" -> "w-12"
        | _ -> "w-9"
    html
        $"""
        <button
            @click={handler c}
                class="flex items-center justify-center rounded mx-0.5 {width} h-14 {colour} uppercase text-white"
        >{c}</button>
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

                state |> submitEntry |> setGameState
            )

        let keyboardKey = keyboardChar state.UsedLetters onKeyClick
        let stats = sprintf "Won: %d, Lost: %d" state.GamesWon state.GamesLost
        let message =
            match state.State with
            | NotStarted | Started ->
                sprintf "Today's phonic hint is: %s" state.Hint
            | Won ->
                sprintf "Congratulations! %s" stats
            | Lost ->
                sprintf "Today's wordle was %s: %s" state.Wordle stats

        html
            $"""
            <div class="min-h-screen space-y-3 bg-stone-900">
                <div class="mb-2">
                    <div class="flex items-center justify-between h-12 px-5">
                        <svg xmlns="http://www.w3.org/2000/svg" class="h-7 w-7 text-white" fill="none" viewBox="0 0 24 24" stroke="currentColor" stroke-width="2">
                        <path stroke-linecap="round" stroke-linejoin="round" d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
                        </svg>
                        <p class="ml-2.5 justify-center font-mono text-3xl text-white">Aurelia-dle</p>
                        <div class="flex">
                            <svg xmlns="http://www.w3.org/2000/svg" class="h-7 w-7 mr-3 text-white"  fill="none" viewBox="0 0 24 24" stroke="currentColor" stroke-width="2">
                                <path stroke-linecap="round" stroke-linejoin="round" d="M10.325 4.317c.426-1.756 2.924-1.756 3.35 0a1.724 1.724 0 002.573 1.066c1.543-.94 3.31.826 2.37 2.37a1.724 1.724 0 001.065 2.572c1.756.426 1.756 2.924 0 3.35a1.724 1.724 0 00-1.066 2.573c.94 1.543-.826 3.31-2.37 2.37a1.724 1.724 0 00-2.572 1.065c-.426 1.756-2.924 1.756-3.35 0a1.724 1.724 0 00-2.573-1.066c-1.543.94-3.31-.826-2.37-2.37a1.724 1.724 0 00-1.065-2.572c-1.756-.426-1.756-2.924 0-3.35a1.724 1.724 0 001.066-2.573c-.94-1.543.826-3.31 2.37-2.37.996.608 2.296.07 2.572-1.065z" />
                                <path stroke-linecap="round" stroke-linejoin="round" d="M15 12a3 3 0 11-6 0 3 3 0 016 0z" />
                            </svg>
                            <svg xmlns="http://www.w3.org/2000/svg" class="h-7 w-7 text-white" viewBox="0 0 20 20" fill="currentColor">
                                <path d="M2 11a1 1 0 011-1h2a1 1 0 011 1v5a1 1 0 01-1 1H3a1 1 0 01-1-1v-5zM8 7a1 1 0 011-1h2a1 1 0 011 1v9a1 1 0 01-1 1H9a1 1 0 01-1-1V7zM14 4a1 1 0 011-1h2a1 1 0 011 1v12a1 1 0 01-1 1h-2a1 1 0 01-1-1V4z" />
                            </svg>
                        </div>
                    </div>
                    <hr></hr>
                </div>
                <div class="flex justify-center font-mono text-white">
                    {message}
                </div>
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
    | NotStarted ->
        gameState |> writeState
    | Started ->
        gameState |> writeState
    | Won ->
        gameState
        |> writeState
    | Lost ->
        gameState |> writeState
