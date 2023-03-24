module Common

open Domain
open Wordles

module DisplayUtils =
    let parseWordGrapheme grapheme word =

        let word = word |> Seq.toList
        let wordLength = word |> List.length

        let graphemes =
            let graphemeList = grapheme |> Seq.toList
            let graphemeLength = List.length graphemeList
            let foundIndex =
                List.windowed graphemeLength word
                |> List.tryFindIndex (fun w ->
                    match (w, graphemeList) with
                    | t, g when t = g -> true
                    | h :: m :: t, h1 :: m1 :: t1 when m1 = '-' && h = h1 && t = t1 -> true 
                    | _ -> false)
            match foundIndex with
            | Some index -> List.init graphemeLength (fun i -> i + index)
            | None -> []
        
        //assume all yellows, then populate the greens if any
        List.init wordLength (fun _ -> DarkGreen)
        |> List.mapi (fun i v ->
            if List.contains i graphemes
            then DarkRed
            else v)
        |> List.zip word 

module Letter =
    let toString guessLetter = defaultArg guessLetter.Letter ""

    let toUpper (letter: string) = letter.ToUpper()

    let unpack guessLetter =
        toString guessLetter |> toUpper, guessLetter.Status

    let toOption letter =
        if letter = "" then
            None
        else
            Some letter

module Guess =
    let guessToWord guess =
        guess.Letters
        |> List.map (Letter.toString >> Letter.toUpper)
        |> Seq.fold (+) ""

    let getLetter (_, guess) = guess.Letters |> List.map Letter.unpack

module List =
    let set list value pos =
        list
        |> List.mapi (fun i v -> if i = pos then value else v)

module Counter =
    let createCounter items =
        items
        |> Seq.filter (fun (a, g) -> a <> g)
        |> Seq.map fst
        |> Seq.countBy id
        |> Map.ofSeq

    let countOf counter item =
        match Map.tryFind item counter with
        | Some c -> c
        | None -> 0

    let updateCount counter item =
        match Map.tryFind item counter with
        | Some c -> Map.add item (c - 1) counter
        | None -> counter

module Validate =

    let letterPosition letters n = n >= 0 && n < letters

    let round rounds state =
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

module Score = 
    /// Calculated the answer mask for the guessed word, based on the current 'wordle'
    let getAnswerMask actualWord guessWord =
        let letters = Seq.zip actualWord guessWord

        let folder (count, mask) (actualLetter, guessLetter) =
            if actualLetter = guessLetter then
                count, Green :: mask
            elif Seq.contains guessLetter actualWord && Counter.countOf count guessLetter > 0 then
                Counter.updateCount count guessLetter, Yellow :: mask
            else
                count, Grey :: mask

        letters
        |> Seq.fold folder (Counter.createCounter letters, [])
        |> snd
        |> Seq.rev

    // Score the guessed word based on the wordle.
    let scoreGuess actualWord guessWord =
        { Letters =
            getAnswerMask actualWord guessWord
            |> Seq.zip guessWord
            |> Seq.toList
            |> List.map (fun (a, m) -> { Letter = Some(string a); Status = m }) }

module State = 
    // Updates the colour of the relevant keyboard keys based on the current guess
    // Note this may update a state set previously, i.e. Yellow goes to Green in the current round.
    let updateKeyboardState guesses (keyBoardState: Map<string, Status>) =
        guesses
        |> List.fold (fun (state: Map<string, Status>) guessLetter ->
            let letter = Letter.toString guessLetter
            let status = guessLetter.Status

            if status = Green
               || (status = Grey  && not <| state.ContainsKey letter)  
               || (status = Yellow && not <| state.ContainsKey letter) then
                state.Add(letter, status)
            else
                state)
            keyBoardState
    
    let updateState numberOfRounds state updateFunction =
        if Validate.round numberOfRounds state then
            let word = List.item state.Round state.Guesses
            let updatedWord = updateFunction word
            { state with Guesses = List.set state.Guesses updatedWord state.Round }
        else
            state

    // Action to be taken when a letter is submitted from the keyboard.
    let submitLetter numberOfRounds numberOfLetters letter state =
        // Function to update state based on entered letter and the current state
        let add letter (position, guessLetters) =
            if Validate.letterPosition numberOfLetters position then
                position + 1, { Letters = List.set guessLetters.Letters { Letter = Some letter; Status = Black } position }
            else
                position, { Letters = guessLetters.Letters }

        letter |> add |> updateState numberOfRounds state

    // Action to be taken when a letter is deleted via the keyboard (prior to submitting a guess word)
    let submitDelete numberOfRounds numberOfLetters state =
        let deleteLetter (position, guessLetters) =
            let deletePosition = position - 1

            if Validate.letterPosition numberOfLetters deletePosition then
                deletePosition, { Letters = List.set guessLetters.Letters { Letter = None; Status = Black } deletePosition }
            else
                position, { Letters = guessLetters.Letters }

        deleteLetter |> updateState numberOfRounds state

    // This function is called when 'enter' on the keyboard is clicked.
    // If the round is valid and all letters are valid (i.e. the word formed by the letters exists in the dictionary)
    // then the word for that row is submitted as a guess.
    let submitEnter numberOfRounds numberOfLetters state =
        let submitGuess ((position, guess): Position * Guess) =
            let guessWord = Guess.guessToWord guess
            let scoredGuess = guessWord |> Score.scoreGuess state.Wordle

            let updatedUsedLetters = updateKeyboardState scoredGuess.Letters state.UsedLetters

            let updatedGuess = (position, scoredGuess)

            let updatedState =
                if guessWord = state.Wordle then
                    Won
                elif state.Round = (numberOfRounds - 1) then
                    Lost
                else
                    Started

            updatedGuess, updatedState, updatedUsedLetters

        if Validate.round numberOfRounds state then
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

                // set all letters to invalid word.
                // this requires the user to delete them from final position
                let updated =
                    { invalidGuess with
                        Letters =
                            invalidGuess.Letters
                            |> List.map (fun ls -> { ls with Status = Invalid }) }

                { state with Guesses = List.set state.Guesses (numberOfLetters, updated) state.Round }
        else
            state

let keyBoard =
    { Top =
        [ "q"; "w"; "e"; "r"; "t"; "y"; "u"; "i"; "o"; "p" ]
      Middle =
        [ "a"; "s"; "d"; "f"; "g"; "h"; "j"; "k"; "l" ]
      Bottom =
        [ "Ent"; "z"; "x"; "c"; "v"; "b"; "n"; "m"; "Del" ] }
