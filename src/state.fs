module State

open Domain
open Display
open Common 

let updateState state updateFunction =
    if Validate.round state then
        let word = List.item state.Round state.Guesses
        let updatedWord = updateFunction word
        { state with Guesses = List.set state.Guesses updatedWord state.Round }
    else
        state

// Action to be taken when a letter is submitted from the keyboard.
let submitLetter letter state =
    // Function to update state based on entered letter and the current state
    let add letter (position, guessLetters) =
        if Validate.letterPosition position then
            position + 1, { Letters = List.set guessLetters.Letters { Letter = Some letter; Status = Black } position }
        else
            position, { Letters = guessLetters.Letters }

    letter |> add |> updateState state

// Action to be taken when a letter is deleted via the keyboard (prior to submitting a guess word)
let submitDelete state =
    let deleteLetter (position, guessLetters) =
        let deletePosition = position - 1

        if Validate.letterPosition deletePosition then
            deletePosition, { Letters = List.set guessLetters.Letters { Letter = None; Status = Black } deletePosition }
        else
            position, { Letters = guessLetters.Letters }

    deleteLetter |> updateState state

// Updates the colour of the relevant keyboard keys based on the current guess
// Note this may update a state set previously, i.e. Yellow goes to Green in the current round.
let updateKeyboardState guesses (keyBoardState: Map<string, Status>) =
    (keyBoardState, guesses)
    ||> List.fold (fun state guessLetter ->
        let letter = Letter.toString guessLetter
        let status = guessLetter.Status

        if status = Green
           || (status = Grey  && not <| state.ContainsKey letter)  
           || (status = Yellow && not <| state.ContainsKey letter) then
            state.Add(letter, status)
        else
            state)

// This function is called when 'enter' on the keyboard is clicked.
// If the round is valid and all letters are valid (i.e. the word formed by the letters exists in the dictionary)
// then the word for that row is submitted as a guess.
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

            // set all letters to invalid word.
            // this requires the user to delete them from final position
            let updated =
                { invalidGuess with
                    Letters =
                        invalidGuess.Letters
                        |> List.map (fun ls -> { ls with Status = Invalid }) }

            { state with Guesses = List.set state.Guesses (letters, updated) state.Round }
    else
        state

