/// Small view pieces: tiles, keyboard keys and messages.
module Components

open Lit
open Domain

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

/// Creates a game tile that displays the guessed word's status.
let gameTile position (c, status) =
    let isValid = not <| (status = Status.Invalid)
    // sets css based on the status of the tiled character
    let classes =
        Lit.classes
            [
                "cell-black", status = Black
                "flipin-wrong cell-black", status = Grey
                "flipin-correct cell-black", status = Green
                "flipin-present cell-black", status = Yellow
                "jiggle cell-black", status = Invalid
                "cell-slow-1", position = 0 && isValid
                "cell-slow-2", position = 1 && isValid
                "cell-slow-3", position = 2 && isValid
                "cell-slow-4", position = 3 && isValid
                "cell-slow-5", position = 4 && isValid
            ]

    html
        $"""
        <div class="tile {classes}" data-letter="{c}">
            <div>
                {c}
            </div>
        </div>
    """
/// Creates a keyboard character and sets it's colour depending on the guessed letter status
let keyboardChar usedLetters handler (c: string) =
    let colour =
        let letterStatus =
            match Map.tryFind (c.ToUpper()) usedLetters with
            | Some x -> x
            | None -> Black

        match letterStatus with
        | Black -> "keyboard-grey" // see css for these definitions 
        | Yellow -> "cell-yellow"
        | Grey -> "cell-grey"
        | Green -> "cell-green"
        | _ -> "bg-gray-400"

    let width, label, ariaLabel =
        match c with
        | "Ent" -> "key-other-size", html $"Enter", "Enter"
        | "Del" ->
            "key-other-size",
            html
                $"""
                <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" class="h-6 w-6" fill="none" stroke="currentColor" stroke-width="2" aria-hidden="true">
                    <path stroke-linecap="round" stroke-linejoin="round" d="M12 9.75 14.25 12m0 0 2.25 2.25M14.25 12l2.25-2.25M14.25 12 12 14.25m-2.58 4.92-6.374-6.375a1.125 1.125 0 0 1 0-1.59L9.42 4.83c.21-.211.497-.33.795-.33H19.5a2.25 2.25 0 0 1 2.25 2.25v10.5a2.25 2.25 0 0 1-2.25 2.25h-9.284c-.298 0-.585-.119-.795-.33Z" />
                </svg>
            """,
            "Delete"
        | _ -> "key-size", html $"{c}", c

    html
        $"""
        <button @click={handler c} class="keyboard {width} {colour}" aria-label={ariaLabel}>
            {label}
        </button>
    """

/// A short message shown over the board, e.g. "Not in word list".
let toastView (message: string option) =
    match message with
    | Some m ->
        html
            $"""
            <div class="toast" role="status">{m}</div>
        """
    | None -> Lit.nothing

/// Creates a mini boxed character to be used when displaying graphemes and phonemes. 
let littleBoxedChar (c, status) =
    let colourBorder =
        match status with
        | HintBlack -> "cell-black"
        | DarkRed -> "bg-red-800 border-red-800"
        | DarkGreen -> "bg-green-700 border-green-700"
        | DarkYellow -> "bg-yellow-600 border-yellow-600"
        | HintInvalid -> "bg-neutral-400 border-neutral-400"

    html
        $"""
        <div class="little-tile font-sans {colourBorder}">{c}</div>
    """    
