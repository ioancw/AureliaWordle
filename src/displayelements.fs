module Display

open Lit
open Domain

/// Creates a game tile that displays the guessed word's status.
let gameTile position (c, status) =
    let isValid = not <| (status = Status.Invalid)
    // sets css based on the status of the tiled character
    let classes =
        Lit.classes
            [
                "cell-black", status = Black
                "cell-reveal.absent cell-grey", status = Grey
                "cell-green", status = Green
                "cell-yellow", status = Yellow
                "jiggle cell-black", status = Invalid
                "cell-slow-1", position = 0 && isValid
                "cell-slow-2", position = 1 && isValid
                "cell-slow-3", position = 2 && isValid
                "cell-slow-4", position = 3 && isValid
                "cell-slow-5", position = 4 && isValid
            ]

    html
        $"""
        <div class="tile {classes}">
            <div">
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
        | Black -> "bg-neutral-500" // see css for these definitions 
        | Yellow -> "cell-yellow"
        | Grey -> "cell-grey"
        | Green -> "cell-green"
        | _ -> "bg-gray-400"

    let width =
        match c with
        | "Del"
        | "Ent" -> "key-other-size"
        | _ -> "key-size"

    html
        $"""
        <button @click={handler c} class="keyboard {width} {colour}">
            {c}
        </button>
    """
    
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
        <div class="little-tile font-sans {colourBorder}">{c}</button>
    """    
