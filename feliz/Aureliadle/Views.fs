/// Aureliadle's board, keyboard and pop-up contents, in Feliz. The markup matches the live
/// version, so it shares main.css and its animations.
module Aureliadle.Views

open Feliz
open Domain
open GameRules
open Phonics
open Words
open Aureliadle.Rules

let private keyRows =
    [ [ "q"; "w"; "e"; "r"; "t"; "y"; "u"; "i"; "o"; "p" ]
      [ "a"; "s"; "d"; "f"; "g"; "h"; "j"; "k"; "l" ]
      [ "Ent"; "z"; "x"; "c"; "v"; "b"; "n"; "m"; "Del" ] ]

let private winMessages = [| "Genius!"; "Magnificent!"; "Impressive!"; "Splendid!"; "Great!"; "Phew!" |]

/// A board tile. Its classes start the flip/pop animations in main.css.
let private tile (position: int) (letter: string, status) =
    Html.div [
        prop.key position
        prop.className [
            "tile"
            match status with
            | Black -> "cell-black"
            | Grey -> "flipin-wrong cell-black"
            | Green -> "flipin-correct cell-black"
            | Yellow -> "flipin-present cell-black"
            | Invalid -> "jiggle cell-black"
            if status <> Invalid then $"cell-slow-{position + 1}"
        ]
        prop.custom ("data-letter", letter)
        prop.children [ Html.div letter ]
    ]

let board (state: State) =
    React.Fragment [
        for i, guess in List.indexed state.Guesses ->
            Html.div [
                prop.key i
                prop.className "board-row flex justify-center"
                prop.children (Guess.getLetter guess |> List.mapi tile)
            ]
    ]

let private backspaceIcon =
    """<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" class="h-6 w-6" fill="none" stroke="currentColor" stroke-width="2" aria-hidden="true"><path stroke-linecap="round" stroke-linejoin="round" d="M12 9.75 14.25 12m0 0 2.25 2.25M14.25 12l2.25-2.25M14.25 12 12 14.25m-2.58 4.92-6.374-6.375a1.125 1.125 0 0 1 0-1.59L9.42 4.83c.21-.211.497-.33.795-.33H19.5a2.25 2.25 0 0 1 2.25 2.25v10.5a2.25 2.25 0 0 1-2.25 2.25h-9.284c-.298 0-.585-.119-.795-.33Z" /></svg>"""

/// An on-screen key, coloured by what's known about its letter.
let private key (usedLetters: Map<string, Status>) send (k: string) =
    let colour =
        match Map.tryFind (k.ToUpper()) usedLetters |> Option.defaultValue Black with
        | Yellow -> "cell-yellow"
        | Grey -> "cell-grey"
        | Green -> "cell-green"
        | _ -> "keyboard-grey"

    let size, label, input =
        match k with
        | "Ent" -> "key-other-size", "Enter", Enter
        | "Del" -> "key-other-size", "Delete", Delete
        | _ -> "key-size", k, Letter k

    Html.button [
        prop.key k
        prop.className [ "keyboard"; size; colour ]
        prop.ariaLabel label
        prop.onClick (fun _ -> send input)
        match k with
        | "Ent" -> prop.text "Enter"
        | "Del" -> prop.children [ Html.span [ prop.dangerouslySetInnerHTML backspaceIcon ] ]
        | _ -> prop.text k
    ]

let keyboard (state: State) send =
    let row extra (keys: string list) =
        Html.div [
            prop.className "keyboard-row"
            prop.children [
                if extra then Html.div [ prop.className "keyboard-spacer" ]
                yield! keys |> List.map (key state.UsedLetters send)
                if extra then Html.div [ prop.className "keyboard-spacer" ]
            ]
        ]

    React.Fragment [
        row false keyRows.[0]
        row true keyRows.[1]
        row false keyRows.[2]
    ]

let hintBar (state: State) openHelp =
    Html.div [
        prop.className "hint-bar"
        prop.children [
            Html.button [
                prop.className "hint-button"
                prop.ariaLabel $"Today's sound: {state.Phonics.Hint}. Show spellings"
                prop.onClick (fun _ -> openHelp ())
                prop.children [
                    Html.span [ prop.className "hint-label"; prop.text "Today's sound" ]
                    Html.span [ prop.className "hint-sound"; prop.text state.Phonics.Hint ]
                ]
            ]
        ]
    ]

/// A small coloured letter box, used for spellings in the pop-ups.
let private littleBox (i: int) (c: char, colour) =
    Html.div [
        prop.key i
        prop.className [
            "little-tile font-sans"
            match colour with
            | HintBlack -> "cell-black"
            | DarkRed -> "bg-red-800 border-red-800"
            | DarkGreen -> "bg-green-700 border-green-700"
            | DarkYellow -> "bg-yellow-600 border-yellow-600"
            | HintInvalid -> "bg-neutral-400 border-neutral-400"
        ]
        prop.text (string c)
    ]

let help (state: State) =
    let hint = state.Phonics.Hint
    let examples = Map.tryFind hint phonemeGraphemeCorresspondances |> Option.defaultValue []
    let longest = examples |> List.map (fst >> String.length) |> List.fold max 0

    Html.div [
        prop.className "modal-body p-2 text-slate-800 text-center"
        prop.children [
            yield Html.p "Today's phonic hint is:"
            yield Html.div [
                prop.className "flex justify-center mb-3"
                prop.children (hint |> Seq.mapi (fun i c -> littleBox i (c, DarkYellow)) |> List.ofSeq)
            ]
            yield Html.p [ prop.className "mb-3"; prop.text "The graphemes corresponding to this phoneme:" ]
            for grapheme, example in examples ->
                Html.div [
                    prop.key grapheme
                    prop.className "flex justify-left mb-1"
                    prop.children [
                        let padded =
                            [ for g in grapheme -> g, DarkRed ]
                            @ List.replicate (longest - grapheme.Length + 1) (' ', HintInvalid)

                        yield! padded |> List.mapi littleBox
                        yield! parseWordGrapheme grapheme example |> List.mapi (fun i l -> littleBox (100 + i) l)
                    ]
                ]
        ]
    ]

let about =
    Html.div [
        prop.className "modal-body p-2 text-slate-800 space-y-3"
        prop.children [
            Html.p "This is a wordle type game to help children with their phonics."
            Html.p "For each wordle, a phonic hint is given as a phoneme (i.e. the sound)."
            Html.p [
                Html.text "For example, if the word to be guessed is "
                Html.span [ prop.className "text-green-700 font-bold"; prop.text "SHACK" ]
                Html.text ", then the phoneme hint given is "
                Html.span [ prop.className "text-red-800 font-bold"; prop.text "/sh/" ]
                Html.text ". Not all phonemes in the word are provided, instead one of the phonemes is given in the hint."
            ]
            Html.p "Children can use their grapheme-phoneme correspondence knowledge in order to determine the appropriate grapheme (spelling) for the phoneme in question."
            Html.p "GPC examples for the phoneme hint can be seen by tapping the hint under the title, or the ? button."
            Html.p [
                Html.text "This version is written in "
                Html.a [ prop.href "https://fsharp.org"; prop.className "text-blue-700 underline"; prop.text "F#" ]
                Html.text " with Fable and "
                Html.a [ prop.href "https://zaid-ajaj.github.io/Feliz/"; prop.className "text-blue-700 underline"; prop.text "Feliz" ]
                Html.text "."
            ]
        ]
    ]

let answer (state: State) =
    Html.div [
        prop.className "modal-body p-2 text-slate-800 text-center"
        prop.children [
            Html.p "Oh well, never mind."
            Html.div [
                prop.className "flex justify-center my-2"
                prop.children (parseWordGrapheme (state.Phonics.Grapheme.ToUpper()) state.Wordle |> List.mapi littleBox)
            ]
            Html.p "Better luck next time."
        ]
    ]

let keyToInput (key: string) =
    match key with
    | "Enter" -> Some Enter
    | "Backspace" -> Some Delete
    | k when k.Length = 1 && System.Char.IsLetter k.[0] && k.ToLower() >= "a" && k.ToLower() <= "z" -> Some(Letter(k.ToLower()))
    | _ -> None

let gameView: Shell.GameView<State, Input> =
    { Board = board
      Controls = keyboard
      SubHeader = hintBar
      HelpTitle = "Grapheme Phoneme Correspondence"
      Help = help
      About = about
      Answer = answer
      KeyToInput = keyToInput
      // once the winning row has finished flipping and bouncing (see main.css)
      CelebrateAfterMs = 2300
      WinMessage = fun attempts -> winMessages.[attempts - 1] }
