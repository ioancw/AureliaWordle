/// View pieces: tiles, keys, messages and pop-ups. They produce the same markup as the
/// Fable (Lit) version, so both share main.css, including the animations.
module Aureliadle.Views

open Bolero.Html
open Domain
open GameRules
open Phonics
open Words

/// A board tile. Its classes start the flip/pop animations in main.css.
let tile position (letter: string, status) =
    let isValid = status <> Invalid

    let classes =
        [ match status with
          | Black -> "cell-black"
          | Grey -> "flipin-wrong cell-black"
          | Green -> "flipin-correct cell-black"
          | Yellow -> "flipin-present cell-black"
          | Invalid -> "jiggle cell-black"
          if isValid then $"cell-slow-{position + 1}" ]

    div {
        attr.``class`` ("tile " + String.concat " " classes)
        "data-letter" => letter
        div { text letter }
    }

let boardRow (_, guess: Guess) =
    div {
        attr.``class`` "board-row flex justify-center"
        forEach (Guess.getLetter ((), guess) |> List.indexed) (fun (i, l) -> tile i l)
    }

let private backspaceIcon =
    rawHtml
        """<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" class="h-6 w-6" fill="none" stroke="currentColor" stroke-width="2" aria-hidden="true"><path stroke-linecap="round" stroke-linejoin="round" d="M12 9.75 14.25 12m0 0 2.25 2.25M14.25 12l2.25-2.25M14.25 12 12 14.25m-2.58 4.92-6.374-6.375a1.125 1.125 0 0 1 0-1.59L9.42 4.83c.21-.211.497-.33.795-.33H19.5a2.25 2.25 0 0 1 2.25 2.25v10.5a2.25 2.25 0 0 1-2.25 2.25h-9.284c-.298 0-.585-.119-.795-.33Z" /></svg>"""

/// An on-screen key, coloured by what's known about its letter.
let key (usedLetters: Map<string, Status>) (onPress: string -> unit) (k: string) =
    let colour =
        match Map.tryFind (k.ToUpper()) usedLetters |> Option.defaultValue Black with
        | Yellow -> "cell-yellow"
        | Grey -> "cell-grey"
        | Green -> "cell-green"
        | _ -> "keyboard-grey"

    let size, label =
        match k with
        | "Ent" -> "key-other-size", "Enter"
        | "Del" -> "key-other-size", "Delete"
        | _ -> "key-size", k

    button {
        attr.``class`` $"keyboard {size} {colour}"
        attr.aria "label" label
        on.click (fun _ -> onPress k)

        match k with
        | "Ent" -> text "Enter"
        | "Del" -> backspaceIcon
        | _ -> text k
    }

/// Always rendered (hidden when there's no message): Blazor matches elements by their position,
/// so adding or removing an element here would re-create the board after it and replay its animations.
let toast (message: string option) =
    div {
        attr.``class`` (if message.IsSome then "toast" else "toast hidden")
        "role" => "status"
        text (message |> Option.defaultValue "")
    }

/// A small coloured letter box, used for spellings in the pop-ups.
let littleBox (c: char, colour) =
    let colourClasses =
        match colour with
        | HintBlack -> "cell-black"
        | DarkRed -> "bg-red-800 border-red-800"
        | DarkGreen -> "bg-green-700 border-green-700"
        | DarkYellow -> "bg-yellow-600 border-yellow-600"
        | HintInvalid -> "bg-neutral-400 border-neutral-400"

    div {
        attr.``class`` $"little-tile font-sans {colourClasses}"
        text (string c)
    }

/// The pop-up frame. extraClass is added to the outer element, e.g. to delay its appearance.
let modal extraClass (title: string) (isOpen: bool) (onClose: unit -> unit) (body: Bolero.Node) =
    div {
        attr.``class`` (
            "modal fixed inset-0 flex items-start justify-center outline-none overflow-x-hidden overflow-y-auto z-50 pt-10 px-3 "
            + (if isOpen then "" else "hidden ")
            + extraClass
        )

        div {
            attr.``class`` "modal-dialog pointer-events-none w-full max-w-sm"

            div {
                attr.``class`` "modal-content border-none shadow-lg relative flex flex-col w-full pointer-events-auto bg-neutral-400 bg-clip-padding rounded-md outline-none text-current max-h-screen-3/4 overflow-y-auto"

                div {
                    attr.``class`` "modal-header flex flex-shrink-0 items-center justify-between p-1 border-b border-stone-600 rounded-t-md"
                    h5 {
                        attr.``class`` "text-lg text-left font-medium leading-normal text-stone-800"
                        text title
                    }
                    button {
                        attr.``type`` "button"
                        attr.aria "label" "Close"
                        attr.``class`` "px-2 py-1 bg-stone-800 text-white font-bold text-xs leading-tight uppercase rounded shadow-md"
                        on.click (fun _ -> onClose ())
                        text "X"
                    }
                }

                body
            }
        }
    }

let aboutBody (highContrast: bool) (onToggleHighContrast: unit -> unit) =
    div {
        attr.``class`` "modal-body p-2 text-slate-800 space-y-3"
        p { text "This is a wordle type game to help children with their phonics." }
        p { text "For each wordle, a phonic hint is given as a phoneme (i.e. the sound)." }
        p {
            text "For example, if the word to be guessed is "
            span { attr.``class`` "text-green-700 font-bold"; text "SHACK" }
            text ", then the phoneme hint given is "
            span { attr.``class`` "text-red-800 font-bold"; text "/sh/" }
            text ". Not all phonemes in the word are provided, instead one of the phonemes is given in the hint."
        }
        p { text "Children can use their grapheme-phoneme correspondence knowledge in order to determine the appropriate grapheme (spelling) for the phoneme in question." }
        p { text "GPC examples for the phoneme hint can be seen by tapping the hint under the title, or the ? button." }
        p {
            text "This version is written in "
            a { attr.href "https://fsharp.org"; attr.``class`` "text-blue-700 underline"; text "F#" }
            text " with "
            a { attr.href "https://fsbolero.io"; attr.``class`` "text-blue-700 underline"; text "Bolero" }
            text ", running as WebAssembly."
        }
        div {
            attr.``class`` "border-t border-stone-600 pt-3"
            label {
                attr.``class`` "flex items-center justify-between gap-3 font-medium"
                span {
                    text "High contrast colours"
                    span {
                        attr.``class`` "block text-xs font-normal"
                        text "Orange and blue instead of green and yellow, for colour vision differences."
                    }
                }
                input {
                    attr.``type`` "checkbox"
                    attr.``class`` "h-5 w-5 shrink-0"
                    attr.``checked`` highContrast
                    on.change (fun _ -> onToggleHighContrast ())
                }
            }
        }
    }

let helpBody (state: State) =
    let hint = state.Phonics.Hint
    let examples = Map.tryFind hint phonemeGraphemeCorresspondances |> Option.defaultValue []
    let longest = examples |> List.map (fst >> String.length) |> List.fold max 0

    div {
        attr.``class`` "modal-body p-2 text-slate-800 text-center"
        p { text "Today's phonic hint is:" }
        div {
            attr.``class`` "flex justify-center mb-3"
            forEach hint (fun c -> littleBox (c, DarkYellow))
        }
        p { attr.``class`` "mb-3"; text "The graphemes corresponding to this phoneme:" }
        forEach examples (fun (grapheme, example) ->
            div {
                attr.``class`` "flex justify-left mb-1"
                forEach grapheme (fun g -> littleBox (g, DarkRed))
                forEach [ 0 .. longest - grapheme.Length ] (fun _ -> littleBox (' ', HintInvalid))
                forEach (parseWordGrapheme grapheme example) littleBox
            })
    }

let statsBody (state: State) (onShare: unit -> unit) =
    let stat (label: string) (value: string) =
        div {
            attr.``class`` "items-center justify-center text-center"
            div { attr.``class`` "text-3xl font-bold mr-2"; text value }
            div { attr.``class`` "text-xs mr-2"; text label }
        }

    let played = state.GamesWon + state.GamesLost
    let successRate = if played = 0 then 0 else int (round (100. * float state.GamesWon / float played))
    let mostWins = state.WinDistribution |> List.fold max 0

    div {
        attr.``class`` "modal-body p-2"
        div {
            attr.``class`` "flex items-center justify-center my-2 m-4"
            stat "Games Played" (string played)
            stat "Games Won" (string state.GamesWon)
            stat "Games Lost" (string state.GamesLost)
            stat "Success Rate" $"{successRate}%%"
        }
        h4 { attr.``class`` "flex text-lg justify-center items-center font-medium"; text "Guess Distribution" }
        div {
            attr.``class`` "columns-1 justify-left m-2 text-sm text-white"
            forEach (List.indexed state.WinDistribution) (fun (round, wins) ->
                // bars are relative to the most common result, with a minimum so the count stays readable
                let percent = if mostWins = 0 then 8. else max 8. (100. * float wins / float mostWins)

                div {
                    attr.``class`` "flex justify-left m-1"
                    div { attr.``class`` "items-center justify-center w-2"; text (string (round + 1)) }
                    div {
                        attr.``class`` "w-full ml-2"
                        div {
                            attr.``class`` "text-xs text-right font-medium p-0.5 pr-2 bg-pink-600"
                            attr.style $"width: {percent}%%"
                            text (string wins)
                        }
                    }
                })
        }
        // always rendered, for the same reason as the toast
        div {
            attr.``class`` (if state.State = Won || state.State = Lost then "flex justify-center my-3" else "hidden")
            button {
                attr.``type`` "button"
                attr.``class`` "share-button"
                on.click (fun _ -> onShare ())
                text "Share"
            }
        }
    }

let answerBody (state: State) =
    div {
        attr.``class`` "modal-body p-2 text-slate-800 text-center"
        p { text "Oh well, never mind." }
        div {
            attr.``class`` "flex justify-center my-2"
            forEach (parseWordGrapheme (state.Phonics.Grapheme.ToUpper()) state.Wordle) littleBox
        }
        p { text "Better luck next time." }
    }
