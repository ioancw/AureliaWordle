/// Numberdle's board, keypad and pop-up contents.
module Numberdle.Views

open Feliz
open Numberdle.Rules

let private warmthClass warmth =
    match warmth with
    | Hot -> "nd-hot"
    | Warm -> "nd-warm"
    | Cold -> "nd-cold"

let private row (i: int) (content: ReactElement list) (extraClass: string) =
    Html.div [
        prop.key i
        prop.className [ "nd-row"; extraClass ]
        prop.children content
    ]

let board (state: State) =
    let guessed =
        state.Guesses
        |> List.mapi (fun i g ->
            let hint, label =
                match g.Direction with
                | Correct -> "✓", "Correct!"
                | Higher -> "↑", "Higher"
                | Lower -> "↓", "Lower"

            row i [
                Html.div [ prop.className "nd-number"; prop.text g.Value ]
                Html.div [
                    prop.className [ "nd-hint"; if g.Direction = Correct then "nd-correct" else warmthClass g.Warmth ]
                    prop.ariaLabel label
                    prop.children [
                        Html.span [ prop.className "nd-arrow"; prop.text hint ]
                        Html.span [
                            prop.className "nd-word"
                            prop.text (
                                match g.Direction, g.Warmth with
                                | Correct, _ -> "Got it!"
                                | _, Hot -> "Hot"
                                | _, Warm -> "Warm"
                                | _, Cold -> "Cold"
                            )
                        ]
                    ]
                ]
            ] "nd-guessed")

    let playing = (outcome state).IsNone
    let current = state.Guesses.Length

    let rest =
        [ for i in current .. attempts - 1 ->
              if i = current && playing then
                  row i [ Html.div [ prop.className "nd-number nd-entry"; prop.text state.Entry ]; Html.div [ prop.className "nd-hint nd-empty" ] ] "nd-current"
              else
                  row i [ Html.div [ prop.className "nd-number nd-empty" ]; Html.div [ prop.className "nd-hint nd-empty" ] ] "" ]

    Html.div [ prop.className "nd-board"; prop.children (guessed @ rest) ]

let private key (label: string) (aria: string) wide send input =
    Html.button [
        prop.key aria
        prop.className [ "keyboard keyboard-grey"; (if wide then "key-other-size" else "key-size") ]
        prop.ariaLabel aria
        prop.onClick (fun _ -> send input)
        prop.text label
    ]

let keypad (_: State) send =
    let digit d = key (string d) (string d) false send (Digit d)

    React.fragment [
        Html.div [ prop.className "keyboard-row"; prop.children [ for d in 1..5 -> digit d ] ]
        Html.div [ prop.className "keyboard-row"; prop.children [ for d in [ 6; 7; 8; 9; 0 ] -> digit d ] ]
        Html.div [
            prop.className "keyboard-row"
            prop.children [ key "Delete" "Delete" true send Delete; key "Enter" "Enter" true send Enter ]
        ]
    ]

let banner (_: State) openHelp =
    Html.div [
        prop.className "hint-bar"
        prop.children [
            Html.button [
                prop.className "hint-button"
                prop.onClick (fun _ -> openHelp ())
                prop.children [
                    Html.span [ prop.className "hint-label"; prop.text "Guess the number" ]
                    Html.span [ prop.className "hint-sound"; prop.text $"1 – {highest}" ]
                ]
            ]
        ]
    ]

let help (_: State) =
    Html.div [
        prop.className "modal-body p-2 text-slate-800 space-y-3"
        prop.children [
            Html.p $"Guess today's number from 1 to {highest}. You have {attempts} tries."
            Html.p "After each guess you'll see whether the number is higher or lower, and how close you are:"
            Html.ul [
                prop.className "space-y-1"
                prop.children [
                    Html.li [ Html.span [ prop.className "nd-chip nd-hot"; prop.text "Hot" ]; Html.text " within 5" ]
                    Html.li [ Html.span [ prop.className "nd-chip nd-warm"; prop.text "Warm" ]; Html.text " within 15" ]
                    Html.li [ Html.span [ prop.className "nd-chip nd-cold"; prop.text "Cold" ]; Html.text " further away" ]
                ]
            ]
            Html.p "Tip: start in the middle, at 50, and halve the gap each time."
        ]
    ]

let about =
    Html.div [
        prop.className "modal-body p-2 text-slate-800 space-y-3"
        prop.children [
            Html.p "Numberdle is a number-of-the-day game for practising place value and halving."
            Html.p "It's built on the same F# daily-game engine as Aureliadle: only the rules, board and keypad are its own."
        ]
    ]

let answer (state: State) =
    Html.div [
        prop.className "modal-body p-2 text-slate-800 text-center"
        prop.children [
            Html.p "Oh well, never mind. Today's number was"
            Html.div [ prop.className "text-4xl font-bold my-2"; prop.text state.Target ]
            Html.p "Better luck next time."
        ]
    ]

let keyToInput (key: string) =
    match key with
    | "Enter" -> Some Enter
    | "Backspace" -> Some Delete
    | k when k.Length = 1 && k.[0] >= '0' && k.[0] <= '9' -> Some(Digit(int k.[0] - int '0'))
    | _ -> None

let private winMessages = [| "Genius!"; "Amazing!"; "Brilliant!"; "Great!"; "Well done!"; "Good!"; "Phew!" |]

let gameView: Shell.GameView<State, Input> =
    { Board = board
      Controls = keypad
      SubHeader = banner
      HelpTitle = "How to play"
      Help = help
      About = about
      Answer = answer
      KeyToInput = keyToInput
      CelebrateAfterMs = 500
      WinMessage = fun attempts -> winMessages.[attempts - 1] }
