/// Silent Letters' board (progress, the word card with its picture, clue and gap, and the
/// last word with its rule), letter buttons and pop-ups.
module SilentLetters.Views

open Fable.Core
open Feliz
open SilentLetters.Rules

/// Says a word with the browser's speech voice, where there is one.
[<Emit("(window.speechSynthesis && (window.speechSynthesis.cancel(), window.speechSynthesis.speak(Object.assign(new SpeechSynthesisUtterance($0), { lang: 'en-GB', rate: 0.8 }))))")>]
let private speak (word: string) : unit = jsNative

let private sayButton (word: string) =
    Html.button [
        prop.className "sl-say"
        prop.ariaLabel "Say the word"
        prop.title "Hear the word"
        prop.onClick (fun e ->
            e.stopPropagation ()
            speak word)
        prop.text "🔊"
    ]

/// The word as letter tiles: the gap shows ?, or the silent letter, greyed, once it's done.
let private tiles (w: Word) (filled: bool) (small: bool) =
    Html.div [
        prop.className [ "sl-tiles"; if small then "sl-tiles-small" ]
        prop.ariaLabel (if filled then w.Text else w.Text.Substring(0, w.Gap) + " gap " + w.Text.Substring(w.Gap + 1))
        prop.children [
            for i, c in Seq.indexed w.Text ->
                let isGap = i = w.Gap

                Html.span [
                    prop.key i
                    prop.className [
                        "sl-tile"
                        if isGap && filled then "sl-silent"
                        elif isGap then "sl-gap"
                    ]
                    prop.text (if isGap && not filled then "?" else string c)
                ]
        ]
    ]

let private progress (state: State) =
    Html.div [
        prop.className "sl-status"
        prop.children [
            Html.div [
                prop.className "sl-dots"
                prop.ariaLabel $"{state.Current} of {state.Words.Length} done"
                prop.children [
                    for i in 0 .. state.Words.Length - 1 ->
                        Html.span [
                            prop.key i
                            prop.className [
                                "sl-dot"
                                if i < state.Current then
                                    (if List.contains i state.Missed then "sl-dot-retry" else "sl-dot-done")
                                elif i = state.Current then "sl-dot-current"
                            ]
                        ]
                ]
            ]
            Html.div [
                prop.className "sl-hearts"
                prop.ariaLabel $"{max 0 (hearts - state.Mistakes)} hearts left"
                prop.text (String.replicate (max 0 (hearts - state.Mistakes)) "❤️" + String.replicate (min hearts state.Mistakes) "🤍")
            ]
        ]
    ]

let private levelName level =
    match level with
    | 1 -> "Warm-up"
    | 2 -> "Tricky"
    | _ -> "Silent letter challenge"

/// A finished word, its silent letter greyed, with the rule.
let private doneWord (w: Word) (key: string) (extra: string) =
    Html.div [
        prop.key key
        prop.className [ "sl-last"; extra ]
        prop.children [
            Html.div [
                prop.className "sl-last-word"
                prop.children [
                    Html.span [ prop.className "sl-last-emoji"; prop.text w.Emoji ]
                    tiles w true true
                    sayButton w.Text
                ]
            ]
            Html.p [ prop.className "sl-tip"; prop.text ("🤫 " + w.Tip) ]
        ]
    ]

let board (state: State) =
    Html.div [
        prop.className "sl-board"
        prop.children [
            yield progress state
            match currentWord state, outcome state with
            | Some w, None ->
                yield Html.div [
                    prop.key state.Current
                    prop.className "sl-card"
                    prop.children [
                        Html.div [
                            prop.className "sl-level"
                            prop.ariaLabel $"Level {w.Level} of {levels}"
                            prop.children [
                                Html.span [ prop.className "sl-stars"; prop.text (String.replicate w.Level "⭐") ]
                                Html.span (levelName w.Level)
                                if isPractice state then
                                    Html.span [
                                        prop.className "sl-practice"
                                        prop.title "You got this one wrong before: have another go"
                                        prop.text "🔁 Practice"
                                    ]
                            ]
                        ]
                        Html.div [ prop.className "sl-emoji"; prop.text w.Emoji ]
                        Html.p [ prop.className "sl-clue"; prop.text w.Clue ]
                        Html.div [
                            prop.className "sl-word-row"
                            prop.children [ tiles w false false; sayButton w.Text ]
                        ]
                        if not state.Wrong.IsEmpty then
                            Html.p [
                                prop.className "sl-wrong"
                                prop.text (
                                    "Not "
                                    + (state.Wrong |> List.map (fun l -> w.Text.Substring(0, w.Gap) + l + w.Text.Substring(w.Gap + 1)) |> String.concat " or ")
                                    + ". Which letter do you write but not say?"
                                )
                            ]
                    ]
                ]

                // the word just finished, with its rule
                if state.Current > 0 then
                    yield doneWord bank.[state.Words.[state.Current - 1]] $"last-{state.Current}" ""
            | _ ->
                // finished: the day's words with their rules
                yield Html.div [
                    prop.className "sl-review"
                    prop.children [
                        for i, n in List.indexed state.Words ->
                            doneWord bank.[n] (string i) (if i < state.Current then "" else "sl-not-reached")
                    ]
                ]
        ]
    ]

let letters (state: State) send =
    match currentWord state, outcome state with
    | Some w, None ->
        Html.div [
            prop.className "sl-choices"
            prop.children [
                for i, letter in List.indexed w.Choices ->
                    let tried = List.contains letter state.Wrong

                    Html.button [
                        prop.key letter
                        prop.className [ "sl-choice"; if tried then "sl-tried" ]
                        prop.disabled tried
                        prop.ariaLabel $"{i + 1}: {letter}"
                        prop.onClick (fun _ -> send (Pick letter))
                        prop.text letter
                    ]
            ]
        ]
    | _ -> Html.div [ prop.className "sl-choices sl-finished"; prop.text "Shh! See you tomorrow." ]

let banner (_: State) openHelp =
    Html.div [
        prop.className "hint-bar"
        prop.children [
            Html.button [
                prop.className "hint-button"
                prop.onClick (fun _ -> openHelp ())
                prop.children [
                    Html.span [ prop.className "hint-label"; prop.text "Find the silent letter" ]
                    Html.span [ prop.className "hint-sound"; prop.text "Help" ]
                ]
            ]
        ]
    ]

let help (state: State) =
    Html.div [
        prop.className "modal-body p-2 text-slate-800 space-y-3"
        prop.children [
            Html.p "Some words have a letter you write but don't say, like the k in knife. They're called silent letters."
            Html.p $"Each day there are {perDay} words. Look at the picture and the clue, then pick the silent letter that fills the gap. Tap 🔊 to hear the word."
            Html.p "They get harder as you go: two warm-ups ⭐, two tricky ones ⭐⭐, then two silent letter challenges ⭐⭐⭐."
            Html.p $"You have {hearts} hearts. A wrong letter costs a heart, and you try again."
            if not state.Practice.IsEmpty then
                Html.div [
                    Html.p [ prop.className "font-medium"; prop.text "Words you're practising:" ]
                    Html.p (state.Practice |> List.map (fun i -> bank.[i].Text) |> String.concat ", ")
                    Html.p [
                        prop.className "text-sm"
                        prop.text "Words you get wrong come back on another day, marked 🔁. Spell one right first time and it's done."
                    ]
                ]
            Html.p [ prop.className "text-sm"; prop.text "Keyboard: type the letter, or press 1, 2 or 3." ]
        ]
    ]

let about =
    Html.div [
        prop.className "modal-body p-2 text-slate-800 space-y-3"
        prop.children [
            Html.p "Silent Letters practises spelling words with letters you write but don't say: kn, wr, mb, gn, walk and half, listen and castle, autumn, scissors, guitar and more."
            Html.p "It's built on the same F# daily-game engine as Aureliadle, Which Witch?, Numberdle and Fractions."
        ]
    ]

let answer (state: State) =
    Html.div [
        prop.className "modal-body p-2 text-slate-800"
        prop.children [
            yield Html.p [ prop.className "text-center mb-2"; prop.text "Out of hearts! Here are today's words:" ]
            for i, n in List.indexed state.Words ->
                let w = bank.[n]
                Html.p [
                    prop.key i
                    prop.children [
                        Html.text (w.Emoji + " ")
                        Html.text (w.Text.Substring(0, w.Gap))
                        Html.span [ prop.className "sl-answer-letter"; prop.text (string w.Text.[w.Gap]) ]
                        Html.text (w.Text.Substring(w.Gap + 1))
                    ]
                ]
        ]
    ]

let keyToInput (key: string) =
    match key with
    | "1" | "2" | "3" -> Some(Choose(int key - 1))
    | k when k.Length = 1 && System.Char.IsLetter k.[0] -> Some(Pick(k.ToLowerInvariant()))
    | _ -> None

let private winMessages = [| "Super speller!"; "Great spelling!"; "Phew!" |]

let gameView: Shell.GameView<State, Input> =
    { Board = board
      Controls = letters
      SubHeader = banner
      HelpTitle = "How to play"
      Help = help
      About = about
      Answer = answer
      KeyToInput = keyToInput
      CelebrateAfterMs = 700
      WinMessage = fun attempts -> winMessages.[attempts - 1]
      DistributionTitle = "Mistakes"
      DistributionLabel = fun attempts -> string (attempts - 1)
      SiteRoot = "../" }
