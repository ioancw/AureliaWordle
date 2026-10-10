/// Which Witch?'s board (progress, hearts, the sentence and explanations), word buttons and pop-ups.
module WhichWitch.Views

open Feliz
open WhichWitch.Rules

/// The sentence with its blank: the answer once it's done, otherwise a gap.
let private sentenceView (s: Sentence) (filled: bool) (extraClass: string) =
    let before, after =
        match s.Text.Split([| "___" |], System.StringSplitOptions.None) with
        | [| b; a |] -> b, a
        | _ -> s.Text, ""

    Html.p [
        prop.className [ "ww-sentence"; extraClass ]
        prop.children [
            Html.text before
            Html.span [
                prop.className [ "ww-blank"; if filled then "ww-filled" ]
                prop.text (if filled then s.Answer else "?")
            ]
            Html.text after
        ]
    ]

let private meaning (word: string) =
    Html.li [
        prop.key word
        prop.children [
            Html.span [ prop.className "ww-word"; prop.text word ]
            Html.text (" means " + (meanings |> Map.tryFind word |> Option.defaultValue ""))
        ]
    ]

let private progress (state: State) =
    Html.div [
        prop.className "ww-status"
        prop.children [
            Html.div [
                prop.className "ww-dots"
                prop.ariaLabel $"{state.Current} of {state.Questions.Length} done"
                prop.children [
                    for i in 0 .. state.Questions.Length - 1 ->
                        Html.span [
                            prop.key i
                            prop.className [
                                "ww-dot"
                                if i < state.Current then
                                    (if List.contains i state.Missed then "ww-dot-retry" else "ww-dot-done")
                                elif i = state.Current then "ww-dot-current"
                            ]
                        ]
                ]
            ]
            Html.div [
                prop.className "ww-hearts"
                prop.ariaLabel $"{max 0 (hearts - state.Mistakes)} hearts left"
                prop.text (String.replicate (max 0 (hearts - state.Mistakes)) "❤️" + String.replicate (min hearts state.Mistakes) "🤍")
            ]
        ]
    ]

let board (state: State) =
    Html.div [
        prop.className "ww-board"
        prop.children [
            yield progress state
            match currentSentence state, outcome state with
            | Some s, None ->
                // the sentence just finished, with the right word filled in
                if state.Current > 0 then
                    yield Html.div [
                        prop.key $"last-{state.Current}"
                        prop.className "ww-last"
                        prop.children [ Html.span [ prop.className "ww-tick"; prop.text "✓" ]; sentenceView bank.[state.Questions.[state.Current - 1]] true "ww-small" ]
                    ]

                // key by position, so each new sentence animates in
                yield Html.div [
                    prop.key state.Current
                    prop.className "ww-card"
                    prop.children [
                        Html.div [
                            prop.className "ww-level"
                            prop.ariaLabel $"Level {s.Level} of {levels}"
                            prop.children [
                                Html.span [ prop.className "ww-stars"; prop.text (String.replicate s.Level "⭐") ]
                                Html.span [
                                    prop.text (
                                        match s.Level with
                                        | 1 -> "Warm-up"
                                        | 2 -> "Tricky"
                                        | _ -> "Apostrophe challenge"
                                    )
                                ]
                                if isPractice state then
                                    Html.span [
                                        prop.className "ww-practice"
                                        prop.title "You got this one wrong before: have another go"
                                        prop.text "🔁 Practice"
                                    ]
                            ]
                        ]
                        sentenceView s false ""
                        if not state.Wrong.IsEmpty then
                            Html.div [
                                prop.className "ww-explain"
                                prop.children [
                                    Html.p [ prop.className "ww-explain-title"; prop.text "Not that one:" ]
                                    Html.ul (state.Wrong |> List.rev |> List.map meaning)
                                ]
                            ]
                    ]
                ]
            | _ ->
                // finished: review the day's sentences with their answers
                yield Html.div [
                    prop.className "ww-review"
                    prop.children [
                        for i, q in List.indexed state.Questions ->
                            let done' = i < state.Current
                            Html.div [
                                prop.key i
                                prop.className [ "ww-review-row"; if not done' then "ww-not-reached" ]
                                prop.children [ sentenceView bank.[q] done' "ww-small" ]
                            ]
                    ]
                ]
        ]
    ]

let choices (state: State) send =
    match currentSentence state, outcome state with
    | Some s, None ->
        Html.div [
            prop.className "ww-choices"
            prop.children [
                for i, word in List.indexed s.Choices ->
                    let tried = List.contains word state.Wrong

                    Html.button [
                        prop.key word
                        prop.className [ "ww-choice"; if tried then "ww-tried" ]
                        prop.disabled tried
                        prop.ariaLabel $"{i + 1}: {word}"
                        prop.onClick (fun _ -> send (Choose i))
                        prop.text word
                    ]
            ]
        ]
    | _ -> Html.div [ prop.className "ww-choices ww-finished"; prop.text "See you tomorrow!" ]

let banner (_: State) openHelp =
    Html.div [
        prop.className "hint-bar"
        prop.children [
            Html.button [
                prop.className "hint-button"
                prop.onClick (fun _ -> openHelp ())
                prop.children [
                    Html.span [ prop.className "hint-label"; prop.text "Pick the right word" ]
                    Html.span [ prop.className "hint-sound"; prop.text "Help" ]
                ]
            ]
        ]
    ]

/// Help: how to play, and what today's words mean.
let help (state: State) =
    Html.div [
        prop.className "modal-body p-2 text-slate-800 space-y-3"
        prop.children [
            Html.p $"Each day there are {perDay} sentences with a missing word. Tap the right word to fill the gap."
            Html.p "They get harder as you go: two warm-ups ⭐, two tricky ones ⭐⭐, then two apostrophe challenges ⭐⭐⭐."
            Html.p $"You have {hearts} hearts. A wrong word costs a heart, shows what that word means, and you try again."
            match currentSentence state, outcome state with
            | Some s, None ->
                Html.div [
                    Html.p [ prop.className "font-medium"; prop.text "The words in this sentence:" ]
                    Html.ul [ prop.className "ww-help-list"; prop.children (s.Choices |> List.map meaning) ]
                ]
            | _ -> Html.none
            if not state.Practice.IsEmpty then
                Html.div [
                    Html.p [ prop.className "font-medium"; prop.text "Words you're practising:" ]
                    Html.p (
                        state.Practice
                        |> List.map (fun i -> bank.[i].Choices |> String.concat " / ")
                        |> List.distinct
                        |> String.concat ", "
                    )
                    Html.p [
                        prop.className "text-sm"
                        prop.text "Sentences you get wrong come back on another day, marked 🔁. Get one right first time and it's done."
                    ]
                ]
            Html.p "Tip: if a word has an apostrophe, try saying it the long way. They're → they are, could've → could have. The apostrophe goes where letters are missing."
        ]
    ]

let about =
    Html.div [
        prop.className "modal-body p-2 text-slate-800 space-y-3"
        prop.children [
            Html.p "Which Witch? practises words that sound the same but are spelt differently (their, there and they're; its and it's; to, too and two) and apostrophes (could've, not could of; didn't, not did'nt; the dog's bone, but three dogs)."
            Html.p "It's built on the same F# daily-game engine as Aureliadle and Numberdle."
        ]
    ]

let answer (state: State) =
    Html.div [
        prop.className "modal-body p-2 text-slate-800"
        prop.children [
            yield Html.p [ prop.className "text-center mb-2"; prop.text "Out of hearts! Here are today's answers:" ]
            for i, q in List.indexed state.Questions ->
                Html.div [ prop.key i; prop.children [ sentenceView bank.[q] true "ww-small ww-on-light" ] ]
        ]
    ]

let keyToInput (key: string) =
    match key with
    | "1" | "2" | "3" -> Some(Choose(int key - 1))
    | _ -> None

let private winMessages = [| "Perfect!"; "Great job!"; "Phew!" |]

let gameView: Shell.GameView<State, Input> =
    { Board = board
      Controls = choices
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
