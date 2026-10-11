/// 11+ Fractions' board (progress, the question, and the last question's worked solution),
/// answer buttons and pop-ups.
module ElevenPlus.Views

open Feliz
open ElevenPlus.Rules

/// A number written properly: fractions top over bottom, mixed numbers with the whole beside.
let private number (num: Num) =
    let stacked n d =
        Html.span [
            prop.className "ep-frac"
            prop.children [
                Html.span [ prop.className "ep-top"; prop.text (string n) ]
                Html.span [ prop.className "ep-bottom"; prop.text (string d) ]
            ]
        ]

    Html.span [
        prop.className "ep-num"
        prop.ariaLabel (show num)
        prop.children [
            match num with
            | Frac(n, d) -> stacked n d
            | Mixed(w, n, d) ->
                Html.span [ prop.className "ep-whole"; prop.text (string w) ]
                stacked n d
            | other -> Html.span (show other)
        ]
    ]

let private questionView question (extraClass: string) =
    Html.div [
        prop.className [ "ep-question"; extraClass ]
        prop.children [
            for i, token in List.indexed (prompt question) ->
                match token with
                | T "?" -> Html.span [ prop.key i; prop.className "ep-mark"; prop.text "?" ]
                | T words -> Html.span [ prop.key i; prop.className "ep-words"; prop.text words ]
                | N num -> Html.span [ prop.key i; prop.children [ number num ] ]
        ]
    ]

let private progress (state: State) =
    let done' = results state

    Html.div [
        prop.className "ep-status"
        prop.children [
            Html.div [
                prop.className "ep-dots"
                prop.ariaLabel $"{state.Picked.Length} of {state.Questions.Length} done"
                prop.children [
                    for i in 0 .. state.Questions.Length - 1 ->
                        Html.span [
                            prop.key i
                            prop.className [
                                "ep-dot"
                                match List.tryItem i done' with
                                | Some true -> "ep-dot-right"
                                | Some false -> "ep-dot-wrong"
                                | None -> if i = state.Picked.Length then "ep-dot-current" else ""
                            ]
                        ]
                ]
            ]
            Html.div [
                prop.className "ep-score"
                prop.text $"{done' |> List.filter id |> List.length} of {done'.Length} right"
            ]
        ]
    ]

let private levelName level =
    match level with
    | 1 -> "Warm-up"
    | 2 -> "Tricky"
    | _ -> "11+ challenge"

/// A question answered: right or wrong, the answer, and how it's done.
let private worked question picked (key: string) (extra: string) =
    let right = isRight question picked
    let answer = answerOf question

    Html.div [
        prop.key key
        prop.className [ "ep-worked"; (if right then "ep-right" else "ep-wrong"); extra ]
        prop.children [
            Html.div [
                prop.className "ep-worked-head"
                prop.children [
                    Html.span [ prop.className "ep-tick"; prop.text (if right then "✓" else "✗") ]
                    questionView question "ep-small"
                    Html.span [ prop.className "ep-answer"; prop.children [ number answer ] ]
                ]
            ]
            if not right then
                Html.p [
                    prop.className "ep-picked"
                    prop.text ("You picked " + show (choices question).[picked] + ".")
                ]
            Html.p [ prop.className "ep-explain"; prop.text (explain question) ]
        ]
    ]

let board (state: State) =
    Html.div [
        prop.className "ep-board"
        prop.children [
            yield progress state
            match currentQuestion state with
            | Some q ->
                let i = state.Picked.Length
                let level = levelOf i

                yield Html.div [
                    prop.key i
                    prop.className "ep-card"
                    prop.children [
                        Html.div [
                            prop.className "ep-level"
                            prop.ariaLabel $"Question {i + 1}, level {level} of 3"
                            prop.children [
                                Html.span [ prop.className "ep-qnum"; prop.text $"Q{i + 1}" ]
                                Html.span [ prop.className "ep-stars"; prop.text (String.replicate level "⭐") ]
                                Html.span (levelName level)
                                if isPractice state then
                                    Html.span [
                                        prop.className "ep-practice"
                                        prop.title "You got this one wrong before: have another go"
                                        prop.text "🔁 Practice"
                                    ]
                            ]
                        ]
                        questionView q ""
                    ]
                ]

                if i > 0 then
                    yield worked state.Questions.[i - 1] state.Picked.[i - 1] $"worked-{i}" ""
            | None ->
                // finished: every question with its answer and working
                yield Html.div [
                    prop.className "ep-review"
                    prop.children [
                        for i, (q, p) in List.indexed (List.zip state.Questions state.Picked) ->
                            worked q p (string i) "ep-in-review"
                    ]
                ]
        ]
    ]

let answers (state: State) send =
    match currentQuestion state with
    | Some q ->
        let cs = choices q

        Html.div [
            prop.className [ "ep-choices"; if cs.Length = 3 then "ep-three" ]
            prop.children [
                for i, c in List.indexed cs ->
                    Html.button [
                        prop.key i
                        prop.className "ep-choice"
                        prop.ariaLabel $"{i + 1}: {show c}"
                        prop.onClick (fun _ -> send (Choose i))
                        prop.children [ number c ]
                    ]
            ]
        ]
    | None ->
        let score = results state |> List.filter id |> List.length
        Html.div [ prop.className "ep-choices ep-finished"; prop.text $"You scored {score} out of {perDay}. See you tomorrow!" ]

let banner (_: State) openHelp =
    Html.div [
        prop.className "hint-bar"
        prop.children [
            Html.button [
                prop.className "hint-button"
                prop.onClick (fun _ -> openHelp ())
                prop.children [
                    Html.span [ prop.className "hint-label"; prop.text "Ten 11+ questions" ]
                    Html.span [ prop.className "hint-sound"; prop.text "Help" ]
                ]
            ]
        ]
    ]

let help (state: State) =
    Html.div [
        prop.className "modal-body p-2 text-slate-800 space-y-3"
        prop.children [
            Html.p $"Ten fraction questions a day, like the 11+ maths paper. Pick the right answer from the choices. You're scored out of {perDay}."
            Html.p "They get harder: three warm-ups ⭐, four tricky ones ⭐⭐, then three 11+ challenges ⭐⭐⭐. After each one you'll see how it's done."
            Html.p [ prop.className "font-medium"; prop.text "Tips:" ]
            Html.ul [
                prop.className "ep-help-list"
                prop.children [
                    Html.li "Mixed number to improper: whole × bottom + top, over the same bottom. 2 3/4 = (2 × 4 + 3)/4 = 11/4."
                    Html.li "Mixed number × whole number: make it improper first, or multiply the whole and the fraction and add them. Don't forget the fraction!"
                    Html.li "Multiplying fractions: multiply the tops, multiply the bottoms, then simplify."
                    Html.li "Dividing by a fraction: keep, change, flip. 3/4 ÷ 1/2 = 3/4 × 2/1."
                    Html.li "Decimal or fraction? Turn the fraction into a decimal: 3/4 = 0.75, 3/5 = 0.6, 1/8 = 0.125."
                ]
            ]
            if not state.Practice.IsEmpty then
                Html.p [
                    prop.className "text-sm"
                    prop.text $"You're practising {state.Practice.Length} question(s) you got wrong before. They come back on another day, marked 🔁, until you get them right."
                ]
            Html.p [ prop.className "text-sm"; prop.text "Keyboard: press 1, 2, 3 or 4." ]
        ]
    ]

let about =
    Html.div [
        prop.className "modal-body p-2 text-slate-800 space-y-3"
        prop.children [
            Html.p "11+ Fractions practises the fraction questions in the 11+ maths paper: improper fractions and mixed numbers, multiplying and dividing fractions, fractions of amounts, and comparing decimals with fractions. The wrong answers are the mistakes people usually make."
            Html.p "It's built on the same F# daily-game engine as the other games."
        ]
    ]

let keyToInput (key: string) =
    match key with
    | "1" | "2" | "3" | "4" -> Some(Choose(int key - 1))
    | _ -> None

let private winMessage attempts =
    match perDay - (attempts - 1) with
    | 10 -> "Full marks!"
    | n when n >= 8 -> $"{n} out of 10: excellent!"
    | n when n >= 6 -> $"{n} out of 10: well done!"
    | n -> $"{n} out of 10: keep practising!"

let gameView: Shell.GameView<State, Input> =
    { Board = board
      Controls = answers
      SubHeader = banner
      HelpTitle = "How to play"
      Help = help
      About = about
      Answer = fun _ -> Html.none
      KeyToInput = keyToInput
      CelebrateAfterMs = 600
      WinMessage = winMessage
      DistributionTitle = "Score"
      DistributionLabel = fun attempts -> string (perDay - (attempts - 1))
      SiteRoot = "../" }
