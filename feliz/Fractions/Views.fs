/// Fractions' board (the row of cards, the card to beat and what the last flip showed),
/// Higher / Lower buttons and pop-ups.
module Fractions.Views

open Feliz
open Fractions.Rules

/// A pie cut into D slices with N shaded, as SVG markup.
let private pie (f: Fraction) =
    let r = 45.0
    let point i =
        let angle = 2.0 * System.Math.PI * float i / float f.D - System.Math.PI / 2.0
        50.0 + r * cos angle, 50.0 + r * sin angle

    let num (x: float) = System.Math.Round(x, 2).ToString(System.Globalization.CultureInfo.InvariantCulture)

    let slice i =
        let x1, y1 = point i
        let x2, y2 = point (i + 1)
        let large = if f.D = 1 then 1 else 0
        let fill = if i < f.N then "fr-pie-on" else "fr-pie-off"
        $"""<path class="{fill}" d="M50 50 L{num x1} {num y1} A45 45 0 {large} 1 {num x2} {num y2} Z"/>"""

    let slices = [ 0 .. f.D - 1 ] |> List.map slice |> String.concat ""
    $"""<svg viewBox="0 0 100 100" aria-hidden="true">{slices}</svg>"""

/// A fraction written the school way: top number over a line over the bottom number.
let private stacked (f: Fraction) =
    Html.span [
        prop.className "fr-stack"
        prop.ariaLabel (show f)
        prop.children [
            Html.span [ prop.className "fr-top"; prop.text f.N ]
            Html.span [ prop.className "fr-bottom"; prop.text f.D ]
        ]
    ]

let private resultMark result =
    match result with
    | Right -> "✓"
    | Wrong -> "✗"
    | Pair -> "="

let private resultClass result =
    match result with
    | Right -> "fr-right"
    | Wrong -> "fr-wrong"
    | Pair -> "fr-pair"

/// The day's row of cards along the top: face up as they're turned over, each marked with how the guess went.
let private row (state: State) =
    Html.div [
        prop.className "fr-row"
        prop.children [
            for i, card in List.indexed state.Cards ->
                let up = i < state.Revealed
                let result = if i > 0 then List.tryItem (i - 1) state.Results else None

                Html.div [
                    prop.key i
                    prop.className [
                        "fr-mini"
                        if up then "fr-up" else "fr-down"
                        if i = state.Revealed - 1 && i > 0 then "fr-flip"
                        if i = state.Revealed - 1 then "fr-current"
                        match result with
                        | Some r -> resultClass r
                        | None -> ()
                    ]
                    prop.children [
                        if up then stacked card else Html.span [ prop.className "fr-back"; prop.text "?" ]
                        match result with
                        | Some r -> Html.span [ prop.className "fr-mark"; prop.text (resultMark r) ]
                        | None -> ()
                    ]
                ]
        ]
    ]

let private bigCard (f: Fraction) (key: string) (extra: string) =
    Html.div [
        prop.key key
        prop.className [ "fr-card"; extra ]
        prop.children [
            Html.div [ prop.className "fr-pie"; prop.dangerouslySetInnerHTML (pie f) ]
            Html.div [ prop.className "fr-big"; prop.children [ stacked f ] ]
        ]
    ]

let private levelName level =
    match level with
    | 1 -> "Warm-up"
    | 2 -> "Getting harder"
    | _ -> "Close call"

let private explainBox (a, b, result) (key: string) =
    Html.div [
        prop.key key
        prop.className [ "fr-explain"; resultClass result ]
        prop.children [
            if result = Pair then Html.p [ prop.className "fr-pair-title"; prop.text "You get nothing for a pair!" ]
            Html.p (explain a b)
        ]
    ]

let private hearts' (state: State) =
    let left = max 0 (hearts - state.Mistakes)

    Html.div [
        prop.className "fr-hearts"
        prop.ariaLabel $"{left} hearts left"
        prop.text (String.replicate left "❤️" + String.replicate (hearts - left) "🤍")
    ]

let board (state: State) =
    Html.div [
        prop.className "fr-board"
        prop.children [
            yield row state
            match outcome state with
            | None ->
                let j = state.Revealed - 1
                let current = state.Cards.[j]
                let level = comparisonLevels |> List.tryItem j |> Option.defaultValue 3

                yield Html.div [
                    prop.className "fr-status"
                    prop.children [
                        Html.div [
                            prop.className "fr-level"
                            prop.ariaLabel $"Level {level} of 3"
                            prop.children [
                                Html.span [ prop.className "fr-stars"; prop.text (String.replicate level "⭐") ]
                                Html.span (levelName level)
                                if isPractice state j then
                                    Html.span [
                                        prop.className "fr-practice"
                                        prop.title "You got this one wrong before: have another go"
                                        prop.text "🔁 Practice"
                                    ]
                            ]
                        ]
                        hearts' state
                    ]
                ]

                yield Html.div [
                    prop.className "fr-table"
                    prop.children [
                        bigCard current $"card-{j}" (if j > 0 then "fr-flip" else "")
                        Html.div [ prop.className "fr-vs"; prop.text "next?" ]
                        Html.div [
                            prop.className "fr-card fr-card-back"
                            prop.children [ Html.span [ prop.className "fr-back"; prop.text "?" ] ]
                        ]
                    ]
                ]

                match lastFlip state with
                | Some flip -> yield explainBox flip $"explain-{j}"
                | None ->
                    yield Html.p [
                        prop.className "fr-prompt"
                        prop.text "Will the next card be higher or lower?"
                    ]
            | Some _ ->
                // finished: every flip of the day with its explanation
                yield Html.div [ prop.className "fr-status fr-status-end"; prop.children [ hearts' state ] ]
                yield Html.div [
                    prop.className "fr-review"
                    prop.children [
                        for j, result in List.indexed state.Results ->
                            let a, b = state.Cards.[j], state.Cards.[j + 1]
                            Html.div [
                                prop.key j
                                prop.className [ "fr-review-row"; resultClass result ]
                                prop.children [
                                    Html.span [ prop.className "fr-mark"; prop.text (resultMark result) ]
                                    Html.span (explain a b)
                                ]
                            ]
                    ]
                ]
        ]
    ]

let controls (state: State) send =
    match outcome state with
    | None ->
        Html.div [
            prop.className "fr-buttons"
            prop.children [
                Html.button [
                    prop.className "fr-button fr-higher"
                    prop.ariaLabel "Higher"
                    prop.onClick (fun _ -> send (Guess Higher))
                    prop.text "▲ Higher"
                ]
                Html.button [
                    prop.className "fr-button fr-lower"
                    prop.ariaLabel "Lower"
                    prop.onClick (fun _ -> send (Guess Lower))
                    prop.text "▼ Lower"
                ]
            ]
        ]
    | Some _ -> Html.div [ prop.className "fr-buttons fr-finished"; prop.text "Good game, good game! See you tomorrow." ]

let banner (_: State) openHelp =
    Html.div [
        prop.className "hint-bar"
        prop.children [
            Html.button [
                prop.className "hint-button"
                prop.onClick (fun _ -> openHelp ())
                prop.children [
                    Html.span [ prop.className "hint-label"; prop.text "Higher or lower?" ]
                    Html.span [ prop.className "hint-sound"; prop.text "Help" ]
                ]
            ]
        ]
    ]

let help (state: State) =
    Html.div [
        prop.className "modal-body p-2 text-slate-800 space-y-3"
        prop.children [
            Html.p $"Each day there's a row of {cardsPerDay} fraction cards. Guess whether the next card is higher ▲ or lower ▼ than the one showing."
            Html.p "It starts with halves and quarters ⭐, then thirds, fifths and eighths ⭐⭐, then close calls ⭐⭐⭐."
            Html.p $"You have {hearts} hearts. A wrong guess costs a heart, but the card still turns over and you carry on."
            Html.p "If the next card is the same amount (like 1/2 and 2/4), you get nothing for a pair! It doesn't cost a heart."
            Html.p [ prop.className "font-medium"; prop.text "How to compare fractions:" ]
            Html.ul [
                prop.className "fr-help-list"
                prop.children [
                    Html.li "Same bottom number? The bigger top number wins: 3/5 is more than 2/5."
                    Html.li "Same top number? The smaller bottom number wins, because the pieces are bigger: 1/3 is more than 1/4."
                    Html.li "Otherwise, make the bottoms the same: 2/3 = 8/12 and 3/4 = 9/12, so 3/4 is more."
                ]
            ]
            if not state.Practice.IsEmpty then
                Html.div [
                    Html.p [ prop.className "font-medium"; prop.text "Comparisons you're practising:" ]
                    Html.p (state.Practice |> List.map (fun (a, b) -> $"{show a} and {show b}") |> String.concat ", ")
                    Html.p [
                        prop.className "text-sm"
                        prop.text "Ones you get wrong come back on another day, marked 🔁. Get one right and it's done."
                    ]
                ]
            Html.p [ prop.className "text-sm"; prop.text "Keyboard: ↑ or H for higher, ↓ or L for lower." ]
        ]
    ]

let about =
    Html.div [
        prop.className "modal-body p-2 text-slate-800 space-y-3"
        prop.children [
            Html.p "Fractions is a higher-or-lower card game (in honour of Brucie's Play Your Cards Right) for practising comparing fractions."
            Html.p "It's built on the same F# daily-game engine as Aureliadle, Which Witch? and Numberdle."
        ]
    ]

let answer (state: State) =
    Html.div [
        prop.className "modal-body p-2 text-slate-800 space-y-2"
        prop.children [
            yield Html.p [ prop.className "text-center"; prop.text "Out of hearts! Here's today's row:" ]
            for j in 0 .. state.Cards.Length - 2 ->
                Html.p [ prop.key j; prop.className "text-sm"; prop.text (explain state.Cards.[j] state.Cards.[j + 1]) ]
        ]
    ]

let keyToInput (key: string) =
    match key with
    | "ArrowUp" | "h" | "H" -> Some(Guess Higher)
    | "ArrowDown" | "l" | "L" -> Some(Guess Lower)
    | _ -> None

let private winMessages = [| "Good game, good game!"; "Didn't you do well!"; "Phew!" |]

let gameView: Shell.GameView<State, Input> =
    { Board = board
      Controls = controls
      SubHeader = banner
      HelpTitle = "How to play"
      Help = help
      About = about
      Answer = answer
      KeyToInput = keyToInput
      CelebrateAfterMs = 600
      WinMessage = fun attempts -> winMessages.[attempts - 1]
      DistributionTitle = "Mistakes"
      DistributionLabel = fun attempts -> string (attempts - 1)
      SiteRoot = "../" }
