/// The app, in the Elmish (Model-View-Update) style: every key press, timer and storage change
/// is a message, and `update` is the only place the state changes.
module Aureliadle.Main

open Elmish
open Bolero
open Bolero.Html
open Microsoft.JSInterop
open Domain
open GameRules
open Game

/// Time for a submitted row to finish flipping; matches --row-reveal-duration in main.css.
let rowRevealMs = 1700

type Model =
    { Game: State
      OpenModal: Modal option
      AnswerDismissed: bool
      HighContrast: bool
      Toast: string option
      ToastId: int }

type Message =
    | KeyPressed of string
    | StorageChanged
    | ToggleModal of Modal
    | CloseModal
    | DismissAnswer
    | ToggleHighContrast
    | ShowToast of message: string * durationMs: int
    | HideToast of id: int
    | OpenStats
    | Share
    | Shared of result: string

let init js =
    let game = Game.today (Interop.latestSave js)
    Interop.saveGame js game

    { Game = game
      OpenModal = None
      AnswerDismissed = false
      HighContrast = Interop.loadHighContrast js
      Toast = None
      ToastId = 0 }

let private after (ms: int) msg =
    Cmd.OfAsync.perform (fun () -> Async.Sleep ms) () (fun () -> msg)

/// Replaces the game and saves it straight away, so storage always holds the latest game.
let private withGame js game model =
    Interop.saveGame js game
    { model with Game = game }

let private pressKey js key model =
    // Start from the latest save, so an out-of-date tab (another tab played, or it's a new day)
    // never writes old progress or stats over newer ones.
    let current = Game.refresh (Interop.latestSave js) model.Game

    match key with
    | "Ent" ->
        let message =
            if not (Validate.round numberOfRounds current) then None
            elif not (Validate.allLetters current) then Some "Not enough letters"
            elif not (Validate.word current) then Some "Not in word list"
            else None

        let next = Play.submitEnter numberOfRounds numberOfLetters current

        let celebrate =
            if next.State = Won && current.State <> Won then
                // once the winning row has finished flipping and bouncing, then the stats
                [ after (rowRevealMs + 600) (ShowToast(winMessages.[next.Round], 2000))
                  after (rowRevealMs + 2600) OpenStats ]
            else
                []

        withGame js next model,
        Cmd.batch [ yield! message |> Option.map (fun m -> Cmd.ofMsg (ShowToast(m, 1000))) |> Option.toList
                    yield! celebrate ]
    | "Del" -> withGame js (Play.submitDelete numberOfRounds numberOfLetters current) model, Cmd.none
    | letter -> withGame js (Play.submitLetter numberOfRounds numberOfLetters letter current) model, Cmd.none

let update (js: IJSRuntime) message model =
    match message with
    | KeyPressed _ when model.OpenModal.IsSome -> model, Cmd.none
    | KeyPressed key -> pressKey js key model
    | StorageChanged ->
        let latest = Game.refresh (Interop.latestSave js) model.Game
        (if latest = model.Game then model else withGame js latest model), Cmd.none
    | ToggleModal m -> { model with OpenModal = (if model.OpenModal = Some m then None else Some m) }, Cmd.none
    | CloseModal -> { model with OpenModal = None }, Cmd.none
    | DismissAnswer -> { model with AnswerDismissed = true }, Cmd.none
    | ToggleHighContrast ->
        Interop.saveHighContrast js (not model.HighContrast)
        { model with HighContrast = not model.HighContrast }, Cmd.none
    | ShowToast(text, ms) ->
        let id = model.ToastId + 1
        { model with Toast = Some text; ToastId = id }, after ms (HideToast id)
    | HideToast id -> (if id = model.ToastId then { model with Toast = None } else model), Cmd.none
    | OpenStats -> { model with OpenModal = Some Stats }, Cmd.none
    | Share -> model, Cmd.OfTask.perform (Interop.share js) (shareText model.HighContrast model.Game) Shared
    | Shared "copied" -> model, Cmd.ofMsg (ShowToast("Copied results to clipboard", 1500))
    | Shared "failed" -> model, Cmd.ofMsg (ShowToast("Couldn't share results", 1500))
    | Shared _ -> model, Cmd.none

let private icon (svg: string) = rawHtml svg

let view js model dispatch =
    let game = model.Game
    let onKey k =
        Interop.blurActive js
        dispatch (KeyPressed k)
    let keyRow keys = forEach keys (Views.key game.UsedLetters onKey)

    div {
        attr.``class`` ("game flex flex-col" + (if model.HighContrast then " high-contrast" else ""))
        attr.style "background-color: var(--background)"

        div {
            attr.``class`` "flex-none"
            div {
                attr.``class`` "relative flex items-center justify-between px-2"
                attr.style "height: var(--header-h); border-bottom: 1px solid #3a3a3c"
                button {
                    attr.aria "label" "About and settings"
                    attr.``class`` "p-2 text-white"
                    on.click (fun _ -> dispatch (ToggleModal Info))
                    icon """<svg xmlns="http://www.w3.org/2000/svg" class="h-7 w-7" fill="none" viewBox="0 0 24 24" stroke="currentColor" stroke-width="2"><path stroke-linecap="round" stroke-linejoin="round" d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" /></svg>"""
                }
                div { attr.``class`` "aurelia-header"; text "Aureliadle" }
                div {
                    attr.``class`` "flex"
                    button {
                        attr.aria "label" "Statistics"
                        attr.``class`` "p-2 text-white mr-1"
                        on.click (fun _ -> dispatch (ToggleModal Stats))
                        icon """<svg xmlns="http://www.w3.org/2000/svg" class="h-7 w-7" viewBox="0 0 20 20" fill="currentColor"><path d="M2 11a1 1 0 011-1h2a1 1 0 011 1v5a1 1 0 01-1 1H3a1 1 0 01-1-1v-5zM8 7a1 1 0 011-1h2a1 1 0 011 1v9a1 1 0 01-1 1H9a1 1 0 01-1-1V7zM14 4a1 1 0 011-1h2a1 1 0 011 1v12a1 1 0 01-1 1h-2a1 1 0 01-1-1V4z" /></svg>"""
                    }
                    button {
                        attr.aria "label" "Help"
                        attr.``class`` "p-2 text-white"
                        on.click (fun _ -> dispatch (ToggleModal Help))
                        icon """<svg xmlns="http://www.w3.org/2000/svg" class="h-7 w-7" fill="none" viewBox="0 0 24 24" stroke="currentColor" stroke-width="2"><path stroke-linecap="round" stroke-linejoin="round" d="M8.228 9c.549-1.165 2.03-2 3.772-2 2.21 0 4 1.343 4 3 0 1.4-1.278 2.575-3.006 2.907-.542.104-.994.54-.994 1.093m0 3h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" /></svg>"""
                    }
                }
            }
            div {
                attr.``class`` "hint-bar"
                button {
                    attr.``class`` "hint-button"
                    attr.aria "label" $"Today's sound: {game.Phonics.Hint}. Show spellings"
                    on.click (fun _ -> dispatch (ToggleModal Help))
                    span { attr.``class`` "hint-label"; text "Today's sound" }
                    span { attr.``class`` "hint-sound"; text game.Phonics.Hint }
                }
            }
        }

        let close () = dispatch CloseModal
        Views.modal "" "About" (model.OpenModal = Some Info) close (Views.aboutBody model.HighContrast (fun () -> dispatch ToggleHighContrast))
        Views.modal "" "Game Statistics" (model.OpenModal = Some Stats) close (Views.statsBody game (fun () -> dispatch Share))
        Views.modal "" "Grapheme Phoneme Correspondence" (model.OpenModal = Some Help) close (Views.helpBody game)
        // waits for the last row to finish flipping (see .modal-after-reveal in main.css)
        Views.modal "modal-after-reveal" "Today's Answer" (game.State = Lost && not model.AnswerDismissed) (fun () -> dispatch DismissAnswer) (Views.answerBody game)
        Views.toast model.Toast

        div {
            attr.``class`` "board flex-1 min-h-0 flex flex-col justify-center"
            forEach game.Guesses Views.boardRow
        }

        div {
            attr.``class`` "flex-none keyboard-safe-area"
            div { attr.``class`` "keyboard-row"; keyRow keyBoard.Top }
            div {
                attr.``class`` "keyboard-row"
                div { attr.``class`` "keyboard-spacer" }
                keyRow keyBoard.Middle
                div { attr.``class`` "keyboard-spacer" }
            }
            div { attr.``class`` "keyboard-row"; keyRow keyBoard.Bottom }
        }
    }

type App() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        let js = this.JSRuntime

        let browserEvents _ =
            [ [ "browser-events" ],
              fun dispatch -> Interop.listen js (fun key -> dispatch (KeyPressed key)) (fun () -> dispatch StorageChanged) ]

        Program.mkProgram (fun _ -> init js, Cmd.none) (update js) (view js)
        |> Program.withSubscription browserEvents
