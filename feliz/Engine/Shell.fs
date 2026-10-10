/// The app around any daily game, in Feliz (React) with Elmish: header, pop-ups, messages,
/// stats, sharing and settings, plus keyboard and storage events. A game supplies its board,
/// controls and help through a GameView.
module Shell

open System
open Elmish
open Feliz
open Fable.Core
open Engine

/// The parts of the page each game draws itself.
type GameView<'State, 'Input> =
    { Board: 'State -> ReactElement
      /// The on-screen keyboard or keypad; the function sends an input.
      Controls: 'State -> ('Input -> unit) -> ReactElement
      /// Under the header, e.g. today's hint; the function opens the help pop-up.
      SubHeader: 'State -> (unit -> unit) -> ReactElement
      HelpTitle: string
      Help: 'State -> ReactElement
      About: ReactElement
      /// Shown when the game is lost.
      Answer: 'State -> ReactElement
      /// A physical key (KeyboardEvent.key, e.g. "Enter", "a", "7") as an input, if it is one.
      KeyToInput: string -> 'Input option
      /// How long after the winning input to celebrate, e.g. once tiles have finished flipping.
      CelebrateAfterMs: int
      /// Shown after a win, given the number of attempts.
      WinMessage: int -> string
      /// The stats chart's title and row labels (rows are numbered by attempts, from 1).
      DistributionTitle: string
      DistributionLabel: int -> string
      /// The path from this game's page to the site root, for the menu links: "./" or "../".
      SiteRoot: string }

type Modal =
    | About
    | Help
    | Stats

type Model<'State> =
    { Saved: Saved<'State>
      Modal: Modal option
      MenuOpen: bool
      AnswerDismissed: bool
      HighContrast: bool
      Toast: string option
      ToastId: int }

type Msg<'Input> =
    | Input of 'Input
    | StorageChanged
    | Toggle of Modal
    | CloseModal
    | ToggleMenu
    | DismissAnswer
    | ToggleHighContrast
    | ShowToast of message: string * durationMs: int
    | HideToast of id: int
    | OpenStats
    | Share
    | Shared of result: string

/// Share sheet on phones, clipboard elsewhere. Resolves to "shared", "copied", "cancelled" or "failed".
[<Emit("""(navigator.share && matchMedia('(pointer: coarse)').matches
    ? navigator.share({ text: $0 }).then(() => 'shared', () => 'cancelled')
    : navigator.clipboard.writeText($0).then(() => 'copied', () => 'failed'))""")>]
let private shareOrCopy (text: string) : JS.Promise<string> = jsNative

let private after (ms: int) msg =
    Cmd.OfAsync.perform (fun () -> Async.Sleep ms) () (fun () -> msg)

let init game () =
    let saved = resume game (today game) (BrowserStorage.load game)
    BrowserStorage.save game saved

    { Saved = saved
      Modal = None
      MenuOpen = false
      AnswerDismissed = false
      HighContrast = BrowserStorage.loadHighContrast game
      Toast = None
      ToastId = 0 },
    Cmd.none

let update game (view: GameView<'State, 'Input>) msg (model: Model<'State>) =
    match msg with
    | Input _ when model.Modal.IsSome || model.MenuOpen -> model, Cmd.none
    | Input input ->
        // Start from the latest save, so an out-of-date tab (another tab played, or it's a new
        // day) never writes old progress or stats over newer ones.
        let current = refresh game (today game) (BrowserStorage.load game) model.Saved
        let next, message = play game input current
        BrowserStorage.save game next

        let celebrate =
            match game.Outcome current.State, game.Outcome next.State with
            | None, Some(Solved attempts) ->
                [ after view.CelebrateAfterMs (ShowToast(view.WinMessage attempts, 2000))
                  after (view.CelebrateAfterMs + 2000) OpenStats ]
            | _ -> []

        { model with Saved = next },
        Cmd.batch [ yield! message |> Option.map (fun m -> Cmd.ofMsg (ShowToast(m, 1000))) |> Option.toList
                    yield! celebrate ]
    | StorageChanged ->
        let latest = refresh game (today game) (BrowserStorage.load game) model.Saved

        if latest = model.Saved then
            model, Cmd.none
        else
            BrowserStorage.save game latest
            { model with Saved = latest }, Cmd.none
    | Toggle m -> { model with Modal = (if model.Modal = Some m then None else Some m) }, Cmd.none
    | CloseModal -> { model with Modal = None }, Cmd.none
    | ToggleMenu -> { model with MenuOpen = not model.MenuOpen }, Cmd.none
    | DismissAnswer -> { model with AnswerDismissed = true }, Cmd.none
    | ToggleHighContrast ->
        BrowserStorage.saveHighContrast game (not model.HighContrast)
        { model with HighContrast = not model.HighContrast }, Cmd.none
    | ShowToast(text, ms) ->
        let id = model.ToastId + 1
        { model with Toast = Some text; ToastId = id }, after ms (HideToast id)
    | HideToast id -> (if id = model.ToastId then { model with Toast = None } else model), Cmd.none
    | OpenStats -> { model with Modal = Some Stats }, Cmd.none
    | Share ->
        model, Cmd.OfPromise.perform shareOrCopy (shareText game model.HighContrast model.Saved) Shared
    | Shared "copied" -> model, Cmd.ofMsg (ShowToast("Copied results to clipboard", 1500))
    | Shared "failed" -> model, Cmd.ofMsg (ShowToast("Couldn't share results", 1500))
    | Shared _ -> model, Cmd.none

/// Key presses, and anything that may have changed the save: another tab saving, the page
/// coming back into view (phones keep pages open for days), or the date changing.
let subscriptions (view: GameView<'State, 'Input>) (_: Model<'State>) : Sub<Msg<'Input>> =
    let listen (dispatch: Msg<'Input> -> unit) =
        let window = Browser.Dom.window
        let document = Browser.Dom.document

        let onKey (e: Browser.Types.Event) =
            let e = e :?> Browser.Types.KeyboardEvent

            if not (e.ctrlKey || e.metaKey || e.altKey) then
                match view.KeyToInput e.key with
                | Some input ->
                    e.preventDefault ()
                    dispatch (Input input)
                | None -> ()

        let onChange (_: Browser.Types.Event) = dispatch StorageChanged

        let onVisible (_: Browser.Types.Event) =
            if document.visibilityState = "visible" then dispatch StorageChanged

        window.addEventListener ("keydown", onKey)
        window.addEventListener ("storage", onChange)
        window.addEventListener ("pageshow", onVisible)
        document.addEventListener ("visibilitychange", onVisible)
        let timer = JS.setInterval (fun () -> dispatch StorageChanged) 60000

        { new IDisposable with
            member _.Dispose() =
                window.removeEventListener ("keydown", onKey)
                window.removeEventListener ("storage", onChange)
                window.removeEventListener ("pageshow", onVisible)
                document.removeEventListener ("visibilitychange", onVisible)
                JS.clearInterval timer }

    [ [ "browser-events" ], listen ]

// Views

let private icon (svg: string) =
    Html.span [ prop.className "block"; prop.dangerouslySetInnerHTML svg ]

let private infoIcon =
    """<svg xmlns="http://www.w3.org/2000/svg" class="h-7 w-7" fill="none" viewBox="0 0 24 24" stroke="currentColor" stroke-width="2"><path stroke-linecap="round" stroke-linejoin="round" d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" /></svg>"""

let private statsIcon =
    """<svg xmlns="http://www.w3.org/2000/svg" class="h-7 w-7" viewBox="0 0 20 20" fill="currentColor"><path d="M2 11a1 1 0 011-1h2a1 1 0 011 1v5a1 1 0 01-1 1H3a1 1 0 01-1-1v-5zM8 7a1 1 0 011-1h2a1 1 0 011 1v9a1 1 0 01-1 1H9a1 1 0 01-1-1V7zM14 4a1 1 0 011-1h2a1 1 0 011 1v12a1 1 0 01-1 1h-2a1 1 0 01-1-1V4z" /></svg>"""

let private helpIcon =
    """<svg xmlns="http://www.w3.org/2000/svg" class="h-7 w-7" fill="none" viewBox="0 0 24 24" stroke="currentColor" stroke-width="2"><path stroke-linecap="round" stroke-linejoin="round" d="M8.228 9c.549-1.165 2.03-2 3.772-2 2.21 0 4 1.343 4 3 0 1.4-1.278 2.575-3.006 2.907-.542.104-.994.54-.994 1.093m0 3h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" /></svg>"""

let private headerButton (label: string) (svg: string) extraClass onClick =
    Html.button [
        prop.ariaLabel label
        prop.className ("header-button text-white " + extraClass)
        prop.onClick (fun _ -> onClick ())
        prop.children [ icon svg ]
    ]

/// A pop-up. extraClass is added to the outer element, e.g. to delay its appearance.
let modal (extraClass: string) (title: string) (isOpen: bool) (onClose: unit -> unit) (body: ReactElement) =
    Html.div [
        prop.className [
            "modal fixed inset-0 flex items-start justify-center outline-none overflow-x-hidden overflow-y-auto z-50 pt-10 px-3"
            if not isOpen then "hidden"
            extraClass
        ]
        prop.children [
            Html.div [
                prop.className "modal-dialog pointer-events-none w-full max-w-sm"
                prop.children [
                    Html.div [
                        prop.className "modal-content border-none shadow-lg relative flex flex-col w-full pointer-events-auto bg-neutral-400 bg-clip-padding rounded-md outline-none text-current max-h-screen-3/4 overflow-y-auto"
                        prop.children [
                            Html.div [
                                prop.className "modal-header flex flex-shrink-0 items-center justify-between p-1 border-b border-stone-600 rounded-t-md"
                                prop.children [
                                    Html.h5 [ prop.className "text-lg text-left font-medium leading-normal text-stone-800"; prop.text title ]
                                    Html.button [
                                        prop.type'.button
                                        prop.ariaLabel "Close"
                                        prop.className "px-2 py-1 bg-stone-800 text-white font-bold text-xs leading-tight uppercase rounded shadow-md"
                                        prop.onClick (fun _ -> onClose ())
                                        prop.text "X"
                                    ]
                                ]
                            ]
                            body
                        ]
                    ]
                ]
            ]
        ]
    ]

let private menuIcon =
    """<svg xmlns="http://www.w3.org/2000/svg" class="h-7 w-7" fill="none" viewBox="0 0 24 24" stroke="currentColor" stroke-width="2"><path stroke-linecap="round" stroke-linejoin="round" d="M4 6h16M4 12h16M4 18h16" /></svg>"""

/// The ☰ menu: a drawer listing the suite's games.
let private menu (siteRoot: string) (currentId: string) (isOpen: bool) dispatch =
    React.Fragment [
        Html.div [
            prop.className [ "menu-backdrop"; if not isOpen then "hidden" ]
            prop.onClick (fun _ -> dispatch ToggleMenu)
        ]
        Html.nav [
            prop.className [ "menu-drawer"; if isOpen then "open" ]
            prop.ariaLabel "Games"
            prop.ariaHidden (not isOpen)
            prop.children [
                yield Html.div [
                    prop.className "menu-heading"
                    prop.children [
                        Html.span "Daily games"
                        Html.button [
                            prop.ariaLabel "Close menu"
                            prop.className "menu-close"
                            prop.onClick (fun _ -> dispatch ToggleMenu)
                            prop.text "✕"
                        ]
                    ]
                ]
                for g in Suite.games ->
                    Html.a [
                        prop.key g.Id
                        prop.href (siteRoot + g.Path)
                        prop.tabIndex (if isOpen then 0 else -1)
                        prop.className [ "menu-item"; if g.Id = currentId then "current" ]
                        prop.children [
                            Html.span [ prop.className "menu-title"; prop.text g.Title ]
                            Html.span [ prop.className "menu-blurb"; prop.text g.Blurb ]
                        ]
                    ]
            ]
        ]
    ]

let private statsBody game (gameView: GameView<'State, 'Input>) (model: Model<'State>) dispatch =
    let stats = model.Saved.Stats
    let winRate = if stats.Played = 0 then 0 else int (round (100. * float stats.Won / float stats.Played))
    let mostWins = stats.Distribution |> List.fold max 0

    let stat (label: string) (value: int) =
        Html.div [
            prop.className "text-center px-1"
            prop.children [
                Html.div [ prop.className "text-3xl font-bold"; prop.text value ]
                Html.div [ prop.className "text-xs"; prop.text label ]
            ]
        ]

    Html.div [
        prop.className "modal-body p-2"
        prop.children [
            Html.div [
                prop.className "flex items-start justify-center gap-2 my-2"
                prop.children [
                    stat "Played" stats.Played
                    stat "Win %" winRate
                    stat "Current streak" (currentStreak model.Saved.Day stats)
                    stat "Best streak" stats.MaxStreak
                ]
            ]
            Html.h4 [ prop.className "flex text-lg justify-center items-center font-medium"; prop.text gameView.DistributionTitle ]
            Html.div [
                prop.className "m-2 text-sm text-white"
                prop.children [
                    for attempts, wins in List.indexed stats.Distribution ->
                        // bars are relative to the most common result, with a minimum so the count stays readable
                        let percent = if mostWins = 0 then 8. else max 8. (100. * float wins / float mostWins)

                        Html.div [
                            prop.key attempts
                            prop.className "flex m-1"
                            prop.children [
                                Html.div [ prop.className "shrink-0 text-right"; prop.style [ style.minWidth (length.em 1) ]; prop.text (gameView.DistributionLabel(attempts + 1)) ]
                                Html.div [
                                    prop.className "w-full ml-2"
                                    prop.children [
                                        Html.div [
                                            prop.className "text-xs text-right font-medium p-0.5 pr-2 bg-pink-600"
                                            prop.style [ style.width (length.percent percent) ]
                                            prop.text wins
                                        ]
                                    ]
                                ]
                            ]
                        ]
                ]
            ]
            Html.div [
                prop.className [ "flex justify-center my-3"; if (game.Outcome model.Saved.State).IsNone then "hidden" ]
                prop.children [
                    Html.button [
                        prop.type'.button
                        prop.className "share-button"
                        prop.onClick (fun _ -> dispatch Share)
                        prop.text "Share"
                    ]
                ]
            ]
        ]
    ]

let private settings (model: Model<'State>) dispatch =
    Html.div [
        prop.className "modal-body px-2 pb-3 text-slate-800"
        prop.children [
            Html.label [
                prop.className "flex items-center justify-between gap-3 font-medium border-t border-stone-600 pt-3"
                prop.children [
                    Html.span [
                        prop.children [
                            Html.text "High contrast colours"
                            Html.span [
                                prop.className "block text-xs font-normal"
                                prop.text "Orange and blue instead of green and yellow, for colour vision differences."
                            ]
                        ]
                    ]
                    Html.input [
                        prop.type'.checkbox
                        prop.className "h-5 w-5 shrink-0"
                        prop.isChecked model.HighContrast
                        prop.onChange (fun (_: bool) -> dispatch ToggleHighContrast)
                    ]
                ]
            ]
        ]
    ]

let view game (gameView: GameView<'State, 'Input>) (model: Model<'State>) dispatch =
    let state = model.Saved.State
    let close () = dispatch CloseModal

    let sendInput input =
        // Stop the pressed key keeping focus, so a later physical Enter/Space doesn't press it again.
        match Browser.Dom.document.activeElement with
        | :? Browser.Types.HTMLElement as e -> e.blur ()
        | _ -> ()

        dispatch (Input input)

    Html.div [
        prop.className [ "game flex flex-col"; if model.HighContrast then "high-contrast" ]
        prop.style [ style.backgroundColor "var(--background)" ]
        prop.children [
            Html.div [
                prop.className "flex-none"
                prop.children [
                    Html.div [
                        prop.className "relative flex items-center justify-between px-2"
                        prop.style [ style.custom ("height", "var(--header-h)"); style.custom ("borderBottom", "1px solid #3a3a3c") ]
                        prop.children [
                            Html.div [
                                prop.className "flex"
                                prop.children [
                                    headerButton "Games" menuIcon "" (fun () -> dispatch ToggleMenu)
                                    headerButton "About and settings" infoIcon "" (fun () -> dispatch (Toggle About))
                                ]
                            ]
                            Html.div [ prop.className "aurelia-header"; prop.text game.Title ]
                            Html.div [
                                prop.className "flex"
                                prop.children [
                                    headerButton "Statistics" statsIcon "mr-1" (fun () -> dispatch (Toggle Stats))
                                    headerButton "Help" helpIcon "" (fun () -> dispatch (Toggle Help))
                                ]
                            ]
                        ]
                    ]
                    gameView.SubHeader state (fun () -> dispatch (Toggle Help))
                ]
            ]

            menu gameView.SiteRoot game.Id model.MenuOpen dispatch
            modal "" "About" (model.Modal = Some About) close (Html.div [ gameView.About; settings model dispatch ])
            modal "" "Statistics" (model.Modal = Some Stats) close (statsBody game gameView model dispatch)
            modal "" gameView.HelpTitle (model.Modal = Some Help) close (gameView.Help state)
            // waits for the last move's animation (see .modal-after-reveal in main.css)
            modal "modal-after-reveal" "Today's Answer" (game.Outcome state = Some Failed && not model.AnswerDismissed) (fun () -> dispatch DismissAnswer) (gameView.Answer state)

            Html.div [
                prop.className [ "toast"; if model.Toast.IsNone then "hidden" ]
                prop.role "status"
                prop.text (model.Toast |> Option.defaultValue "")
            ]

            Html.div [
                prop.className "board flex-1 min-h-0 flex flex-col justify-center"
                prop.children [ gameView.Board state ]
            ]

            Html.div [
                prop.className "flex-none keyboard-safe-area"
                prop.children [ gameView.Controls state sendInput ]
            ]
        ]
    ]

/// The Elmish program for a game; mount it with Program.withReactSynchronous and Program.run.
let program game (gameView: GameView<'State, 'Input>) =
    Program.mkProgram (init game) (update game gameView) (view game gameView)
    |> Program.withSubscription (subscriptions gameView)
