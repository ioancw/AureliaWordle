module Lit.Wordle

open System
open Lit
open Domain
open GameRules
open Game
open Components
open Modals
open Fable.Core

JsInterop.importSideEffects "./index.css"

/// Time for a submitted row to finish flipping; matches --row-reveal-duration in main.css.
let rowRevealMs = 1700

/// Shown after a win, indexed by the round the word was guessed in.
let winMessages = [| "Genius!"; "Magnificent!"; "Impressive!"; "Splendid!"; "Great!"; "Phew!" |]

/// Shares the text on phones (share sheet) or copies it to the clipboard elsewhere.
/// Resolves to "shared", "copied", "cancelled" or "failed".
[<Emit("""(navigator.share && matchMedia('(pointer: coarse)').matches
    ? navigator.share({ text: $0 }).then(() => 'shared', () => 'cancelled')
    : navigator.clipboard.writeText($0).then(() => 'copied', () => 'failed'))""")>]
let shareOrCopy (text: string) : JS.Promise<string> = jsNative

/// Today's result as an emoji grid, like Wordle's share text.
let shareText highContrast state =
    let square status =
        match status with
        | Green -> if highContrast then "🟧" else "🟩"
        | Yellow -> if highContrast then "🟦" else "🟨"
        | _ -> "⬛"

    let score = if state.State = Won then string (state.Round + 1) else "X"

    let grid =
        state.Guesses
        |> List.take (state.Round + 1)
        |> List.map (fun (_, guess) -> guess.Letters |> List.map (fun l -> square l.Status) |> String.concat "")
        |> String.concat "\n"

    $"Aureliadle {Daily.dayNumber ()} {score}/{numberOfRounds}\n\n{grid}"

/// Maps a physical key press to the on-screen key it stands for.
let keyFromKeyboardEvent (ev: Browser.Types.KeyboardEvent) =
    if ev.ctrlKey || ev.metaKey || ev.altKey then
        None
    else
        match ev.key with
        | "Enter" -> Some "Ent"
        | "Backspace" -> Some "Del"
        | key when key.Length = 1 ->
            let lower = key.ToLower()
            if lower >= "a" && lower <= "z" then Some lower else None
        | _ -> None

/// The main part of the application, responsible for updating state and returning HTML.
[<LitElement("wordle-app")>]
let MatchComponent () =
    let _ = LitElement.init (fun cfg -> cfg.useShadowDom <- false)

    // Starts a new game, or resumes today's game from local storage.
    let state, setState =
        Hook.useState (init = fun () ->
            let game = Game.load ()
            Game.save game
            game)
    let showHelpModal, setShowHelpModal = Hook.useState false
    let showInfoModal, setShowInfoModal = Hook.useState false
    let showStatsModal, setShowStatsModal = Hook.useState false
    let highContrast, setHighContrast = Hook.useState (init = fun () -> Storage.loadHighContrast ())
    let toast, setToast = Hook.useState (None: string option)
    let toastId = Hook.useRef 0
    // Latest key handler, so the window keydown listener (added once) never sees stale state.
    let keyHandler = Hook.useRef (fun (_: string) -> ())

    // Every change is saved straight away, so storage always holds the latest game
    // (key presses start from storage; see handleKey).
    let setGameState next =
        Game.save next
        setState next

    let showToast durationMs message =
        toastId.Value <- toastId.Value + 1
        let id = toastId.Value
        setToast (Some message)
        JS.setTimeout (fun () -> if toastId.Value = id then setToast None) durationMs |> ignore

    let submitEnter current =
        if Validate.round numberOfRounds current then
            if not (Validate.allLetters current) then
                showToast 1000 "Not enough letters"
            elif not (Validate.word current) then
                showToast 1000 "Not in word list"

        let next = Play.submitEnter numberOfRounds numberOfLetters current
        setGameState next

        // Celebrate once the winning row has finished flipping and bouncing, then show the stats.
        if next.State = Won && current.State <> Won then
            JS.setTimeout (fun () -> showToast 2000 winMessages.[next.Round]) (rowRevealMs + 600) |> ignore
            JS.setTimeout (fun () -> setShowStatsModal true) (rowRevealMs + 2600) |> ignore

    let handleKey (key: string) =
        // Start from the latest saved game, so a tab that's out of date (another tab played,
        // or it's a new day) never writes old progress or stats over newer ones.
        let current = Game.refresh state

        match key with
        | "Ent" -> submitEnter current
        | "Del" -> current |> Play.submitDelete numberOfRounds numberOfLetters |> setGameState
        | letter -> current |> Play.submitLetter numberOfRounds numberOfLetters letter |> setGameState

    // Latest refresh, for listeners added once.
    let refreshHandler = Hook.useRef (fun () -> ())

    refreshHandler.Value <-
        (fun () ->
            let latest = Game.refresh state
            if latest <> state then setGameState latest)

    let anyModalOpen = showHelpModal || showInfoModal || showStatsModal
    keyHandler.Value <- (fun key -> if not anyModalOpen then handleKey key)

    Hook.useEffectOnce (fun () ->
        let onKeyDown (ev: Browser.Types.Event) =
            let ev = ev :?> Browser.Types.KeyboardEvent
            match keyFromKeyboardEvent ev with
            | Some key ->
                ev.preventDefault ()
                keyHandler.Value key
            | None -> ()

        // Pick up changes made elsewhere: another tab saving, the page coming back into view
        // (phones keep pages open for days), or the date changing while the page is open.
        let onStorage (ev: Browser.Types.Event) =
            let ev = ev :?> Browser.Types.StorageEvent
            if isNull ev.key || ev.key = Storage.gameKey then refreshHandler.Value ()

        let onVisible (_: Browser.Types.Event) =
            if Browser.Dom.document.visibilityState = "visible" then refreshHandler.Value ()

        let window = Browser.Dom.window
        window.addEventListener ("keydown", onKeyDown)
        window.addEventListener ("storage", onStorage)
        window.addEventListener ("pageshow", onVisible)
        Browser.Dom.document.addEventListener ("visibilitychange", onVisible)
        let timer = JS.setInterval (fun () -> refreshHandler.Value ()) 60000

        { new IDisposable with
            member _.Dispose() =
                window.removeEventListener ("keydown", onKeyDown)
                window.removeEventListener ("storage", onStorage)
                window.removeEventListener ("pageshow", onVisible)
                Browser.Dom.document.removeEventListener ("visibilitychange", onVisible)
                JS.clearInterval timer })

    let letterToDisplayBox (letters: (Position * Guess)) =
        letters
        |> Guess.getLetter
        |> List.mapi gameTile

    let boardRow guess =
        html
            $"""
            <div class="board-row flex justify-center">{letterToDisplayBox guess}</div>
        """

    let onKeyClick (c: string) =
        Ev (fun ev ->
            ev.preventDefault ()
            // Stop the key keeping focus, so a later physical Enter/Space doesn't press it again.
            (ev.currentTarget :?> Browser.Types.HTMLElement).blur ()
            handleKey c)

    let onModalClick modalType =
        Ev (fun ev ->
            ev.preventDefault ()

            match modalType with
            | Info -> setShowInfoModal (not showInfoModal)
            | Stats -> setShowStatsModal (not showStatsModal)
            | Help -> setShowHelpModal (not showHelpModal))

    let onToggleHighContrast =
        Ev (fun _ ->
            Storage.saveHighContrast (not highContrast)
            setHighContrast (not highContrast))

    let onShare =
        Ev (fun ev ->
            ev.preventDefault ()

            shareOrCopy (shareText highContrast state)
            |> Promise.iter (fun result ->
                match result with
                | "copied" -> showToast 1500 "Copied results to clipboard"
                | "failed" -> showToast 1500 "Couldn't share results"
                | _ -> ()))

    let keyboardKey = keyboardChar state.UsedLetters onKeyClick
    let contrastClass = if highContrast then "high-contrast" else ""

    html
        $"""
        <div class="game {contrastClass} flex flex-col bg-stone-900">
            <div class="flex-none">
                <div class="relative flex items-center justify-between px-2 border-b border-neutral-600" style="height:var(--header-h)">
                    <button @click={onModalClick Info} aria-label="About and settings" class="p-2 text-white">
                        <svg xmlns="http://www.w3.org/2000/svg" class="h-7 w-7" fill="none" viewBox="0 0 24 24" stroke="currentColor" stroke-width="2">
                            <path stroke-linecap="round" stroke-linejoin="round" d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
                        </svg>
                    </button>
                    <div class="aurelia-header">Aureliadle</div>
                    <div class="flex">
                        <button @click={onModalClick Stats} aria-label="Statistics" class="p-2 text-white mr-1">
                            <svg xmlns="http://www.w3.org/2000/svg" class="h-7 w-7" viewBox="0 0 20 20" fill="currentColor">
                                <path d="M2 11a1 1 0 011-1h2a1 1 0 011 1v5a1 1 0 01-1 1H3a1 1 0 01-1-1v-5zM8 7a1 1 0 011-1h2a1 1 0 011 1v9a1 1 0 01-1 1H9a1 1 0 01-1-1V7zM14 4a1 1 0 011-1h2a1 1 0 011 1v12a1 1 0 01-1 1h-2a1 1 0 01-1-1V4z" />
                            </svg>
                        </button>
                        <button @click={onModalClick Help} aria-label="Help" class="p-2 text-white">
                            <svg xmlns="http://www.w3.org/2000/svg" class="h-7 w-7" fill="none" viewBox="0 0 24 24" stroke="currentColor" stroke-width="2">
                                <path stroke-linecap="round" stroke-linejoin="round" d="M8.228 9c.549-1.165 2.03-2 3.772-2 2.21 0 4 1.343 4 3 0 1.4-1.278 2.575-3.006 2.907-.542.104-.994.54-.994 1.093m0 3h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
                            </svg>
                        </button>
                    </div>
                </div>
                <div class="hint-bar">
                    <button @click={onModalClick Help} class="hint-button" aria-label="Today's sound: {state.Phonics.Hint}. Show spellings">
                        <span class="hint-label">Today's sound</span>
                        <span class="hint-sound">{state.Phonics.Hint}</span>
                    </button>
                </div>
            </div>

            {modal "About" (infoText highContrast onToggleHighContrast) showInfoModal (onModalClick Info)}
            {modal "Game Statistics" (statsText state onShare) showStatsModal (onModalClick Stats)}
            {modal "Grapheme Phoneme Correspondence" (helpText state) showHelpModal (onModalClick Help)}
            {LostModal state}
            {toastView toast}

            <div class="flex-1 min-h-0 flex flex-col justify-center">
                {state.Guesses |> List.map boardRow}
            </div>

            <div class="flex-none keyboard-safe-area">
                <div class="keyboard-row">
                    {keyBoard.Top |> List.map keyboardKey}
                </div>
                <div class="keyboard-row">
                    <div class="keyboard-spacer"></div>
                    {keyBoard.Middle |> List.map keyboardKey}
                    <div class="keyboard-spacer"></div>
                </div>
                <div class="keyboard-row">
                    {keyBoard.Bottom |> List.map keyboardKey}
                </div>
            </div>
        </div>
    """
