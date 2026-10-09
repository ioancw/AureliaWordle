module Modals

open Lit
open Words
open Domain
open Phonics
open Components

/// Creates the modal HTML. extraClass is added to the outer element, e.g. to delay its appearance.
let modalWithClass extraClass customHead bodyText modalDisplayState handler =
    let hidden =
        match modalDisplayState with
        | true -> ""
        | false -> "hidden"

    html
        $"""
        <div class="modal fixed inset-0 flex items-start justify-center {hidden} {extraClass} outline-none overflow-x-hidden overflow-y-auto z-50 pt-10 px-3">
            <div class="modal-dialog pointer-events-none w-full max-w-sm">
                <div class="modal-content border-none shadow-lg relative flex flex-col w-full pointer-events-auto bg-neutral-400 bg-clip-padding rounded-md outline-none text-current max-h-screen-3/4 overflow-y-auto">
                    <div class="modal-header flex flex-shrink-0 items-center justify-between p-1 border-b border-stone-600 rounded-t-md">
                        <h5 class="text-lg text-left font-medium leading-normal text-stone-800">
                            {customHead}
                        </h5>
                        <button type="button" @click={handler} aria-label="Close" class="px-2 py-1 bg-stone-800 text-white font-bold text-xs leading-tight uppercase rounded shadow-md">
                            X
                        </button>
                    </div>
                    {bodyText}
                </div>
            </div>
        </div>
    """

/// Creates the modal HTML.
let modal customHead bodyText modalDisplayState handler =
    modalWithClass "" customHead bodyText modalDisplayState handler

/// Information modal, including settings.
let infoText highContrast onToggleHighContrast =
    html
        $"""
        <div class="modal-body p-2 text-slate-800 space-y-3">
            <p>This is a wordle type game to help children with their phonics.</p>
            <p>For each wordle, a phonic hint is given as a phoneme (i.e. the sound).</p>
            <p>For example, if the word to be guessed is <span class="text-green-700 font-bold">SHACK</span>, then the phoneme hint given is <span class="text-red-800 font-bold">/sh/</span>.
               Not all phonemes in the word are provided, instead one of the phonemes is given in the hint.</p>
            <p>Children can use their grapheme-phoneme correspondence knowledge in order to determine the appropriate grapheme (spelling) for the phoneme in question.</p>
            <p>GPC examples for the phoneme hint can be seen by tapping the hint under the title, or the ? button.</p>
            <p>This application was developed using the <a href="https://fsharp.org" class="text-blue-700 underline">F#</a> language using <a href="https://fable.io/Fable.Lit/" class="text-blue-700 underline">Fable.Lit</a></p>
            <div class="border-t border-stone-600 pt-3">
                <label class="flex items-center justify-between gap-3 font-medium">
                    <span>
                        High contrast colours
                        <span class="block text-xs font-normal">Orange and blue instead of green and yellow, for colour vision differences.</span>
                    </span>
                    <input type="checkbox" class="h-5 w-5 shrink-0" .checked={highContrast} @change={onToggleHighContrast}>
                </label>
            </div>
        </div>
    """

/// Help modal.
let helpText state =
    //go get the graphemes from the phonemes.
    let hint = state.Phonics.Hint
    let hintedGraphemes =
        defaultArg (Map.tryFind hint phonemeGraphemeCorresspondances) []

    let graphemes =
        // fold rather than List.max, which throws on an empty list
        let maxLenGrapheme =
            hintedGraphemes
            |> List.map (fst >> String.length)
            |> List.fold max 0

        [ for grapheme, exampleWord in hintedGraphemes do
              let pad = maxLenGrapheme - (String.length grapheme)

              let padded =
                  Seq.concat [ grapheme |> Seq.map (fun g -> g, DarkRed)
                               Seq.init (pad + 1) (fun _ -> ' ', HintInvalid) ]

              html
                $"""
                <div class="flex justify-left mb-1">
                    {padded |> Seq.map littleBoxedChar}
                    {exampleWord |> parseWordGrapheme grapheme |> Seq.map littleBoxedChar}
                </div>
              """ ]

    html
        $"""
        <div class="modal-body p-2 text-slate-800 text-center">
            <p>Today's phonic hint is:</p>
            <div class="flex justify-center mb-3">
                {hint |> Seq.map (fun l -> (l, DarkYellow) |> littleBoxedChar)}
            </div>
            <p class="mb-3">The graphemes corresponding to this phoneme:</p>
            <div>{graphemes}</div>
        </div>
    """

/// The stats modal displaying histogram of results, with a share button once today's game is over.
let statsText state onShare =
    let statRow label value =
        html
            $"""
            <div class="items-center justify-center text-center">
                <div class="text-3xl font-bold mr-2">{value}</div>
                <div class="text-xs mr-2">{label}</div>
            </div>
        """

    let progress round (percent: float, label) =
        html
            $"""
            <div class="flex justify-left m-1">
                <div class="items-center justify-center w-2">{round + 1}</div>
                <div class="w-full ml-2">
                    <div class="text-xs text-right font-medium p-0.5 pr-2 bg-pink-600" style="width: {percent}%%">
                        {label}
                    </div>
                </div>
            </div>
        """

    let histogramRow =
        let maxValue = state.WinDistribution |> List.fold max 0
        // bars are relative to the most common result, with a minimum so the count stays readable
        let toPercent value =
            let percent =
                if maxValue = 0 then 0. else 100. * double value / double maxValue
            max 8. percent, value

        html
            $"""
            <div class="columns-1 justify-left m-2 text-sm text-white">
                {[ for i in [0..5] -> List.item i state.WinDistribution |> toPercent |> progress i ]}
            </div>
        """
    let totalGames = double state.GamesLost + double state.GamesWon

    let successRate =
        if totalGames = 0. then 0.
        else (double state.GamesWon / totalGames) * 100.0
        |> round

    let shareButton =
        if state.State = Won || state.State = Lost then
            html
                $"""
                <div class="flex justify-center my-3">
                    <button type="button" @click={onShare} class="share-button">
                        Share
                    </button>
                </div>
            """
        else
            Lit.nothing

    html
        $"""
        <div class="modal-body p-2">
            <div class="flex items-center justify-center my-2 m-4">
                {statRow "Games Played" totalGames}
                {statRow "Games Won" state.GamesWon}
                {statRow "Games Lost" state.GamesLost}
                {statRow "Success Rate" (sprintf "%A%%" successRate)}
            </div>
            <h4 class="flex text-lg justify-center items-center font-medium">
                Guess Distribution
            </h4>
            {histogramRow}
            {shareButton}
        </div>
    """

/// The modal displayed when the game is lost. Shows the answer to be guessed.
/// It appears once the last row has finished revealing (see .modal-after-reveal in main.css).
[<HookComponent>]
let LostModal state =
    let modalDismissed, setModalDismissed = Hook.useState false

    let onModalClick =
        Ev (fun ev ->
            ev.preventDefault ()
            setModalDismissed true)

    let displayModal = ((state.State = Lost) && not modalDismissed)
    let grapheme = state.Phonics.Grapheme.ToUpper()
    let wordle = state.Wordle

    let bodyText =
        html
            $"""
            <div class="modal-body p-2 text-slate-800 text-center">
                <p>Oh well, never mind.</p>
                <div class="flex justify-center my-2">
                    {wordle |> parseWordGrapheme grapheme |> Seq.map littleBoxedChar}
                </div>
                <p>Better luck next time.</p>
            </div>
        """

    modalWithClass "modal-after-reveal" "Today's Answer" bodyText displayModal onModalClick
