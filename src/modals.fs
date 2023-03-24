module Modals

open Lit
open Wordles
open Domain
open Common
open Display

/// Function which creates the modal HTML.
let modal customHead bodyText modalDisplayState handler =
    let hidden =
        match modalDisplayState with
        | true -> ""
        | false -> "hidden"
    //TO DO - move all this to css.
    html
        $"""
        <div class="modal fixed inset-0 flex justify-center {hidden} outline-none overflow-x-hidden overflow-y-auto">
            <div class="modal-dialog pointer-events-none">
                <div class="modal-content border-none shadow-lg relative flex flex-col w-full pointer-events-auto bg-neutral-400 bg-clip-padding rounded-md outline-none text-current">
                    <div class="modal-header flex flex-shrink-0 items-center justify-between p-1 border-b border-stone-600 rounded-t-md">
                        <h5 class="text-lg text-left font-medium leading-normal text-stone-800">
                            {customHead}
                        </h5>
                        <button type="button" @click={handler} class="px-2
                            py-1
                            bg-stone-800
                            text-white
                            font-bold
                            text-xs
                            leading-tight
                            uppercase
                            rounded
                            shadow-md" data-bs-dismiss="modal">
                            X
                        </button>
                    </div>
                    {bodyText}
                </div>
            </div>
        </div>
    """

/// Information modal
let infoText =
    html
        $"""
        <div class="modal-body p-2 text-slate-800">
            <p>This is a wordle type game to help children with their phonics.</p>
            </br>
            <p>For each wordle, a phonic hint is given as a phoneme (i.e. the sound).</p>
            </br>
            <p>For example, if the word to be guessed is <span class="text-green-700 font-bold">SHACK</span>, then the phoneme hint given is <span class="text-red-800 font-bold">/sh/</span>.</p>
            <p>Not all phonemes in the word are provided, instead one of the phonemes is given in the hint.</p>
            </br>
            <p>Children can use their grapheme-phoneme correspondence knowledge in order to determine the appropriate</p>
            <p>grapheme (spelling) for the phoneme in question.</p>
            </br>
            <p>GPC examples for the phoneme hint can be seen by clicking the ? button.</p>
            </br>
            <p>This application was developed using the <a href="https://fsharp.org" class="text-blue-600">F#</a> language using <a href="https://fable.io/Fable.Lit/" class="text-blue-600">Fable.Lit</a></p>
        </div>
    """

/// Help modal.
let helpText state =
    //go get the graphemes from the phonemes.
    let hint = state.Phonics.Hint
    let hintedGraphemes =
        defaultArg (Map.tryFind hint phonemeGraphemeCorresspondances) []

    let graphemes =
        let maxLenGrapheme =
            hintedGraphemes
            |> List.map (fst >> String.length)
            |> List.max

        [ for grapheme, exampleWord in hintedGraphemes do
              let pad = maxLenGrapheme - (String.length grapheme)

              let padded =
                  Seq.concat [ grapheme |> Seq.map (fun g -> g, DarkRed)
                               Seq.init (pad + 1) (fun _ -> ' ', HintInvalid) ]

              html
                $"""
                <div class="flex justify-left mb-1">
                    {padded |> Seq.map littleBoxedChar}
                    {exampleWord |> DisplayUtils.parseWordGrapheme grapheme |> Seq.map littleBoxedChar}
                </div>
              """ ]

    html
        $"""
        <div class="modal-body p-2 text-slate-800 text-center">
            <p>Today's phonic hint is:
                <div class="flex justify-center mb-1">
                    {hint |> Seq.map (fun l -> (l, DarkYellow) |> littleBoxedChar)}
                </div>
            </p>
            <p>The graphemes corresponding to this phoneme:</p>
            </br>
            <p>{graphemes}</p>
        </div>
    """

/// The stats modal displaying histogram of results.
let statsText state =
    let statRow label value =
        html
            $"""
            <div class="items-center justify-center text-center">
                <div class="text-3xl font-bold mr-2">{value}</div>
                <div class="text-xs mr-2">{label}</div>
            </div>
        """

    // maybe there is a better way to do this.
    let progress round (size, label) =
        let width =
            match (int size) with
            | n when n <= 10 -> "w-1/12"
            | n when n > 10 && n <= 20 -> "w-2/12"
            | n when n > 20 && n <= 30 -> "w-3/12"
            | n when n > 30 && n <= 40 -> "w-4/12"
            | n when n > 40 && n <= 50 -> "w-5/12"
            | n when n > 50 && n <= 60 -> "w-1/2"
            | n when n > 60 && n <= 70 -> "w-7/12"
            | n when n > 70 && n <= 80 -> "w-8/12"
            | n when n > 80 && n <= 90 -> "w-9/12"
            | n when n > 90 && n <= 100 -> "w-10/12"
            | n when n > 100 && n <= 110 -> "w-11/12"
            | n when n > 110 && n <= 120 -> "w-full"
            | _ -> "w-1/12"

        html
            $"""
            <div class="flex justify-left m-1">
                <div class="items-center justify-center w-2">{round + 1}</div>
                    <div class="w-full ml-2">
                        <div class="text-xs text-right font-medium p-0.5 pr-2 {width} bg-pink-600">
                            {label}
                        </div>
                </div>
            </div>
        """

    let histogramRow=
        let maxValue = state.WinDistribution |> List.max
        let toSize value = 120. * (double value / double maxValue), value
        html
            $"""
            <div class="columns-1 justify-left m-2 text-sm text-white">
                {[ for i in [0..5] -> List.item i state.WinDistribution |> toSize |> progress i ]}
            </div>
        """
    let totalGames = double state.GamesLost + double state.GamesWon

    let successRate =
        if totalGames = 0. then 0.
        else (double state.GamesWon / totalGames) * 100.0
        |> round

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
        </div>
    """
    
/// The modal displayed when the game is lost. Shows the answer to be guessed.
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
                <p>
                    Oh well, never mind.
                    <div class="flex justify-center mb-1">
                        {wordle |> DisplayUtils.parseWordGrapheme grapheme |> Seq.map littleBoxedChar}
                    </div>
                    Better luck next time.
                </p>
            </div>
        """    

    html
        $"""
        {modal "Today's Answer" bodyText displayModal onModalClick}
    """    


