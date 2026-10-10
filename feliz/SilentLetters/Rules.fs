/// Silent Letters: spell words with a letter you write but don't say. Each word has a picture
/// and a clue, and a gap where its silent letter goes (_nife); pick the letter. Six words a day,
/// getting harder, and three hearts. A wrong letter costs a heart and you try again.
module SilentLetters.Rules

open System
open Engine
open Thoth.Json.Core

let perDay = 6
let hearts = 3
let levels = 3

type Word =
    { Family: int
      Level: int
      /// The word with its silent letter: Text.[Gap] is the silent letter.
      Text: string
      Gap: int
      Emoji: string
      Clue: string
      /// Three letters to pick from, one of them the silent letter, in alphabetical order.
      Choices: string list
      Tip: string }

/// Silent letter patterns: (level, letters to choose from besides the answer, the rule).
/// The last family has no rule of its own: each of its words has its own.
let private families =
    [| 1, [ "c"; "g" ], "K is silent before N at the start of a word: knife, knee, know."
       1, [ "h"; "r" ], "W is silent before R at the start of a word: write, wrong, wrap."
       2, [ "e"; "p" ], "B is silent after M at the end of a word: lamb, thumb, climb."
       2, [ "h"; "k" ], "G is silent before N: gnome, gnat, sign."
       2, [ "r"; "u" ], "L is often silent: walk, half, calm, yolk, could."
       2, [ "e"; "r" ], "In wh words you can hardly hear the H: whale, wheel, white."
       3, [ "e"; "w" ], "H is silent in hour and honest, and after G, R or C: ghost, rhino, school."
       3, [ "d"; "s" ], "T is silent in -sten and -stle: listen, castle, whistle."
       3, [ "e"; "m" ], "N is silent after M at the end of a word: autumn, column, hymn."
       3, [ "k"; "s" ], "C is silent after S before E or I: science, scissors, scene."
       3, [ "a"; "i" ], "U is silent after G or B: guitar, guess, build, biscuit."
       3, [ "h"; "u" ], "W is silent in answer, sword, two and who."
       3, [], "" |]

let private odd = families.Length - 1

/// A word with its silent letter in brackets: "[k]nife".
let private wordWith family (marked: string) emoji clue (others: string list) tip =
    let gap = marked.IndexOf '['
    let text = marked.Replace("[", "").Replace("]", "")
    let level, familyOthers, familyTip = families.[family]
    let answer = string text.[gap]

    { Family = family
      Level = level
      Text = text
      Gap = gap
      Emoji = emoji
      Clue = clue
      Choices = answer :: (if others.IsEmpty then familyOthers else others) |> List.sort
      Tip = if tip = "" then familyTip else tip }

let private word family marked emoji clue = wordWith family marked emoji clue [] ""

/// A word with a rule of its own.
let private oddOne marked emoji clue others tip = wordWith odd marked emoji clue others tip

let bank: Word array =
    [| // kn
       word 0 "[k]nife" "🔪" "You cut with it"
       word 0 "[k]nee" "🦵" "The bend in the middle of your leg"
       word 0 "[k]nock" "🚪" "Tap tap tap on the door"
       word 0 "[k]not" "🪢" "Tie a bow or one of these in your laces"
       word 0 "[k]night" "🛡️" "A soldier in armour on a horse"
       word 0 "[k]now" "🧠" "To have it in your head"
       word 0 "[k]nit" "🧶" "Make a scarf with wool and needles"
       word 0 "[k]nuckle" "✊" "A bump on your finger when you make a fist"
       word 0 "[k]nob" "🔘" "A round handle on a door"
       word 0 "[k]neel" "🧎" "Go down so your legs touch the floor"
       word 0 "[k]nickers" "🩲" "Pants!"
       // wr
       word 1 "[w]rite" "✏️" "Put words on paper"
       word 1 "[w]rong" "❌" "Not right"
       word 1 "[w]rist" "⌚" "Where you wear a watch"
       word 1 "[w]rap" "🎁" "Cover a present in paper"
       word 1 "[w]reck" "🚢" "A smashed-up ship"
       word 1 "[w]riggle" "🪱" "What a worm does"
       word 1 "[w]ren" "🐦" "A tiny brown bird"
       word 1 "[w]rinkle" "👵" "A line on an old face"
       word 1 "[w]rote" "📝" "Did some writing yesterday"
       // mb
       word 2 "lam[b]" "🐑" "A baby sheep"
       word 2 "thum[b]" "👍" "Your shortest, fattest finger"
       word 2 "clim[b]" "🧗" "Go up a tree or a wall"
       word 2 "com[b]" "🪮" "Tidy your hair with it"
       word 2 "crum[b]" "🍞" "A tiny bit of bread"
       word 2 "bom[b]" "💣" "It goes BOOM"
       word 2 "num[b]" "🥶" "So cold you can't feel your fingers"
       word 2 "plum[b]er" "🔧" "Fixes your taps and pipes"
       word 2 "tom[b]" "⚰️" "Where a mummy is buried"
       // gn
       word 3 "[g]nome" "🧙" "A little garden man with a pointy hat"
       word 3 "[g]nat" "🦟" "A tiny flying insect"
       word 3 "si[g]n" "🪧" "A board with words that tell you something"
       word 3 "[g]naw" "🐭" "What a mouse does to cheese"
       word 3 "desi[g]n" "📐" "Draw a plan for something new"
       word 3 "rei[g]n" "👑" "How long a king or queen rules for"
       // l
       word 4 "wa[l]k" "🚶" "Go along on your feet"
       word 4 "ta[l]k" "🗣️" "Say words out loud"
       word 4 "cha[l]k" "🖍️" "Draw on the playground with it"
       word 4 "ha[l]f" "🌗" "One of two equal parts"
       word 4 "ca[l]f" "🐄" "A baby cow"
       word 4 "ca[l]m" "😌" "Quiet and still, not cross"
       word 4 "pa[l]m" "🌴" "A tree on the beach, or the inside of your hand"
       word 4 "yo[l]k" "🥚" "The yellow part of an egg"
       word 4 "cou[l]d" "🏊" "Was able to: I ... swim when I was five"
       word 4 "shou[l]d" "🪥" "Ought to: you ... brush your teeth"
       // wh
       word 5 "w[h]ale" "🐋" "The biggest animal in the sea"
       word 5 "w[h]eel" "🛞" "A car has four of them"
       word 5 "w[h]ite" "⬜" "The colour of snow"
       word 5 "w[h]isper" "🤫" "Talk very quietly"
       word 5 "w[h]iskers" "🐱" "A cat's long face hairs"
       word 5 "w[h]en" "⏰" "... is your birthday?"
       word 5 "w[h]ere" "📍" "... do you live?"
       // h
       word 6 "[h]our" "⏳" "Sixty minutes"
       word 6 "[h]onest" "😇" "Always telling the truth"
       word 6 "g[h]ost" "👻" "A spooky see-through spirit"
       word 6 "r[h]ino" "🦏" "A big grey animal with a horn"
       word 6 "r[h]yme" "🎵" "Cat and hat do it"
       word 6 "sc[h]ool" "🏫" "Where you go to learn"
       word 6 "c[h]aracter" "🎭" "A person in a story"
       // t
       word 7 "lis[t]en" "👂" "Use your ears"
       word 7 "cas[t]le" "🏰" "Where a king and queen live"
       word 7 "whis[t]le" "😗" "Blow through your lips to make a tune"
       word 7 "fas[t]en" "💺" "Do up your seatbelt"
       word 7 "Chris[t]mas" "🎄" "The 25th of December"
       word 7 "this[t]le" "🌵" "A prickly purple weed"
       word 7 "glis[t]en" "✨" "Shine and sparkle"
       // mn
       word 8 "autum[n]" "🍂" "The season when leaves fall"
       word 8 "colum[n]" "🏛️" "A tall stone pillar"
       word 8 "hym[n]" "🎶" "A song sung in church"
       // sc
       word 9 "s[c]issors" "✂️" "You cut paper with them"
       word 9 "s[c]ience" "🔬" "Experiments and test tubes"
       word 9 "s[c]ene" "🎬" "Part of a play or a film"
       word 9 "mus[c]le" "💪" "It makes your arm strong"
       // u
       word 10 "g[u]itar" "🎸" "You strum its strings"
       word 10 "g[u]ess" "❓" "Have a go when you don't know"
       word 10 "g[u]ard" "💂" "Keeps watch at the palace"
       word 10 "g[u]ide" "🧭" "Show someone the way"
       word 10 "b[u]ild" "🧱" "Make something with bricks"
       word 10 "bisc[u]it" "🍪" "A crunchy snack to dunk in milk"
       // w
       word 11 "ans[w]er" "🙋" "Reply to a question"
       word 11 "s[w]ord" "⚔️" "A knight's long sharp blade"
       word 11 "t[w]o" "2️⃣" "One more than one"
       word 11 "[w]ho" "🚪" "... is at the door?"
       // odd ones, each with a rule of its own
       oddOne "i[s]land" "🏝️" "Land with sea all around it" [ "e"; "l" ] "The S in island is silent: is-land is said eye-land."
       oddOne "We[d]nesday" "📅" "The day after Tuesday" [ "n"; "t" ] "The first D in Wednesday is silent. Say it Wed-nes-day to spell it!"
       oddOne "dou[b]t" "🤔" "Not be sure" [ "p"; "u" ] "The B in doubt is silent."
       oddOne "cu[p]board" "🗄️" "Where the cups and plates are kept" [ "b"; "e" ] "The P in cupboard is silent: it's a board for cups."
       oddOne "sa[l]mon" "🐟" "A pink fish" [ "m"; "r" ] "The L in salmon is silent."
       oddOne "ya[c]ht" "⛵" "A smart sailing boat" [ "g"; "t" ] "The C in yacht is silent: it sounds like yot." |]

/// Six words for each day of the year, getting harder: two easy, two medium, two hard,
/// each from a different family. (Park-Miller generator: JavaScript and .NET agree exactly.)
let puzzles: int list array =
    Array.init 366 (fun day ->
        let seed = ref (int64 day * 6007L + 29L)

        let next () =
            seed.Value <- seed.Value * 16807L % 2147483647L
            seed.Value

        let order = Array.init bank.Length id

        for i in order.Length - 1 .. -1 .. 1 do
            let j = int (next () % int64 (i + 1))
            let t = order.[i]
            order.[i] <- order.[j]
            order.[j] <- t

        let pick level =
            order
            |> Array.filter (fun i -> bank.[i].Level = level)
            |> Array.distinctBy (fun i -> bank.[i].Family)
            |> Array.truncate 2
            |> List.ofArray

        pick 1 @ pick 2 @ pick 3)

type State =
    { Words: int list
      /// How many words are done.
      Current: int
      Mistakes: int
      /// Words (by position) that needed more than one try.
      Missed: int list
      /// Wrong letters tried on the current word.
      Wrong: string list
      /// Words (bank numbers) got wrong on earlier days, oldest first. Up to two are
      /// practised each day; one leaves the list when it's spelt right first time.
      Practice: int list }

/// A letter picked for the gap: by the letter, or by its position (0, 1, 2) among the choices.
type Input =
    | Pick of string
    | Choose of int

let start words =
    { Words = words
      Current = 0
      Mistakes = 0
      Missed = []
      Wrong = []
      Practice = [] }

let currentWord state =
    state.Words |> List.tryItem state.Current |> Option.map (fun i -> bank.[i])

let outcome state =
    if state.Mistakes >= hearts then Some Failed
    elif state.Current >= state.Words.Length then Some(Solved(state.Mistakes + 1))
    else None

let private praise = [| "Shh! Well done!"; "Correct!"; "Brilliant!"; "Yes!"; "Super speller!"; "Spot on!" |]

let apply input state =
    let letter =
        match input, currentWord state with
        | Pick l, _ -> l.ToLowerInvariant()
        | Choose i, Some w -> List.tryItem i w.Choices |> Option.defaultValue ""
        | Choose _, None -> ""

    match currentWord state, outcome state with
    | Some w, None when List.contains letter w.Choices ->
        if letter = string w.Text.[w.Gap] then
            { state with Current = state.Current + 1; Wrong = [] }, Some praise.[state.Current % praise.Length]
        elif List.contains letter state.Wrong then
            state, None
        else
            { state with
                Mistakes = state.Mistakes + 1
                Missed = (if List.contains state.Current state.Missed then state.Missed else state.Missed @ [ state.Current ])
                Wrong = state.Wrong @ [ letter ] },
            Some "Not quite, try again"
    | _ -> state, None

// Practice

let practicePerDay = 2

/// The most words kept to practise; the oldest drop off beyond this.
let practiceLimit = 12

/// Today's words with up to two practice ones swapped in, each in place of one of today's at
/// the same level (preferring the same family), so the day still gets harder.
let withPractice (practice: int list) (words: int list) =
    let already = practice |> List.filter (fun p -> List.contains p words)

    let toPlace =
        practice
        |> List.filter (fun p -> not (List.contains p words))
        |> List.truncate (max 0 (practicePerDay - already.Length))

    let place (ws: int list, replaced: int list) (p: int) =
        let fits i = not (List.contains i replaced) && bank.[ws.[i]].Level = bank.[p].Level
        let positions = [ 0 .. ws.Length - 1 ] |> List.filter fits

        let chosen =
            positions
            |> List.tryFind (fun i -> bank.[ws.[i]].Family = bank.[p].Family)
            |> Option.orElse (List.tryHead positions)

        match chosen with
        | Some i -> List.updateAt i p ws, i :: replaced
        | None -> ws, replaced

    toPlace |> List.fold place (words, []) |> fst

/// A new day: words got wrong last time join the practice list, practice words spelt right
/// first time leave it, and up to two are swapped into today's words.
let carryOver (last: State) (today: State) =
    let doneFirstTime =
        [ for i in 0 .. min last.Current last.Words.Length - 1 do
              if not (List.contains i last.Missed) then
                  last.Words.[i] ]

    let missed = last.Missed |> List.choose (fun i -> List.tryItem i last.Words)

    let practice =
        (last.Practice |> List.filter (fun p -> not (List.contains p doneFirstTime))) @ missed
        |> List.distinct
        |> List.rev
        |> List.truncate practiceLimit
        |> List.rev

    { today with
        Words = withPractice practice today.Words
        Practice = practice }

/// Whether the current word is one being practised.
let isPractice state =
    match List.tryItem state.Current state.Words with
    | Some w -> List.contains w state.Practice
    | None -> false

// Saving

let encode state =
    Encode.object
        [ "words", state.Words |> List.map Encode.int |> Encode.list
          "current", Encode.int state.Current
          "mistakes", Encode.int state.Mistakes
          "missed", state.Missed |> List.map Encode.int |> Encode.list
          "wrong", state.Wrong |> List.map Encode.string |> Encode.list
          "practice", state.Practice |> List.map Encode.int |> Encode.list ]

// Fields are read first and checked afterwards (Thoth's object builder keeps running after a failure).
let decoder: Decoder<State> =
    Decode.object (fun get ->
        get.Required.Field "words" (Decode.list Decode.int),
        get.Required.Field "current" Decode.int,
        get.Required.Field "mistakes" Decode.int,
        get.Optional.Field "missed" (Decode.list Decode.int),
        get.Optional.Field "wrong" (Decode.list Decode.string),
        get.Optional.Field "practice" (Decode.list Decode.int))
    |> Decode.andThen (fun (words, current, mistakes, missed, wrong, practice) ->
        let inBank = List.forall (fun i -> i >= 0 && i < bank.Length)

        if inBank words && current >= 0 && mistakes >= 0 then
            Decode.succeed
                { Words = words
                  Current = min current words.Length
                  Mistakes = mistakes
                  Missed = missed |> Option.defaultValue []
                  Wrong = wrong |> Option.defaultValue []
                  Practice = practice |> Option.defaultValue [] |> List.filter (fun i -> i >= 0 && i < bank.Length) }
        else
            Decode.fail "Invalid Silent Letters game")

let scoreText outcome =
    match outcome with
    | Some(Solved 1) -> "no mistakes"
    | Some(Solved 2) -> "1 mistake"
    | Some(Solved n) -> $"{n - 1} mistakes"
    | Some Failed -> "out of hearts"
    | None -> "unfinished"

/// One square per word: green first time, yellow after a mistake, white not reached.
let shareGrid (highContrast: bool) state =
    let squares =
        [ for i in 0 .. state.Words.Length - 1 ->
              if i >= state.Current then "⬜"
              elif List.contains i state.Missed then (if highContrast then "🟦" else "🟨")
              else (if highContrast then "🟧" else "🟩") ]
        |> String.concat ""

    let heartsLeft = max 0 (hearts - state.Mistakes)
    squares + " " + String.replicate heartsLeft "❤️" + String.replicate (hearts - heartsLeft) "🤍"

let game: DailyGame<int list, State, Input> =
    { Id = "silentletters"
      Title = "Silent Letters"
      FirstDay = DateTime(2026, 10, 10)
      Puzzles = puzzles
      // the stats chart counts mistakes: finished with 0, 1 or 2
      MaxAttempts = hearts
      Start = start
      Apply = apply
      Outcome = outcome
      Encode = encode
      Decoder = decoder
      ScoreText = scoreText
      ShareGrid = shareGrid
      Legacy = None
      CarryOver = Some carryOver }
