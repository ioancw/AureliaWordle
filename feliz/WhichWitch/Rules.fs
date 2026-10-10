/// Which Witch?: pick the right word for each sentence (their / there / they're, its / it's,
/// could've / could of ...). Six sentences a day, getting harder, and three hearts. A wrong pick costs a heart and explains that word,
/// then you try again, so every sentence ends with the right answer.
module WhichWitch.Rules

open System
open Engine
open Thoth.Json.Core

let perDay = 6
let hearts = 3

/// Child-friendly meanings, shown when a word is picked by mistake and in the help.
let meanings =
    Map [ "their", "belongs to them (their bikes)"
          "there", "a place, or \"there is\" (over there)"
          "they're", "short for they are"
          "its", "belongs to it (the dog wagged its tail)"
          "it's", "short for it is, or it has"
          "your", "belongs to you (your coat)"
          "you're", "short for you are"
          "to", "towards, or before a doing word (go to school, to swim)"
          "too", "as well, or more than enough (too hot)"
          "two", "the number 2"
          "where", "which place? (where is it?)"
          "were", "was, for more than one (we were late)"
          "wear", "putting on clothes (wear a hat)"
          "which", "asks about a choice (which one?)"
          "witch", "someone with a broomstick and spells"
          "here", "this place (come here)"
          "hear", "what your ears do"
          "no", "the opposite of yes"
          "know", "have it in your head (silent k)"
          "new", "not old"
          "knew", "did know, in the past"
          "our", "belongs to us"
          "are", "we are, they are, you are"
          "of", "part of something (a cup of tea)"
          "off", "the opposite of on"
          "whose", "asks who something belongs to"
          "who's", "short for who is, or who has"
          "see", "what your eyes do"
          "sea", "lots of salty water"
          "write", "put words on paper"
          "right", "correct, or the opposite of left"
          "for", "meant for someone (a gift for you)"
          "four", "the number 4"
          // apostrophes
          "we're", "short for we are"
          "could've", "short for could have"
          "should've", "short for should have"
          "would've", "short for would have"
          "could of", "sounds right, but it's never correct: say could've (could have)"
          "should of", "sounds right, but it's never correct: say should've (should have)"
          "would of", "sounds right, but it's never correct: say would've (would have)"
          "let's", "short for let us (let's go!)"
          "lets", "allows (Mum lets me play)"
          "doesn't", "short for does not: the apostrophe is where the o was"
          "does'nt", "has the apostrophe in the wrong place: does not → doesn't"
          "can't", "short for cannot: the apostrophe is where the no was"
          "ca'nt", "has the apostrophe in the wrong place: cannot → can't"
          "isn't", "short for is not: the apostrophe is where the o was"
          "is'nt", "has the apostrophe in the wrong place: is not → isn't"
          "don't", "short for do not: the apostrophe is where the o was"
          "do'nt", "has the apostrophe in the wrong place: do not → don't"
          "didn't", "short for did not: the apostrophe is where the o was"
          "did'nt", "has the apostrophe in the wrong place: did not → didn't"
          "wasn't", "short for was not: the apostrophe is where the o was"
          "was'nt", "has the apostrophe in the wrong place: was not → wasn't"
          "aren't", "short for are not: the apostrophe is where the o was"
          "are'nt", "has the apostrophe in the wrong place: are not → aren't"
          "shouldn't", "short for should not: the apostrophe is where the o was"
          "should'nt", "has the apostrophe in the wrong place: should not → shouldn't"
          "dog's", "belongs to the dog (the dog's bone)"
          "dogs", "more than one dog"
          "brother's", "belongs to my brother"
          "brothers", "more than one brother"
          "cat's", "belongs to the cat"
          "cats", "more than one cat"
          "cow's", "belongs to the cow"
          "cows", "more than one cow"
          "girl's", "belongs to the girl"
          "girls", "more than one girl"
          "horse's", "belongs to the horse"
          "horses", "more than one horse" ]

/// Word families: (level, the usual choices). Level 1 is easy, 2 medium, 3 apostrophes.
/// Families with no usual choices give each sentence its own (e.g. could've / could of).
let private families =
    [| 2, [ "their"; "there"; "they're" ]
       2, [ "its"; "it's" ]
       2, [ "your"; "you're" ]
       2, [ "to"; "too"; "two" ]
       2, [ "where"; "were"; "wear" ]
       1, [ "which"; "witch" ]
       1, [ "here"; "hear" ]
       1, [ "no"; "know" ]
       1, [ "new"; "knew" ]
       2, [ "our"; "are" ]
       1, [ "of"; "off" ]
       3, [ "whose"; "who's" ]
       1, [ "see"; "sea" ]
       1, [ "write"; "right" ]
       1, [ "for"; "four" ]
       3, [ "we're"; "were"; "where" ]
       3, [] // could've / should've / would've versus "could of"
       3, [] // where the apostrophe goes: didn't, not did'nt
       3, [] // belonging or more than one: dog's or dogs
       3, [ "let's"; "lets" ] |]

/// The their / there / they're family, which every day includes.
let theirFamily = 0

let levels = 3

type Sentence =
    { /// "___" marks the blank.
      Text: string
      /// The word family, so a day's sentences are all different.
      Family: int
      /// 1 easy, 2 medium, 3 apostrophes.
      Level: int
      Choices: string list
      Answer: string }

let private sentence family (text: string) answer =
    { Text = text
      Family = family
      Level = fst families.[family]
      Choices = snd families.[family]
      Answer = answer }

/// A sentence with its own choices, for families that don't have usual ones.
let private sentenceWith family choices (text: string) answer =
    { sentence family text answer with Choices = choices }

let bank: Sentence array =
    [| // their / there / they're
       sentence 0 "The children put on ___ coats." "their"
       sentence 0 "I left my bag over ___." "there"
       sentence 0 "Mum says ___ coming for tea." "they're"
       sentence 0 "Is ___ any cake left?" "there"
       sentence 0 "The twins love ___ new puppy." "their"
       sentence 0 "I think ___ late for school." "they're"
       sentence 0 "We sat ___ and ate our lunch." "there"
       sentence 0 "The birds built ___ nest in the tree." "their"
       sentence 0 "Look at the ducks, ___ swimming!" "they're"
       sentence 0 "My friends forgot ___ lunch boxes." "their"
       sentence 0 "Put the books over ___, please." "there"
       sentence 0 "When ___ ready, we can go." "they're"
       sentence 0 "Is ___ a park near your house?" "there"
       sentence 0 "Ask them if ___ coming to the party." "they're"
       sentence 0 "The teachers drank ___ tea." "their"
       // its / it's
       sentence 1 "The dog wagged ___ tail." "its"
       sentence 1 "I think ___ going to rain." "it's"
       sentence 1 "The cat licked ___ paws." "its"
       sentence 1 "Hurry up, ___ time for bed!" "it's"
       sentence 1 "The tree lost all ___ leaves." "its"
       sentence 1 "Can you tell me what ___ called?" "it's"
       sentence 1 "The bird flapped ___ wings." "its"
       sentence 1 "Wear a coat because ___ cold outside." "it's"
       sentence 1 "The car has lost ___ wheel." "its"
       sentence 1 "Do you know if ___ your birthday soon?" "it's"
       // your / you're
       sentence 2 "Don't forget ___ water bottle." "your"
       sentence 2 "I think ___ very kind." "you're"
       sentence 2 "Is this ___ pencil?" "your"
       sentence 2 "Tell me when ___ ready." "you're"
       sentence 2 "Please wash ___ hands." "your"
       sentence 2 "Shout if ___ stuck." "you're"
       sentence 2 "I like ___ new shoes." "your"
       sentence 2 "Well done, ___ a star!" "you're"
       sentence 2 "Put ___ coat on the peg." "your"
       sentence 2 "Mum says ___ allowed to stay up late." "you're"
       // to / too / two
       sentence 3 "I have ___ sisters." "two"
       sentence 3 "We are going ___ the zoo." "to"
       sentence 3 "This soup is ___ hot!" "too"
       sentence 3 "Can I come ___?" "too"
       sentence 3 "She ate ___ apples." "two"
       sentence 3 "I want ___ play outside." "to"
       sentence 3 "My little brother is ___ small to ride it." "too"
       sentence 3 "We walked ___ the shop." "to"
       sentence 3 "There are ___ cats on the wall." "two"
       sentence 3 "He wants to come ___." "too"
       sentence 3 "Give the ball ___ me." "to"
       sentence 3 "Half past ___ is home time." "two"
       // where / were / wear
       sentence 4 "Do you know ___ my shoes are?" "where"
       sentence 4 "We ___ playing in the garden." "were"
       sentence 4 "I will ___ my red hat." "wear"
       sentence 4 "Tell me ___ you live." "where"
       sentence 4 "They ___ very happy." "were"
       sentence 4 "You have to ___ wellies in the rain." "wear"
       sentence 4 "Can you see ___ the cat went?" "where"
       sentence 4 "The children ___ singing a song." "were"
       sentence 4 "What will you ___ to the party?" "wear"
       sentence 4 "We ___ late for school yesterday." "were"
       // which / witch
       sentence 5 "The ___ flew on her broomstick." "witch"
       sentence 5 "Do you know ___ bus goes to town?" "which"
       sentence 5 "The ___ stirred her magic pot." "witch"
       sentence 5 "I don't know ___ cake to choose." "which"
       sentence 5 "A ___ has a black cat and a tall hat." "witch"
       sentence 5 "Tell me ___ book you like best." "which"
       // here / hear
       sentence 6 "Come over ___ and sit down." "here"
       sentence 6 "Can you ___ the birds singing?" "hear"
       sentence 6 "I have lived ___ all my life." "here"
       sentence 6 "I can't ___ you, it's too noisy!" "hear"
       sentence 6 "Put the box down ___." "here"
       sentence 6 "Did you ___ that funny noise?" "hear"
       // no / know
       sentence 7 "Do you ___ the answer?" "know"
       sentence 7 "There is ___ milk left." "no"
       sentence 7 "I ___ how to ride a bike." "know"
       sentence 7 "Mum said ___ to more sweets." "no"
       sentence 7 "Did you ___ that owls can turn their heads?" "know"
       sentence 7 "I have ___ idea where it is." "no"
       // new / knew
       sentence 8 "I got a ___ bike for my birthday." "new"
       sentence 8 "She ___ all the words to the song." "knew"
       sentence 8 "We moved to a ___ house." "new"
       sentence 8 "I ___ you would come!" "knew"
       sentence 8 "He ___ the way home." "knew"
       sentence 8 "Our class has a ___ teacher." "new"
       // our / are
       sentence 9 "We love ___ dog." "our"
       sentence 9 "They ___ going swimming." "are"
       sentence 9 "This is ___ classroom." "our"
       sentence 9 "You ___ my best friend." "are"
       sentence 9 "Come and see ___ new kittens." "our"
       sentence 9 "Where ___ my socks?" "are"
       // of / off
       sentence 10 "I had a cup ___ milk." "of"
       sentence 10 "Please turn ___ the light." "off"
       sentence 10 "He fell ___ the wall." "off"
       sentence 10 "She ate a piece ___ cake." "of"
       sentence 10 "Take ___ your muddy boots." "off"
       sentence 10 "We saw lots ___ ducks." "of"
       // whose / who's
       sentence 11 "Do you know ___ coat this is?" "whose"
       sentence 11 "Guess ___ coming to tea!" "who's"
       sentence 11 "I wonder ___ hat this is." "whose"
       sentence 11 "Tell me ___ next in the line." "who's"
       // see / sea
       sentence 12 "We swam in the ___." "sea"
       sentence 12 "I can ___ a rainbow!" "see"
       sentence 12 "Fish live in the ___." "sea"
       sentence 12 "Come and ___ my picture." "see"
       // write / right
       sentence 13 "Can you ___ your name?" "write"
       sentence 13 "Turn ___ at the shop." "right"
       sentence 13 "I got all my sums ___!" "right"
       sentence 13 "Please ___ a story about a dragon." "write"
       // for / four
       sentence 14 "I have ___ crayons in my pencil case." "four"
       sentence 14 "This present is ___ you." "for"
       sentence 14 "A cat has ___ legs." "four"
       sentence 14 "We waited ___ the bus." "for"
       // we're / were / where
       sentence 15 "Hurry up, ___ going to be late!" "we're"
       sentence 15 "I think ___ having pizza for tea." "we're"
       sentence 15 "Yesterday we ___ at the beach." "were"
       sentence 15 "Mum says ___ going on holiday soon." "we're"
       sentence 15 "We ___ so tired after the long walk." "were"
       sentence 15 "Do you know ___ the park is?" "where"
       sentence 15 "Look, ___ nearly home!" "we're"
       // could've / should've / would've
       sentenceWith 16 [ "could've"; "could of" ] "I ___ won if I had run faster." "could've"
       sentenceWith 16 [ "should of"; "should've" ] "You ___ seen the size of the cake!" "should've"
       sentenceWith 16 [ "would've"; "would of" ] "She ___ come if she wasn't ill." "would've"
       sentenceWith 16 [ "should've"; "should of" ] "We ___ brought our coats." "should've"
       sentenceWith 16 [ "could of"; "could've" ] "He ___ fallen off the wall!" "could've"
       sentenceWith 16 [ "would of"; "would've" ] "I ___ liked a bigger slice." "would've"
       sentenceWith 16 [ "should've"; "should of" ] "They ___ told us sooner." "should've"
       sentenceWith 16 [ "could've"; "could of" ] "It ___ been worse!" "could've"
       // where the apostrophe goes
       sentenceWith 17 [ "does'nt"; "doesn't" ] "The cat ___ want to come in." "doesn't"
       sentenceWith 17 [ "can't"; "ca'nt" ] "I ___ find my shoes." "can't"
       sentenceWith 17 [ "isn't"; "is'nt" ] "It ___ raining any more." "isn't"
       sentenceWith 17 [ "do'nt"; "don't" ] "We ___ go to school on Sundays." "don't"
       sentenceWith 17 [ "didn't"; "did'nt" ] "She ___ eat her peas." "didn't"
       sentenceWith 17 [ "was'nt"; "wasn't" ] "He ___ at home yesterday." "wasn't"
       sentenceWith 17 [ "aren't"; "are'nt" ] "They ___ ready yet." "aren't"
       sentenceWith 17 [ "should'nt"; "shouldn't" ] "You ___ forget your bag." "shouldn't"
       // belonging or more than one
       sentenceWith 18 [ "dog's"; "dogs" ] "The ___ bone is under the sofa." "dog's"
       sentenceWith 18 [ "dogs"; "dog's" ] "There are three ___ in the garden." "dogs"
       sentenceWith 18 [ "brother's"; "brothers" ] "My ___ name is Sam." "brother's"
       sentenceWith 18 [ "brothers"; "brother's" ] "I have two ___ and a sister." "brothers"
       sentenceWith 18 [ "cats"; "cat's" ] "The ___ tail is very fluffy." "cat's"
       sentenceWith 18 [ "cow's"; "cows" ] "We saw lots of ___ at the farm." "cows"
       sentenceWith 18 [ "girls"; "girl's" ] "That ___ hat blew away in the wind!" "girl's"
       sentenceWith 18 [ "horse's"; "horses" ] "The ___ mane was long and shiny." "horse's"
       // let's / lets
       sentence 19 "Come on, ___ go to the park!" "let's"
       sentence 19 "Mum ___ me stay up late on Fridays." "lets"
       sentence 19 "I know, ___ play a game!" "let's"
       sentence 19 "My teacher ___ us play outside." "lets"
       sentence 19 "It's sunny, so ___ have a picnic." "let's" |]

/// Six sentences for each day of the year, getting harder: two easy, two medium (always
/// including their / there / they're), then two apostrophe challenges. Each from a different
/// word family. (Park-Miller generator: JavaScript and .NET agree exactly.)
let puzzles: int list array =
    Array.init 366 (fun day ->
        let seed = ref (int64 day * 7919L + 17L)

        let next () =
            seed.Value <- seed.Value * 16807L % 2147483647L
            seed.Value

        let order = Array.init bank.Length id

        for i in order.Length - 1 .. -1 .. 1 do
            let j = int (next () % int64 (i + 1))
            let t = order.[i]
            order.[i] <- order.[j]
            order.[j] <- t

        let pick level count (except: int list) =
            order
            |> Array.filter (fun i -> bank.[i].Level = level && not (List.contains bank.[i].Family except))
            |> Array.distinctBy (fun i -> bank.[i].Family)
            |> Array.truncate count
            |> List.ofArray

        let their = order |> Array.find (fun i -> bank.[i].Family = theirFamily)
        let medium = pick 2 1 [ theirFamily ]
        // their / there / they're comes first or second of the medium pair
        let mediumPair = if next () % 2L = 0L then their :: medium else medium @ [ their ]

        pick 1 2 [] @ mediumPair @ pick 3 2 [])

type State =
    { Questions: int list
      /// How many sentences are done.
      Current: int
      Mistakes: int
      /// Sentences (by position) that needed more than one try.
      Missed: int list
      /// Wrong picks on the current sentence, most recent first.
      Wrong: string list
      /// Sentences (bank numbers) got wrong on earlier days, oldest first. Up to two are
      /// practised each day; one leaves the list when it's answered right first time.
      Practice: int list }

/// The position (0, 1, 2) of a choice in the current sentence.
type Input = Choose of int

let start questions =
    { Questions = questions
      Current = 0
      Mistakes = 0
      Missed = []
      Wrong = []
      Practice = [] }

let practicePerDay = 2

/// The most sentences kept to practise; the oldest drop off beyond this.
let practiceLimit = 12

/// Today's sentences with up to two practice ones swapped in. Each takes the place of one of
/// today's at the same level (preferring the same word family), so the day still gets harder,
/// and the day's their / there / they're sentence is only replaced by another of those.
let withPractice (practice: int list) (questions: int list) =
    let toPlace =
        practice
        |> List.filter (fun p -> not (List.contains p questions))
        |> List.truncate (max 0 (practicePerDay - (practice |> List.filter (fun p -> List.contains p questions) |> List.length)))

    let place (qs: int list, replaced: int list) (p: int) =
        let fits i =
            not (List.contains i replaced)
            && bank.[qs.[i]].Level = bank.[p].Level
            && (bank.[qs.[i]].Family <> theirFamily || bank.[p].Family = theirFamily)

        let positions = [ 0 .. qs.Length - 1 ] |> List.filter fits

        let chosen =
            positions
            |> List.tryFind (fun i -> bank.[qs.[i]].Family = bank.[p].Family)
            |> Option.orElse (List.tryHead positions)

        match chosen with
        | Some i -> List.updateAt i p qs, i :: replaced
        | None -> qs, replaced

    toPlace |> List.fold place (questions, []) |> fst

/// A new day: sentences got wrong last time join the practice list, practice sentences answered
/// right first time leave it, and up to two are swapped into today's sentences.
let carryOver (last: State) (today: State) =
    let doneFirstTime =
        [ for i in 0 .. min last.Current last.Questions.Length - 1 do
              if not (List.contains i last.Missed) then
                  last.Questions.[i] ]

    let missed = last.Missed |> List.choose (fun i -> List.tryItem i last.Questions)

    let practice =
        (last.Practice |> List.filter (fun p -> not (List.contains p doneFirstTime))) @ missed
        |> List.distinct
        |> List.rev
        |> List.truncate practiceLimit
        |> List.rev

    { today with
        Questions = withPractice practice today.Questions
        Practice = practice }

/// Whether the current sentence is one being practised.
let isPractice state =
    match List.tryItem state.Current state.Questions with
    | Some q -> List.contains q state.Practice
    | None -> false

let currentSentence state =
    state.Questions |> List.tryItem state.Current |> Option.map (fun i -> bank.[i])

let outcome state =
    if state.Mistakes >= hearts then Some Failed
    elif state.Current >= state.Questions.Length then Some(Solved(state.Mistakes + 1))
    else None

let private praise = [| "Correct!"; "Well done!"; "Brilliant!"; "Yes!"; "Super!"; "Spot on!" |]

let apply (Choose index) state =
    match currentSentence state, outcome state with
    | Some s, None ->
        match List.tryItem index s.Choices with
        | Some word when word = s.Answer ->
            { state with Current = state.Current + 1; Wrong = [] }, Some praise.[state.Current % praise.Length]
        | Some word when not (List.contains word state.Wrong) ->
            { state with
                Mistakes = state.Mistakes + 1
                Missed = (if List.contains state.Current state.Missed then state.Missed else state.Missed @ [ state.Current ])
                Wrong = word :: state.Wrong },
            Some "Not quite, try again"
        | _ -> state, None
    | _ -> state, None

let encode state =
    Encode.object
        [ "questions", state.Questions |> List.map Encode.int |> Encode.list
          "current", Encode.int state.Current
          "mistakes", Encode.int state.Mistakes
          "missed", state.Missed |> List.map Encode.int |> Encode.list
          "wrong", state.Wrong |> List.map Encode.string |> Encode.list
          "practice", state.Practice |> List.map Encode.int |> Encode.list ]

// Fields are read first and checked afterwards (Thoth's object builder keeps running after a failure).
let decoder: Decoder<State> =
    Decode.object (fun get ->
        get.Required.Field "questions" (Decode.list Decode.int),
        get.Required.Field "current" Decode.int,
        get.Required.Field "mistakes" Decode.int,
        get.Optional.Field "missed" (Decode.list Decode.int),
        get.Optional.Field "wrong" (Decode.list Decode.string),
        // saves from before practice was added don't have it
        get.Optional.Field "practice" (Decode.list Decode.int))
    |> Decode.andThen (fun (questions, current, mistakes, missed, wrong, practice) ->
        let inBank = List.forall (fun i -> i >= 0 && i < bank.Length)

        if inBank questions && current >= 0 && mistakes >= 0 then
            Decode.succeed
                { Questions = questions
                  Current = min current questions.Length
                  Mistakes = mistakes
                  Missed = missed |> Option.defaultValue []
                  Wrong = wrong |> Option.defaultValue []
                  Practice = practice |> Option.defaultValue [] |> List.filter (fun i -> i >= 0 && i < bank.Length) }
        else
            Decode.fail "Invalid Which Witch? game")

let scoreText outcome =
    match outcome with
    | Some(Solved 1) -> "no mistakes"
    | Some(Solved 2) -> "1 mistake"
    | Some(Solved n) -> $"{n - 1} mistakes"
    | Some Failed -> "out of hearts"
    | None -> "unfinished"

/// One square per sentence: green first time, yellow after a mistake, white not reached.
let shareGrid (highContrast: bool) state =
    let squares =
        [ for i in 0 .. state.Questions.Length - 1 ->
              if i >= state.Current then "⬜"
              elif List.contains i state.Missed then (if highContrast then "🟦" else "🟨")
              else (if highContrast then "🟧" else "🟩") ]
        |> String.concat ""

    let heartsLeft = max 0 (hearts - state.Mistakes)
    squares + " " + String.replicate heartsLeft "❤️" + String.replicate (hearts - heartsLeft) "🤍"

let game: DailyGame<int list, State, Input> =
    { Id = "whichwitch"
      Title = "Which Witch?"
      FirstDay = DateTime(2026, 10, 10)
      Puzzles = puzzles
      // the stats chart counts mistakes: solved with 0, 1 or 2
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
