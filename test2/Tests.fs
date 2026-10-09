module Tests

open System
open Xunit
open FsUnit.Xunit
open Lit.Wordle
open Words
open Domain
open GameRules
open Phonics


[<Fact>]
let ``Test masker`` () =
    let actual = Score.scoreGuess "FAVOR" "AROSE"
    let expected =
        { Letters =
            [
                { Letter = Some("A"); Status = Yellow }
                { Letter = Some("R"); Status = Yellow }
                { Letter = Some("O"); Status = Yellow }
                { Letter = Some("S"); Status = Grey }
                { Letter = Some("E"); Status = Grey }
            ]
        }
    Assert.Equal(expected, actual)

[<Fact>]
let ``Test masker double letter no green`` () =
    let actual = Score.scoreGuess "AROSE" "SPEED"
    let expected =
        { Letters =
            [
                { Letter = Some("S"); Status = Yellow }
                { Letter = Some("P"); Status = Grey }
                { Letter = Some("E"); Status = Yellow }
                { Letter = Some("E"); Status = Grey }
                { Letter = Some("D"); Status = Grey }
            ]
        }
    Assert.Equal(expected, actual)

[<Fact>]
let ``Test masker double letter with one green`` () =
    let actual = Score.scoreGuess "TREAT" "SPEED"
    let expected =
        { Letters =
            [
                { Letter = Some("S"); Status = Grey }
                { Letter = Some("P"); Status = Grey }
                { Letter = Some("E"); Status = Green }
                { Letter = Some("E"); Status = Grey }
                { Letter = Some("D"); Status = Grey }
            ]
        }
    Assert.Equal(expected, actual)

type TestType () =
    static member TestProperty
        with get() : obj[] list =
        [
            [| "TREAT"; "SPEED"; [Grey; Grey; Green; Grey; Grey] |]
            [| "AROSE"; "SPEED"; [Yellow; Grey; Yellow; Grey; Grey] |]
            [| "FAVOR"; "AROSE"; [Yellow; Yellow; Yellow; Grey; Grey] |]
            [| "FAVOR"; "RATIO"; [Yellow; Green; Grey; Grey; Yellow] |]
            [| "FAVOR"; "CAROL"; [Grey; Green; Yellow; Green; Grey] |]
            [| "FAVOR"; "VAPOR"; [Yellow; Green; Grey; Green; Green] |]
        ]

    [<Theory>]
    [<MemberData("TestProperty")>]
    member t.TestMethod (wordle: string) (guess: string) (expectedMask: Status list) =
        let actual = Score.getAnswerMask wordle guess
        Assert.Equal(expectedMask, actual)

[<Fact>]
let ``Keyboard status when letters have been used`` ()=
    let keyboardStatus = Map.empty
    let guesses =
        [
            { Letter = Some("S"); Status = Grey }
            { Letter = Some("P"); Status = Grey }
            { Letter = Some("E"); Status = Green }
            { Letter = Some("E"); Status = Grey }
            { Letter = Some("D"); Status = Grey }
        ]

    let expectedKeyboardStatus=
        [
            "S", Grey;
            "P", Grey;
            "E", Green;
            "D", Grey
        ]
        |> Map.ofList

    Play.updateKeyboardState guesses keyboardStatus |> should equal expectedKeyboardStatus

[<Fact>]
let ``Keyboard status new Yellow status `` ()=
    let initialKeyboardStatus=
        [
            "S", Grey;
            "P", Grey;
            "E", Green;
            "D", Grey
        ]
        |> Map.ofList

    let guesses =
        [
            { Letter = Some("S"); Status = Grey }
            { Letter = Some("T"); Status = Grey }
            { Letter = Some("A"); Status = Yellow }
            { Letter = Some("I"); Status = Grey }
            { Letter = Some("N"); Status = Grey }
        ]

    let expectedKeyboardStatus=
        [
            "S", Grey
            "P", Grey
            "E", Green
            "D", Grey
            "T", Grey
            "A", Yellow
            "I", Grey
            "N", Grey
        ]
        |> Map.ofList

    Play.updateKeyboardState guesses initialKeyboardStatus |> should equal expectedKeyboardStatus

[<Fact>]
let ``Keyboard status old Yellow is now Green `` ()=
    let initialKeyboardStatus=
        [
            "S", Grey
            "P", Grey
            "E", Green
            "D", Grey
            "T", Grey
            "A", Yellow
            "I", Grey
            "N", Grey
        ]
        |> Map.ofList

    let guesses =
        [
            { Letter = Some("S"); Status = Grey }
            { Letter = Some("T"); Status = Grey }
            { Letter = Some("I"); Status = Grey }
            { Letter = Some("A"); Status = Green }
            { Letter = Some("N"); Status = Grey }
        ]

    let expectedKeyboardStatus=
        [
            "S", Grey
            "P", Grey
            "E", Green
            "D", Grey
            "T", Grey
            "A", Green
            "I", Grey
            "N", Grey
        ]
        |> Map.ofList

    Play.updateKeyboardState guesses initialKeyboardStatus |> should equal expectedKeyboardStatus

[<Fact>]
let ``Keyboard status old Green is still Green when mask letter is Yellow `` ()=
    let initialKeyboardStatus=
        [
            "S", Grey
            "P", Grey
            "E", Green
            "D", Grey
            "T", Grey
            "A", Green
            "I", Grey
            "N", Grey
        ]
        |> Map.ofList

    let guesses =
        [
            { Letter = Some("S"); Status = Grey }
            { Letter = Some("T"); Status = Grey }
            { Letter = Some("I"); Status = Grey }
            { Letter = Some("A"); Status = Green }
            { Letter = Some("E"); Status = Yellow }
        ]

    let expectedKeyboardStatus=
        [
            "S", Grey
            "P", Grey
            "E", Green
            "D", Grey
            "T", Grey
            "A", Green
            "I", Grey
            "N", Grey
        ]
        |> Map.ofList

    Play.updateKeyboardState guesses initialKeyboardStatus |> should equal expectedKeyboardStatus

[<Fact>]
let ``Keyboard status existing greens in same position `` ()=
    let initialKeyboardStatus=
        [
            "S", Grey
            "P", Grey
            "E", Green
            "D", Grey
            "T", Grey
            "A", Green
            "I", Grey
            "N", Grey
        ]
        |> Map.ofList

    let guesses =
        [
            { Letter = Some("S"); Status = Grey }
            { Letter = Some("T"); Status = Grey }
            { Letter = Some("E"); Status = Green }
            { Letter = Some("A"); Status = Green }
            { Letter = Some("N"); Status = Grey }
        ]

    let expectedKeyboardStatus=
        [
            "S", Grey
            "P", Grey
            "E", Green
            "D", Grey
            "T", Grey
            "A", Green
            "I", Grey
            "N", Grey
        ]
        |> Map.ofList

    Play.updateKeyboardState guesses initialKeyboardStatus |> should equal expectedKeyboardStatus

[<Fact>]
let ``Keyboard status existing greens new yellows`` ()=
    let initialKeyboardStatus=
        [
            "S", Grey
            "P", Grey
            "E", Green
            "D", Grey
            "T", Grey
            "A", Green
            "I", Grey
            "N", Grey
        ]
        |> Map.ofList

    let guesses =
        [
            { Letter = Some("C"); Status = Yellow }
            { Letter = Some("R"); Status = Yellow }
            { Letter = Some("E"); Status = Green }
            { Letter = Some("A"); Status = Green }
            { Letter = Some("X"); Status = Yellow }
        ]

    let expectedKeyboardStatus=
        [
            "S", Grey
            "P", Grey
            "E", Green
            "D", Grey
            "T", Grey
            "A", Green
            "I", Grey
            "N", Grey
            "C", Yellow
            "R", Yellow
            "X", Yellow
        ]
        |> Map.ofList

    Play.updateKeyboardState guesses initialKeyboardStatus |> should equal expectedKeyboardStatus

[<Fact>]
let ``All yellows go green`` ()=
    let initialKeyboardStatus=
        [
            "S", Grey
            "P", Grey
            "E", Green
            "D", Grey
            "T", Grey
            "A", Green
            "I", Grey
            "N", Grey
            "C", Yellow
            "R", Yellow
            "X", Yellow
        ]
        |> Map.ofList

    let guesses =
        [
            { Letter = Some("R"); Status = Green }
            { Letter = Some("X"); Status = Green }
            { Letter = Some("E"); Status = Green }
            { Letter = Some("A"); Status = Green }
            { Letter = Some("C"); Status = Green }
        ]

    let expectedKeyboardStatus=
        [
            "S", Grey
            "P", Grey
            "E", Green
            "D", Grey
            "T", Grey
            "A", Green
            "I", Grey
            "N", Grey
            "C", Green
            "R", Green
            "X", Green
        ]
        |> Map.ofList

    Play.updateKeyboardState guesses initialKeyboardStatus |> should equal expectedKeyboardStatus

[<Fact>]
let ``Find phoneme in string`` () =
    let grapheme = "ie"
    let word = "fried"
    let expected =
        [
            ('f', DarkGreen)
            ('r', DarkGreen)
            ('i', DarkRed)
            ('e', DarkRed)
            ('d', DarkGreen)
        ]
    
    let test = parseWordGrapheme grapheme word
    test |> should equal expected
    
type TestTypeGrapheme () =
    static member TestProperty
        with get() : obj[] list =
        [
            [| "treat"; "ea"; [DarkGreen;DarkGreen; DarkRed; DarkRed; DarkGreen] |]
            [| "bread"; "ea"; [DarkGreen; DarkGreen; DarkRed; DarkRed; DarkGreen] |]
            [| "turn"; "ur"; [DarkGreen; DarkRed; DarkRed; DarkGreen] |]
            [| "touch"; "ch"; [DarkGreen; DarkGreen; DarkGreen; DarkRed; DarkRed] |]
            [| "straight"; "aigh"; [DarkGreen; DarkGreen; DarkGreen; DarkRed; DarkRed; DarkRed; DarkRed; DarkGreen] |]
            [| "note"; "bla"; [DarkGreen; DarkGreen; DarkGreen; DarkGreen] |] // grapheme not found, list all DarkGreens
            [| "note"; "o-e"; [DarkGreen; DarkRed; DarkRed; DarkRed] |]
            [| "note"; "a-e"; [DarkGreen; DarkGreen; DarkGreen; DarkGreen] |]
        ]

    [<Theory>]
    [<MemberData("TestProperty")>]
    member t.TestMethod (word: string) (grapheme: string) (expected: HelpTextColour list) =
        let actual = parseWordGrapheme grapheme word
        let expectedZip = List.zip (word |> Seq.toList) expected
        actual |> should equal expectedZip   
let private guessRow (word: string) statuses =
    5, { Letters = List.zip (List.ofSeq word) statuses |> List.map (fun (c, s) -> { Letter = Some(string c); Status = s }) }

let private emptyRow = 0, { Letters = List.init 5 (fun _ -> { Letter = None; Status = Black }) }

let private finishedGame state round guesses =
    { Wordle = "CHEAP"
      Phonics = { Hint = "/ee/"; Grapheme = "ea" }
      Guesses = guesses @ List.init (6 - List.length guesses) (fun _ -> emptyRow)
      UsedLetters = Map.empty
      State = state
      Round = round
      GamesWon = 0
      GamesLost = 0
      WinDistribution = List.init 6 (fun _ -> 0) }

[<Fact>]
let ``Share text has the score and one emoji row per guess`` () =
    let state =
        finishedGame Won 1
            [ guessRow "CRANE" [ Green; Grey; Yellow; Grey; Yellow ]
              guessRow "CHEAP" [ Green; Green; Green; Green; Green ] ]

    shareText false state |> should endWith "2/6\n\n🟩⬛🟨⬛🟨\n🟩🟩🟩🟩🟩"
    shareText true state |> should endWith "2/6\n\n🟧⬛🟦⬛🟦\n🟧🟧🟧🟧🟧"

[<Fact>]
let ``Share text shows X when the game is lost`` () =
    let rows = List.init 6 (fun _ -> guessRow "CRANE" [ Green; Grey; Yellow; Grey; Yellow ])
    shareText false (finishedGame Lost 5 rows) |> should haveSubstring "X/6"

[<Fact>]
let ``Only dictionary words are valid guesses`` () =
    let withGuess word = { finishedGame Started 0 [ guessRow word (List.init 5 (fun _ -> Black)) ] with State = Started }
    Validate.word (withGuess "CHEAP") |> should equal true
    Validate.word (withGuess "XQZZZ") |> should equal false
