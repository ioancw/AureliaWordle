module Tests

open System
open Xunit
open FsUnit.Xunit
open Lit.Wordle


[<Fact>]
let ``Test masker`` () =
    let actual = scoreTheGuessedWord "FAVOR" "AROSE"
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
    let actual = scoreTheGuessedWord "AROSE" "SPEED"
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
    let actual = scoreTheGuessedWord "TREAT" "SPEED"
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
        let actual = getAnswerMask wordle guess
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

    getUsedLetters guesses keyboardStatus |> should equal expectedKeyboardStatus

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

    getUsedLetters guesses initialKeyboardStatus |> should equal expectedKeyboardStatus

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

    getUsedLetters guesses initialKeyboardStatus |> should equal expectedKeyboardStatus

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

    getUsedLetters guesses initialKeyboardStatus |> should equal expectedKeyboardStatus

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

    getUsedLetters guesses initialKeyboardStatus |> should equal expectedKeyboardStatus

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

    getUsedLetters guesses initialKeyboardStatus |> should equal expectedKeyboardStatus

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

    getUsedLetters guesses initialKeyboardStatus |> should equal expectedKeyboardStatus