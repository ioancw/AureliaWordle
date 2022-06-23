module Tests

open System
open Xunit
open Lit.Wordle


[<Fact>]
let ``Test masker`` () =
    let actual = applyAnswerMaskToGuess "FAVOR" "AROSE"
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
    let actual = applyAnswerMaskToGuess "AROSE" "SPEED"
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
    let actual = applyAnswerMaskToGuess "TREAT" "SPEED"
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
