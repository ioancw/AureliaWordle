module Tests

open System
open Xunit
open Lit.Wordle


[<Fact>]
let ``My test`` () =
    Assert.True(true)

[<Fact>]
let ``Test masker`` () =
    let actual = getAnswerMask "FAVOR" "AROSE"
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