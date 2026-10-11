module ElevenPlusTests

open Xunit
open FsUnit.Xunit
open Engine
open ElevenPlus.Rules

let private rightChoice q = choices q |> List.findIndex ((=) (answerOf q))
let private wrongChoice q = choices q |> List.findIndex ((<>) (answerOf q))

let private playDay wrongAt state =
    (state, [ 0 .. perDay - 1 ])
    ||> List.fold (fun s i ->
        match currentQuestion s with
        | Some q -> apply (Choose(if List.contains i wrongAt then wrongChoice q else rightChoice q)) s |> fst
        | None -> s)

let private isSimplify q = match q with Simplify _ -> true | _ -> false

[<Fact>]
let ``The answers are right`` () =
    answerOf (ToImproper(2, 3, 4)) |> should equal (Frac(11, 4))
    answerOf (ToMixed(17, 5)) |> should equal (Mixed(3, 2, 5))
    answerOf (Simplify(18, 24)) |> should equal (Frac(3, 4))
    answerOf (FractionOf(3, 5, 40)) |> should equal (Whole 24)
    answerOf (FracTimesWhole(3, 8, 12)) |> should equal (Mixed(4, 1, 2))
    answerOf (MixedTimesWhole(2, 1, 3, 4)) |> should equal (Mixed(9, 1, 3))
    answerOf (FracTimesFrac(2, 3, 3, 5)) |> should equal (Frac(2, 5))
    answerOf (FracDivWhole(3, 4, 3)) |> should equal (Frac(1, 4))
    answerOf (WholeDivFrac(6, 2, 3)) |> should equal (Whole 9)
    answerOf (FracDivFrac(3, 4, 1, 2)) |> should equal (Mixed(1, 1, 2))
    answerOf (MixedTimesMixed(1, 1, 2, 2, 2, 3)) |> should equal (Whole 4)
    answerOf (MixedDivWhole(3, 3, 4, 5)) |> should equal (Frac(3, 4))
    answerOf (Compare(Dec(7, 1), Frac(3, 4))) |> should equal (Frac(3, 4))
    answerOf (Compare(Frac(2, 3), Dec(65, 2))) |> should equal (Frac(2, 3))
    answerOf (Compare(Dec(75, 2), Frac(3, 4))) |> should equal same
    answerOf (Largest [ Frac(5, 8); Dec(6, 1); Frac(2, 3); Dec(65, 2) ]) |> should equal (Frac(2, 3))

[<Fact>]
let ``The wrong choices include the usual mistakes`` () =
    // multiplying only the whole number
    choices (MixedTimesWhole(2, 1, 3, 4)) |> should contain (Mixed(8, 1, 3))
    // multiplying the wholes and the fractions separately
    choices (MixedTimesMixed(1, 1, 2, 2, 2, 3)) |> should contain (Mixed(2, 1, 3))
    // multiplying instead of dividing
    choices (FracDivFrac(3, 4, 1, 2)) |> should contain (Frac(3, 8))
    // not fully simplified
    choices (Simplify(18, 24)) |> should contain (Frac(9, 12))

[<Fact>]
let ``Numbers are written properly`` () =
    show (Dec(65, 2)) |> should equal "0.65"
    show (Dec(5, 2)) |> should equal "0.05"
    show (Dec(7, 1)) |> should equal "0.7"
    show (Dec(80, 2)) |> should equal "0.8"
    show (Mixed(2, 3, 4)) |> should equal "2 3/4"
    decimalOf (rational 5 8) |> should equal "0.625"
    decimalOf (rational 2 3) |> should equal "≈ 0.667"

[<Fact>]
let ``Worked solutions show the method`` () =
    explain (ToImproper(2, 3, 4)) |> should equal "2 wholes are 8/4 (2 × 4 = 8). Add the 3/4: 8 + 3 = 11, so 2 3/4 = 11/4."
    explain (MixedTimesWhole(2, 1, 3, 4)) |> should equal "Make it improper: 2 1/3 = 7/3. Times 4: 28/3 = 9 1/3. Or: 2 × 4 = 8, and 1/3 × 4 = 4/3; 8 + 4/3 = 9 1/3. Don't forget to multiply the fraction too!"
    explain (FracDivFrac(3, 4, 1, 2)) |> should equal "Keep, change, flip: 3/4 × 2/1 = 6/4 = 1 1/2."
    explain (Compare(Dec(7, 1), Frac(3, 4))) |> should equal "Turn the fraction into a decimal: 0.7, 3/4 = 0.75. 3/4 is bigger."
    explain (Largest [ Frac(2, 3); Dec(6, 1) ]) |> should equal "Write them all as decimals: 2/3 ≈ 0.667, 0.6. The largest is 2/3."

[<Fact>]
let ``Every day has ten good questions, including improper fractions, mixed number times whole, and decimals against fractions`` () =
    for day in puzzles do
        day |> List.length |> should equal perDay
        day |> List.exists (fun q -> match q with ToImproper _ -> true | _ -> false) |> should equal true
        day |> List.exists (fun q -> match q with MixedTimesWhole _ -> true | _ -> false) |> should equal true
        day |> List.filter (fun q -> match q with Compare _ -> true | _ -> false) |> List.length |> should equal 2

        for q in day do
            let cs = choices q
            let answer = answerOf q

            // four choices (three for which is bigger), all different, exactly one right
            cs |> List.length |> should equal (match q with Compare _ -> 3 | _ -> 4)
            cs |> List.distinct |> List.length |> should equal cs.Length
            cs |> List.filter ((=) answer) |> List.length |> should equal 1

            // the wrong ones are wrong
            for c in cs do
                match c with
                | Words _ -> ()
                | _ ->
                    let v = valueOf c
                    (v.N > 0 && v.D > 0) |> should equal true
                    if c <> answer && not (isSimplify q) then v |> should not' (equal (valueOf answer))

            // a worked solution that ends with the answer
            explain q |> should haveSubstring (show answer)

[<Fact>]
let ``Questions are varied over the year`` () =
    let kinds = puzzles |> Seq.concat |> Seq.map kindOf |> Seq.distinct |> Seq.length
    kinds |> should equal 14
    let equalPairs = puzzles |> Seq.concat |> Seq.filter (fun q -> match q with Compare _ -> answerOf q = same | _ -> false) |> Seq.length
    equalPairs |> should be (greaterThan 10)

[<Fact>]
let ``Every question is answered once; the score is out of ten`` () =
    let s0 = start puzzles.[0]
    let q = (currentQuestion s0).Value
    let s1, msg = apply (Choose(wrongChoice q)) s0
    s1.Picked.Length |> should equal 1
    msg |> should equal (Some "Not quite: see how it's done")
    mistakes s1 |> should equal 1

    let perfect = s0 |> playDay []
    outcome perfect |> should equal (Some(Solved 1))
    scoreText (outcome perfect) |> should equal "10/10"

    let three = s0 |> playDay [ 2; 5; 9 ]
    outcome three |> should equal (Some(Solved 4))
    scoreText (outcome three) |> should equal "7/10"
    shareGrid false three |> should equal "🟩🟩🟥🟩🟩🟥🟩🟩🟩🟥"
    apply (Choose 0) three |> fst |> should equal three

[<Fact>]
let ``Missed questions come back the next day until got right`` () =
    let day0 = start puzzles.[0] |> playDay [ 4; 7 ]
    let missed = [ puzzles.[0].[4]; puzzles.[0].[7] ]
    let day1 = carryOver day0 (start puzzles.[1])
    day1.Practice |> should equal missed
    day1.Questions |> should contain missed.[0]
    day1.Questions |> should contain missed.[1]
    day1.Questions |> List.length |> should equal perDay
    // put in place of the same kind of question
    day1.Questions.[4] |> should equal missed.[0]

    let day2 = carryOver (day1 |> playDay []) (start puzzles.[2])
    day2.Practice |> List.isEmpty |> should equal true

[<Fact>]
let ``Saves round trip, including comparisons, and a broken game starts afresh`` () =
    let s = { start puzzles.[3] with Picked = [ 0; 1; 2 ]; Practice = [ Largest [ Frac(5, 8); Dec(6, 1); Frac(2, 3) ]; Compare(Dec(75, 2), Frac(3, 4)) ] }
    let saved = { resume game 3 None with State = s }
    saved |> toJson game |> fromJson game |> should equal (Some saved)

    let broken = (toJson game saved).Replace("\"picked\":[0,1,2]", "\"picked\":[0,1,9]")
    (fromJson game broken).Value.State |> should equal (start puzzles.[3])

