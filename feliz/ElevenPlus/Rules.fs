/// 11+ Fractions: ten multiple-choice questions a day in the style of the 11+ maths paper:
/// mixed numbers and improper fractions, multiplying and dividing fractions, fractions of
/// amounts, and comparing decimals with fractions. Each answer is followed by a worked
/// solution; the wrong choices are the usual mistakes. Scored out of ten.
module ElevenPlus.Rules

open System
open Engine
open Thoth.Json.Core

// Exact fractions

type Rational = { N: int; D: int }

let rec private gcd a b = if b = 0 then abs a else gcd b (a % b)

let rational n d =
    let g = max 1 (gcd n d)
    let s = if d < 0 then -1 else 1
    { N = s * n / g; D = s * d / g }

let private times a b = rational (a.N * b.N) (a.D * b.D)
let private over a b = rational (a.N * b.D) (a.D * b.N)
let private plus a b = rational (a.N * b.D + b.N * a.D) (a.D * b.D)
let private whole n = rational n 1

/// A number as it's written in a question or an answer.
type Num =
    | Whole of int
    | Frac of int * int
    | Mixed of int * int * int
    /// Digits and decimal places: Dec(65, 2) is 0.65.
    | Dec of int * int
    | Words of string

let valueOf num =
    match num with
    | Whole n -> whole n
    | Frac(n, d) -> rational n d
    | Mixed(w, n, d) -> rational (w * d + n) d
    | Dec(digits, places) -> rational digits (pown 10 places)
    | Words _ -> whole 0

/// The simplest way to write a value: a whole number, a proper fraction or a mixed number.
let simplest (x: Rational) =
    if x.D = 1 then Whole x.N
    elif x.N < x.D then Frac(x.N, x.D)
    else Mixed(x.N / x.D, x.N % x.D, x.D)

let show num =
    match num with
    | Whole n -> string n
    | Frac(n, d) -> $"{n}/{d}"
    | Mixed(w, n, d) -> $"{w} {n}/{d}"
    | Dec(digits, places) ->
        // no trailing zeros: 0.80 is written 0.8
        let rec trim d p = if p > 0 && d % 10 = 0 then trim (d / 10) (p - 1) else d, p
        let digits, places = trim digits places
        let s = string digits
        let s = if s.Length <= places then String('0', places - s.Length + 1) + s else s
        s.Substring(0, s.Length - places) + "." + s.Substring(s.Length - places)
    | Words s -> s

let private showValue x = show (simplest x)

/// A value as a decimal for explanations: exact if it has up to three places, otherwise ≈.
let decimalOf (x: Rational) =
    let thousandths = float x.N * 1000.0 / float x.D
    let rounded = Math.Round thousandths
    let text = (rounded / 1000.0).ToString("0.###", Globalization.CultureInfo.InvariantCulture)
    if abs (thousandths - rounded) < 1e-9 then text else "≈ " + text

// Questions

type Question =
    | ToImproper of int * int * int
    | ToMixed of int * int
    | Simplify of int * int
    | FractionOf of int * int * int
    | FracTimesWhole of int * int * int
    | MixedTimesWhole of int * int * int * int
    | FracTimesFrac of int * int * int * int
    | FracDivWhole of int * int * int
    | WholeDivFrac of int * int * int
    | FracDivFrac of int * int * int * int
    | MixedTimesMixed of int * int * int * int * int * int
    | MixedDivWhole of int * int * int * int
    /// Is the decimal or the fraction bigger, or are they equal?
    | Compare of Num * Num
    | Largest of Num list

/// Part of a question: words, or a number written properly.
type Token =
    | T of string
    | N of Num

let prompt question =
    match question with
    | ToImproper(w, n, d) -> [ T "Write"; N(Mixed(w, n, d)); T "as an improper fraction" ]
    | ToMixed(n, d) -> [ T "Write"; N(Frac(n, d)); T "as a mixed number" ]
    | Simplify(n, d) -> [ T "Write"; N(Frac(n, d)); T "in its simplest form" ]
    | FractionOf(n, d, a) -> [ T "What is"; N(Frac(n, d)); T "of"; N(Whole a); T "?" ]
    | FracTimesWhole(n, d, k) -> [ N(Frac(n, d)); T "×"; N(Whole k); T "=" ]
    | MixedTimesWhole(w, n, d, k) -> [ N(Mixed(w, n, d)); T "×"; N(Whole k); T "=" ]
    | FracTimesFrac(a, b, c, d) -> [ N(Frac(a, b)); T "×"; N(Frac(c, d)); T "=" ]
    | FracDivWhole(n, d, k) -> [ N(Frac(n, d)); T "÷"; N(Whole k); T "=" ]
    | WholeDivFrac(k, n, d) -> [ N(Whole k); T "÷"; N(Frac(n, d)); T "=" ]
    | FracDivFrac(a, b, c, d) -> [ N(Frac(a, b)); T "÷"; N(Frac(c, d)); T "=" ]
    | MixedTimesMixed(w1, n1, d1, w2, n2, d2) -> [ N(Mixed(w1, n1, d1)); T "×"; N(Mixed(w2, n2, d2)); T "=" ]
    | MixedDivWhole(w, n, d, k) -> [ N(Mixed(w, n, d)); T "÷"; N(Whole k); T "=" ]
    | Compare(a, b) -> [ T "Which is bigger:"; N a; T "or"; N b; T "?" ]
    | Largest _ -> [ T "Which is the largest?" ]

let same = Words "They're equal"

/// The right answer.
let answerOf question =
    match question with
    | ToImproper(w, n, d) -> Frac(w * d + n, d)
    | ToMixed(n, d) -> simplest (rational n d)
    | Simplify(n, d) -> simplest (rational n d)
    | FractionOf(n, d, a) -> Whole(a / d * n)
    | FracTimesWhole(n, d, k) -> simplest (rational (n * k) d)
    | MixedTimesWhole(w, n, d, k) -> simplest (times (rational (w * d + n) d) (whole k))
    | FracTimesFrac(a, b, c, d) -> simplest (rational (a * c) (b * d))
    | FracDivWhole(n, d, k) -> simplest (rational n (d * k))
    | WholeDivFrac(k, n, d) -> simplest (rational (k * d) n)
    | FracDivFrac(a, b, c, d) -> simplest (rational (a * d) (b * c))
    | MixedTimesMixed(w1, n1, d1, w2, n2, d2) -> simplest (times (rational (w1 * d1 + n1) d1) (rational (w2 * d2 + n2) d2))
    | MixedDivWhole(w, n, d, k) -> simplest (rational (w * d + n) (d * k))
    | Compare(a, b) ->
        let va, vb = valueOf a, valueOf b
        let c = compare (va.N * vb.D) (vb.N * va.D)
        if c > 0 then a elif c < 0 then b else same
    | Largest items -> items |> List.maxBy (fun x -> float (valueOf x).N / float (valueOf x).D)

/// Wrong answers worth offering: the usual mistakes, as values.
let private usualMistakes question =
    let r = rational

    match question with
    | ToImproper(w, n, d) -> [ r (w + n) d; r (w * n + d) d; r (w * d + n + d) d; r (w * d - n) d ]
    | ToMixed(n, d) -> [ r (n / d * d + d - n % d) d; r (n + d) d; plus (r n d) (whole 1) ]
    | Simplify(n, d) -> [ r (n / gcd n d + 1) (d / gcd n d); r (n / gcd n d) (d / gcd n d + 1); r (d / gcd n d - n / gcd n d) (d / gcd n d) ]
    | FractionOf(n, d, a) -> [ r (a / d) 1; r (a - a / d * n) 1; r (a / d * (n + 1)) 1; r (a / d * n + n) 1 ]
    // multiplying top and bottom; multiplying the bottom; adding
    | FracTimesWhole(n, d, k) -> [ r n (d * k); r (n + k) d; r (n * k) (d * k) ]
    // multiplying just the whole number, or just the fraction
    | MixedTimesWhole(w, n, d, k) -> [ plus (whole (w * k)) (r n d); plus (whole w) (r (n * k) d); r ((w * d + n) * k) (d * k); plus (r ((w * d + n) * k) d) (whole 1) ]
    // dividing instead, or adding the bottoms
    | FracTimesFrac(a, b, c, d) -> [ r (a * d) (b * c); r (a * c) (b + d); r (a + c) (b + d) ]
    | FracDivWhole(n, d, k) -> [ r (n * k) d; r n (d + k); r (n * k) (d * k) ]
    | WholeDivFrac(k, n, d) -> [ r (k * n) d; r k (n * d); r n (k * d) ]
    // multiplying instead, flipping the wrong fraction
    | FracDivFrac(a, b, c, d) -> [ r (a * c) (b * d); r (b * c) (a * d); r (a + d) (b + c) ]
    // multiplying the wholes and the fractions separately
    | MixedTimesMixed(w1, n1, d1, w2, n2, d2) -> [ plus (whole (w1 * w2)) (r (n1 * n2) (d1 * d2)); plus (whole (w1 * w2)) (r (n1 + n2) (d1 + d2)); r ((w1 * d1 + n1) * (w2 * d2 + n2)) (d1 + d2) ]
    | MixedDivWhole(w, n, d, k) -> [ r ((w * d + n) * k) d; plus (r w k) (r n d); r (w * d + n) (d + k) ]
    | Compare _
    | Largest _ -> []

/// A small number from a seed, for shuffling choices (Park-Miller, so JavaScript and .NET agree).
let private seedOf (ints: int list) =
    ints |> List.fold (fun s i -> (s * 31L + int64 (abs i) + 7L) % 2147483647L) 17L |> max 1L

let private encodeQuestion question =
    let num n =
        match n with
        | Dec(a, b) -> [ 0; a; b ]
        | Frac(a, b) -> [ 1; a; b ]
        | Whole a -> [ 2; a; 1 ]
        | _ -> [ 3; 0; 0 ]

    match question with
    | ToImproper(a, b, c) -> "improper", [ a; b; c ]
    | ToMixed(a, b) -> "mixed", [ a; b ]
    | Simplify(a, b) -> "simplify", [ a; b ]
    | FractionOf(a, b, c) -> "of", [ a; b; c ]
    | FracTimesWhole(a, b, c) -> "fxw", [ a; b; c ]
    | MixedTimesWhole(a, b, c, d) -> "mxw", [ a; b; c; d ]
    | FracTimesFrac(a, b, c, d) -> "fxf", [ a; b; c; d ]
    | FracDivWhole(a, b, c) -> "f/w", [ a; b; c ]
    | WholeDivFrac(a, b, c) -> "w/f", [ a; b; c ]
    | FracDivFrac(a, b, c, d) -> "f/f", [ a; b; c; d ]
    | MixedTimesMixed(a, b, c, d, e, f) -> "mxm", [ a; b; c; d; e; f ]
    | MixedDivWhole(a, b, c, d) -> "m/w", [ a; b; c; d ]
    | Compare(a, b) -> "compare", num a @ num b
    | Largest items -> "largest", items |> List.collect num

/// What kind of question it is, for putting practice questions in the right place.
let kindOf question = fst (encodeQuestion question)

/// The choices: the answer and three wrong ones (two for comparisons), in a fixed shuffled order.
let choices question =
    let answer = answerOf question

    let options =
        match question with
        | Compare(a, b) -> [ a; b; same ]
        | Largest items -> items
        | _ ->
            let value = valueOf answer
            let asImproper = match question with ToImproper _ -> true | _ -> false
            let write (x: Rational) = if asImproper then Frac(x.N, x.D) else simplest x

            // nearby values, in case the usual mistakes run out
            let near =
                [ for k in 1..6 do
                      plus value (rational k value.D)
                      plus value (rational -k value.D)
                      plus value (whole k) ]

            let simplifyQuestion = match question with Simplify _ -> true | _ -> false

            // for simplest form, a fraction that isn't fully simplified is a fair wrong answer
            let notSimplest =
                match question with
                | Simplify(n, d) ->
                    let g = gcd n d
                    [ for f in 2..g - 1 do
                          if g % f = 0 then Frac(n / f, d / f) ]
                | _ -> []

            let wrong =
                (notSimplest
                 @ (usualMistakes question @ near
                    |> List.filter (fun x -> x.N > 0 && x.D > 0 && (x.D > 1 || not asImproper) && x <> value)
                    |> List.distinct
                    |> List.map write))
                |> List.filter (fun w -> w <> answer && (simplifyQuestion || valueOf w <> value))
                |> List.distinct
                |> List.truncate 3

            answer :: wrong

    // shuffle
    let seed = ref (seedOf (snd (encodeQuestion question)))
    let a = Array.ofList options

    for i in a.Length - 1 .. -1 .. 1 do
        seed.Value <- seed.Value * 16807L % 2147483647L
        let j = int (seed.Value % int64 (i + 1))
        let t = a.[i]
        a.[i] <- a.[j]
        a.[j] <- t

    List.ofArray a

/// A fraction with its decimal: 3/4 = 0.75, 2/3 ≈ 0.667.
let private withDecimal x =
    let d = decimalOf (valueOf x)
    if d.StartsWith "≈" then $"{show x} {d}" else $"{show x} = {d}"

/// A worked solution.
let explain question =
    let f n d = show (Frac(n, d))
    let answer = show (answerOf question)
    let thenSimplify (x: Rational) unsimplified = if show (simplest x) = unsimplified then "" else $" = {show (simplest x)}"

    match question with
    | ToImproper(w, n, d) ->
        $"{w} wholes are {f (w * d) d} ({w} × {d} = {w * d}). Add the {f n d}: {w * d} + {n} = {w * d + n}, so {show (Mixed(w, n, d))} = {f (w * d + n) d}."
    | ToMixed(n, d) ->
        $"{n} ÷ {d} = {n / d} remainder {n % d}, so {f n d} = {n / d} {f (n % d) d}{thenSimplify (rational n d) (show (Mixed(n / d, n % d, d)))}."
    | Simplify(n, d) ->
        let g = gcd n d
        $"The biggest number that divides {n} and {d} is {g}. {n} ÷ {g} = {n / g} and {d} ÷ {g} = {d / g}, so {f n d} = {answer}."
    | FractionOf(n, d, a) ->
        $"First find {f 1 d}: {a} ÷ {d} = {a / d}. Then {f n d} is {n} of those: {a / d} × {n} = {a / d * n}."
    | FracTimesWhole(n, d, k) ->
        $"Multiply the top by {k}: {n} × {k} = {n * k}, so it's {f (n * k) d}{thenSimplify (rational (n * k) d) (f (n * k) d)}."
    | MixedTimesWhole(w, n, d, k) ->
        let top = w * d + n
        $"Make it improper: {show (Mixed(w, n, d))} = {f top d}. Times {k}: {f (top * k) d}{thenSimplify (rational (top * k) d) (f (top * k) d)}. Or: {w} × {k} = {w * k}, and {f n d} × {k} = {f (n * k) d}; {w * k} + {f (n * k) d} = {answer}. Don't forget to multiply the fraction too!"
    | FracTimesFrac(a, b, c, d) ->
        $"Multiply the tops and the bottoms: {a} × {c} = {a * c} and {b} × {d} = {b * d}, so {f (a * c) (b * d)}{thenSimplify (rational (a * c) (b * d)) (f (a * c) (b * d))}."
    | FracDivWhole(n, d, k) ->
        $"Dividing by {k} is the same as multiplying by {f 1 k}: {f n d} × {f 1 k} = {f n (d * k)}{thenSimplify (rational n (d * k)) (f n (d * k))}."
    | WholeDivFrac(k, n, d) ->
        $"Keep, change, flip: {k} × {f d n} = {f (k * d) n}{thenSimplify (rational (k * d) n) (f (k * d) n)}. (How many {f n d}s fit into {k}? {answer}.)"
    | FracDivFrac(a, b, c, d) ->
        $"Keep, change, flip: {f a b} × {f d c} = {f (a * d) (b * c)}{thenSimplify (rational (a * d) (b * c)) (f (a * d) (b * c))}."
    | MixedTimesMixed(w1, n1, d1, w2, n2, d2) ->
        let t1, t2 = w1 * d1 + n1, w2 * d2 + n2
        $"Make them both improper: {f t1 d1} × {f t2 d2} = {f (t1 * t2) (d1 * d2)}{thenSimplify (rational (t1 * t2) (d1 * d2)) (f (t1 * t2) (d1 * d2))}. (Multiplying the wholes and the fractions separately doesn't work!)"
    | MixedDivWhole(w, n, d, k) ->
        let top = w * d + n
        $"Make it improper: {show (Mixed(w, n, d))} = {f top d}. Dividing by {k} multiplies the bottom by {k}: {f top (d * k)}{thenSimplify (rational top (d * k)) (f top (d * k))}."
    | Compare(a, b) ->
        let asDecimal x =
            match x with
            | Frac _ -> withDecimal x
            | _ -> show x

        let verdict = if answerOf question = same then "They're equal." else $"{answer} is bigger."
        $"Turn the fraction into a decimal: {asDecimal a}, {asDecimal b}. {verdict}"
    | Largest items ->
        let parts =
            items
            |> List.map (fun x ->
                match x with
                | Frac _ -> withDecimal x
                | _ -> show x)
            |> String.concat ", "

        $"Write them all as decimals: {parts}. The largest is {answer}."

// Each day's questions

let perDay = 10

/// The level of each question: three warm-ups, four tricky ones, three hard ones.
let levelOf i = if i < 3 then 1 elif i < 7 then 2 else 3

let puzzles: Question list array =
    Array.init 366 (fun day ->
        let seed = ref (int64 day * 48271L + 101L)

        let next () =
            seed.Value <- seed.Value * 16807L % 2147483647L
            seed.Value

        let between lo hi = lo + int (next () % int64 (hi - lo + 1))
        let oneOf (items: 'a list) = items.[int (next () % int64 items.Length)]

        /// A proper fraction in its simplest form, with a bottom number from lo to hi.
        let proper lo hi =
            let d = between lo hi
            let ns = [ 1 .. d - 1 ] |> List.filter (fun n -> gcd n d = 1)
            oneOf ns, d

        let toImproper () =
            let n, d = proper 2 10
            ToImproper(between 1 5, n, d)

        let warmUp () =
            match between 0 2 with
            | 0 ->
                let n, d = proper 2 9
                ToMixed(between 1 5 * d + n, d)
            | 1 ->
                let n, d = proper 3 10
                let g = between 2 6
                Simplify(n * g, d * g)
            | _ ->
                let n, d = proper 3 10
                FractionOf(n, d, d * between 2 12)

        // a decimal near a fraction: easy ones are at least 0.1 apart, close ones nearer
        let compareQuestion close =
            let n, d = oneOf [ 1, 2; 1, 4; 3, 4; 1, 5; 2, 5; 3, 5; 4, 5; 1, 8; 3, 8; 5, 8; 7, 8; 1, 3; 2, 3; 7, 10; 3, 10; 9, 20; 11, 20 ]
            let value = float n / float d
            let places = if close then 2 else 1
            let scale = pown 10 places
            let exact = float scale * value

            let digits =
                if close then
                    // equal a fifth of the time when the fraction has an exact two-place decimal
                    if abs (exact - Math.Round exact) < 1e-9 && next () % 5L = 0L then
                        int (Math.Round exact)
                    else
                        let offset = between 1 5 * (if next () % 2L = 0L then 1 else -1)
                        let digits = int (Math.Round exact) + offset
                        if digits = int (Math.Round exact) then digits + 1 else digits
                else
                    let offset = between 1 4 * (if next () % 2L = 0L then 1 else -1)
                    int (Math.Floor exact) + (if offset > 0 then offset else offset + 1)

            let digits = max 1 (min (scale - 1) digits)
            let dec = Dec(digits, places)
            let frac = Frac(n, d)

            // nudge an accidental tie on an easy question
            let dec =
                if not close && valueOf dec = rational n d then Dec(digits + (if digits < scale - 1 then 1 else -1), places) else dec

            if next () % 2L = 0L then Compare(dec, frac) else Compare(frac, dec)

        let fracTimesWhole () =
            let n, d = proper 3 12
            FracTimesWhole(n, d, between 2 9)

        let mixedTimesWhole () =
            let n, d = proper 2 8
            MixedTimesWhole(between 1 4, n, d, between 2 6)

        let tricky () =
            match between 0 2 with
            | 0 ->
                let a, b = proper 2 9
                let c, d = proper 2 9
                FracTimesFrac(a, b, c, d)
            | 1 ->
                let n, d = proper 2 9
                FracDivWhole(n, d, between 2 6)
            | _ ->
                let n, d = proper 2 8
                WholeDivFrac(between 2 9, n, d)

        let fracDivFrac () =
            let a, b = proper 2 9
            let rec other () =
                let c, d = proper 2 9
                if (c, d) = (a, b) then other () else c, d
            let c, d = other ()
            FracDivFrac(a, b, c, d)

        let hard () =
            match between 0 2 with
            | 0 ->
                let n1, d1 = proper 2 4
                let n2, d2 = proper 2 5
                MixedTimesMixed(between 1 3, n1, d1, between 1 2, n2, d2)
            | 1 ->
                let n, d = proper 2 6
                MixedDivWhole(between 1 5, n, d, between 2 5)
            | _ ->
                let n, d = proper 3 9
                MixedTimesWhole(between 3 6, n, d, between 4 9)

        let largest () =
            // two fractions and two decimals, all different, between a quarter and a whole
            let rec pick (acc: Num list) =
                if acc.Length = 4 then
                    acc
                else
                    let candidate =
                        if acc.Length % 2 = 0 then
                            let n, d = proper 3 12
                            Frac(n, d)
                        else
                            let digits = between 25 95
                            if digits % 10 = 0 then Dec(digits / 10, 1) else Dec(digits, 2)

                    let v = valueOf candidate
                    let fine = float v.N / float v.D >= 0.25 && acc |> List.forall (fun x -> valueOf x <> v)
                    pick (if fine then acc @ [ candidate ] else acc)

            Largest(pick [])

        [ toImproper ()
          warmUp ()
          compareQuestion false
          fracTimesWhole ()
          mixedTimesWhole ()
          tricky ()
          compareQuestion true
          fracDivFrac ()
          hard ()
          largest () ])

// Playing

type State =
    { Questions: Question list
      /// The choice picked for each question answered so far.
      Picked: int list
      /// Questions got wrong on earlier days, oldest first; up to two come back each day.
      Practice: Question list }

type Input = Choose of int

let start questions =
    { Questions = questions
      Picked = []
      Practice = [] }

let isRight question picked =
    List.tryItem picked (choices question) = Some(answerOf question)

let results state =
    List.zip (List.truncate state.Picked.Length state.Questions) state.Picked
    |> List.map (fun (q, p) -> isRight q p)

let mistakes state = results state |> List.filter not |> List.length

let currentQuestion state = List.tryItem state.Picked.Length state.Questions

let outcome state =
    if state.Picked.Length >= state.Questions.Length then Some(Solved(mistakes state + 1)) else None

let private praise = [| "Correct!"; "Spot on!"; "Brilliant!"; "Yes!"; "Super!"; "Well done!" |]

let apply (Choose index) state =
    match currentQuestion state with
    | Some q when index >= 0 && index < (choices q).Length ->
        let next = { state with Picked = state.Picked @ [ index ] }
        let message = if isRight q index then praise.[state.Picked.Length % praise.Length] else "Not quite: see how it's done"
        next, Some message
    | _ -> state, None

/// Whether the current question is one being practised.
let isPractice state =
    match currentQuestion state with
    | Some q -> List.contains q state.Practice
    | None -> false

// Practice

let practicePerDay = 2
let practiceLimit = 12

/// Today's questions with up to two practice ones swapped in, each in place of one of the
/// same kind if there is one, otherwise one at the same level.
let withPractice (practice: Question list) (questions: Question list) =
    let levelOfKind q =
        match q with
        | ToImproper _ | ToMixed _ | Simplify _ | FractionOf _ -> 1
        | Compare(_, Dec(_, 1)) | Compare(Dec(_, 1), _) -> 1
        | FracDivFrac _ | MixedTimesMixed _ | MixedDivWhole _ | Largest _ -> 3
        | _ -> 2

    let place (qs: Question list, replaced: int list) p =
        let free = [ 0 .. qs.Length - 1 ] |> List.filter (fun i -> not (List.contains i replaced))

        let chosen =
            free
            |> List.tryFind (fun i -> kindOf qs.[i] = kindOf p)
            |> Option.orElse (free |> List.tryFind (fun i -> levelOf i = levelOfKind p))

        match chosen with
        | Some i -> List.updateAt i p qs, i :: replaced
        | None -> qs, replaced

    practice
    |> List.filter (fun p -> not (List.contains p questions))
    |> List.truncate practicePerDay
    |> List.fold place (questions, [])
    |> fst

/// A new day: questions got wrong last time join the practice list, practice ones got right
/// leave it, and up to two are put into today's questions.
let carryOver (last: State) (today: State) =
    let answered = List.zip (List.truncate last.Picked.Length last.Questions) (results last)
    let gotRight = answered |> List.filter snd |> List.map fst
    let missed = answered |> List.filter (snd >> not) |> List.map fst

    let practice =
        (last.Practice |> List.filter (fun p -> not (List.contains p gotRight))) @ missed
        |> List.distinct
        |> List.rev
        |> List.truncate practiceLimit
        |> List.rev

    { today with
        Questions = withPractice practice today.Questions
        Practice = practice }

// Saving

let private encodeQ q =
    let tag, ints = encodeQuestion q
    Encode.list (Encode.string tag :: (ints |> List.map Encode.int))

let private questionDecoder: Decoder<Question> =
    let parts =
        Decode.index 0 Decode.string
        |> Decode.andThen (fun tag ->
            Decode.list (Decode.oneOf [ Decode.int; Decode.string |> Decode.map (fun _ -> 0) ])
            |> Decode.map (fun xs -> tag, List.tail xs))

    let num kind a b =
        match kind with
        | 0 -> Some(Dec(a, b))
        | 1 -> Some(Frac(a, b))
        | 2 -> Some(Whole a)
        | _ -> None

    let rec nums (xs: int list) =
        match xs with
        | k :: a :: b :: rest ->
            match num k a b, nums rest with
            | Some n, Some ns -> Some(n :: ns)
            | _ -> None
        | [] -> Some []
        | _ -> None

    parts
    |> Decode.andThen (fun (tag, xs) ->
        let q =
            match tag, xs with
            | "improper", [ a; b; c ] -> Some(ToImproper(a, b, c))
            | "mixed", [ a; b ] -> Some(ToMixed(a, b))
            | "simplify", [ a; b ] -> Some(Simplify(a, b))
            | "of", [ a; b; c ] -> Some(FractionOf(a, b, c))
            | "fxw", [ a; b; c ] -> Some(FracTimesWhole(a, b, c))
            | "mxw", [ a; b; c; d ] -> Some(MixedTimesWhole(a, b, c, d))
            | "fxf", [ a; b; c; d ] -> Some(FracTimesFrac(a, b, c, d))
            | "f/w", [ a; b; c ] -> Some(FracDivWhole(a, b, c))
            | "w/f", [ a; b; c ] -> Some(WholeDivFrac(a, b, c))
            | "f/f", [ a; b; c; d ] -> Some(FracDivFrac(a, b, c, d))
            | "mxm", [ a; b; c; d; e; f ] -> Some(MixedTimesMixed(a, b, c, d, e, f))
            | "m/w", [ a; b; c; d ] -> Some(MixedDivWhole(a, b, c, d))
            | "compare", xs ->
                match nums xs with
                | Some [ a; b ] -> Some(Compare(a, b))
                | _ -> None
            | "largest", xs ->
                match nums xs with
                | Some ns when ns.Length >= 2 -> Some(Largest ns)
                | _ -> None
            | _ -> None

        // every number in a question is positive, and every bottom number is too
        match q with
        | Some q when snd (encodeQuestion q) |> List.forall (fun i -> i >= 0) && (choices q).Length >= 2 -> Decode.succeed q
        | _ -> Decode.fail "Invalid question")

let encode state =
    Encode.object
        [ "questions", state.Questions |> List.map encodeQ |> Encode.list
          "picked", state.Picked |> List.map Encode.int |> Encode.list
          "practice", state.Practice |> List.map encodeQ |> Encode.list ]

// Fields are read first and checked afterwards (Thoth's object builder keeps running after a failure).
let decoder: Decoder<State> =
    Decode.object (fun get ->
        get.Required.Field "questions" (Decode.list questionDecoder),
        get.Required.Field "picked" (Decode.list Decode.int),
        get.Optional.Field "practice" (Decode.list questionDecoder))
    |> Decode.andThen (fun (questions, picked, practice) ->
        if picked.Length <= questions.Length && List.forall2 (fun q p -> p >= 0 && p < (choices q).Length) (List.truncate picked.Length questions) picked then
            Decode.succeed
                { Questions = questions
                  Picked = picked
                  Practice = practice |> Option.defaultValue [] }
        else
            Decode.fail "Invalid 11+ game")

let scoreText outcome =
    match outcome with
    | Some(Solved n) -> $"{perDay - (n - 1)}/{perDay}"
    | Some Failed -> $"0/{perDay}"
    | None -> "unfinished"

/// One square per question: right or wrong.
let shareGrid (highContrast: bool) state =
    results state
    |> List.map (fun ok -> if ok then (if highContrast then "🟧" else "🟩") else "🟥")
    |> String.concat ""

let game: DailyGame<Question list, State, Input> =
    { Id = "elevenplus"
      Title = "11+ Fractions"
      FirstDay = DateTime(2026, 10, 11)
      Puzzles = puzzles
      // the stats chart counts mistakes, from 0 to 10
      MaxAttempts = perDay + 1
      Start = start
      Apply = apply
      Outcome = outcome
      Encode = encode
      Decoder = decoder
      ScoreText = scoreText
      ShareGrid = shareGrid
      Legacy = None
      CarryOver = Some carryOver }
