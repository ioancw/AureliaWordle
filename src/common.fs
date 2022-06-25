module Common

open Domain

module Letter =
    let toString guessLetter = defaultArg guessLetter.Letter ""

    let toUpper (letter: string) = letter.ToUpper()

    let unpack guessLetter =
        toString guessLetter |> toUpper, guessLetter.Status

    let toOption letter = if letter = "" then None else Some letter

module Guess =
    let guessToWord guess =
        guess.Letters
        |> List.map (Letter.toString >> Letter.toUpper)
        |> Seq.fold (+) ""

    let getLetter (_, guess) =
        guess.Letters |> List.map Letter.unpack

module List =
    let set list value pos =
        list
        |> List.mapi (fun i v -> if i = pos then value else v)

module Counter =
    let createCounter items =
        items
        |> Seq.filter (fun (a, g) -> a <> g)
        |> Seq.map fst
        |> Seq.countBy id
        |> Map.ofSeq

    let countOf counter item =
        match Map.tryFind item counter with
        | Some c -> c
        | None -> 0

    let updateCount counter item =
        match Map.tryFind item counter with
        | Some c -> Map.add item (c - 1) counter
        | None -> counter
