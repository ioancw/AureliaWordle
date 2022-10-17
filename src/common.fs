module Common

open Domain

module Display =
    let parseWordGrapheme grapheme word =

        let word = word |> Seq.toList
        let wordLength = word |> List.length

        let graphemes =
            let graphemeList = grapheme |> Seq.toList
            let graphemeLength = List.length graphemeList
            let foundIndex =
                List.windowed graphemeLength word
                |> List.tryFindIndex (fun w ->
                    match (w, graphemeList) with
                    | t, g when t = g -> true
                    | h :: m :: t, h1 :: m1 :: t1 when m1 = '-' && h = h1 && t = t1 -> true 
                    | _ -> false)
            match foundIndex with
            | Some index -> List.init graphemeLength (fun i -> i + index)
            | None -> []
        
        //assume all yellows, then populate the greens if any
        List.init wordLength (fun _ -> DarkGreen)
        |> List.mapi (fun i v ->
            if List.contains i graphemes
            then DarkRed
            else v)
        |> List.zip word 

module Letter =
    let toString guessLetter = defaultArg guessLetter.Letter ""

    let toUpper (letter: string) = letter.ToUpper()

    let unpack guessLetter =
        toString guessLetter |> toUpper, guessLetter.Status

    let toOption letter =
        if letter = "" then
            None
        else
            Some letter

module Guess =
    let guessToWord guess =
        guess.Letters
        |> List.map (Letter.toString >> Letter.toUpper)
        |> Seq.fold (+) ""

    let getLetter (_, guess) = guess.Letters |> List.map Letter.unpack

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

let keyBoard =
    { Top =
        [ "q"
          "w"
          "e"
          "r"
          "t"
          "y"
          "u"
          "i"
          "o"
          "p" ]
      Middle =
        [ "a"
          "s"
          "d"
          "f"
          "g"
          "h"
          "j"
          "k"
          "l" ]
      Bottom =
        [ "Ent"
          "z"
          "x"
          "c"
          "v"
          "b"
          "n"
          "m"
          "Del" ] }
