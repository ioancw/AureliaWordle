/// Phonics helpers: matching graphemes (spellings) inside words.
module Phonics

open Domain

/// Pairs each letter of the word with a colour: the grapheme's letters red, the rest green.
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
