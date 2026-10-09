/// Which puzzle is today's.
module Daily

open System
open Words

/// Days since the first puzzle; also the puzzle number shown when sharing.
let dayNumberOn (date: DateTime) =
    let startDate = DateTime(2022, 6, 4)
    let day = DateTime(date.Year, date.Month, date.Day)
    // round rather than truncate, so a 23 or 25 hour day (clocks changing) still counts as one
    round ((day - startDate).TotalHours / 24.) |> int

let dayNumber () = dayNumberOn DateTime.Now

/// The wordle, its phonic hint and the grapheme the hint refers to, for a given day.
let puzzleFor day : string * string * string =
    wordles.[day % wordles.Length]

let todaysPuzzle () = puzzleFor (dayNumber ())
