/// The engine, tested through Numberdle (the simpler game).
module EngineTests

open Xunit
open FsUnit.Xunit
open Engine
open Numberdle.Rules

let game = Numberdle.Rules.game
let day = 10

let private typeNumber n saved =
    let digits = [ for c in string n -> Digit(int c - int '0') ]
    (saved, digits @ [ Enter ]) ||> List.fold (fun s i -> play game i s |> fst)

let private todaysTarget d = puzzleFor game d

/// Plays guesses that miss (never the target), then optionally the target.
let private playDay d misses solve saved =
    let target = todaysTarget d
    let wrong = [ 1..100 ] |> List.filter ((<>) target) |> List.take misses
    let s = (resume game d saved, wrong) ||> List.fold (fun s n -> typeNumber n s)
    if solve then typeNumber target s else s

[<Fact>]
let ``The first visit starts today's puzzle with empty stats`` () =
    let s = resume game day None
    s.Day |> should equal day
    s.State.Target |> should equal (todaysTarget day)
    s.Stats |> should equal (emptyStats game)

[<Fact>]
let ``Solving counts the win, the attempts and the streak`` () =
    let s = playDay day 2 true None
    game.Outcome s.State |> should equal (Some(Solved 3))
    s.Stats.Played |> should equal 1
    s.Stats.Won |> should equal 1
    s.Stats.Distribution |> should equal [ 0; 0; 1; 0; 0; 0; 0 ]
    s.Stats.CurrentStreak |> should equal 1
    s.Stats.LastWonDay |> should equal (Some day)

[<Fact>]
let ``Input after the game is over is ignored and not counted twice`` () =
    let s = playDay day 0 true None
    let after = s |> typeNumber 50
    after |> should equal s

[<Fact>]
let ``Failing counts a game played and ends the streak`` () =
    let won = playDay day 0 true None
    let failed = playDay (day + 1) 7 false (Some won)
    game.Outcome failed.State |> should equal (Some Failed)
    failed.Stats.Played |> should equal 2
    failed.Stats.Won |> should equal 1
    failed.Stats.CurrentStreak |> should equal 0
    failed.Stats.MaxStreak |> should equal 1

[<Fact>]
let ``Streaks grow on consecutive days and reset after a missed day`` () =
    let d1 = playDay day 0 true None
    let d2 = playDay (day + 1) 1 true (Some d1)
    d2.Stats.CurrentStreak |> should equal 2
    // nothing played the next day: the streak shown is broken...
    currentStreak (day + 3) d2.Stats |> should equal 0
    // ...and a win after the gap starts again from 1
    let d4 = playDay (day + 3) 0 true (Some d2)
    d4.Stats.CurrentStreak |> should equal 1
    d4.Stats.MaxStreak |> should equal 2

[<Fact>]
let ``A new day starts a fresh puzzle and keeps the stats`` () =
    let yesterday = playDay day 1 true None
    let today = resume game (day + 1) (Some yesterday)
    today.Day |> should equal (day + 1)
    today.State |> should equal (start (todaysTarget (day + 1)))
    today.Stats |> should equal yesterday.Stats

[<Fact>]
let ``A game in progress survives saving and loading`` () =
    let s = resume game day None |> typeNumber 50 |> play game (Digit 4) |> fst
    s |> toJson game |> fromJson game |> should equal (Some s)

[<Fact>]
let ``A damaged game state starts the day afresh but keeps the stats`` () =
    let s = playDay day 1 true None
    let json = (toJson game s).Replace("\"guesses\":[", "\"guesses\":[\"oops\",")
    let loaded = fromJson game json |> Option.get
    loaded.State |> should equal (start (todaysTarget day))
    loaded.Stats |> should equal s.Stats

[<Theory>]
[<InlineData("")>]
[<InlineData("not json")>]
[<InlineData("""{"state":{}}""")>]
let ``Unreadable saves are ignored`` (json: string) =
    fromJson game json |> should equal None

[<Fact>]
let ``Refreshing picks up a newer save from another tab`` () =
    let here = resume game day None
    let otherTab = here |> typeNumber 50
    refresh game day (Some otherTab) here |> should equal otherTab

[<Fact>]
let ``Refreshing without a readable save keeps the game in memory`` () =
    let here = resume game day None |> typeNumber 50
    refresh game day None here |> should equal here

[<Fact>]
let ``Refreshing on a new day moves on to the new puzzle`` () =
    let here = playDay day 0 true None
    let next = refresh game (day + 1) None here
    next.Day |> should equal (day + 1)
    next.Stats |> should equal here.Stats

[<Fact>]
let ``Share text has the title, day, score and grid`` () =
    let s = playDay day 2 true None
    let text = shareText game false s
    text |> should startWith $"Numberdle {day} 3/7\n\n"
    text |> should endWith "✅"

[<Fact>]
let ``Days change at midnight, including when the clocks change`` () =
    dayOn game (System.DateTime(2026, 10, 10, 0, 1, 0)) |> should equal 0
    dayOn game (System.DateTime(2026, 10, 10, 23, 59, 0)) |> should equal 0
    dayOn game (System.DateTime(2026, 10, 26)) - dayOn game (System.DateTime(2026, 10, 25)) |> should equal 1
