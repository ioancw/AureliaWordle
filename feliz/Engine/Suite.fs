/// The games in the suite, for the menu. Paths are relative to the site root (Aureliadle is the root).
module Suite

type GameLink =
    { Id: string
      Title: string
      Blurb: string
      Path: string }

let games =
    [ { Id = "aureliadle"
        Title = "Aureliadle"
        Blurb = "Phonics word of the day"
        Path = "" }
      { Id = "whichwitch"
        Title = "Which Witch?"
        Blurb = "Their, there or they're? Pick the right word"
        Path = "whichwitch/" }
      { Id = "numberdle"
        Title = "Numberdle"
        Blurb = "Number of the day, 1 to 100"
        Path = "numberdle/" } ]
