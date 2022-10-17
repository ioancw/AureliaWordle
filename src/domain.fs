module Domain

type KeyBoard =
    { Top: string list
      Middle: string list
      Bottom: string list }

type Modal =
    | Info
    | Help
    | Stats

type Status =
    | Green
    | Yellow
    | Grey
    | Black
    | Invalid
    | DarkRed
    | DarkGreen
    | DarkYellow

type GuessLetter =
    { Letter: string option
      Status: Status }

type LocalStorageGameState =
    { Guesses: (int * (string * string) []) [] //guess contains a phonic hint.
      Solution: string
      Round: int
      State: string
      GamesWon: int
      GamesLost: int
      WinDistribution: int [] }

type Position = int

type Guess =
    { Letters: GuessLetter list }

type GameState =
    | NotStarted
    | Won
    | Lost
    | Started

type State =
    { Wordle: string
      Hint: string
      Grapheme: string
      Guesses: (Position * Guess) list
      ShowInfo: bool
      ShowStats: bool
      ShowHelp: bool
      UsedLetters: Map<string, Status>
      State: GameState
      Round: int
      GamesWon: int
      GamesLost: int
      WinDistribution: int list }
