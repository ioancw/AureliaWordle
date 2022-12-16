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

type HelpTextColour =
    | HintInvalid
    | HintBlack
    | DarkRed
    | DarkGreen
    | DarkYellow

type Position = int

type LocalStorageGameState =
    { Wordle: string
      Guesses: (Position * (string * string) []) []
      State: string
      Round: int
      GamesWon: int
      GamesLost: int
      WinDistribution: int [] }

type GuessLetter =
    { Letter: string option
      Status: Status }

type Guess =
    { Letters: GuessLetter list }

type GameState =
    | NotStarted
    | Started
    | Won
    | Lost
    
type Phonics =
    { Hint: string
      Grapheme: string }

type State =
    { Wordle: string
      Phonics: Phonics
      Guesses: (Position * Guess) list
      UsedLetters: Map<string, Status>
      State: GameState
      Round: int
      GamesWon: int
      GamesLost: int
      WinDistribution: int list }
