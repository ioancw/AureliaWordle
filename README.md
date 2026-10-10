# Lit.AureliaWordle

Originally forked from https://aaronmu.github.io/MathGame/

This is designed for R and Y1/Y2 children to use phonics, so that a hint is provided for one of the phonic sounds in the word.
As an example:
If the wordle is RAINS, then the phonic hint with be /ai/.
The sound /ai/ is also a phonic hint for the AY graphemes, so it would also be a valid
hint for SPRAY and.

Further ideas
* phonic keyboard, i.e. the button represents the phonic, which also allows you to choose the corresponding grapheme
    e.g. if the button is /ai/ then it would show AI, AY etc
* Automated parsing of words into phonemes.

## Development

Requires the [.NET 8 SDK](https://dotnet.microsoft.com/download) and Node.js 20+.
[Fable](https://fable.io) compiles the F# in `src/` to JavaScript, and [Fable.Lit](https://github.com/fable-compiler/Fable.Lit) renders it.

```bash
npm install     # also restores the Fable tool
npm start       # dev server with live reload
npm test        # run the F# unit tests in test2/
npm run build   # optimised site in dist/
```

## Code layout

The F# in `src/` is compiled in this order:

| File | What it does |
|---|---|
| `Domain.fs` | The game's types |
| `Words.fs` | The daily wordles, their phonic hints, and the dictionary of valid guesses |
| `Phonics.fs` | Finding a grapheme (spelling) inside a word |
| `GameRules.fs` | Building, validating and scoring guesses; turning key presses into the next state |
| `Daily.fs` | Which puzzle is today's |
| `Storage.fs` | Saving and loading via local storage, using Thoth.Json |
| `Game.fs` | Resuming today's game from a save, or starting a fresh one |
| `Components.fs` | Tiles, keyboard keys and messages |
| `Modals.fs` | The About, Help, Statistics and answer pop-ups |
| `App.fs` | The app component: state, keyboard input and layout |

Saved games use the same JSON shape as earlier versions, so players keep their stats.
`Storage` and `Game` are tested on .NET with the same Thoth.Json decoders the browser uses.

## Daily-games engine with Feliz (experimental)

`feliz/` is a generic engine for "puzzle of the day" games, with Aureliadle ported onto it and a
second game, Numberdle, to prove it's generic. Built with Fable, [Feliz](https://zaid-ajaj.github.io/Feliz/) (React)
and Elmish.

| Folder | What it is |
|---|---|
| `feliz/Engine/Engine.fs` | Pure F#: today's puzzle, saving (Thoth.Json), resuming, new days, stats and streaks |
| `feliz/Engine/BrowserStorage.fs` | Local storage, including reading a game's older save format |
| `feliz/Engine/Shell.fs` | The app around any game (Feliz + Elmish): header, pop-ups, messages, stats, share, settings, keyboard and tab sync |
| `feliz/Aureliadle/` | Aureliadle's rules (reusing `src/` unchanged) and its board, keyboard and help |
| `feliz/Numberdle/` | Guess the number from 1 to 100: rules, board and keypad |
| `feliz/Tests/` | Engine and game tests on .NET |
| `feliz/site/` | The HTML pages |

A game is a `DailyGame` record (its rules: start, apply an input, outcome, save format, share grid)
plus a `GameView` (its board, controls and help). Everything else comes from the engine.
Aureliadle on the engine reads saves from the live version, so players keep their stats.

```bash
npm run test:feliz    # engine and game tests
npm run build:feliz   # both games into dist-feliz/
```

## Deployment

GitHub Actions (`.github/workflows/build-deploy.yml`) runs the tests and builds the site on every pull request.
Every merge to `main` is also deployed to GitHub Pages (the `gh-pages` branch), so there's no need to publish by hand.
`npm run publish` still works as a manual fallback.
