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

## Deployment

GitHub Actions (`.github/workflows/build-deploy.yml`) runs the tests and builds the site on every pull request.
Every merge to `main` is also deployed to GitHub Pages (the `gh-pages` branch), so there's no need to publish by hand.
`npm run publish` still works as a manual fallback.
