# Aureliadle and friends

Daily word and number games for children, played at https://ioancw.github.io/AureliaWordle/.
Originally forked from https://aaronmu.github.io/MathGame/.

- **Aureliadle** (the site root): a wordle designed for Reception and Y1/Y2 children learning phonics.
  A hint is given for one of the phonic sounds in the word: if the wordle is RAINS, the hint is /ai/.
  The sound /ai/ is also the hint for the AY grapheme, so it would be a valid hint for SPRAY too.
- **Which Witch?** (`whichwitch/`): pick the right word for each sentence: their / there / they're,
  its / it's, to / too / two, could've (not could of), didn't (not did'nt), the dog's bone or three dogs...
  Six sentences a day getting harder (two warm-ups, two tricky, two apostrophe challenges), three hearts,
  and a child-friendly explanation whenever a wrong word is picked.
- **Numberdle** (`numberdle/`): guess the number from 1 to 100 in seven tries, told higher or lower and how close.

The ☰ menu moves between them. Each has daily puzzles, stats and streaks, sharing, and a high contrast setting.

Further ideas
* Phonics keyboard: a button per sound, which then lets you choose the grapheme,
  e.g. the /ai/ button would offer AI, AY etc.
* Automated parsing of words into phonemes.

## Development

Requires the [.NET 10 SDK](https://dotnet.microsoft.com/download) and Node.js 20+.
The games are written in F#, compiled to JavaScript by [Fable](https://fable.io), with
[Feliz](https://zaid-ajaj.github.io/Feliz/) (React) and Elmish for the views.

```bash
npm install     # also restores the Fable tool
npm start       # dev server for Aureliadle with live reload
npm test        # all the F# tests
npm run build   # the whole site in dist/
```

## Code layout

A game is a `DailyGame` record (its rules: starting, applying an input, the outcome, its save format
and share grid) plus a `GameView` (its board, controls and help pop-up). The engine does the rest:
today's puzzle, saving and resuming, new days, stats and streaks, keeping tabs in sync, and the app
around the game (header, menu, pop-ups, messages, sharing and settings).

| Path | What it is |
|---|---|
| `feliz/Engine/Engine.fs` | Pure F#: today's puzzle, saving (Thoth.Json), resuming, new days, stats and streaks |
| `feliz/Engine/BrowserStorage.fs` | Local storage, including reading a game's older save format |
| `feliz/Engine/Shell.fs` | The app around any game (Feliz + Elmish) |
| `feliz/Engine/Suite.fs` | The games listed in the ☰ menu |
| `feliz/Aureliadle/` | Aureliadle's board, keyboard and help; its rules are in `src/` |
| `feliz/WhichWitch/` | Which Witch?: the sentence bank and explanations (`Rules.fs`), board and word buttons |
| `feliz/Numberdle/` | Numberdle: rules, board and keypad |
| `feliz/site/` | The HTML pages and each game's CSS (shared styles are in `public/main.css`) |
| `src/` | Aureliadle's rules: word list and phonics (`Words.fs`, `Phonics.fs`), scoring (`GameRules.fs`), save format (`Storage.fs`, `Game.fs`) |
| `test2/` | Tests for Aureliadle's rules |
| `feliz/Tests/` | Tests for the engine and every game, run on .NET with the same code the browser runs |

Adding sentences to Which Witch? is a one-line change in `feliz/WhichWitch/Rules.fs`; the tests check
every sentence has one blank, a valid answer and an explanation for each choice.

Aureliadle reads saves from its previous version (the `gameStateAureliav3` key), so players keep their stats.

## Deployment

GitHub Actions (`.github/workflows/build-deploy.yml`) runs the tests and builds the site on every pull request,
and every merge to `main` is deployed to GitHub Pages (the `gh-pages` branch).
`npm run publish` still works as a manual fallback.
