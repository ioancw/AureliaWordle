# Lit.AureliaWordle

Originally forked from https://aaronmu.github.io/MathGame/

This is designed for R and Y1/Y2 children to use phonics, so that a hint is provided for one of the phonic sounds in the word.
As an example:
If the wordle is RAINS, then the phonic hint with be /ai/.
The sound /ai/ is also a phonic hint for the AY graphemes, so it would also be a valid
hint for SPRAY and.

TODO
* Properly structure the code into modules etc.
* Sharing the results.
* Use a JSON streamer capable or writing and reading F# types.

Further ideas
* phonic keyboard, i.e. the button represents the phonic, which also allows you to choose the corresponding grapheme
    e.g. if the button is /ai/ then it would show AI, AY etc
* Automated parsing of words into phonemes.

[Fable.Lit](https://github.com/fable-compiler/Fable.Lit) app. To start a development server run:

```
npm install && npm start
```

Other commands:

```bash
npm run build   # Build optimized site for deployment and put in dist/
npm run publish # Build and publish to github pages
```

## Vite.js repository structure conventions

- Put static files in `public/` folder
- Put `index.html` in app root (next to `package.json`)
- Add a reference to the entry JS file (relative path is important):

```html
<script type="module" src="./build/client/App.js"></script>
```
