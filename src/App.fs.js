import { comparePrimitives, partialApply, uncurry, round as round_1, stringHash, equals } from "./.fable/fable-library.3.1.1/Util.js";
import { State as State_3, Guess, GuessLetter, Status as Status_1, GameState } from "./domain.fs.js";
import { max, empty as empty_1, cons, mapIndexed, map as map_2, ofArray, length, fold, initialize, contains, forAll, ofSeq, item } from "./.fable/fable-library.3.1.1/List.js";
import { singleton, initialize as initialize_1, concat, collect, delay, fold as fold_1, reverse, contains as contains_1, zip, filter, map } from "./.fable/fable-library.3.1.1/Seq.js";
import { Guess_getLetter, Counter_createCounter, Counter_updateCount, Counter_countOf, List_set, Letter_toOption, Letter_toString, Guess_guessToWord, Letter_toUpper } from "./common.fs.js";
import { phonemeGraphemeCorresspondances, wordles, words } from "./words.fs.js";
import { tryFind, empty, FSharpMap__Add, FSharpMap__ContainsKey } from "./.fable/fable-library.3.1.1/Map.js";
import { saveGameStateLocalStorage, StateHelpers_stateFromString, StateHelpers_statusFromString, loadGameStateLocalStorage } from "./persistence.fs.js";
import { op_Subtraction, day, month, year, create, now } from "./.fable/fable-library.3.1.1/Date.js";
import { map as map_1 } from "./.fable/fable-library.3.1.1/Array.js";
import { toString } from "./.fable/fable-library.3.1.1/Types.js";
import { LitHelpers_html, Lit_classes_5A743451 } from "./.fable/Fable.Lit.1.3.1/Lit.fs.js";
import { defaultArg } from "./.fable/fable-library.3.1.1/Option.js";
import { Hook_getContext_343DAFF1 } from "./.fable/Fable.Lit.1.3.1/Hook.fs.js";
import "./index.css";


export const rounds = 6;

export const letters = 5;

export function Validate_letterPosition(n) {
    if (n >= 0) {
        return n < letters;
    }
    else {
        return false;
    }
}

export function Validate_round(state) {
    if (equals(state.State, new GameState(2)) ? true : equals(state.State, new GameState(1))) {
        return false;
    }
    else if (state.Round >= 0) {
        return state.Round < rounds;
    }
    else {
        return false;
    }
}

export function Validate_allLetters(n, guesses) {
    const guess = item(n, guesses)[1];
    const fiveLetterWords = ofSeq(map(Letter_toUpper, filter((l) => (l.length === 5), words)));
    if (forAll((gl) => (!equals(gl.Letter, void 0)), guess.Letters)) {
        return contains(Guess_guessToWord(guess), fiveLetterWords, {
            Equals: (x, y) => (x === y),
            GetHashCode: stringHash,
        });
    }
    else {
        return false;
    }
}

export const emptyGuess = [0, new Guess(initialize(letters, (_arg1) => (new GuessLetter(void 0, new Status_1(3)))))];

export const emptyGuesses = initialize(rounds, (_arg1) => emptyGuess);

export function updateKeyboardState(guesses, keyBoardState) {
    return fold((state, guessLetter) => {
        const letter = Letter_toString(guessLetter);
        const status = guessLetter.Status;
        if ((equals(status, new Status_1(0)) ? true : (equals(status, new Status_1(2)) ? (!FSharpMap__ContainsKey(state, letter)) : false)) ? true : (equals(status, new Status_1(1)) ? (!FSharpMap__ContainsKey(state, letter)) : false)) {
            return FSharpMap__Add(state, letter, status);
        }
        else {
            return state;
        }
    }, keyBoardState, guesses);
}

export const startNewGame = (() => {
    const loadedStorage = loadGameStateLocalStorage();
    const guesses = emptyGuesses;
    const wordle = () => {
        const today = now();
        const startDate = create(2022, 6, 4);
        const todayDate = create(year(today), month(today), day(today));
        let differenceMilli;
        let copyOfStruct = op_Subtraction(todayDate, startDate);
        differenceMilli = copyOfStruct;
        const millisInDay = ((60 * 60) * 24) * 1000;
        const differenceDays = (~(~round_1(differenceMilli / millisInDay))) | 0;
        const index = (differenceDays % length(wordles)) | 0;
        return item(index, wordles);
    };
    const patternInput = wordle();
    const wordle_1 = patternInput[0];
    const hint = patternInput[1];
    if (loadedStorage != null) {
        const stored = loadedStorage;
        const localGuesses = ofArray(map_1((tupledArg) => {
            const position = tupledArg[0] | 0;
            const letters_1 = tupledArg[1];
            return [position, new Guess(map_2((tupledArg_1) => {
                const guessLetter = tupledArg_1[0];
                const guessStatus = tupledArg_1[1];
                return new GuessLetter(Letter_toOption(guessLetter), StateHelpers_statusFromString(guessStatus));
            }, ofArray(letters_1)))];
        }, stored.Guesses));
        let localGuessesPlus;
        const list_2 = mapIndexed((i, g) => [g, i], localGuesses);
        localGuessesPlus = fold((s, tupledArg_2) => {
            const g_1 = tupledArg_2[0];
            const i_1 = tupledArg_2[1] | 0;
            return List_set(s, g_1, i_1);
        }, initialize(rounds, (_arg1) => emptyGuess), list_2);
        let storageUsedLetters;
        const list_4 = map_2((tuple) => tuple[1], localGuessesPlus);
        storageUsedLetters = fold((state_1, guess) => updateKeyboardState(guess.Letters, state_1), empty(), list_4);
        let loadedDistro;
        const list_6 = mapIndexed((i_2, d) => [d, i_2], ofArray(stored.WinDistribution));
        loadedDistro = fold((s_1, tupledArg_3) => {
            const d_1 = tupledArg_3[0] | 0;
            const i_3 = tupledArg_3[1] | 0;
            return List_set(s_1, d_1, i_3);
        }, initialize(rounds, (_arg2) => 0), list_6);
        const loadedGame = new State_3(wordle_1, hint, localGuessesPlus, false, false, false, storageUsedLetters, StateHelpers_stateFromString(stored), stored.Round, stored.GamesWon, stored.GamesLost, loadedDistro);
        const newGameWithStats = new State_3(loadedGame.Wordle, loadedGame.Hint, guesses, loadedGame.ShowInfo, loadedGame.ShowStats, loadedGame.ShowHelp, empty(), new GameState(0), 0, loadedGame.GamesWon, loadedGame.GamesLost, loadedGame.WinDistribution);
        return (wordle_1 !== stored.Solution) ? newGameWithStats : loadedGame;
    }
    else {
        return new State_3(wordle_1, hint, guesses, false, false, false, empty(), new GameState(0), 0, 0, 0, initialize(rounds, (_arg3) => 0));
    }
})();

export function getAnswerMask(actualWord, guessWord) {
    const letters_1 = zip(actualWord, guessWord);
    const folder = (tupledArg) => {
        const count = tupledArg[0];
        const mask = tupledArg[1];
        return (tupledArg_1) => {
            const actualLetter = tupledArg_1[0];
            const guessLetter = tupledArg_1[1];
            return equals(actualLetter, guessLetter) ? [count, cons(new Status_1(0), mask)] : ((contains_1(guessLetter, actualWord) ? (Counter_countOf(count, guessLetter) > 0) : false) ? [Counter_updateCount(count, guessLetter), cons(new Status_1(1), mask)] : [count, cons(new Status_1(2), mask)]);
        };
    };
    return reverse(fold_1(uncurry(2, folder), [Counter_createCounter(letters_1), empty_1()], letters_1)[1]);
}

export function scoreGuess(actualWord, guessWord) {
    return new Guess(map_2((tupledArg) => {
        const a = tupledArg[0];
        const m = tupledArg[1];
        return new GuessLetter(toString(a), m);
    }, ofSeq(zip(guessWord, getAnswerMask(actualWord, guessWord)))));
}

export function updateState(state, updateFunction) {
    if (Validate_round(state)) {
        const word = item(state.Round, state.Guesses);
        const updatedWord = updateFunction(word);
        return new State_3(state.Wordle, state.Hint, List_set(state.Guesses, updatedWord, state.Round), state.ShowInfo, state.ShowStats, state.ShowHelp, state.UsedLetters, state.State, state.Round, state.GamesWon, state.GamesLost, state.WinDistribution);
    }
    else {
        return state;
    }
}

export function submitLetter(letter, state) {
    const add = (letter_1, tupledArg) => {
        const position = tupledArg[0] | 0;
        const guessLetters = tupledArg[1];
        if (Validate_letterPosition(position)) {
            return [position + 1, new Guess(List_set(guessLetters.Letters, new GuessLetter(letter_1, new Status_1(3)), position))];
        }
        else {
            return [position, new Guess(guessLetters.Letters)];
        }
    };
    return updateState(state, partialApply(1, add, [letter]));
}

export function submitDelete(state) {
    const deleteLetter = (tupledArg) => {
        const position = tupledArg[0] | 0;
        const guessLetters = tupledArg[1];
        const deletePosition = (position - 1) | 0;
        if (Validate_letterPosition(deletePosition)) {
            return [deletePosition, new Guess(List_set(guessLetters.Letters, new GuessLetter(void 0, new Status_1(3)), deletePosition))];
        }
        else {
            return [position, new Guess(guessLetters.Letters)];
        }
    };
    return updateState(state, deleteLetter);
}

export function submitEnter(state) {
    const submitGuess = (_arg1) => {
        const position = _arg1[0] | 0;
        const guess = _arg1[1];
        const guessWord = Guess_guessToWord(guess);
        const scoredGuess = scoreGuess(state.Wordle.split(""), guessWord.split(""));
        const updatedUsedLetters = updateKeyboardState(scoredGuess.Letters, state.UsedLetters);
        const updatedGuess = [position, scoredGuess];
        const updatedState = (guessWord === state.Wordle) ? (new GameState(1)) : ((state.Round === (rounds - 1)) ? (new GameState(2)) : (new GameState(3)));
        return [updatedGuess, updatedState, updatedUsedLetters];
    };
    if (Validate_round(state)) {
        if (Validate_allLetters(state.Round, state.Guesses)) {
            const patternInput = submitGuess(item(state.Round, state.Guesses));
            const updatedUsedLetters_1 = patternInput[2];
            const updatedGameState = patternInput[1];
            const scoredGuess_1 = patternInput[0];
            const winDistribution = item(state.Round, state.WinDistribution) | 0;
            return new State_3(state.Wordle, state.Hint, List_set(state.Guesses, scoredGuess_1, state.Round), state.ShowInfo, state.ShowStats, state.ShowHelp, updatedUsedLetters_1, updatedGameState, (equals(updatedGameState, new GameState(1)) ? true : equals(updatedGameState, new GameState(2))) ? state.Round : (state.Round + 1), equals(updatedGameState, new GameState(1)) ? (state.GamesWon + 1) : state.GamesWon, equals(updatedGameState, new GameState(2)) ? (state.GamesLost + 1) : state.GamesLost, equals(updatedGameState, new GameState(1)) ? List_set(state.WinDistribution, winDistribution + 1, state.Round) : state.WinDistribution);
        }
        else {
            const invalidGuess = item(state.Round, state.Guesses)[1];
            const updated = new Guess(map_2((ls) => (new GuessLetter(ls.Letter, new Status_1(4))), invalidGuess.Letters));
            return new State_3(state.Wordle, state.Hint, List_set(state.Guesses, [letters, updated], state.Round), state.ShowInfo, state.ShowStats, state.ShowHelp, state.UsedLetters, state.State, state.Round, state.GamesWon, state.GamesLost, state.WinDistribution);
        }
    }
    else {
        return state;
    }
}

export function gameTile(position, c, status) {
    const isValid = !equals(status, new Status_1(4));
    const classes = Lit_classes_5A743451([["cell-black", equals(status, new Status_1(3))], ["cell-reveal.absent cell-grey", equals(status, new Status_1(2))], ["cell-green", equals(status, new Status_1(0))], ["cell-yellow", equals(status, new Status_1(1))], ["jiggle cell-black", equals(status, new Status_1(4))], ["cell-slow-1", (position === 0) ? isValid : false], ["cell-slow-2", (position === 1) ? isValid : false], ["cell-slow-3", (position === 2) ? isValid : false], ["cell-slow-4", (position === 3) ? isValid : false], ["cell-slow-5", (position === 4) ? isValid : false]]);
    return LitHelpers_html(null);
}

export function littleBoxedChar(c, status) {
    const colourBorder = (status.tag === 2) ? "bg-red-800 border-red-800" : ((status.tag === 0) ? "bg-green-700 border-green-700" : ((status.tag === 1) ? "bg-yellow-600 border-yellow-600" : ((status.tag === 4) ? "bg-neutral-400 border-neutral-400" : "cell-black")));
    return LitHelpers_html(null);
}

export function keyboardChar(usedLetters, handler, c) {
    let colour;
    let letterStatus;
    const matchValue = tryFind(c.toLocaleUpperCase(), usedLetters);
    if (matchValue == null) {
        letterStatus = (new Status_1(3));
    }
    else {
        const x = matchValue;
        letterStatus = x;
    }
    switch (letterStatus.tag) {
        case 1: {
            colour = "cell-yellow";
            break;
        }
        case 2: {
            colour = "cell-grey";
            break;
        }
        case 0: {
            colour = "cell-green";
            break;
        }
        case 4: {
            colour = "bg-gray-400";
            break;
        }
        default: {
            colour = "bg-neutral-500";
        }
    }
    const width = (c === "Del") ? "key-other-size" : ((c === "Ent") ? "key-other-size" : "key-size");
    return LitHelpers_html(null);
}

export function modal(customHead, bodyText, modalDisplayState, handler) {
    const hidden = modalDisplayState ? "" : "hidden";
    return LitHelpers_html(null);
}

export const infoText = LitHelpers_html(null);

export function helpText(state) {
    const hint = state.Hint;
    const wordle = state.Wordle;
    const hintedGraphemes = defaultArg(tryFind(hint, phonemeGraphemeCorresspondances), empty_1());
    let graphemes;
    const maxLenGrapheme = max(map_2((arg) => arg[0].length, hintedGraphemes), {
        Compare: comparePrimitives,
    }) | 0;
    graphemes = ofSeq(delay(() => collect((matchValue) => {
        const grapheme = matchValue[0];
        const exampleWord = matchValue[1];
        const pad = (maxLenGrapheme - grapheme.length) | 0;
        const padded = concat([map((g) => [g, new Status_1(0)], grapheme.split("")), initialize_1(pad + 1, (_arg1) => [" ", new Status_1(4)])]);
        const letters_1 = map((l) => [l, contains_1(l, grapheme.split("")) ? (new Status_1(0)) : (new Status_1(1))], exampleWord.split(""));
        return singleton(LitHelpers_html(null));
    }, hintedGraphemes)));
    return LitHelpers_html(null);
}

export function statsText(state) {
    const statRow = (label, value) => LitHelpers_html(null);
    const progress = (round, tupledArg) => {
        let n, n_2, n_4, n_6, n_8, n_10, n_12, n_14, n_16, n_18, n_20, n_22;
        const size = tupledArg[0];
        const label_1 = tupledArg[1];
        let width;
        const matchValue = (~(~size)) | 0;
        if (n = (matchValue | 0), n <= 10) {
            const n_1 = matchValue | 0;
            width = "w-1/12";
        }
        else if (n_2 = (matchValue | 0), (n_2 > 10) ? (n_2 <= 20) : false) {
            const n_3 = matchValue | 0;
            width = "w-2/12";
        }
        else if (n_4 = (matchValue | 0), (n_4 > 20) ? (n_4 <= 30) : false) {
            const n_5 = matchValue | 0;
            width = "w-3/12";
        }
        else if (n_6 = (matchValue | 0), (n_6 > 30) ? (n_6 <= 40) : false) {
            const n_7 = matchValue | 0;
            width = "w-4/12";
        }
        else if (n_8 = (matchValue | 0), (n_8 > 40) ? (n_8 <= 50) : false) {
            const n_9 = matchValue | 0;
            width = "w-5/12";
        }
        else if (n_10 = (matchValue | 0), (n_10 > 50) ? (n_10 <= 60) : false) {
            const n_11 = matchValue | 0;
            width = "w-1/2";
        }
        else if (n_12 = (matchValue | 0), (n_12 > 60) ? (n_12 <= 70) : false) {
            const n_13 = matchValue | 0;
            width = "w-7/12";
        }
        else if (n_14 = (matchValue | 0), (n_14 > 70) ? (n_14 <= 80) : false) {
            const n_15 = matchValue | 0;
            width = "w-8/12";
        }
        else if (n_16 = (matchValue | 0), (n_16 > 80) ? (n_16 <= 90) : false) {
            const n_17 = matchValue | 0;
            width = "w-9/12";
        }
        else if (n_18 = (matchValue | 0), (n_18 > 90) ? (n_18 <= 100) : false) {
            const n_19 = matchValue | 0;
            width = "w-10/12";
        }
        else if (n_20 = (matchValue | 0), (n_20 > 100) ? (n_20 <= 110) : false) {
            const n_21 = matchValue | 0;
            width = "w-11/12";
        }
        else if (n_22 = (matchValue | 0), (n_22 > 110) ? (n_22 <= 120) : false) {
            const n_23 = matchValue | 0;
            width = "w-full";
        }
        else {
            width = "w-1/12";
        }
        return LitHelpers_html(null);
    };
    let histogramRow;
    const maxValue = max(state.WinDistribution, {
        Compare: comparePrimitives,
    }) | 0;
    const toSize = (value_1) => [120 * (value_1 / maxValue), value_1];
    histogramRow = LitHelpers_html(null);
    const totalGames = state.GamesLost + state.GamesWon;
    const successRate = round_1((totalGames === 0) ? 0 : ((state.GamesWon / totalGames) * 100));
    return LitHelpers_html(null);
}

export function MatchComponent() {
    (this).init((arg) => {
        let a;
        arg.useShadowDom = false;
        return Promise.resolve(undefined);
    });
    const startedGame = startNewGame;
    const patternInput = Hook_getContext_343DAFF1(this).useState(() => startedGame);
    const setGameState = patternInput[1];
    const gameState = patternInput[0];
    const writeState = (state) => {
        saveGameStateLocalStorage(state);
        const letterToDisplayBox = (letters_1) => {
            let tupledArg;
            return mapIndexed((i, gl) => gameTile(i, gl[0], gl[1]), (tupledArg = letters_1, Guess_getLetter(tupledArg[0], tupledArg[1])));
        };
        const onKeyClick = (c_1) => ((ev) => {
            ev.preventDefault();
            const submitEntry = (c_1 === "Ent") ? (submitEnter) : ((c_1 === "Del") ? (submitDelete) : ((state_3) => submitLetter(c_1, state_3)));
            setGameState(submitEntry(state));
        });
        const onModalClick = (modalType) => ((ev_1) => {
            ev_1.preventDefault();
            setGameState((modalType.tag === 2) ? (new State_3(state.Wordle, state.Hint, state.Guesses, state.ShowInfo, state.ShowStats !== true, state.ShowHelp, state.UsedLetters, state.State, state.Round, state.GamesWon, state.GamesLost, state.WinDistribution)) : ((modalType.tag === 1) ? (new State_3(state.Wordle, state.Hint, state.Guesses, state.ShowInfo, state.ShowStats, state.ShowHelp !== true, state.UsedLetters, state.State, state.Round, state.GamesWon, state.GamesLost, state.WinDistribution)) : (new State_3(state.Wordle, state.Hint, state.Guesses, state.ShowInfo !== true, state.ShowStats, state.ShowHelp, state.UsedLetters, state.State, state.Round, state.GamesWon, state.GamesLost, state.WinDistribution))));
        });
        const keyboardKey = (c_2) => keyboardChar(state.UsedLetters, onKeyClick, c_2);
        return LitHelpers_html(null);
    };
    const matchValue = gameState.State;
    switch (matchValue.tag) {
        case 3: {
            return writeState(gameState);
        }
        case 1: {
            return writeState(gameState);
        }
        case 2: {
            return writeState(gameState);
        }
        default: {
            return writeState(gameState);
        }
    }
}

//# sourceMappingURL=App.fs.js.map
