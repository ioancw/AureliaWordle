import { LocalStorageGameState, Status, GameState } from "./domain.fs.js";
import { map } from "./.fable/fable-library.3.1.1/List.js";
import { Letter_toString } from "./common.fs.js";

export const gameStateKey = "gameStateAureliav2";

export function StateHelpers_statusToString(status) {
    switch (status.tag) {
        case 2: {
            return "Grey";
        }
        case 3: {
            return "Black";
        }
        case 0: {
            return "Green";
        }
        case 4: {
            return "Invalid";
        }
        default: {
            return "Yellow";
        }
    }
}

export function StateHelpers_stateToString(state) {
    switch (state.tag) {
        case 1: {
            return "Won";
        }
        case 2: {
            return "Lost";
        }
        case 3: {
            return "Started";
        }
        default: {
            return "Not Started";
        }
    }
}

export function StateHelpers_stateFromString(stored) {
    const matchValue = stored.State;
    switch (matchValue) {
        case "Not Started": {
            return new GameState(0);
        }
        case "Won": {
            return new GameState(1);
        }
        case "Lost": {
            return new GameState(2);
        }
        case "Started": {
            return new GameState(3);
        }
        default: {
            return new GameState(0);
        }
    }
}

export function StateHelpers_statusFromString(guessStatus) {
    switch (guessStatus) {
        case "Yellow": {
            return new Status(1);
        }
        case "Grey": {
            return new Status(2);
        }
        case "Black": {
            return new Status(3);
        }
        case "Green": {
            return new Status(0);
        }
        default: {
            return new Status(4);
        }
    }
}

export function saveGameStateLocalStorage(state) {
    const guessedWords = Array.from(map((tupledArg) => {
        const position = tupledArg[0] | 0;
        const guess = tupledArg[1];
        return [position, Array.from(map((gl) => [Letter_toString(gl), StateHelpers_statusToString(gl.Status)], guess.Letters))];
    }, state.Guesses));
    localStorage.setItem(gameStateKey, JSON.stringify(new LocalStorageGameState(guessedWords, state.Wordle, state.Round, StateHelpers_stateToString(state.State), state.GamesWon, state.GamesLost, Int32Array.from(state.WinDistribution))));
}

export function loadGameStateLocalStorage() {
    const localState = localStorage.getItem(gameStateKey);
    if (localState === null) {
        return void 0;
    }
    else {
        return JSON.parse(localState);
    }
}

//# sourceMappingURL=persistence.fs.js.map
