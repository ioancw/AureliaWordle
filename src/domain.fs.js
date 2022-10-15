import { Union, Record } from "./.fable/fable-library.3.1.1/Types.js";
import { class_type, bool_type, array_type, tuple_type, int32_type, option_type, union_type, record_type, list_type, string_type } from "./.fable/fable-library.3.1.1/Reflection.js";

export class KeyBoard extends Record {
    constructor(Top, Middle, Bottom) {
        super();
        this.Top = Top;
        this.Middle = Middle;
        this.Bottom = Bottom;
    }
}

export function KeyBoard$reflection() {
    return record_type("Domain.KeyBoard", [], KeyBoard, () => [["Top", list_type(string_type)], ["Middle", list_type(string_type)], ["Bottom", list_type(string_type)]]);
}

export class Modal extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["Info", "Help", "Stats"];
    }
}

export function Modal$reflection() {
    return union_type("Domain.Modal", [], Modal, () => [[], [], []]);
}

export class Status extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["Green", "Yellow", "Grey", "Black", "Invalid"];
    }
}

export function Status$reflection() {
    return union_type("Domain.Status", [], Status, () => [[], [], [], [], []]);
}

export class GuessLetter extends Record {
    constructor(Letter, Status) {
        super();
        this.Letter = Letter;
        this.Status = Status;
    }
}

export function GuessLetter$reflection() {
    return record_type("Domain.GuessLetter", [], GuessLetter, () => [["Letter", option_type(string_type)], ["Status", Status$reflection()]]);
}

export class LocalStorageGameState extends Record {
    constructor(Guesses, Solution, Round, State, GamesWon, GamesLost, WinDistribution) {
        super();
        this.Guesses = Guesses;
        this.Solution = Solution;
        this.Round = (Round | 0);
        this.State = State;
        this.GamesWon = (GamesWon | 0);
        this.GamesLost = (GamesLost | 0);
        this.WinDistribution = WinDistribution;
    }
}

export function LocalStorageGameState$reflection() {
    return record_type("Domain.LocalStorageGameState", [], LocalStorageGameState, () => [["Guesses", array_type(tuple_type(int32_type, array_type(tuple_type(string_type, string_type))))], ["Solution", string_type], ["Round", int32_type], ["State", string_type], ["GamesWon", int32_type], ["GamesLost", int32_type], ["WinDistribution", array_type(int32_type)]]);
}

export class Guess extends Record {
    constructor(Letters) {
        super();
        this.Letters = Letters;
    }
}

export function Guess$reflection() {
    return record_type("Domain.Guess", [], Guess, () => [["Letters", list_type(GuessLetter$reflection())]]);
}

export class GameState extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["NotStarted", "Won", "Lost", "Started"];
    }
}

export function GameState$reflection() {
    return union_type("Domain.GameState", [], GameState, () => [[], [], [], []]);
}

export class State extends Record {
    constructor(Wordle, Hint, Guesses, ShowInfo, ShowStats, ShowHelp, UsedLetters, State, Round, GamesWon, GamesLost, WinDistribution) {
        super();
        this.Wordle = Wordle;
        this.Hint = Hint;
        this.Guesses = Guesses;
        this.ShowInfo = ShowInfo;
        this.ShowStats = ShowStats;
        this.ShowHelp = ShowHelp;
        this.UsedLetters = UsedLetters;
        this.State = State;
        this.Round = (Round | 0);
        this.GamesWon = (GamesWon | 0);
        this.GamesLost = (GamesLost | 0);
        this.WinDistribution = WinDistribution;
    }
}

export function State$reflection() {
    return record_type("Domain.State", [], State, () => [["Wordle", string_type], ["Hint", string_type], ["Guesses", list_type(tuple_type(int32_type, Guess$reflection()))], ["ShowInfo", bool_type], ["ShowStats", bool_type], ["ShowHelp", bool_type], ["UsedLetters", class_type("Microsoft.FSharp.Collections.FSharpMap`2", [string_type, Status$reflection()])], ["State", GameState$reflection()], ["Round", int32_type], ["GamesWon", int32_type], ["GamesLost", int32_type], ["WinDistribution", list_type(int32_type)]]);
}

//# sourceMappingURL=domain.fs.js.map
