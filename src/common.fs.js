import { ofArray, map, contains, mapIndexed, zip, initialize, empty, windowed, tryFindIndex, length, ofSeq } from "./.fable/fable-library.3.1.1/List.js";
import { structuralHash, numberHash, equals } from "./.fable/fable-library.3.1.1/Util.js";
import { KeyBoard, Status } from "./domain.fs.js";
import { defaultArg } from "./.fable/fable-library.3.1.1/Option.js";
import { filter, map as map_1, fold } from "./.fable/fable-library.3.1.1/Seq.js";
import { add, tryFind, countBy, ofSeq as ofSeq_1 } from "./.fable/fable-library.3.1.1/Map.js";

export function Display_parseWordGrapheme(grapheme, word) {
    const word_1 = ofSeq(word);
    const wordLength = length(word_1) | 0;
    let greens;
    const graphemeList = ofSeq(grapheme);
    const graphemeLength = length(graphemeList) | 0;
    const foundIndex = tryFindIndex((w) => {
        let t, g, t1, t_2, m1, m, h1, h;
        const matchValue = [w, graphemeList];
        if (t = matchValue[0], (g = matchValue[1], equals(t, g))) {
            const t_1 = matchValue[0];
            const g_1 = matchValue[1];
            return true;
        }
        else {
            let pattern_matching_result;
            if (matchValue[0].tail != null) {
                if (matchValue[0].tail.tail != null) {
                    if (matchValue[1].tail != null) {
                        if (matchValue[1].tail.tail != null) {
                            if (t1 = matchValue[1].tail.tail, (t_2 = matchValue[0].tail.tail, (m1 = matchValue[1].tail.head, (m = matchValue[0].tail.head, (h1 = matchValue[1].head, (h = matchValue[0].head, ((m1 === "-") ? (h === h1) : false) ? equals(t_2, t1) : false)))))) {
                                pattern_matching_result = 0;
                            }
                            else {
                                pattern_matching_result = 1;
                            }
                        }
                        else {
                            pattern_matching_result = 1;
                        }
                    }
                    else {
                        pattern_matching_result = 1;
                    }
                }
                else {
                    pattern_matching_result = 1;
                }
            }
            else {
                pattern_matching_result = 1;
            }
            switch (pattern_matching_result) {
                case 0: {
                    return true;
                }
                case 1: {
                    return false;
                }
            }
        }
    }, windowed(graphemeLength, word_1));
    if (foundIndex == null) {
        greens = empty();
    }
    else {
        const index = foundIndex | 0;
        greens = initialize(graphemeLength, (i) => (i + index));
    }
    return zip(word_1, mapIndexed((i_1, v) => {
        if (contains(i_1, greens, {
            Equals: (x, y) => (x === y),
            GetHashCode: numberHash,
        })) {
            return new Status(0);
        }
        else {
            return v;
        }
    }, initialize(wordLength, (_arg1) => (new Status(1)))));
}

export function Letter_toString(guessLetter) {
    return defaultArg(guessLetter.Letter, "");
}

export function Letter_toUpper(letter) {
    return letter.toLocaleUpperCase();
}

export function Letter_unpack(guessLetter) {
    return [Letter_toUpper(Letter_toString(guessLetter)), guessLetter.Status];
}

export function Letter_toOption(letter) {
    if (letter === "") {
        return void 0;
    }
    else {
        return letter;
    }
}

export function Guess_guessToWord(guess) {
    return fold((x, y) => (x + y), "", map((arg) => Letter_toUpper(Letter_toString(arg)), guess.Letters));
}

export function Guess_getLetter(_arg1, guess) {
    return map(Letter_unpack, guess.Letters);
}

export function List_set(list, value, pos) {
    return mapIndexed((i, v) => {
        if (i === pos) {
            return value;
        }
        else {
            return v;
        }
    }, list);
}

export function Counter_createCounter(items) {
    return ofSeq_1(countBy((x) => x, map_1((tuple) => tuple[0], filter((tupledArg) => {
        const a = tupledArg[0];
        const g = tupledArg[1];
        return !equals(a, g);
    }, items)), {
        Equals: equals,
        GetHashCode: structuralHash,
    }));
}

export function Counter_countOf(counter, item) {
    const matchValue = tryFind(item, counter);
    if (matchValue == null) {
        return 0;
    }
    else {
        const c = matchValue | 0;
        return c | 0;
    }
}

export function Counter_updateCount(counter, item) {
    const matchValue = tryFind(item, counter);
    if (matchValue == null) {
        return counter;
    }
    else {
        const c = matchValue | 0;
        return add(item, c - 1, counter);
    }
}

export const keyBoard = new KeyBoard(ofArray(["q", "w", "e", "r", "t", "y", "u", "i", "o", "p"]), ofArray(["a", "s", "d", "f", "g", "h", "j", "k", "l"]), ofArray(["Ent", "z", "x", "c", "v", "b", "n", "m", "Del"]));

//# sourceMappingURL=common.fs.js.map
