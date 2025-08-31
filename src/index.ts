function isIdentifierLetter(char: string) {
    return (char >= "a" && char <= "z") || (char >= "A" && char <= "Z") || char === "$" || char === "_";
}
function isDecimalDigit(char: string) {
    return char >= "0" && char <= "9";
}

type Keywords = "break" | "for" | "new" | "var" | "continue" | "function" | "return" | "void" | "delete" | "if" | "this" | "while" | "else" | "in" | "typeof" | "with";
export type ReservedWord = {
    type: "reservedWord";
    value: string;
};
export type Identifier = {
    type: "identifier";
    value: string;
};
export type Punctuator = {
    type: "punctuator";
    value: string;
};
export type NumericLiteral = {
    type: "numericLiteral";
    value: string;
};
export type StringLiteral = {
    type: "stringLiteral";
    value: string;
};
export type Token = Identifier;
export function* parse(source: string) {
    for (let i = 0; i < source.length;) {
        const start = i;
        const char = source.charAt(i);
        if (isIdentifierLetter(char)) {
            i++;
            for (; i < source.length;) {
                const char = source.charAt(i);
                if (isIdentifierLetter(char) || isDecimalDigit(char)) {
                    continue;
                }
                break;
            }
            yield { type: "identifier", value: source.substring(start, i) };
            continue;
        }
    }
}
