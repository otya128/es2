export type Identifier = {
    type: "identifier";
    value: string;
    start: Position;
    end: Position;
};

export type Keyword = {
    type: "keyword";
    value: Keywords;
    start: Position;
    end: Position;
};

export type Punctuator = {
    type: "punctuator";
    value: Punctuators;
    start: Position;
    end: Position;
};

export type EndToken = {
    type: "end";
    value: "<EOF>";
    start: Position;
    end: Position;
};

const keywords = [
    "abstract",
    "double",
    "local",
    "raises",
    "typedef",
    "any",
    "exception",
    "long",
    "readonly",
    "unsigned",
    "attribute",
    "enum",
    "module",
    "sequence",
    "union",
    "boolean",
    "factory",
    "native",
    "short",
    "ValueBase",
    "case",
    "FALSE",
    "Object",
    "string",
    "valuetype",
    "char",
    "fixed",
    "octet",
    "struct",
    "void",
    "const",
    "float",
    "oneway",
    "supports",
    "wchar",
    "context",
    "in",
    "out",
    "switch",
    "wstring",
    "custom",
    "inout",
    "private",
    "TRUE",
    "default",
    "interface",
    "public",
    "truncatable",
] as const;

const keywordsSet = new Set(keywords);
const lowerCaseKeywordsSet = new Set(keywords.map((x) => x.toLowerCase()));

const punctuators = [
    ";",
    "{",
    "}",
    "::",
    ":",
    ",",
    "=",
    "+",
    "-",
    "(",
    ")",
    "<",
    ">",
    "[",
    "]",
    "'",
    '"',
    "\\",
    "|",
    "^",
    "&",
    "*",
    "/",
    "%",
    "~",
] as const;
type Punctuators = (typeof punctuators)[number];
type Keywords = (typeof keywords)[number];
export type SourceInfo = {
    name: string;
    source?: string;
};

export type Position = {
    index: number;
    line: number;
    column: number;
    sourceInfo: SourceInfo | undefined;
};

class Reader {
    private source: string;
    private prevIndex: number;
    private prevLine: number;
    private prevColumn: number;
    private index: number;
    private line: number;
    private column: number;
    private sourceInfo: SourceInfo | undefined;
    constructor(source: string, sourceInfo?: SourceInfo) {
        this.source = source;
        this.index = 0;
        this.line = 1;
        this.column = 1;
        this.prevIndex = 0;
        this.prevLine = 1;
        this.prevColumn = 1;
        this.sourceInfo = sourceInfo;
    }
    get prevPosition(): Position {
        return {
            index: this.prevIndex,
            line: this.prevLine,
            column: this.prevColumn,
            sourceInfo: this.sourceInfo,
        };
    }
    get position(): Position {
        return {
            index: this.index,
            line: this.line,
            column: this.column,
            sourceInfo: this.sourceInfo,
        };
    }
    get current(): string {
        return this.source.charAt(this.index);
    }
    next(): string {
        this.prevIndex = this.index;
        this.prevLine = this.line;
        this.prevColumn = this.column;
        this.column++;
        if (this.source.charAt(this.index) === "\n") {
            this.column = 1;
            this.line++;
        }
        this.index++;
        return this.source.charAt(this.index);
    }
    consume(count: number): void {
        for (let i = 0; i < count; i++) {
            this.next();
        }
    }
    get eof(): boolean {
        return this.source.length <= this.index;
    }
    peek(count: number): string {
        return this.source.substring(this.index, this.index + count);
    }
    substring(start: Position, end: Position): string {
        return this.source.substring(start.index, end.index);
    }
}

export type Token = Identifier | Keyword | Punctuator | EndToken;
export function* tokenize(source: string, sourceInfo?: SourceInfo): Generator<Token, EndToken> {
    const reader = new Reader(source, sourceInfo);
    while (!reader.eof) {
        const char = reader.current;
        const start = reader.position;
        if ((char >= "A" && char <= "Z") || (char >= "a" && char <= "z") || char === "_") {
            let char: string;
            do {
                char = reader.next();
            } while (
                (char >= "A" && char <= "Z") ||
                (char >= "a" && char <= "z") ||
                char === "_" ||
                (char >= "0" && char <= "9")
            );
            const value = reader.substring(start, reader.position);
            const lowerCase = value.toLowerCase();
            //if (lowerCaseKeywordsSet.has(lowerCase)) {
            if (keywordsSet.has(value as Keywords)) {
                yield { type: "keyword", value: value as Keywords, start, end: reader.prevPosition };
                continue;
            }
            // throw new Error(`keyword: ${value}`);
            //}
            if (value.startsWith("_")) {
                yield { type: "identifier", value: value.substring(1), start, end: reader.prevPosition };
            } else {
                yield { type: "identifier", value, start, end: reader.prevPosition };
            }
            continue;
        }
        const chars = reader.peek(2);
        if (chars === "//") {
            reader.consume(2);
            while (!reader.eof) {
                const char = reader.next();
                if (char === "\r" || char === "\n") {
                    break;
                }
            }
            continue;
        }
        if (chars === "/*") {
            reader.consume(2);
            while (!reader.eof) {
                if (reader.peek(2) === "*/") {
                    reader.consume(2);
                    break;
                }
                reader.next();
            }
            continue;
        }
        const p = readPunctuator(reader);
        if (p != null) {
            yield p;
            continue;
        }
        if (char === " " || char === "\t" || char === "\v" || char === "\n" || char === "\r" || char === "\f") {
            reader.next();
            continue;
        }
        reader.next();
        throw new Error(`unexpected token: ${char}`);
    }
    return { type: "end", value: "<EOF>", start: reader.position, end: reader.position };
}

function readPunctuator(reader: Reader): Punctuator | undefined {
    const start = reader.position;
    const chars = reader.peek(2);
    for (const punc of punctuators) {
        if (chars.startsWith(punc)) {
            reader.consume(punc.length);
            return { type: "punctuator", value: punc, start, end: reader.prevPosition };
        }
    }
}

export class Tokenizer {
    private iterator: Generator<Token, EndToken>;
    private _current: Token;
    private _prevPosition: Position;
    constructor(source: string, sourceInfo?: SourceInfo) {
        this.iterator = tokenize(source, sourceInfo);
        this._prevPosition = {
            index: 0,
            line: 1,
            column: 1,
            sourceInfo,
        };
        this._current = this.iterator.next().value;
    }
    next(): Token {
        this._prevPosition = this._current.end;
        this._current = this.iterator.next().value;
        return this._current;
    }
    get current(): Token {
        return this._current;
    }
    get prevPosition(): Position {
        return this._prevPosition;
    }
}
