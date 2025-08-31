function isLineTerminator(char: string) {
    return char === "\n" || char === "\r";
}
function isWhiteSpace(char: string) {
    return char === "\u0009" || char === "\u000B" || char === "\u000C" || char === "\u0020";
}
function isExponentIndicator(char: string) {
    return char === "e" || char === "E";
}
function isIdentifierLetter(char: string) {
    return (char >= "a" && char <= "z") || (char >= "A" && char <= "Z") || char === "$" || char === "_";
}
function isDecimalDigit(char: string) {
    return char >= "0" && char <= "9";
}
function isNonZeroDigit(char: string) {
    return char >= "1" && char <= "9";
}
function isZeroToThree(char: string) {
    return char >= "0" && char <= "3";
}
function isHexDigit(char: string) {
    return (char >= "0" && char <= "9") || (char >= "a" && char <= "f") || (char >= "A" && char <= "F");
}
function isOctalDigit(char: string) {
    return char >= "0" && char <= "7";
}

const keywords = [
    "break",
    "for",
    "new",
    "var",
    "continue",
    "function",
    "return",
    "void",
    "delete",
    "if",
    "this",
    "while",
    "else",
    "in",
    "typeof",
    "with",
] as const;

const keywordsSet = new Set(keywords);

const futureReservedWords = [
    "abstract",
    "do",
    "import",
    "short",
    "boolean",
    "double",
    "instanceof",
    "static",
    "byte",
    "enum",
    "int",
    "super",
    "case",
    "export",
    "interface",
    "switch",
    "catch",
    "extends",
    "long",
    "synchronized",
    "char",
    "final",
    "native",
    "throw",
    "class",
    "finally",
    "package",
    "throws",
    "const",
    "float",
    "private",
    "transient",
    "debugger",
    "goto",
    "protected",
    "try",
    "default",
    "implements",
    "public",
    "volatile",
] as const;
const futureReservedWordsSet = new Set(futureReservedWords);
const punctuators = [
    "~",
    "}",
    "||",
    "|=",
    "|",
    "{",
    "^=",
    "^",
    "]",
    "[",
    "?",
    ">>>=",
    ">>>",
    ">>=",
    ">>",
    ">=",
    ">",
    "==",
    "=",
    "<=",
    "<<=",
    "<<",
    "<",
    ";",
    ":",
    "/=",
    "/",
    ".",
    "-=",
    "--",
    "-",
    ",",
    "+=",
    "++",
    "+",
    "*=",
    "*",
    ")",
    "(",
    "&=",
    "&&",
    "&",
    "%=",
    "%",
    "!=",
    "!",
] as const;
type Punctuators = (typeof punctuators)[number];
type Keywords = (typeof keywords)[number];
type FutureReservedWords = (typeof futureReservedWords)[number];
export type Position = {
    index: number;
    line: number;
    column: number;
};

export type Keyword = {
    type: "keyword";
    value: Keywords;
    start: Position;
    end: Position;
};
export type FutureReservedWord = {
    type: "futureReservedWord";
    value: FutureReservedWords;
    start: Position;
    end: Position;
};
export type NullLiteral = {
    type: "nullLiteral";
    value: null;
    start: Position;
    end: Position;
};
export type BooleanLiteral = {
    type: "booleanLiteral";
    value: boolean;
    start: Position;
    end: Position;
};
export type ReservedWord = Keyword | FutureReservedWord | NullLiteral | BooleanLiteral;
export type Literal = NullLiteral | BooleanLiteral | NumericLiteral | StringLiteral;
export type Identifier = {
    type: "identifier";
    value: string;
    start: Position;
    end: Position;
};
export type Punctuator = {
    type: "punctuator";
    value: Punctuators;
    start: Position;
    end: Position;
};
export type NumericLiteral = {
    type: "numericLiteral";
    value: number;
    start: Position;
    end: Position;
};
export type StringLiteral = {
    type: "stringLiteral";
    value: string;
    start: Position;
    end: Position;
};
export type LineTerminator = {
    type: "lineTerminator";
    value: "\n";
    start: Position;
    end: Position;
};
export type EndToken = {
    type: "end";
    value: "<EOF>";
    start: Position;
    end: Position;
};
export type Token = ReservedWord | Identifier | Punctuator | NumericLiteral | StringLiteral | LineTerminator | EndToken;

class UnexpectedCharacterError extends Error {
    constructor(syntax: string, expected: string, actual: string, position: Position) {
        if (actual === "") {
            actual = "<EOF>";
        } else if (actual === "\n") {
            actual = "<LF>";
        } else if (actual === "\b") {
            actual = "<BS>";
        } else if (actual === "\t") {
            actual = "<HT>";
        } else if (actual === "\f") {
            actual = "<FF>";
        } else if (actual === "\r") {
            actual = "<CR>";
        }
        super(`${syntax}: Expected ${expected}, Actual: ${actual}, ${position.line}:${position.column}`);
    }
}

class UnexpectedTokenError extends Error {
    constructor(syntax: string, expected: string, actual: Token) {
        super(`${syntax}: Expected ${expected}, Actual: ${actual.value}, ${actual.start.line}:${actual.start.column}`);
    }
}

class Reader {
    private source: string;
    private prevIndex: number;
    private prevLine: number;
    private prevColumn: number;
    private index: number;
    private line: number;
    private column: number;
    constructor(source: string) {
        this.source = source;
        this.index = 0;
        this.line = 1;
        this.column = 1;
        this.prevIndex = 0;
        this.prevLine = 1;
        this.prevColumn = 1;
    }
    get prevPosition(): Position {
        return {
            index: this.prevIndex,
            line: this.prevLine,
            column: this.prevColumn,
        };
    }
    get position(): Position {
        return {
            index: this.index,
            line: this.line,
            column: this.column,
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
        this.index++;
        const char = this.source.charAt(this.index);
        if (char === "\n") {
            this.column = 1;
            this.line++;
        }
        return char;
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

export function* tokenize(source: string): Generator<Token | LineTerminator, EndToken> {
    const reader = new Reader(source);
    while (!reader.eof) {
        const start = reader.position;
        const char = reader.current;
        if (isIdentifierLetter(char)) {
            yield parseReservedWordOrToken(reader);
            continue;
        }
        if (char === "=") {
            const char = reader.next();
            if (char === "=") {
                reader.next();
                yield { type: "punctuator", value: "==", start, end: reader.prevPosition };
            } else {
                yield { type: "punctuator", value: "=", start, end: reader.prevPosition };
            }
            continue;
        }
        if (char === ".") {
            const chars = reader.peek(2)[1];
            if (isDecimalDigit(chars ?? "")) {
                yield parseDecimalLiteral(reader, start);
                continue;
            }
        }
        if (isNonZeroDigit(char)) {
            yield parseDecimalLiteral(reader, start);
            continue;
        }
        if (char === "0") {
            const char = reader.next();
            if (char === "x" || char === "X") {
                yield parseHexIntegerLiteral(reader, start);
            } else if (isOctalDigit(char)) {
                yield parseOctalIntegerLiteral(reader, start);
            } else {
                yield parseDecimalLiteral(reader, start);
            }
            continue;
        }
        if (char === '"' || char === "'") {
            yield readStringLiteral(reader);
            continue;
        }
        if (char === "/") {
            const chars = reader.peek(2);
            if (chars === "/*") {
                reader.next();
                if (parseMultiLineComment(reader)) {
                    yield { type: "lineTerminator", value: "\n", start, end: reader.position };
                }
                continue;
            } else if (chars === "//") {
                reader.next();
                parseSingleLineComment(reader);
                continue;
            }
        }
        const p = readPunctuator(reader);
        if (p != null) {
            yield p;
            continue;
        }
        if (isLineTerminator(char)) {
            reader.next();
            yield { type: "lineTerminator", value: "\n", start, end: reader.position };
            continue;
        }
        if (isWhiteSpace(char)) {
            reader.next();
            continue;
        }
        throw new UnexpectedCharacterError(
            "InputElement",
            "WhiteSpace or LineTerminator or Comment or Token",
            char,
            reader.position
        );
    }
    return { type: "end", value: "<EOF>", start: reader.position, end: reader.position };
}

function parseMultiLineComment(reader: Reader): boolean {
    let hasLineTerminator = false;
    while (!reader.eof) {
        const char = reader.next();
        if (isLineTerminator(char)) {
            hasLineTerminator = true;
        }
        if (char === "*") {
            // PostAsteriskCommentChars
            while (!reader.eof) {
                const char = reader.next();
                if (char !== "*" && char !== "/") {
                    // MultiLineNotForwardSlashOrAsteriskChar
                    if (isLineTerminator(char)) {
                        hasLineTerminator = true;
                    }
                    continue;
                }
                if (char === "*") {
                    continue;
                }
                if (char === "/") {
                    reader.next();
                    return hasLineTerminator;
                }
            }
        }
    }
    throw new UnexpectedCharacterError("MultiLineComment", "SourceCharacter", reader.current, reader.position);
}

function parseSingleLineComment(reader: Reader) {
    while (!reader.eof) {
        const char = reader.next();
        if (isLineTerminator(char)) {
            break;
        }
    }
}

function parseDecimalLiteral(reader: Reader, start: Position): NumericLiteral {
    // current: 0 or NonZeroDigit or .
    if (reader.current === ".") {
        reader.next();
        parseDecimalDigits(reader);
    } else if (isDecimalDigit(reader.current)) {
        parseDecimalDigits(reader);
        if (reader.current === ".") {
            reader.next();
            if (isDecimalDigit(reader.current)) {
                parseDecimalDigits(reader);
            }
        }
    } else {
        throw new UnexpectedCharacterError("DecimalLiteral", ". or DecimalDigit", reader.current, reader.position);
    }
    if (isExponentIndicator(reader.current)) {
        parseExponentPart(reader);
    }
    const value = reader.substring(start, reader.position);
    return { type: "numericLiteral", value: parseFloat(value), start, end: reader.prevPosition };
}

function parseDecimalDigits(reader: Reader) {
    const char = reader.current;
    if (!isDecimalDigit(char)) {
        throw new UnexpectedCharacterError("DecimalDigits", "DecimalDigit", char, reader.position);
    }
    while (!reader.eof) {
        const char = reader.next();
        if (!isDecimalDigit(char)) {
            break;
        }
    }
}

function parseExponentPart(reader: Reader) {
    if (!isExponentIndicator(reader.current)) {
        throw new UnexpectedCharacterError("ExponentPart", "ExponentIndicator", reader.current, reader.position);
    }
    let char = reader.next();
    if (char === "+" || char === "-") {
        char = reader.next();
    }
    parseDecimalDigits(reader);
}

function parseHexIntegerLiteral(reader: Reader, start: Position): NumericLiteral {
    const char = reader.next();
    if (!isHexDigit(char)) {
        throw new UnexpectedCharacterError("HexIntegerLiteral", "HexDigit", char, reader.position);
    }
    while (!reader.eof) {
        if (!isHexDigit(reader.next())) {
            break;
        }
    }
    const value = reader.substring(start, reader.position);
    return { type: "numericLiteral", value: parseInt(value), start, end: reader.prevPosition };
}

function parseOctalIntegerLiteral(reader: Reader, start: Position): NumericLiteral {
    const char = reader.current;
    if (!isOctalDigit(char)) {
        throw new UnexpectedCharacterError("OctalIntegerLiteral", "OctalDigit", char, reader.position);
    }
    while (!reader.eof) {
        if (!isOctalDigit(reader.next())) {
            break;
        }
    }
    const value = reader.substring(start, reader.position);
    return { type: "numericLiteral", value: parseInt(value, 8), start, end: reader.prevPosition };
}

function parseReservedWordOrToken(reader: Reader): ReservedWord | Token {
    const start = reader.position;
    while (!reader.eof) {
        const char = reader.next();
        if (isIdentifierLetter(char) || isDecimalDigit(char)) {
            continue;
        }
        break;
    }
    const value = reader.substring(start, reader.position);
    if (keywordsSet.has(value as Keywords)) {
        return { type: "keyword", value: value as Keywords, start, end: reader.prevPosition };
    }
    if (futureReservedWordsSet.has(value as FutureReservedWords)) {
        return { type: "futureReservedWord", value: value as FutureReservedWords, start, end: reader.prevPosition };
    }
    if (value === "null") {
        return { type: "nullLiteral", value: null, start, end: reader.prevPosition };
    }
    if (value === "true") {
        return { type: "booleanLiteral", value: true, start, end: reader.prevPosition };
    }
    if (value === "false") {
        return { type: "booleanLiteral", value: false, start, end: reader.prevPosition };
    }
    return { type: "identifier", value, start, end: reader.prevPosition };
}

function readStringLiteral(reader: Reader): StringLiteral {
    const start = reader.position;
    const c = reader.current;
    let value = "";
    while (!reader.eof) {
        const char = reader.next();
        if (char === c) {
            reader.next();
            return { type: "stringLiteral", value, start, end: reader.prevPosition };
        } else if (char === "\\") {
            // SingleEscapeCharacter
            const char = reader.next();
            switch (char) {
                case "'":
                case '"':
                case "\\":
                    value += char;
                    break;
                case "b":
                    value += "\u0008";
                    break;
                case "f":
                    value += "\u000C";
                    break;
                case "n":
                    value += "\u000A";
                    break;
                case "r":
                    value += "\u000D";
                    break;
                case "t":
                    value += "\u0009";
                    break;
                default:
                    if (char === "x") {
                        // HexEscapeSequence
                        const u1 = reader.next();
                        if (!isHexDigit(u1)) {
                            throw new UnexpectedCharacterError(
                                "HexEscapeSequence",
                                "HexDigit",
                                reader.current,
                                reader.position
                            );
                        }
                        const u2 = reader.next();
                        if (!isHexDigit(u2)) {
                            throw new UnexpectedCharacterError(
                                "HexEscapeSequence",
                                "HexDigit",
                                reader.current,
                                reader.position
                            );
                        }
                        value += String.fromCharCode(parseInt(u1 + u2, 16));
                    } else if (char === "u") {
                        // UnicodeEscapeSequence
                        const u1 = reader.next();
                        if (!isHexDigit(u1)) {
                            throw new UnexpectedCharacterError(
                                "UnicodeEscapeSequence",
                                "HexDigit",
                                reader.current,
                                reader.position
                            );
                        }
                        const u2 = reader.next();
                        if (!isHexDigit(u2)) {
                            throw new UnexpectedCharacterError(
                                "UnicodeEscapeSequence",
                                "HexDigit",
                                reader.current,
                                reader.position
                            );
                        }
                        const u3 = reader.next();
                        if (!isHexDigit(u3)) {
                            throw new UnexpectedCharacterError(
                                "UnicodeEscapeSequence",
                                "HexDigit",
                                reader.current,
                                reader.position
                            );
                        }
                        const u4 = reader.next();
                        if (!isHexDigit(u4)) {
                            throw new UnexpectedCharacterError(
                                "UnicodeEscapeSequence",
                                "HexDigit",
                                reader.current,
                                reader.position
                            );
                        }
                        value += String.fromCharCode(parseInt(u1 + u2 + u3 + u4, 16));
                    } else if (isZeroToThree(char)) {
                        const o1 = reader.next();
                        if (!isOctalDigit(o1)) {
                            throw new UnexpectedCharacterError(
                                "OctalEscapeSequence",
                                "OctalDigit",
                                reader.current,
                                reader.position
                            );
                        }
                        const o2 = reader.next();
                        if (!isOctalDigit(o2)) {
                            throw new UnexpectedCharacterError(
                                "OctalEscapeSequence",
                                "OctalDigit",
                                reader.current,
                                reader.position
                            );
                        }
                        value += String.fromCharCode(parseInt(char + o1 + o2, 8));
                    } else if (isOctalDigit(char)) {
                        let o = char;
                        // OctalEscapeSequence
                        if (isOctalDigit(reader.current)) {
                            o += reader.next();
                        }
                        value += String.fromCharCode(parseInt(o, 8));
                    } else if (isLineTerminator(char)) {
                        throw new UnexpectedCharacterError(
                            "StringLiteral",
                            "not LineTerminator",
                            reader.current,
                            reader.position
                        );
                    } else {
                        value += char;
                    }
                    break;
            }
        } else {
            value += char;
        }
    }
    throw new UnexpectedCharacterError("StringLiteral", "SourceCharacter", reader.current, reader.position);
}

function readPunctuator(reader: Reader): Punctuator | undefined {
    const start = reader.position;
    const chars = reader.peek(4);
    for (const p of punctuators) {
        if (chars.startsWith(p)) {
            reader.consume(p.length);
            return { type: "punctuator", value: p, start, end: reader.prevPosition };
        }
    }
    return undefined;
}

export type Program = {
    type: "program";
    sourceElements: SourceElement[];
    start: Position;
    end: Position;
};

export type SourceElement = Statement | FunctionDeclaration;

export type Statement =
    | Block
    | VariableStatement
    | EmptyStatement
    | ExpressionStatement
    | IfStatement
    | IterationStatement
    | ContinueStatement
    | BreakStatement
    | ReturnStatement
    | WithStatement;

export type Block = {
    type: "block";
    statementList: Statement[];
    start: Position;
    end: Position;
};

export type VariableStatement = {
    type: "variableStatement";
    variableDeclarationList: VariableDeclaration[];
    start: Position;
    end: Position;
};

export type VariableDeclaration = {
    name: string;
    initializer?: AssignmentExpression;
    start: Position;
    end: Position;
};

export type EmptyStatement = {
    type: "emptyStatement";
    start: Position;
    end: Position;
};

export type ExpressionStatement = {
    type: "expressionStatement";
    expression: Expression;
    start: Position;
    end: Position;
};

export type IfStatement = {
    type: "ifStatement";
    expression: Expression;
    thenStatement: Statement;
    elseStatement?: Statement;
    start: Position;
    end: Position;
};

export type IterationStatement = WhileStatement | ForStatement | ForInStatement;

export type WhileStatement = {
    type: "whileStatement";
    expression: Expression;
    statement: Statement;
};

export type ForStatement = {
    type: "forStatement";
    initialization?: Expression | VariableStatement;
    condition?: Expression;
    afterthought?: Expression;
    statement: Statement;
};

export type ForInStatement = {
    type: "forInStatement";
    initialization?: LeftHandSideExpression | VariableDeclaration;
    expression: Expression;
    statement: Statement;
};

export type ContinueStatement = {
    type: "continueStatement";
    start: Position;
    end: Position;
};

export type BreakStatement = {
    type: "breakStatement";
    start: Position;
    end: Position;
};

export type ReturnStatement = {
    type: "returnStatement";
    expression?: Expression;
    start: Position;
    end: Position;
};

export type WithStatement = {
    type: "withStatement";
    expression: Expression;
    statement: Statement;
    start: Position;
    end: Position;
};

export type FunctionDeclaration = {
    type: "functionDeclaration";
    name: string;
    parameters: string[];
    block: Block;
    start: Position;
    end: Position;
};

export type PrimaryExpression = ThisExpression | IdentifierExpression | LiteralExpression | GroupingOperator;

export type GroupingOperator = {
    type: "groupingOperator";
    expression: Expression;
    start: Position;
    end: Position;
};

export type ThisExpression = {
    type: "thisExpression";
    start: Position;
    end: Position;
};

export type IdentifierExpression = {
    type: "identifierExpression";
    name: string;
    start: Position;
    end: Position;
};

export type LiteralExpression = {
    type: "literalExpression";
    value: string | number | boolean | null;
    start: Position;
    end: Position;
};

export type MemberExpression = PrimaryExpression | MemberOperator | NewOperator;

export type MemberOperator =
    | {
          type: "memberOperator";
          left: CallExpression | MemberExpression;
          name: string;
          start: Position;
          end: Position;
      }
    | {
          type: "memberOperator";
          left: CallExpression | MemberExpression;
          right: Expression;
          start: Position;
          end: Position;
      };

export type NewOperator = {
    type: "newOperator";
    expression: CallExpression | MemberExpression;
    argumentList: AssignmentExpression[] | undefined;
    start: Position;
    end: Position;
};

export type CallExpression = {
    type: "callOperator";
    expression: CallExpression | MemberExpression;
    argumentList: AssignmentExpression[];
    start: Position;
    end: Position;
};

export type LeftHandSideExpression = MemberExpression | CallExpression;

export type PostfixExpression = LeftHandSideExpression | PostfixIncrementOperator | PostfixDecrementOperator;

export type PostfixIncrementOperator = {
    type: "postfixIncrementOperator";
    expression: LeftHandSideExpression;
    start: Position;
    end: Position;
};

export type PostfixDecrementOperator = {
    type: "postfixDecrementOperator";
    expression: LeftHandSideExpression;
    start: Position;
    end: Position;
};

export type UnaryExpression =
    | PostfixExpression
    | DeleteOperator
    | VoidOperator
    | TypeofOperator
    | PrefixIncrementOperator
    | PrefixDecrementOperator
    | UnaryPlusOperator
    | UnaryMiusOperator
    | BitwiseNotOperator
    | LogicalNotOperator;

export type DeleteOperator = {
    type: "deleteOperator";
    expression: UnaryExpression;
    start: Position;
    end: Position;
};

export type VoidOperator = {
    type: "voidOperator";
    expression: UnaryExpression;
    start: Position;
    end: Position;
};

export type TypeofOperator = {
    type: "typeofOperator";
    expression: UnaryExpression;
    start: Position;
    end: Position;
};

export type PrefixIncrementOperator = {
    type: "prefixIncrementOperator";
    expression: UnaryExpression;
    start: Position;
    end: Position;
};

export type PrefixDecrementOperator = {
    type: "prefixDecrementOperator";
    expression: UnaryExpression;
    start: Position;
    end: Position;
};

export type UnaryPlusOperator = {
    type: "unaryPlusOperator";
    expression: UnaryExpression;
    start: Position;
    end: Position;
};

export type UnaryMiusOperator = {
    type: "unaryMiusOperator";
    expression: UnaryExpression;
    start: Position;
    end: Position;
};

export type BitwiseNotOperator = {
    type: "bitwiseNotOperator";
    expression: UnaryExpression;
    start: Position;
    end: Position;
};

export type LogicalNotOperator = {
    type: "logicalNotOperator";
    expression: UnaryExpression;
    start: Position;
    end: Position;
};

export type MultiplicativeExpression = UnaryExpression | MultiplyOperator | DivideOperator | ModuloOperator;

export type MultiplyOperator = {
    type: "multiplyOperator";
    left: MultiplicativeExpression;
    right: UnaryExpression;
    start: Position;
    end: Position;
};

export type DivideOperator = {
    type: "divideOperator";
    left: MultiplicativeExpression;
    right: UnaryExpression;
    start: Position;
    end: Position;
};

export type ModuloOperator = {
    type: "moduloOperator";
    left: MultiplicativeExpression;
    right: UnaryExpression;
    start: Position;
    end: Position;
};

export type AdditiveExpression = MultiplicativeExpression | AddOperator | SubtractOperator;

export type AddOperator = {
    type: "addOperator";
    left: AdditiveExpression;
    right: MultiplicativeExpression;
    start: Position;
    end: Position;
};

export type SubtractOperator = {
    type: "subtractOperator";
    left: AdditiveExpression;
    right: MultiplicativeExpression;
    start: Position;
    end: Position;
};

export type ShiftExpression =
    | AdditiveExpression
    | LeftShiftOperator
    | SignedRightShiftOperator
    | UnsignedRightShiftOperator;

export type LeftShiftOperator = {
    type: "leftShiftOperator";
    left: ShiftExpression;
    right: AdditiveExpression;
    start: Position;
    end: Position;
};

export type SignedRightShiftOperator = {
    type: "signedRightShiftOperator";
    left: ShiftExpression;
    right: AdditiveExpression;
    start: Position;
    end: Position;
};

export type UnsignedRightShiftOperator = {
    type: "unsignedRightShiftOperator";
    left: ShiftExpression;
    right: AdditiveExpression;
    start: Position;
    end: Position;
};

export type RelationalExpression =
    | ShiftExpression
    | LessThanOperator
    | GreaterThanOperator
    | LessThanOrEqualOperator
    | GreaterThanOrEqualOperator;

export type LessThanOperator = {
    type: "lessThanOperator";
    left: RelationalExpression;
    right: ShiftExpression;
    start: Position;
    end: Position;
};

export type GreaterThanOperator = {
    type: "greaterThanOperator";
    left: RelationalExpression;
    right: ShiftExpression;
    start: Position;
    end: Position;
};

export type LessThanOrEqualOperator = {
    type: "lessThanOrEqualOperator";
    left: RelationalExpression;
    right: ShiftExpression;
    start: Position;
    end: Position;
};

export type GreaterThanOrEqualOperator = {
    type: "greaterThanOrEqualOperator";
    left: RelationalExpression;
    right: ShiftExpression;
    start: Position;
    end: Position;
};

export type EqualityExpression = RelationalExpression | EqualsOperator | DoesNotEqualsOperator;

export type EqualsOperator = {
    type: "equalsOperator";
    left: EqualityExpression;
    right: RelationalExpression;
    start: Position;
    end: Position;
};

export type DoesNotEqualsOperator = {
    type: "doesNotEqualsOperator";
    left: EqualityExpression;
    right: RelationalExpression;
    start: Position;
    end: Position;
};

export type BitwiseAndExpression = EqualityExpression | BitwiseAndOperator;

export type BitwiseAndOperator = {
    type: "bitwiseAndOperator";
    left: BitwiseAndExpression;
    right: EqualityExpression;
    start: Position;
    end: Position;
};

export type BitwiseXorExpression = BitwiseAndExpression | BitwiseXorOperator;

export type BitwiseXorOperator = {
    type: "bitwiseXorOperator";
    left: BitwiseXorExpression;
    right: BitwiseAndExpression;
    start: Position;
    end: Position;
};

export type BitwiseOrExpression = BitwiseXorExpression | BitwiseOrOperator;

export type BitwiseOrOperator = {
    type: "bitwiseOrOperator";
    left: BitwiseOrExpression;
    right: BitwiseXorExpression;
    start: Position;
    end: Position;
};

export type LogicalAndExpression = BitwiseOrExpression | LogicalAndOperator;

export type LogicalAndOperator = {
    type: "logicalAndOperator";
    left: LogicalAndExpression;
    right: BitwiseOrExpression;
    start: Position;
    end: Position;
};

export type LogicalOrExpression = LogicalAndExpression | LogicalOrOperator;

export type LogicalOrOperator = {
    type: "logicalOrOperator";
    left: LogicalOrExpression;
    right: LogicalAndExpression;
    start: Position;
    end: Position;
};

export type ConditionalExpression = LogicalOrExpression | ConditionalOperator;

export type ConditionalOperator = {
    type: "conditionalOperator";
    conditionExpression: LogicalOrExpression;
    thenExpression: AssignmentExpression;
    elseExpression: AssignmentExpression;
    start: Position;
    end: Position;
};

export type AssignmentExpression = ConditionalExpression | AssignmentOperator;

export type AssignmentOperator = {
    type: "assignmentOperator";
    left: LeftHandSideExpression;
    operator: "=" | "*=" | "/=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | ">>>=" | "&=" | "^=" | "|=";
    right: AssignmentExpression;
    start: Position;
    end: Position;
};

export type Expression = AssignmentExpression | CommaOperator;

export type CommaOperator = {
    type: "commaOperator";
    left: Expression;
    right: AssignmentExpression;
    start: Position;
    end: Position;
};

class Tokenizer {
    private iterator: Generator<Token, EndToken>;
    private _current: Token;
    private _prevPosition: Position = {
        index: 0,
        line: 1,
        column: 1,
    };
    constructor(source: string) {
        this.iterator = tokenize(source);
        while (true) {
            this._current = this.iterator.next().value;
            if (this._current?.type !== "lineTerminator") {
                break;
            }
        }
    }
    next(): Token {
        this._prevPosition = this._current.end;
        while (true) {
            this._current = this.iterator.next().value;
            if (this._current?.type !== "lineTerminator") {
                return this._current;
            }
        }
    }
    get current(): Token {
        return this._current;
    }
    get prevPosition(): Position {
        return this._prevPosition;
    }
}

export function parse(source: string) {
    const tokenizer = new Tokenizer(source);
    return parseProgram(tokenizer);
}

export function parseProgram(tokenizer: Tokenizer): Program {
    const begin = tokenizer.current;
    const sourceElements: SourceElement[] = [];
    while (tokenizer.current.type !== "end") {
        sourceElements.push(parseSourceElement(tokenizer));
    }
    return {
        type: "program",
        sourceElements,
        start: begin.start,
        end: tokenizer.current.end,
    };
}

function parseSourceElement(tokenizer: Tokenizer): SourceElement {
    const token = tokenizer.current;
    if (token.type === "keyword" && token.value === "function") {
        return parseFunctionDeclaration(tokenizer);
    } else {
        return parseStatement(tokenizer);
    }
}

function parseFunctionDeclaration(tokenizer: Tokenizer): FunctionDeclaration {
    const token = tokenizer.current;
    if (token.type !== "keyword" || token.value !== "function") {
        throw new UnexpectedTokenError("FunctionDeclaration", "function", token);
    }
    const name = tokenizer.next();
    if (name.type !== "identifier") {
        throw new UnexpectedTokenError("FunctionDeclaration", "identifier", name);
    }
    const p = tokenizer.next();
    if (p.type !== "punctuator" || p.value !== "(") {
        throw new UnexpectedTokenError("FunctionDeclaration", "(", p);
    }
    const parameters: string[] = [];
    while (true) {
        const parameterOrEnd = tokenizer.next();
        if (parameterOrEnd.type === "punctuator" && parameterOrEnd.value === ")") {
            break;
        }
        if (parameterOrEnd.type !== "identifier") {
            throw new UnexpectedTokenError("FormalParameterList", ") or identifier", parameterOrEnd);
        }
        parameters.push(parameterOrEnd.value);
        const commaOrEnd = tokenizer.next();
        if (commaOrEnd.type === "punctuator" && commaOrEnd.value === ")") {
            break;
        }
        if (commaOrEnd.type === "punctuator" && commaOrEnd.value === ",") {
            continue;
        }
        throw new UnexpectedTokenError("FormalParameterList", ", or )", commaOrEnd);
    }
    tokenizer.next();
    return {
        type: "functionDeclaration",
        name: name.value,
        parameters,
        block: parseBlock(tokenizer),
        start: token.start,
        end: tokenizer.prevPosition,
    };
}

function parseStatement(tokenizer: Tokenizer): Statement {
    const begin = tokenizer.current;
    if (begin.type === "punctuator" && begin.value === "{") {
        return parseBlock(tokenizer);
    }
    if (begin.type === "keyword" && begin.value === "var") {
    }
    if (begin.type === "keyword") {
        if (
            begin.value === "this" ||
            begin.value === "delete" ||
            begin.value === "void" ||
            begin.value === "typeof" ||
            begin.value === "new"
        ) {
            return parseExpressionStatement(tokenizer);
        }
    }
    if (
        begin.type === "identifier" ||
        begin.type === "nullLiteral" ||
        begin.type === "numericLiteral" ||
        begin.type === "stringLiteral" ||
        begin.type === "booleanLiteral"
    ) {
        return parseExpressionStatement(tokenizer);
    }
    if (begin.type === "punctuator") {
        if (
            begin.value === "(" ||
            begin.value === "++" ||
            begin.value === "--" ||
            begin.value === "+" ||
            begin.value === "-" ||
            begin.value === "~" ||
            begin.value === "!"
        ) {
            return parseExpressionStatement(tokenizer);
        }
    }
    if (begin.type === "punctuator" && begin.value === ";") {
    }
    if (begin.type === "keyword" && begin.value === "if") {
    }
    if (begin.type === "keyword" && begin.value === "for") {
    }
    if (begin.type === "keyword" && begin.value === "while") {
    }
    if (begin.type === "keyword" && begin.value === "continue") {
    }
    if (begin.type === "keyword" && begin.value === "break") {
    }
    if (begin.type === "keyword" && begin.value === "return") {
    }
    if (begin.type === "keyword" && begin.value === "with") {
    }
    throw new Error();
}

function parseSemicolon(tokenizer: Tokenizer): boolean {
    const token = tokenizer.current;
    if (token.type === "end") {
        return true;
    }
    if (token.type === "punctuator" && token.value === ";") {
        tokenizer.next();
        return true;
    }
    if (token.type === "punctuator" && token.value === "}") {
        return true;
    }
    if (tokenizer.prevPosition.line !== token.start.line) {
        return true;
    }
    return false;
}

function parseExpressionStatement(tokenizer: Tokenizer): ExpressionStatement {
    const begin = tokenizer.current;
    const statement: ExpressionStatement = {
        type: "expressionStatement",
        expression: parseExpression(tokenizer),
        start: begin.start,
        end: tokenizer.prevPosition,
    };
    if (!parseSemicolon(tokenizer)) {
        throw new UnexpectedTokenError("ExpressionStatement", "; or } or LineTerminator", tokenizer.current);
    }
    return statement;
}

function parseBlock(tokenizer: Tokenizer): Block {
    const begin = tokenizer.current;
    if (begin.type !== "punctuator" || begin.value !== "{") {
        throw new UnexpectedTokenError("Block", "{", begin);
    }
    const statementList: Statement[] = [];
    while (true) {
        const end = tokenizer.next();
        const statement = parseStatement(tokenizer);
        if (statement != null) {
            statementList.push(statement);
            continue;
        }
        if (end.type !== "punctuator" || end.value !== "}") {
            throw new UnexpectedTokenError("Block", "}", end);
        }
        break;
    }
    tokenizer.next();
    return {
        type: "block",
        statementList,
        start: begin.start,
        end: tokenizer.prevPosition,
    };
}

function parsePrimaryExpression(tokenizer: Tokenizer): PrimaryExpression {
    const token = tokenizer.current;
    if (token.type === "keyword" && token.value === "this") {
        tokenizer.next();
        return {
            type: "thisExpression",
            start: token.start,
            end: token.end,
        };
    }
    if (token.type === "identifier") {
        tokenizer.next();
        return {
            type: "identifierExpression",
            name: token.value,
            start: token.start,
            end: token.end,
        };
    }
    if (
        token.type === "nullLiteral" ||
        token.type === "numericLiteral" ||
        token.type === "stringLiteral" ||
        token.type === "booleanLiteral"
    ) {
        tokenizer.next();
        return {
            type: "literalExpression",
            value: token.value,
            start: token.start,
            end: token.end,
        };
    }
    if (token.type === "punctuator" && token.value === "(") {
        tokenizer.next();
        const expression = parseExpression(tokenizer);
        const end = tokenizer.current;
        if (end.type !== "punctuator" || end.value !== ")") {
            throw new UnexpectedTokenError("GroupingOperator", ")", end);
        }
        tokenizer.next();
        return {
            type: "groupingOperator",
            expression,
            start: token.start,
            end: end.end,
        };
    }
    throw new UnexpectedTokenError("PrimaryExpression", "this or Identifier or Literal or (", token);
}

function parseMemberExpression(tokenizer: Tokenizer): MemberExpression {
    const begin = tokenizer.current;
    let left: MemberExpression;
    if (begin.type === "keyword" && begin.value === "new") {
        tokenizer.next();
        const expression = parseMemberExpression(tokenizer);
        const token = tokenizer.current;
        if (token.type === "punctuator" && token.value === "(") {
            left = {
                type: "newOperator",
                expression,
                argumentList: parseArgumentList(tokenizer),
                start: begin.start,
                end: tokenizer.current.end,
            };
            const token = tokenizer.current;
            if (token.type !== "punctuator" && token.value !== ")") {
                throw new UnexpectedTokenError("MemberExpression", ")", token);
            }
            tokenizer.next();
        } else {
            left = {
                type: "newOperator",
                expression,
                argumentList: undefined,
                start: begin.start,
                end: tokenizer.current.end,
            };
        }
    } else {
        left = parsePrimaryExpression(tokenizer);
    }
    while (true) {
        const token = tokenizer.current;
        if (token.type === "punctuator" && token.value === ".") {
            const token = tokenizer.next();
            if (token.type !== "identifier") {
                throw new UnexpectedTokenError("MemberExpression", "Identifier", token);
            }
            left = {
                type: "memberOperator",
                left,
                name: token.value,
                start: left.start,
                end: token.end,
            };
            tokenizer.next();
        } else if (token.type === "punctuator" && token.value === "[") {
            tokenizer.next();
            const right = parseExpression(tokenizer);
            const token = tokenizer.current;
            if (token.type !== "punctuator" || token.value !== "]") {
                throw new UnexpectedTokenError("MemberExpression", "]", token);
            }
            left = {
                type: "memberOperator",
                left,
                right,
                start: left.start,
                end: right.end,
            };
            tokenizer.next();
        } else {
            return left;
        }
    }
}

function parseLeftHandSideExpression(tokenizer: Tokenizer): LeftHandSideExpression {
    let left: LeftHandSideExpression = parseMemberExpression(tokenizer);
    while (true) {
        const token = tokenizer.current;
        if (token.type === "punctuator" && token.value === "(") {
            left = {
                type: "callOperator",
                expression: left,
                argumentList: parseArgumentList(tokenizer),
                start: left.start,
                end: tokenizer.prevPosition,
            };
            const token = tokenizer.current;
            if (token.type !== "punctuator" || token.value !== ")") {
                throw new UnexpectedTokenError("MemberExpression", ")", token);
            }
            tokenizer.next();
        } else if (token.type === "punctuator" && token.value === ".") {
            const token = tokenizer.next();
            if (token.type !== "identifier") {
                throw new UnexpectedTokenError("MemberExpression", "Identifier", token);
            }
            left = {
                type: "memberOperator",
                left,
                name: token.value,
                start: left.start,
                end: token.end,
            };
            tokenizer.next();
        } else if (token.type === "punctuator" && token.value === "[") {
            tokenizer.next();
            const right = parseExpression(tokenizer);
            const token = tokenizer.current;
            if (token.type !== "punctuator" || token.value !== "]") {
                throw new UnexpectedTokenError("MemberExpression", "]", token);
            }
            left = {
                type: "memberOperator",
                left,
                right,
                start: left.start,
                end: right.end,
            };
            tokenizer.next();
        } else {
            return left;
        }
    }
}

function parseArgumentList(tokenizer: Tokenizer): AssignmentExpression[] {
    const argumentList: AssignmentExpression[] = [];
    const begin = tokenizer.current;
    if (begin.type !== "punctuator" || begin.value !== "(") {
        throw new UnexpectedTokenError("ArgumentList", "(", begin);
    }
    while (true) {
        const token = tokenizer.next();
        if (token.type === "punctuator" && token.value === ")") {
            return argumentList;
        }
        argumentList.push(parseAssignmentExpression(tokenizer));
        const endOrComma = tokenizer.current;
        if (endOrComma.type === "punctuator" && endOrComma.value === ")") {
            return argumentList;
        }
        if (endOrComma.type !== "punctuator" && endOrComma.value !== ",") {
            throw new UnexpectedTokenError("ArgumentList", ",", endOrComma);
        }
    }
}

function parsePostfixExpression(tokenizer: Tokenizer): PostfixExpression {
    const begin = tokenizer.current;
    const expression = parseLeftHandSideExpression(tokenizer);
    const token = tokenizer.current;
    if (begin.end.line !== token.end.line) {
        return expression;
    }
    if (token.type === "punctuator" && token.value === "++") {
        tokenizer.next();
        return {
            type: "postfixIncrementOperator",
            expression,
            start: expression.start,
            end: token.end,
        };
    } else if (token.type === "punctuator" && token.value === "--") {
        tokenizer.next();
        return {
            type: "postfixDecrementOperator",
            expression,
            start: expression.start,
            end: token.end,
        };
    } else {
        return expression;
    }
}

function parseUnaryExpression(tokenizer: Tokenizer): UnaryExpression {
    const begin = tokenizer.current;
    if (begin.type === "keyword") {
        switch (begin.value) {
            case "delete": {
                tokenizer.next();
                return {
                    type: "deleteOperator",
                    expression: parseUnaryExpression(tokenizer),
                    start: begin.start,
                    end: tokenizer.prevPosition,
                };
            }
            case "void": {
                tokenizer.next();
                return {
                    type: "voidOperator",
                    expression: parseUnaryExpression(tokenizer),
                    start: begin.start,
                    end: tokenizer.prevPosition,
                };
            }
            case "typeof": {
                tokenizer.next();
                return {
                    type: "typeofOperator",
                    expression: parseUnaryExpression(tokenizer),
                    start: begin.start,
                    end: tokenizer.prevPosition,
                };
            }
        }
    } else if (begin.type === "punctuator") {
        switch (begin.value) {
            case "++": {
                tokenizer.next();
                return {
                    type: "prefixIncrementOperator",
                    expression: parseUnaryExpression(tokenizer),
                    start: begin.start,
                    end: tokenizer.prevPosition,
                };
            }
            case "--": {
                tokenizer.next();
                return {
                    type: "prefixDecrementOperator",
                    expression: parseUnaryExpression(tokenizer),
                    start: begin.start,
                    end: tokenizer.prevPosition,
                };
            }
            case "+": {
                tokenizer.next();
                return {
                    type: "unaryPlusOperator",
                    expression: parseUnaryExpression(tokenizer),
                    start: begin.start,
                    end: tokenizer.prevPosition,
                };
            }
            case "-": {
                tokenizer.next();
                return {
                    type: "unaryMiusOperator",
                    expression: parseUnaryExpression(tokenizer),
                    start: begin.start,
                    end: tokenizer.prevPosition,
                };
            }
            case "~": {
                tokenizer.next();
                return {
                    type: "bitwiseNotOperator",
                    expression: parseUnaryExpression(tokenizer),
                    start: begin.start,
                    end: tokenizer.prevPosition,
                };
            }
            case "!": {
                tokenizer.next();
                return {
                    type: "logicalNotOperator",
                    expression: parseUnaryExpression(tokenizer),
                    start: begin.start,
                    end: tokenizer.prevPosition,
                };
            }
        }
    }
    return parsePostfixExpression(tokenizer);
}

function parseMultiplicativeExpression(tokenizer: Tokenizer): MultiplicativeExpression {
    let left: MultiplicativeExpression = parseUnaryExpression(tokenizer);
    while (true) {
        const token = tokenizer.current;
        if (token.type === "punctuator" && token.value === "*") {
            tokenizer.next();
            left = {
                type: "multiplyOperator",
                left,
                right: parseUnaryExpression(tokenizer),
                start: left.start,
                end: tokenizer.prevPosition,
            };
        } else if (token.type === "punctuator" && token.value === "/") {
            tokenizer.next();
            left = {
                type: "divideOperator",
                left,
                right: parseUnaryExpression(tokenizer),
                start: left.start,
                end: tokenizer.prevPosition,
            };
        } else if (token.type === "punctuator" && token.value === "%") {
            tokenizer.next();
            left = {
                type: "moduloOperator",
                left,
                right: parseUnaryExpression(tokenizer),
                start: left.start,
                end: tokenizer.prevPosition,
            };
        } else {
            return left;
        }
    }
}

function parseAdditiveExpression(tokenizer: Tokenizer): AdditiveExpression {
    let left: AdditiveExpression = parseMultiplicativeExpression(tokenizer);
    while (true) {
        const token = tokenizer.current;
        if (token.type === "punctuator" && token.value === "+") {
            tokenizer.next();
            left = {
                type: "addOperator",
                left,
                right: parseMultiplicativeExpression(tokenizer),
                start: left.start,
                end: tokenizer.prevPosition,
            };
        } else if (token.type === "punctuator" && token.value === "-") {
            tokenizer.next();
            left = {
                type: "subtractOperator",
                left,
                right: parseMultiplicativeExpression(tokenizer),
                start: left.start,
                end: tokenizer.prevPosition,
            };
        } else {
            return left;
        }
    }
}

function parseShiftExpression(tokenizer: Tokenizer): ShiftExpression {
    let left: ShiftExpression = parseAdditiveExpression(tokenizer);
    while (true) {
        const token = tokenizer.current;
        if (token.type === "punctuator" && token.value === "<<") {
            tokenizer.next();
            left = {
                type: "leftShiftOperator",
                left,
                right: parseAdditiveExpression(tokenizer),
                start: left.start,
                end: tokenizer.prevPosition,
            };
        } else if (token.type === "punctuator" && token.value === ">>") {
            tokenizer.next();
            left = {
                type: "signedRightShiftOperator",
                left,
                right: parseAdditiveExpression(tokenizer),
                start: left.start,
                end: tokenizer.prevPosition,
            };
        } else if (token.type === "punctuator" && token.value === ">>>") {
            tokenizer.next();
            left = {
                type: "unsignedRightShiftOperator",
                left,
                right: parseAdditiveExpression(tokenizer),
                start: left.start,
                end: tokenizer.prevPosition,
            };
        } else {
            return left;
        }
    }
}

function parseRelationalExpression(tokenizer: Tokenizer): RelationalExpression {
    let left: RelationalExpression = parseShiftExpression(tokenizer);
    while (true) {
        const token = tokenizer.current;
        if (token.type === "punctuator" && token.value === "<") {
            tokenizer.next();
            left = {
                type: "lessThanOperator",
                left,
                right: parseShiftExpression(tokenizer),
                start: left.start,
                end: tokenizer.prevPosition,
            };
        } else if (token.type === "punctuator" && token.value === ">") {
            tokenizer.next();
            left = {
                type: "greaterThanOperator",
                left,
                right: parseShiftExpression(tokenizer),
                start: left.start,
                end: tokenizer.prevPosition,
            };
        } else if (token.type === "punctuator" && token.value === "<=") {
            tokenizer.next();
            left = {
                type: "lessThanOrEqualOperator",
                left,
                right: parseShiftExpression(tokenizer),
                start: left.start,
                end: tokenizer.prevPosition,
            };
        } else if (token.type === "punctuator" && token.value === ">=") {
            tokenizer.next();
            left = {
                type: "greaterThanOrEqualOperator",
                left,
                right: parseShiftExpression(tokenizer),
                start: left.start,
                end: tokenizer.prevPosition,
            };
        } else {
            return left;
        }
    }
}

function parseEqualityExpression(tokenizer: Tokenizer): EqualityExpression {
    let left: EqualityExpression = parseRelationalExpression(tokenizer);
    while (true) {
        const token = tokenizer.current;
        if (token.type === "punctuator" && token.value === "==") {
            tokenizer.next();
            left = {
                type: "equalsOperator",
                left,
                right: parseRelationalExpression(tokenizer),
                start: left.start,
                end: tokenizer.prevPosition,
            };
        } else if (token.type === "punctuator" && token.value === "!=") {
            tokenizer.next();
            left = {
                type: "doesNotEqualsOperator",
                left,
                right: parseRelationalExpression(tokenizer),
                start: left.start,
                end: tokenizer.prevPosition,
            };
        } else {
            return left;
        }
    }
}

function parseBitwiseAndExpression(tokenizer: Tokenizer): BitwiseAndExpression {
    let left: BitwiseAndExpression = parseEqualityExpression(tokenizer);
    while (true) {
        const token = tokenizer.current;
        if (token.type === "punctuator" && token.value === "&") {
            tokenizer.next();
            left = {
                type: "bitwiseAndOperator",
                left,
                right: parseEqualityExpression(tokenizer),
                start: left.start,
                end: tokenizer.prevPosition,
            };
        } else {
            return left;
        }
    }
}

function parseBitwiseXorExpression(tokenizer: Tokenizer): BitwiseXorExpression {
    let left: BitwiseXorExpression = parseBitwiseAndExpression(tokenizer);
    while (true) {
        const token = tokenizer.current;
        if (token.type === "punctuator" && token.value === "^") {
            tokenizer.next();
            left = {
                type: "bitwiseXorOperator",
                left,
                right: parseBitwiseAndExpression(tokenizer),
                start: left.start,
                end: tokenizer.prevPosition,
            };
        } else {
            return left;
        }
    }
}

function parseBitwiseOrExpression(tokenizer: Tokenizer): BitwiseOrExpression {
    let left: BitwiseOrExpression = parseBitwiseXorExpression(tokenizer);
    while (true) {
        const token = tokenizer.current;
        if (token.type === "punctuator" && token.value === "|") {
            tokenizer.next();
            left = {
                type: "bitwiseOrOperator",
                left,
                right: parseBitwiseXorExpression(tokenizer),
                start: left.start,
                end: tokenizer.prevPosition,
            };
        } else {
            return left;
        }
    }
}

function parseLogicalAndExpression(tokenizer: Tokenizer): LogicalAndExpression {
    let left: LogicalAndExpression = parseBitwiseOrExpression(tokenizer);
    while (true) {
        const token = tokenizer.current;
        if (token.type === "punctuator" && token.value === "&&") {
            tokenizer.next();
            left = {
                type: "logicalAndOperator",
                left,
                right: parseBitwiseOrExpression(tokenizer),
                start: left.start,
                end: tokenizer.prevPosition,
            };
        } else {
            return left;
        }
    }
}

function parseLogicalOrExpression(tokenizer: Tokenizer): LogicalOrExpression {
    let left: LogicalOrExpression = parseLogicalAndExpression(tokenizer);
    while (true) {
        const token = tokenizer.current;
        if (token.type === "punctuator" && token.value === "||") {
            tokenizer.next();
            left = {
                type: "logicalOrOperator",
                left,
                right: parseLogicalAndExpression(tokenizer),
                start: left.start,
                end: tokenizer.prevPosition,
            };
        } else {
            return left;
        }
    }
}

function parseConditionalExpression(tokenizer: Tokenizer): ConditionalExpression {
    let conditionExpression: LogicalOrExpression = parseLogicalOrExpression(tokenizer);
    const token = tokenizer.current;
    if (token.type === "punctuator" && token.value === "?") {
        tokenizer.next();
        const thenExpression = parseAssignmentExpression(tokenizer);
        if (tokenizer.current.type !== "punctuator" || tokenizer.current.value !== ":") {
            throw new UnexpectedTokenError("ConditionalExpression", ":", tokenizer.current);
        }
        tokenizer.next();
        return {
            type: "conditionalOperator",
            conditionExpression,
            thenExpression,
            elseExpression: parseAssignmentExpression(tokenizer),
            start: conditionExpression.start,
            end: tokenizer.prevPosition,
        };
    } else {
        return conditionExpression;
    }
}

function parseAssignmentExpression(tokenizer: Tokenizer): AssignmentExpression {
    const cond = parseConditionalExpression(tokenizer);
    if (
        cond.type === "thisExpression" ||
        cond.type === "identifierExpression" ||
        cond.type === "literalExpression" ||
        cond.type === "groupingOperator" ||
        cond.type === "memberOperator" ||
        cond.type === "callOperator" ||
        cond.type === "newOperator"
    ) {
        const left: LeftHandSideExpression = cond;
        const token = tokenizer.current;
        if (token.type === "punctuator") {
            switch (token.value) {
                case "=":
                case "*=":
                case "/=":
                case "%=":
                case "+=":
                case "-=":
                case "<<=":
                case ">>=":
                case ">>>=":
                case "&=":
                case "^=":
                    tokenizer.next();
                    return {
                        type: "assignmentOperator",
                        left,
                        operator: token.value,
                        right: parseAssignmentExpression(tokenizer),
                        start: left.start,
                        end: tokenizer.prevPosition,
                    };
            }
        }
    }
    return cond;
}

function parseExpression(tokenizer: Tokenizer): Expression {
    let left: Expression = parseAssignmentExpression(tokenizer);
    while (true) {
        const token = tokenizer.current;
        if (token.type === "punctuator" && token.value === ",") {
            tokenizer.next();
            left = {
                type: "commaOperator",
                left,
                right: parseAssignmentExpression(tokenizer),
                start: left.start,
                end: tokenizer.prevPosition,
            };
        } else {
            return left;
        }
    }
}
