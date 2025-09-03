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

class SyntaxError extends Error {
    constructor(syntax: string, message: string, position: Position) {
        super(`${syntax}: ${message}, ${position.line}:${position.column}`);
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
            this.column = 0;
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
                if (char === "." || isDecimalDigit(char) || isExponentIndicator(char)) {
                    yield parseDecimalLiteral(reader, start);
                } else {
                    yield { type: "numericLiteral", value: 0, start, end: reader.prevPosition };
                }
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
    // current: DecimalDigit or .
    if (reader.current === ".") {
        reader.next();
        if (isDecimalDigit(reader.current)) {
            parseDecimalDigits(reader);
        }
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
    return { type: "numericLiteral", value: parseFloat(value), start, end: reader.prevPosition }; // l
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
        } else if (isLineTerminator(char)) {
            throw new UnexpectedCharacterError("StringLiteral", "not LineTerminator", reader.current, reader.position);
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
    type: "variableDeclaration";
    name: string;
    initializer: AssignmentExpression | undefined;
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
    elseStatement: Statement | undefined;
    start: Position;
    end: Position;
};

export type IterationStatement = WhileStatement | ForStatement | ForInStatement;

export type WhileStatement = {
    type: "whileStatement";
    expression: Expression;
    statement: Statement;
    start: Position;
    end: Position;
};

export type ForStatement = {
    type: "forStatement";
    initialization: Expression | VariableStatement | undefined;
    condition: Expression | undefined;
    afterthought: Expression | undefined;
    statement: Statement;
    start: Position;
    end: Position;
};

export type ForInStatement = {
    type: "forInStatement";
    initialization: LeftHandSideExpression | VariableDeclaration;
    expression: Expression;
    statement: Statement;
    start: Position;
    end: Position;
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
    expression: Expression | undefined;
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
    | UnaryMinusOperator
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

export type UnaryMinusOperator = {
    type: "unaryMinusOperator";
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

function visitStatement(statement: Statement, visitor: (statement: Statement) => void) {
    visitor(statement);
    switch (statement.type) {
        case "block":
            for (const s of statement.statementList) {
                visitStatement(s, visitor);
            }
            break;
        case "forStatement": {
            if (statement.initialization?.type === "variableStatement") {
                visitStatement(statement.initialization, visitor);
            }
            visitStatement(statement.statement, visitor);
            break;
        }
        case "forInStatement": {
            visitStatement(statement.statement, visitor);
            break;
        }
        case "whileStatement":
        case "withStatement":
            visitStatement(statement.statement, visitor);
            break;
        case "variableStatement":
        case "emptyStatement":
        case "expressionStatement":
            break;
        case "ifStatement": {
            visitStatement(statement.thenStatement, visitor);
            if (statement.elseStatement != null) {
                visitStatement(statement.elseStatement, visitor);
            }
        }
        case "continueStatement":
        case "breakStatement":
        case "returnStatement":
            break;
        default: {
            const _: never = statement;
            throw new Error("unreachable");
        }
    }
}

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
            if (this._current.type !== "lineTerminator") {
                break;
            }
        }
    }
    next(): Token {
        this._prevPosition = this._current.end;
        while (true) {
            this._current = this.iterator.next().value;
            if (this._current.type !== "lineTerminator") {
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

export function parse(source: string): Program {
    const tokenizer = new Tokenizer(source);
    return parseProgram(tokenizer);
}

export function parseStatementList(source: string): Block {
    const tokenizer = new Tokenizer(source);
    const begin = tokenizer.current;
    const statementList: Statement[] = [];
    while (true) {
        const end = tokenizer.current;
        if (end.type === "end") {
            break;
        }
        statementList.push(parseStatement(tokenizer));
    }
    return {
        type: "block",
        statementList,
        start: begin.start,
        end: tokenizer.prevPosition,
    };
}

function parseProgram(tokenizer: Tokenizer): Program {
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

function parseFormalParameterList(tokenizer: Tokenizer): string[] {
    const parameters: string[] = [];
    while (true) {
        const parameterOrEnd = tokenizer.current;
        if (parameterOrEnd.type !== "identifier") {
            break;
        }
        parameters.push(parameterOrEnd.value);
        const commaOrEnd = tokenizer.next();
        if (commaOrEnd.type === "punctuator" && commaOrEnd.value === ",") {
            tokenizer.next();
            continue;
        }
        break;
    }
    return parameters;
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
    tokenizer.next();
    const parameters: string[] = parseFormalParameterList(tokenizer);
    const end = tokenizer.current;
    if (end.type !== "punctuator" || end.value !== ")") {
        throw new UnexpectedTokenError("FormalParameterList", ")", end);
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
        return parseVariableStatement(tokenizer);
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
        return parseEmptyStatement(tokenizer);
    }
    if (begin.type === "keyword" && begin.value === "if") {
        return parseIfStatement(tokenizer);
    }
    if (begin.type === "keyword" && begin.value === "for") {
        return parseForStatement(tokenizer);
    }
    if (begin.type === "keyword" && begin.value === "while") {
        return parseWhileStatement(tokenizer);
    }
    if (begin.type === "keyword" && begin.value === "continue") {
        return parseContinueStatement(tokenizer);
    }
    if (begin.type === "keyword" && begin.value === "break") {
        return parseBreakStatement(tokenizer);
    }
    if (begin.type === "keyword" && begin.value === "return") {
        return parseReturnStatement(tokenizer);
    }
    if (begin.type === "keyword" && begin.value === "with") {
        return parseWithStatement(tokenizer);
    }
    throw new UnexpectedTokenError(
        "Statement",
        "{ or var or this or delete or void or typeof or new or Identifier or Literal or ( or ++ or -- or + or - or ~ or ! or ; or if or for or while or continue or break or return or with",
        begin
    );
}

function parseBlock(tokenizer: Tokenizer): Block {
    const begin = tokenizer.current;
    if (begin.type !== "punctuator" || begin.value !== "{") {
        throw new UnexpectedTokenError("Block", "{", begin);
    }
    tokenizer.next();
    const statementList: Statement[] = [];
    while (true) {
        const end = tokenizer.current;
        if (end.type === "punctuator" && end.value === "}") {
            tokenizer.next();
            break;
        }
        statementList.push(parseStatement(tokenizer));
    }
    return {
        type: "block",
        statementList,
        start: begin.start,
        end: tokenizer.prevPosition,
    };
}

function parseVariableStatement(tokenizer: Tokenizer): VariableStatement {
    const begin = tokenizer.current;
    if (begin.type !== "keyword" || begin.value !== "var") {
        throw new UnexpectedTokenError("VariableStatement", "var", begin);
    }
    tokenizer.next();
    const variableDeclarationList = parseVariableDeclarationList(tokenizer);
    if (!parseSemicolon(tokenizer)) {
        throw new UnexpectedTokenError("VariableStatement", "; or } or LineTerminator", tokenizer.current);
    }
    return {
        type: "variableStatement",
        variableDeclarationList,
        start: begin.start,
        end: tokenizer.prevPosition,
    };
}

function parseVariableDeclarationList(tokenizer: Tokenizer): VariableDeclaration[] {
    const variableDeclarationList: VariableDeclaration[] = [];
    while (true) {
        const identifier = tokenizer.current;
        if (identifier.type !== "identifier") {
            break;
        }
        variableDeclarationList.push(parseVariableDeclaration(tokenizer));
        const comma = tokenizer.current;
        if (comma.type !== "punctuator" || comma.value !== ",") {
            break;
        }
        tokenizer.next();
    }
    if (variableDeclarationList.length === 0) {
        throw new UnexpectedTokenError("VariableDeclarationList", "identifier", tokenizer.current);
    }
    return variableDeclarationList;
}

function parseVariableDeclaration(tokenizer: Tokenizer): VariableDeclaration {
    const identifier = tokenizer.current;
    if (identifier.type !== "identifier") {
        throw new UnexpectedTokenError("VariableDeclaration", "Identifier", identifier);
    }
    const eq = tokenizer.next();
    let initializer: AssignmentExpression | undefined;
    if (eq.type === "punctuator" && eq.value === "=") {
        tokenizer.next();
        initializer = parseAssignmentExpression(tokenizer);
    }
    return {
        type: "variableDeclaration",
        name: identifier.value,
        initializer,
        start: identifier.start,
        end: tokenizer.prevPosition,
    };
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
    const expression = parseExpression(tokenizer);
    if (!parseSemicolon(tokenizer)) {
        throw new UnexpectedTokenError("ExpressionStatement", "; or } or LineTerminator", tokenizer.current);
    }
    return {
        type: "expressionStatement",
        expression,
        start: begin.start,
        end: tokenizer.prevPosition,
    };
}

function parseEmptyStatement(tokenizer: Tokenizer): EmptyStatement {
    const begin = tokenizer.current;
    if (begin.type !== "punctuator" || begin.value !== ";") {
        throw new UnexpectedTokenError("EmptyStatement", ";", begin);
    }
    tokenizer.next();
    return {
        type: "emptyStatement",
        start: begin.start,
        end: tokenizer.prevPosition,
    };
}

function parseIfStatement(tokenizer: Tokenizer): IfStatement {
    const begin = tokenizer.current;
    if (begin.type !== "keyword" || begin.value !== "if") {
        throw new UnexpectedTokenError("IfStatement", "if", begin);
    }
    const ps = tokenizer.next();
    if (ps.type !== "punctuator" || ps.value !== "(") {
        throw new UnexpectedTokenError("IfStatement", "(", begin);
    }
    tokenizer.next();
    const expression = parseExpression(tokenizer);
    const pe = tokenizer.current;
    if (pe.type !== "punctuator" || pe.value !== ")") {
        throw new UnexpectedTokenError("IfStatement", ")", begin);
    }
    tokenizer.next();
    const thenStatement = parseStatement(tokenizer);
    let elseStatement: Statement | undefined;
    const elseToken = tokenizer.current;
    if (elseToken.type === "keyword" && elseToken.value === "else") {
        tokenizer.next();
        elseStatement = parseStatement(tokenizer);
    }
    return {
        type: "ifStatement",
        expression,
        thenStatement,
        elseStatement,
        start: begin.start,
        end: tokenizer.prevPosition,
    };
}

function parseWhileStatement(tokenizer: Tokenizer): WhileStatement {
    const begin = tokenizer.current;
    if (begin.type !== "keyword" || begin.value !== "while") {
        throw new UnexpectedTokenError("WhileStatement", "while", begin);
    }
    const ps = tokenizer.next();
    if (ps.type !== "punctuator" || ps.value !== "(") {
        throw new UnexpectedTokenError("WhileStatement", "(", ps);
    }
    tokenizer.next();
    const expression = parseExpression(tokenizer);
    const pe = tokenizer.current;
    if (pe.type !== "punctuator" || pe.value !== ")") {
        throw new UnexpectedTokenError("WhileStatement", ")", pe);
    }
    tokenizer.next();
    const statement = parseStatement(tokenizer);
    return {
        type: "whileStatement",
        expression,
        statement,
        start: begin.start,
        end: tokenizer.prevPosition,
    };
}

function parseForStatement(tokenizer: Tokenizer): ForStatement | ForInStatement {
    const begin = tokenizer.current;
    if (begin.type !== "keyword" || begin.value !== "for") {
        throw new UnexpectedTokenError("ForStatement", "for", begin);
    }
    const ps = tokenizer.next();
    if (ps.type !== "punctuator" || ps.value !== "(") {
        throw new UnexpectedTokenError("ForStatement", "(", begin);
    }
    const initToken = tokenizer.next();
    let initialization: VariableStatement | Expression | undefined;
    if (initToken.type === "keyword" && initToken.value === "var") {
        tokenizer.next();
        initialization = {
            type: "variableStatement",
            variableDeclarationList: parseVariableDeclarationList(tokenizer),
            start: initToken.start,
            end: tokenizer.prevPosition,
        };
    } else if (initToken.type !== "punctuator" || initToken.value !== ";") {
        initialization = parseExpression(tokenizer);
    }
    const inOrSemicolon = tokenizer.current;
    if (inOrSemicolon.type === "punctuator" && inOrSemicolon.value === ";") {
        tokenizer.next();
        const secondSemicolonOrExpression = tokenizer.current;
        let condition: Expression | undefined;
        if (secondSemicolonOrExpression.type !== "punctuator" || secondSemicolonOrExpression.value !== ";") {
            condition = parseExpression(tokenizer);
        }
        const secondSemicolon = tokenizer.current;
        if (secondSemicolon.type !== "punctuator" || secondSemicolon.value !== ";") {
            throw new UnexpectedTokenError("ForStatement", ";", secondSemicolon);
        }
        const endOrExpression = tokenizer.next();
        let afterthought: Expression | undefined;
        if (endOrExpression.type !== "punctuator" || endOrExpression.value !== ")") {
            afterthought = parseExpression(tokenizer);
        }
        const end = tokenizer.current;
        if (end.type !== "punctuator" || end.value !== ")") {
            throw new UnexpectedTokenError("ForStatement", ")", secondSemicolon);
        }
        tokenizer.next();
        const statement = parseStatement(tokenizer);
        return {
            type: "forStatement",
            initialization,
            condition,
            afterthought,
            statement,
            start: begin.start,
            end: tokenizer.prevPosition,
        };
    } else if (inOrSemicolon.type === "keyword" && inOrSemicolon.value === "in") {
        let forInIntialization: VariableDeclaration | LeftHandSideExpression;
        if (initialization == null) {
            throw new SyntaxError("ForStatement", "VariableDeclaration", tokenizer.current.start);
        } else if (initialization.type === "variableStatement") {
            if (
                initialization.variableDeclarationList[0] == null ||
                initialization.variableDeclarationList.length !== 1
            ) {
                throw new SyntaxError("ForStatement", "VariableDeclaration", tokenizer.current.start);
            }
            forInIntialization = initialization.variableDeclarationList[0];
        } else {
            switch (initialization.type) {
                case "thisExpression":
                case "identifierExpression":
                case "literalExpression":
                case "groupingOperator":
                case "memberOperator":
                case "newOperator":
                case "callOperator":
                    forInIntialization = initialization;
                    break;
                default:
                    throw new SyntaxError("ForStatement", "LeftHandSideExpression", tokenizer.current.start);
            }
        }
        tokenizer.next();
        const expression = parseExpression(tokenizer);
        const pe = tokenizer.current;
        if (pe.type !== "punctuator" || pe.value !== ")") {
            throw new UnexpectedTokenError("ForStatement", ")", begin);
        }
        tokenizer.next();
        const statement = parseStatement(tokenizer);
        return {
            type: "forInStatement",
            initialization: forInIntialization,
            expression,
            statement,
            start: begin.start,
            end: tokenizer.prevPosition,
        };
    } else {
        throw new UnexpectedTokenError("ForStatement", "in or ;", begin);
    }
}

function parseContinueStatement(tokenizer: Tokenizer): ContinueStatement {
    const token = tokenizer.current;
    if (token.type !== "keyword" || token.value !== "continue") {
        throw new UnexpectedTokenError("ContinueStatement", "continue", token);
    }
    tokenizer.next();
    if (!parseSemicolon(tokenizer)) {
        throw new UnexpectedTokenError("ContinueStatement", "; or } or LineTerminator", token);
    }
    return {
        type: "continueStatement",
        start: token.start,
        end: tokenizer.prevPosition,
    };
}

function parseBreakStatement(tokenizer: Tokenizer): BreakStatement {
    const token = tokenizer.current;
    if (token.type !== "keyword" || token.value !== "break") {
        throw new UnexpectedTokenError("BreakStatement", "break", token);
    }
    tokenizer.next();
    if (!parseSemicolon(tokenizer)) {
        throw new UnexpectedTokenError("BreakStatement", "; or } or LineTerminator", token);
    }
    return {
        type: "breakStatement",
        start: token.start,
        end: tokenizer.prevPosition,
    };
}

function parseReturnStatement(tokenizer: Tokenizer): ReturnStatement {
    const token = tokenizer.current;
    if (token.type !== "keyword" || token.value !== "return") {
        throw new UnexpectedTokenError("ReturnStatement", "return", token);
    }
    tokenizer.next();
    if (!parseSemicolon(tokenizer)) {
        const expression = parseExpression(tokenizer);
        if (!parseSemicolon(tokenizer)) {
            throw new UnexpectedTokenError("ReturnStatement", "; or } or LineTerminator", token);
        }
        return {
            type: "returnStatement",
            expression,
            start: token.start,
            end: tokenizer.prevPosition,
        };
    }
    return {
        type: "returnStatement",
        expression: undefined,
        start: token.start,
        end: tokenizer.prevPosition,
    };
}

function parseWithStatement(tokenizer: Tokenizer): WithStatement {
    const begin = tokenizer.current;
    if (begin.type !== "keyword" || begin.value !== "with") {
        throw new UnexpectedTokenError("WithStatement", "with", begin);
    }
    const ps = tokenizer.next();
    if (ps.type !== "punctuator" || ps.value !== "(") {
        throw new UnexpectedTokenError("WithStatement", "(", ps);
    }
    tokenizer.next();
    const expression = parseExpression(tokenizer);
    const pe = tokenizer.current;
    if (pe.type !== "punctuator" || pe.value !== ")") {
        throw new UnexpectedTokenError("WithStatement", ")", pe);
    }
    tokenizer.next();
    const statement = parseStatement(tokenizer);
    return {
        type: "withStatement",
        expression,
        statement,
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
                    type: "unaryMinusOperator",
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
                case "|=":
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

type Value = null | undefined | number | string | boolean | InterpreterObject;
type PrimitiveValue = undefined | null | boolean | number | string;

type Completion = NormalCompletion | ReturnCompletion | AbruptCompletion;

type NormalCompletion =
    | {
          type: "normalCompletion";
          hasValue: false;
      }
    | {
          type: "normalCompletion";
          hasValue: true;
          value: Value;
      };

type ReturnCompletion = {
    type: "returnCompletion";
    hasValue: true;
    value: Value;
};

type AbruptCompletion =
    | {
          type: "abruptCompletion";
          cause: "break";
          hasValue: false;
      }
    | {
          type: "abruptCompletion";
          cause: "break";
          hasValue: true;
          value: Value;
      }
    | {
          type: "abruptCompletion";
          cause: "continue";
          hasValue: false;
      }
    | {
          type: "abruptCompletion";
          cause: "continue";
          hasValue: true;
          value: Value;
      };

type Reference = {
    baseObject: InterpreterObject | null;
    name: string;
    activation: boolean;
};
type RefOrValue = null | undefined | number | boolean | string | Value | Reference;

function isReference(ref: RefOrValue): ref is Reference {
    if (ref == null || typeof ref !== "object") {
        return false;
    }
    if ("name" in ref) {
        return true;
    }
    return false;
}

type Property = {
    readOnly: boolean;
    dontEnum: boolean;
    dontDelete: boolean;
    internal: boolean;
    value: Value;
};

type DefaultValueHint = "string" | "number" | "default";

type NativeFunction = (ctx: Context, self: InterpreterObject | null, args: Value[]) => Generator<unknown, Value>;
type InterpreterObject = {
    internalProperties: {
        prototype: InterpreterObject | null;
        class: string;
        value: Value;
        get?: (ctx: Context, self: InterpreterObject, propertyName: string) => Generator<unknown, Value>;
        put?: (
            ctx: Context,
            self: InterpreterObject,
            propertyName: string,
            value: Value
        ) => Generator<unknown, unknown>;
        canPut?: (ctx: Context, self: InterpreterObject, propertyName: string) => boolean;
        hasProperty?: (ctx: Context, self: InterpreterObject, propertyName: string) => boolean;
        delete?: (ctx: Context, self: InterpreterObject, propertyName: string) => Value;
        defaultValue?: (ctx: Context, self: InterpreterObject, hint: DefaultValueHint) => Generator<unknown, Value>;
        construct?: (ctx: Context, args: Value[]) => Generator<unknown, Value>;
        call?: NativeFunction;
    };
    properties: Map<string, Property>;
};

function canPutProperty(ctx: Context, self: InterpreterObject, propertyName: string): boolean {
    if (self.internalProperties.canPut) {
        return self.internalProperties.canPut(ctx, self, propertyName);
    }
    const prop = self.properties.get(propertyName);
    if (prop != null) {
        return !prop.readOnly;
    }
    const prototype = self.internalProperties.prototype;
    if (prototype == null) {
        return true;
    }
    // FIXME host object
    return canPutProperty(ctx, prototype, propertyName);
}

function defaultPutProperty(ctx: Context, self: InterpreterObject, propertyName: string, value: Value) {
    if (!canPutProperty(ctx, self, propertyName)) {
        return;
    }
    const prop = self.properties.get(propertyName);
    if (prop != null) {
        prop.value = value;
    } else {
        self.properties.set(propertyName, {
            readOnly: false,
            dontEnum: false,
            dontDelete: false,
            internal: false,
            value,
        });
    }
}
function* putProperty(
    ctx: Context,
    self: InterpreterObject,
    propertyName: string,
    value: Value
): Generator<unknown, unknown> {
    if (self.internalProperties.put) {
        return yield* self.internalProperties.put(ctx, self, propertyName, value);
    }
    defaultPutProperty(ctx, self, propertyName, value);
    return;
}

function isPrimitive(value: Value): value is PrimitiveValue {
    return (
        value === undefined ||
        value === null ||
        typeof value === "boolean" ||
        typeof value === "number" ||
        typeof value === "string"
    );
}

function getType(value: Value): "undefined" | "null" | "boolean" | "number" | "string" | "object" | "function" {
    if (value === null) {
        return "null";
    }
    if (isObject(value)) {
        if (value.internalProperties.call != null) {
            return "function";
        }
        return "object";
    }
    const type = typeof value;
    switch (type) {
        case "string":
        case "number":
        case "boolean":
        case "undefined":
            return type;
        default:
            throw new Error("unreachable");
    }
}

function isObject(value: Value): value is InterpreterObject {
    return !isPrimitive(value);
}

type Scope = {
    parent: Scope | undefined;
    activation: boolean;
    object: InterpreterObject;
};

type Context = {
    scope: Scope;
    this: InterpreterObject | null;
    realm: Realm;
};

type Intrinsics = {
    eval: InterpreterObject;
    parseInt: InterpreterObject;
    parseFloat: InterpreterObject;
    escape: InterpreterObject;
    unescape: InterpreterObject;
    isNaN: InterpreterObject;
    isFinite: InterpreterObject;
    Object: InterpreterObject;
    ObjectPrototype: InterpreterObject;
    Function: InterpreterObject;
    FunctionPrototype: InterpreterObject;
    Array: InterpreterObject;
    ArrayPrototype: InterpreterObject;
    String: InterpreterObject;
    StringPrototype: InterpreterObject;
    Boolean: InterpreterObject;
    BooleanPrototype: InterpreterObject;
    Number: InterpreterObject;
    NumberPrototype: InterpreterObject;
    Math: InterpreterObject;
    Date: InterpreterObject;
    DatePrototype: InterpreterObject;
};

type Realm = {
    intrinsics: Intrinsics;
    globalObject: InterpreterObject;
    globalScope: Scope;
};

function newObject(prototype: InterpreterObject): InterpreterObject {
    return {
        internalProperties: {
            prototype,
            class: "Object",
            value: undefined,
        },
        properties: new Map(),
    };
}

function toObject(intrinsics: Intrinsics, value: Value) {
    switch (typeof value) {
        case "string":
            return newStringObject(intrinsics.StringPrototype, value);
        case "number":
            return newNumberObject(intrinsics.NumberPrototype, value);
        case "boolean":
            return newBooleanObject(intrinsics.BooleanPrototype, value);
        case "undefined":
            throw new TypeError();
        case "object":
            if (value === null) {
                throw new TypeError();
            }
            return value;
        case "bigint":
        case "symbol":
        case "function":
        default:
            throw new TypeError();
    }
}

function newStringObject(prototype: InterpreterObject, value: string): InterpreterObject {
    return {
        internalProperties: {
            prototype: prototype,
            class: "String",
            value,
        },
        properties: new Map([
            [
                "length",
                {
                    readOnly: true,
                    dontEnum: true,
                    dontDelete: true,
                    internal: false,
                    value: value.length,
                },
            ],
        ]),
    };
}

function newNumberObject(prototype: InterpreterObject, value: number): InterpreterObject {
    return {
        internalProperties: {
            prototype,
            class: "Number",
            value,
        },
        properties: new Map([]),
    };
}

function newBooleanObject(prototype: InterpreterObject, value: boolean): InterpreterObject {
    return {
        internalProperties: {
            prototype,
            class: "Boolean",
            value,
        },
        properties: new Map([]),
    };
}

function newNativeFunction(prototype: InterpreterObject, func: NativeFunction, length: number): InterpreterObject {
    return {
        internalProperties: {
            prototype,
            class: "Function",
            value: undefined,
            call: func,
        },
        properties: new Map([
            [
                "length",
                {
                    readOnly: true,
                    dontEnum: true,
                    dontDelete: true,
                    internal: false,
                    value: length,
                },
            ],
        ]),
    };
}

function* getProperty(ctx: Context, value: InterpreterObject, name: string): Generator<unknown, Value> {
    let o: InterpreterObject | null = value;
    while (o != null) {
        if (o.properties.has(name)) {
            if (o.internalProperties.get != null) {
                return yield* o.internalProperties.get(ctx, value, name);
            }
            return o.properties.get(name)?.value;
        }
        o = o.internalProperties.prototype;
    }
    return undefined;
}

function hasProperty(obj: InterpreterObject, name: string): boolean {
    let o: InterpreterObject | null = obj;
    while (o != null) {
        if (o.properties.has(name)) {
            return true;
        }
        o = o.internalProperties.prototype;
    }
    return false;
}

function deleteProperty(obj: InterpreterObject, name: string): boolean {
    const prop = obj.properties.get(name);
    if (prop == null) {
        return true;
    }
    if (prop.dontDelete) {
        return false;
    }
    obj.properties.delete(name);
    return true;
}

function* callObject(
    ctx: Context,
    obj: InterpreterObject,
    self: InterpreterObject,
    args: Value[]
): Generator<unknown, Value> {
    const call = obj.internalProperties.call;
    if (call == null) {
        throw new Error("call");
    }
    return yield* call(ctx, self, args);
}

function* defaultValue(
    ctx: Context,
    value: InterpreterObject,
    hint: DefaultValueHint
): Generator<unknown, PrimitiveValue> {
    // FIXME: Date
    if (hint === "string") {
        const toString = yield* getProperty(ctx, value, "toString");
        if (isObject(toString)) {
            const result = yield* callObject(ctx, toString, value, []);
            if (isPrimitive(result)) {
                return result;
            }
        }
        const valueOf = yield* getProperty(ctx, value, "valueOf");
        if (isObject(valueOf)) {
            const result = yield* callObject(ctx, valueOf, value, []);
            if (isPrimitive(result)) {
                return result;
            }
        }
    } else {
        const valueOf = yield* getProperty(ctx, value, "valueOf");
        if (isObject(valueOf)) {
            const result = yield* callObject(ctx, valueOf, value, []);
            if (isPrimitive(result)) {
                return result;
            }
        }
        const toString = yield* getProperty(ctx, value, "toString");
        if (isObject(toString)) {
            const result = yield* callObject(ctx, toString, value, []);
            if (isPrimitive(result)) {
                return result;
            }
        }
    }
    throw new TypeError("ToPrimitive failed");
}

function* toPrimitive(ctx: Context, value: Value, preferredType: DefaultValueHint): Generator<unknown, PrimitiveValue> {
    if (isPrimitive(value)) {
        return value;
    }
    return yield* defaultValue(ctx, value, preferredType);
}

function* toString(ctx: Context, value: Value): Generator<unknown, string> {
    if (value === undefined) {
        return "undefined";
    }
    if (value === null) {
        return "null";
    }
    if (typeof value === "boolean") {
        return value ? "true" : "false";
    }
    if (typeof value === "number") {
        return String(value); // l
    }
    if (typeof value === "string") {
        return value; // l
    }
    return yield* toString(ctx, yield* toPrimitive(ctx, value, "string"));
}

function* toNumber(ctx: Context, value: Value): Generator<unknown, number> {
    if (value === undefined) {
        return NaN;
    }
    if (value === null) {
        return 0;
    }
    if (typeof value === "boolean") {
        return value ? 1 : 0;
    }
    if (typeof value === "number") {
        return value;
    }
    if (typeof value === "string") {
        return Number(value); // l
    }
    return yield* toNumber(ctx, yield* toPrimitive(ctx, value, "number"));
}

function* toInt32(ctx: Context, value: Value): Generator<unknown, number> {
    return (yield* toNumber(ctx, value)) | 0; // l
}

function toUint32(n: number): number {
    return n >>> 0; // l
}

function toInteger(n: number): number {
    if (isNaN(n)) {
        return 0;
    }
    return Math.trunc(n);
}

function toBoolean(value: Value): boolean {
    if (value === undefined || value === null) {
        return false;
    }
    if (typeof value === "boolean") {
        return value;
    }
    if (typeof value === "number") {
        if (isNaN(value) || value === 0) {
            return false;
        }
        return true;
    }
    if (typeof value === "string") {
        return value !== "";
    }
    return true;
}

function* constructFunction(ctx: Context, args: Value[]): Generator<unknown, InterpreterObject> {
    let body = "";
    let argsStrings = [];
    for (let k = 0; k < args.length; k++) {
        const s = yield* toString(ctx, args[k]);
        if (k < args.length - 1) {
            argsStrings.push(s);
        } else {
            body = s;
        }
    }
    const tokenizer = new Tokenizer(argsStrings.join(","));
    const parameters = parseFormalParameterList(tokenizer);
    if (tokenizer.current.type !== "end") {
        throw new Error("Function: illegal arguments: " + argsStrings.join(","));
    }
    const block = parseStatementList(body);
    return yield* newFunction(ctx, parameters, block);
}

function isArrayIndex(p: string) {
    const u = toUint32(Number(p));
    return String(u) === p && u !== 0xffffffff;
}

function* putArrayProperty(
    ctx: Context,
    self: InterpreterObject,
    propertyName: string,
    value: Value
): Generator<unknown, unknown> {
    if (!canPutProperty(ctx, self, propertyName)) {
        return;
    }
    let desc = self.properties.get(propertyName);
    if (desc == null) {
        desc = {
            readOnly: false,
            dontEnum: false,
            dontDelete: false,
            internal: false,
            value,
        };
        self.properties.set(propertyName, desc);
    } else if (propertyName !== "length") {
        desc.value = value;
    } else {
        const num = yield* toNumber(ctx, value);
        const integer = toInteger(num);
        const uint32 = toUint32(num);
        if (integer !== uint32) {
            throw new RangeError("Invalid array length");
        }
        if (typeof desc.value !== "number") {
            throw new Error();
        }
        for (const key of self.properties.keys()) {
            if (!isArrayIndex(key)) {
                continue;
            }
            if (uint32 <= Number(key) && Number(key) < desc.value) {
                self.properties.delete(key);
            }
        }
        desc.value = uint32;
        return;
    }
    if (!isArrayIndex(propertyName)) {
        return;
    }
    const lengthProp = self.properties.get("length");
    const p = toUint32(Number(propertyName));
    if (typeof lengthProp?.value !== "number") {
        throw new Error();
    }
    if (p < lengthProp.value) {
        return;
    }
    lengthProp.value = p + 1;
}

function* arrayConstructor(ctx: Context, args: Value[]): Generator<unknown, Value> {
    let len;
    let elements: Value[];
    if (args.length === 1 && typeof args[0] === "number") {
        len = toUint32(args[0]);
        if (args[0] !== len) {
            throw new RangeError("Invalid array length");
        }
        elements = [];
    } else {
        len = args.length;
        elements = args;
    }
    const array: InterpreterObject = {
        internalProperties: {
            prototype: ctx.realm.intrinsics.ArrayPrototype,
            class: "Array",
            value: undefined,
            put: putArrayProperty,
        },
        properties: new Map([
            [
                "length",
                {
                    readOnly: false,
                    dontEnum: true,
                    dontDelete: true,
                    internal: false,
                    value: len,
                },
            ],
            ...elements.map((value, i): [string, Property] => [
                String(i),
                {
                    readOnly: false,
                    dontEnum: true,
                    dontDelete: true,
                    internal: false,
                    value,
                },
            ]),
        ]),
    };
    return array;
}

function* arrayPrototypeJoin(ctx: Context, self: InterpreterObject | null, args: Value[]): Generator<unknown, Value> {
    if (self == null) {
        throw new Error("Array.prototype.join: this == null");
    }
    const length = toUint32(yield* toNumber(ctx, yield* getProperty(ctx, self, "length")));
    const separator = args[0] === undefined ? "," : yield* toString(ctx, args[0]);
    if (length === 0) {
        return "";
    }
    let r = "";
    const zero = yield* getProperty(ctx, self, "0");
    if (zero != null) {
        r = yield* toString(ctx, zero);
    }
    for (let k = 1; k !== length; k++) {
        const result = yield* getProperty(ctx, self, String(k));
        r += separator;
        if (result != null) {
            r += yield* toString(ctx, result);
        }
    }
    return r;
}

function* arrayPrototypeToString(
    ctx: Context,
    self: InterpreterObject | null,
    _args: Value[]
): Generator<unknown, Value> {
    return yield* arrayPrototypeJoin(ctx, self, []);
}

function* arrayPrototypeReverse(
    ctx: Context,
    self: InterpreterObject | null,
    _args: Value[]
): Generator<unknown, Value> {
    if (self == null) {
        throw new Error("Array.prototype.reverse: this == null");
    }
    const length = toUint32(yield* toNumber(ctx, yield* getProperty(ctx, self, "length")));
    const mid = length >>> 1;
    for (let k = 0; k !== mid; k++) {
        const result6 = length - k - 1;
        const result7 = String(k);
        const result8 = String(result6);
        const result9 = yield* getProperty(ctx, self, result7);
        const result10 = yield* getProperty(ctx, self, result8);
        const has8 = hasProperty(self, result8);
        const has7 = hasProperty(self, result7);
        if (has8) {
            yield* putProperty(ctx, self, result7, result10);
        } else {
            deleteProperty(self, result7);
        }
        if (has7) {
            yield* putProperty(ctx, self, result8, result9);
        } else {
            deleteProperty(self, result8);
        }
    }
    return self;
}

// naive merge sort implementation
function* mergeSort(
    ctx: Context,
    comparefn: NativeFunction | undefined,
    source: InterpreterObject,
    start: number,
    end: number
): Generator<unknown, void> {
    const length = end - start;
    if (length === 1) {
        return;
    }
    const mid = start + (length >>> 1);
    yield* mergeSort(ctx, comparefn, source, start, mid);
    yield* mergeSort(ctx, comparefn, source, mid, end);
    let i = start,
        j = mid;
    let hasA = hasProperty(source, String(i));
    let hasB = hasProperty(source, String(j));
    let a = yield* getProperty(ctx, source, String(i));
    let b = yield* getProperty(ctx, source, String(j));
    const work: Value[] = [];
    while (i < mid && j < end && hasA && hasB) {
        const compared = yield* arraySortCompare(ctx, a, b, comparefn);
        if (compared <= 0) {
            work.push(a);
            i++;
            if (i < mid) {
                a = yield* getProperty(ctx, source, String(i));
                hasA = hasProperty(source, String(i));
            }
        } else {
            work.push(b);
            j++;
            if (j < end) {
                b = yield* getProperty(ctx, source, String(j));
                hasB = hasProperty(source, String(j));
            }
        }
    }
    while (i < mid && hasA) {
        work.push(a);
        i++;
        if (i < mid) {
            a = yield* getProperty(ctx, source, String(i));
            hasA = hasProperty(source, String(i));
        }
    }
    while (j < end && hasB) {
        work.push(b);
        j++;
        if (j < end) {
            b = yield* getProperty(ctx, source, String(j));
            hasB = hasProperty(source, String(j));
        }
    }
    let k = start;
    for (const v of work) {
        yield* putProperty(ctx, source, String(k), v);
        k++;
    }
    for (; k < end; k++) {
        deleteProperty(source, String(k));
    }
}

function* arraySortCompare(ctx: Context, x: Value, y: Value, comparefn?: NativeFunction): Generator<unknown, number> {
    if (x === undefined && y === undefined) {
        return 0;
    }
    if (x === undefined) {
        return 1;
    }
    if (y === undefined) {
        return -1;
    }
    if (comparefn != null) {
        return yield* toNumber(ctx, yield* comparefn(ctx, null, [x, y])); // FIXME: this
    }
    const result7 = yield* toString(ctx, x);
    const result8 = yield* toString(ctx, y);
    if (result7 < result8) {
        return -1; // l
    }
    if (result7 > result8) {
        return 1; // l
    }
    return 0;
}

function* arrayPrototypeSort(ctx: Context, self: InterpreterObject | null, args: Value[]): Generator<unknown, Value> {
    if (self == null) {
        throw new Error("Array.prototype.sort: this == null");
    }
    const comparefn = args[0];
    const call = isObject(comparefn) ? comparefn.internalProperties.call : undefined;
    if (comparefn !== undefined && call == null) {
        throw new TypeError(`Array.prototype.sort: comparefn is not function: ${yield* toString(ctx, comparefn)}`);
    }
    const length = toUint32(yield* toNumber(ctx, yield* getProperty(ctx, self, "length")));
    yield* mergeSort(ctx, call, self, 0, length);
    return self;
}

function createIntrinsics(): Intrinsics {
    const objectPrototype: InterpreterObject = {
        internalProperties: {
            prototype: null,
            class: "Object",
            value: undefined,
        },
        properties: new Map([]),
    };
    const functionPrototype: InterpreterObject = {
        internalProperties: {
            prototype: objectPrototype,
            class: "Function",
            value: undefined,
            *call(_ctx, _self, _args) {
                return undefined;
            },
        },
        properties: new Map([
            [
                "length",
                {
                    readOnly: true,
                    dontEnum: true,
                    dontDelete: true,
                    internal: false,
                    value: 0,
                },
            ],
        ]),
    };
    const object: InterpreterObject = {
        internalProperties: {
            class: "Function",
            value: undefined,
            prototype: functionPrototype,
            *call(ctx, _self, args) {
                if (args[0] == null) {
                    return newObject(ctx.realm.intrinsics.ObjectPrototype);
                }
                return toObject(ctx.realm.intrinsics, args[0]);
            },
            *construct(ctx, args) {
                const o = args[0];
                if (o == null) {
                    return newObject(ctx.realm.intrinsics.ObjectPrototype);
                }
                if (isObject(o)) {
                    return o;
                }
                if (isPrimitive(o)) {
                    return toObject(ctx.realm.intrinsics, o);
                }
            },
        },
        properties: new Map([
            [
                "length",
                {
                    readOnly: true,
                    dontEnum: true,
                    dontDelete: true,
                    internal: false,
                    value: 1,
                },
            ],
            [
                "prototype",
                {
                    readOnly: true,
                    dontEnum: true,
                    dontDelete: true,
                    internal: false,
                    value: objectPrototype,
                },
            ],
        ]),
    };
    const functionObject: InterpreterObject = {
        internalProperties: {
            prototype: functionPrototype,
            class: "Function",
            value: undefined,
            *call(ctx, _self, args) {
                return yield* constructFunction(ctx, args);
            },
            construct: constructFunction,
        },
        properties: new Map([
            [
                "prototype",
                {
                    readOnly: true,
                    dontEnum: true,
                    dontDelete: true,
                    internal: false,
                    value: functionPrototype,
                },
            ],
            [
                "length",
                {
                    readOnly: true,
                    dontEnum: true,
                    dontDelete: true,
                    internal: false,
                    value: 1,
                },
            ],
        ]),
    };
    functionPrototype.properties.set("constructor", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: functionObject,
    });
    functionPrototype.properties.set("toString", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: newNativeFunction(
            functionPrototype,
            function* functionToString(_ctx, self, _args) {
                // FIXME: improve function detection
                if (self?.internalProperties.call == null) {
                    throw new TypeError("this must be Function");
                }
                // FIXME
                return "Function";
            },
            1
        ),
    });
    objectPrototype.properties.set("constructor", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: object,
    });
    objectPrototype.properties.set("toString", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: newNativeFunction(
            functionPrototype,
            function* objectToString(_ctx, self, _args) {
                // newer ES:
                // undefined => [object Undefined]
                // null => [object Null]
                if (!isObject(self)) {
                    throw new TypeError();
                }
                return `[object ${self.internalProperties.class}]`;
            },
            1
        ),
    });
    const string = newNativeFunction(
        functionPrototype,
        function* string(ctx, _, args) {
            if (args.length === 0) {
                return "";
            }
            return yield* toString(ctx, args[0]);
        },
        1
    );
    string.internalProperties.construct = function* stringConstructor(ctx, args) {
        const value = args.length === 0 ? "" : yield* toString(ctx, args[0]);
        return newStringObject(ctx.realm.intrinsics.StringPrototype, value);
    };
    const stringPrototype = newStringObject(object, "");
    stringPrototype.properties.set("constructor", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: string,
    });
    stringPrototype.properties.set("toString", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: newNativeFunction(
            functionPrototype,
            function* stringToString(_ctx, self, _args) {
                if (!isObject(self) || typeof self.internalProperties.value !== "string") {
                    throw new TypeError("String.prototype.toString: this must be String object");
                }
                return self.internalProperties.value;
            },
            1
        ),
    });
    stringPrototype.properties.set("valueOf", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: newNativeFunction(
            functionPrototype,
            function* stringValueOf(_ctx, self, _args) {
                if (!isObject(self) || typeof self.internalProperties.value !== "string") {
                    throw new TypeError("String.prototype.valueOf: this must be String object");
                }
                return self.internalProperties.value;
            },
            1
        ),
    });
    stringPrototype.properties.set("charAt", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: newNativeFunction(
            functionPrototype,
            function* stringCharAt(ctx, self, args) {
                const str = yield* toString(ctx, self);
                const pos = yield* toNumber(ctx, args[0]);
                return str.charAt(pos); // l
            },
            1
        ),
    });
    stringPrototype.properties.set("charCodeAt", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: newNativeFunction(
            functionPrototype,
            function* stringCharCodeAt(ctx, self, args) {
                const str = yield* toString(ctx, self);
                const pos = yield* toNumber(ctx, args[0]);
                return str.charCodeAt(pos); // l
            },
            1
        ),
    });
    stringPrototype.properties.set("indexOf", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: newNativeFunction(
            functionPrototype,
            function* stringIndexOf(ctx, self, args) {
                const str = yield* toString(ctx, self);
                const searchStr = yield* toString(ctx, args[0]);
                const position = args[1] === undefined ? 0 : toInteger(yield* toNumber(ctx, args[1]));
                return str.indexOf(searchStr, position); // l
            },
            1
        ),
    });
    stringPrototype.properties.set("lastIndexOf", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: newNativeFunction(
            functionPrototype,
            function* stringLastIndexOf(ctx, self, args) {
                const str = yield* toString(ctx, self);
                const searchStr = yield* toString(ctx, args[0]);
                const position = args[1] === undefined ? NaN : toInteger(yield* toNumber(ctx, args[1]));
                return str.lastIndexOf(searchStr, position); // l
            },
            1
        ),
    });
    stringPrototype.properties.set("split", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: newNativeFunction(
            functionPrototype,
            function* stringSplit(ctx, self, args) {
                const str = yield* toString(ctx, self);
                if (args[0] === undefined) {
                    return yield* arrayConstructor(ctx, [str]);
                }
                const separator = yield* toString(ctx, args[0]);
                return yield* arrayConstructor(ctx, str.split(separator));
            },
            1
        ),
    });
    stringPrototype.properties.set("substring", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: newNativeFunction(
            functionPrototype,
            function* stringSubstring(ctx, self, args) {
                const str = yield* toString(ctx, self);
                const start = toInteger(yield* toNumber(ctx, args[0]));
                const end = args[1] == undefined ? undefined : toInteger(yield* toNumber(ctx, args[1]));
                return str.substring(start, end);
            },
            2
        ),
    });
    stringPrototype.properties.set("toLowerCase", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: newNativeFunction(
            functionPrototype,
            function* stringToLowerCase(ctx, self, _args) {
                const str = yield* toString(ctx, self);
                return str.toLowerCase();
            },
            1
        ),
    });
    stringPrototype.properties.set("toUpperCase", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: newNativeFunction(
            functionPrototype,
            function* stringToUpperCase(ctx, self, _args) {
                const str = yield* toString(ctx, self);
                return str.toUpperCase();
            },
            1
        ),
    });
    string.properties.set("prototype", {
        readOnly: true,
        dontEnum: true,
        dontDelete: true,
        internal: false,
        value: stringPrototype,
    });
    const number = newNativeFunction(
        functionPrototype,
        function* Number(ctx, _, args) {
            if (args.length === 0) {
                return 0;
            }
            return yield* toNumber(ctx, args[0]);
        },
        1
    );
    number.internalProperties.construct = function* numberConstructor(ctx, args) {
        const value = args.length === 0 ? 0 : yield* toNumber(ctx, args[0]);
        return newNumberObject(ctx.realm.intrinsics.NumberPrototype, value);
    };
    const numberPrototype = newNumberObject(object, 0);
    numberPrototype.properties.set("constructor", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: number,
    });
    numberPrototype.properties.set("toString", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: newNativeFunction(
            functionPrototype,
            function* numberToString(ctx, self, args) {
                if (!isObject(self) || typeof self.internalProperties.value !== "number") {
                    throw new TypeError("Number.prototype.toString: this must be Number object");
                }
                const value = self.internalProperties.value;
                const radix = args[0] === undefined ? 10 : yield* toNumber(ctx, args[0]);
                // throws RnageError
                return value.toString(radix); // l
            },
            1
        ),
    });
    numberPrototype.properties.set("valueOf", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: newNativeFunction(
            functionPrototype,
            function* numberValueOf(_ctx, self, _args) {
                if (!isObject(self) || typeof self.internalProperties.value !== "number") {
                    throw new TypeError("Number.prototype.valueOf: this must be Number object");
                }
                return self.internalProperties.value;
            },
            1
        ),
    });
    number.properties.set("prototype", {
        readOnly: true,
        dontEnum: true,
        dontDelete: true,
        internal: false,
        value: numberPrototype,
    });
    const boolean = newNativeFunction(
        functionPrototype,
        function* Boolean(ctx, _, args) {
            if (args.length === 0) {
                return false;
            }
            return toBoolean(args[0]);
        },
        1
    );
    boolean.internalProperties.construct = function* booleanConstruct(ctx, args) {
        const value = args.length === 0 ? false : toBoolean(args[0]);
        return newBooleanObject(ctx.realm.intrinsics.BooleanPrototype, value);
    };
    const booleanPrototype = newBooleanObject(object, false);
    booleanPrototype.properties.set("constructor", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: boolean,
    });
    booleanPrototype.properties.set("toString", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: newNativeFunction(
            functionPrototype,
            function* booleanToString(_ctx, self, _args) {
                if (!isObject(self) || typeof self.internalProperties.value !== "boolean") {
                    throw new TypeError("Boolean.prototype.toString: this must be Boolean object");
                }
                const value = self.internalProperties.value;
                return value ? "true" : "false";
            },
            1
        ),
    });
    booleanPrototype.properties.set("valueOf", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: newNativeFunction(
            functionPrototype,
            function* booleanValueOf(_ctx, self, _args) {
                if (!isObject(self) || typeof self.internalProperties.value !== "boolean") {
                    throw new TypeError("Boolean.prototype.valueOf: this must be Boolean object");
                }
                return self.internalProperties.value;
            },
            1
        ),
    });
    boolean.properties.set("prototype", {
        readOnly: true,
        dontEnum: true,
        dontDelete: true,
        internal: false,
        value: booleanPrototype,
    });
    const arrayPrototype: InterpreterObject = {
        internalProperties: {
            prototype: objectPrototype,
            class: "Array",
            value: undefined,
            put: putArrayProperty,
        },
        properties: new Map([
            [
                "length",
                {
                    readOnly: false,
                    dontEnum: true,
                    dontDelete: true,
                    internal: false,
                    value: 0,
                },
            ],
        ]),
    };
    const array = newNativeFunction(
        functionPrototype,
        function* arrayConstruct(ctx, _self, args) {
            return yield* arrayConstructor(ctx, args);
        },
        1
    );
    array.internalProperties.construct = arrayConstructor;
    arrayPrototype.properties.set("constructor", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: array,
    });
    arrayPrototype.properties.set("toString", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: newNativeFunction(functionPrototype, arrayPrototypeToString, 0),
    });
    arrayPrototype.properties.set("join", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: newNativeFunction(functionPrototype, arrayPrototypeJoin, 1),
    });
    arrayPrototype.properties.set("reverse", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: newNativeFunction(functionPrototype, arrayPrototypeReverse, 0),
    });
    arrayPrototype.properties.set("sort", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: newNativeFunction(functionPrototype, arrayPrototypeSort, 1),
    });
    array.properties.set("prototype", {
        readOnly: true,
        dontEnum: true,
        dontDelete: true,
        internal: false,
        value: arrayPrototype,
    });
    const evalFunction = newNativeFunction(
        functionPrototype,
        // If value of the eval property is used in any way other than a direct call (that is, other than by the
        // explicit use of its name as an Identifier which is the MemberExpression in a CallExpression), or if
        // the eval property is assigned to, a runtime error may be generated.
        function* evalFunc(ctx, _self, args) {
            if (typeof args[0] !== "string") {
                return args[0];
            }
            const source = args[0];
            const completion = yield* run(source, ctx);
            if (completion.type === "normalCompletion" && completion.hasValue) {
                return completion.value;
            }
            return undefined;
        },
        1
    );
    return {
        eval: evalFunction,
        Object: object,
        ObjectPrototype: objectPrototype,
        Function: functionObject,
        FunctionPrototype: functionPrototype,
        Array: array,
        ArrayPrototype: arrayPrototype,
        String: string,
        StringPrototype: stringPrototype,
        Boolean: boolean,
        BooleanPrototype: booleanPrototype,
        Number: number,
        NumberPrototype: numberPrototype,
    } as Intrinsics;
}

function createGlobal(intrinsics: Intrinsics): InterpreterObject {
    return {
        internalProperties: {
            prototype: null,
            class: "Global",
            value: undefined,
        },
        properties: new Map([
            [
                "eval",
                {
                    readOnly: false,
                    dontEnum: true,
                    dontDelete: false,
                    internal: false,
                    value: intrinsics.eval,
                },
            ],
            [
                "NaN",
                {
                    readOnly: false,
                    dontEnum: true,
                    dontDelete: false,
                    internal: false,
                    value: NaN,
                },
            ],
            [
                "Infinity",
                {
                    readOnly: false,
                    dontEnum: true,
                    dontDelete: false,
                    internal: false,
                    value: Infinity,
                },
            ],
            [
                "Object",
                {
                    readOnly: false,
                    dontEnum: true,
                    dontDelete: false,
                    internal: false,
                    value: intrinsics.Object,
                },
            ],
            [
                "Function",
                {
                    readOnly: false,
                    dontEnum: true,
                    dontDelete: false,
                    internal: false,
                    value: intrinsics.Function,
                },
            ],
            [
                "Array",
                {
                    readOnly: false,
                    dontEnum: true,
                    dontDelete: false,
                    internal: false,
                    value: intrinsics.Array,
                },
            ],
            [
                "String",
                {
                    readOnly: false,
                    dontEnum: true,
                    dontDelete: false,
                    internal: false,
                    value: intrinsics.String,
                },
            ],
            [
                "Boolean",
                {
                    readOnly: false,
                    dontEnum: true,
                    dontDelete: false,
                    internal: false,
                    value: intrinsics.Boolean,
                },
            ],
            [
                "Number",
                {
                    readOnly: false,
                    dontEnum: true,
                    dontDelete: false,
                    internal: false,
                    value: intrinsics.Number,
                },
            ],
        ]),
    };
}

function* runBlock(ctx: Context, block: Block): Generator<unknown, Completion> {
    let completion1: Completion = {
        type: "normalCompletion",
        hasValue: false,
    };
    for (const statement of block.statementList) {
        if (completion1.type === "abruptCompletion") {
            break;
        }
        const completion3 = yield* runStatement(ctx, statement);
        if (completion3.type === "returnCompletion") {
            return completion3;
        }
        if (completion3.hasValue || !completion1.hasValue) {
            completion1 = completion3;
            continue;
        }
        if (completion3.type === "abruptCompletion" && completion3.cause === "break") {
            completion1 = {
                type: "abruptCompletion",
                cause: "break",
                hasValue: true,
                value: completion1.value,
            };
            continue;
        }
        if (completion3.type === "abruptCompletion" && completion3.cause === "continue") {
            completion1 = {
                type: "abruptCompletion",
                cause: "continue",
                hasValue: true,
                value: completion1.value,
            };
            continue;
        }
        completion1 = {
            type: "normalCompletion",
            hasValue: true,
            value: completion1.value,
        };
    }
    return completion1;
}

function* referenceGetValue(ctx: Context, reference: RefOrValue): Generator<unknown, Value> {
    if (!isReference(reference)) {
        return reference;
    }
    if (reference.baseObject == null) {
        throw new ReferenceError(`${reference.name}`);
    }
    return yield* getProperty(ctx, reference.baseObject, reference.name);
}

function* referencePutValue(ctx: Context, reference: RefOrValue, value: Value): Generator<unknown, unknown> {
    if (!isReference(reference)) {
        throw new ReferenceError("not reference");
    }
    yield* putProperty(ctx, reference.baseObject ?? ctx.realm.globalObject, reference.name, value);
    return;
}

function* runEmptyStatement(_ctx: Context, _statement: EmptyStatement): Generator<unknown, Completion> {
    return {
        type: "normalCompletion",
        hasValue: false,
    };
}

function resolveIdentifier(scope: Scope, name: string): RefOrValue {
    let s: Scope | undefined = scope;
    while (s != null) {
        if (hasProperty(s.object, name)) {
            return {
                baseObject: s.object,
                name,
                activation: s.activation,
            };
        }
        s = s.parent;
    }
    return {
        baseObject: null,
        name,
        activation: false,
    };
}

function* evaluateList(ctx: Context, list: Expression[]): Generator<unknown, Value[]> {
    const result: Value[] = [];
    for (const e of list) {
        result.push(yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, e)));
    }
    return result;
}

function* compareValue(ctx: Context, leftValue: Value, rightValue: Value): Generator<unknown, boolean | undefined> {
    const primitiveLeft = yield* toPrimitive(ctx, leftValue, "number");
    const primitiveRight = yield* toPrimitive(ctx, rightValue, "number");
    if (typeof primitiveLeft !== "string" || typeof primitiveRight !== "string") {
        const numberLeft = yield* toNumber(ctx, primitiveLeft);
        const numberRight = yield* toNumber(ctx, primitiveRight);
        if (isNaN(numberLeft)) {
            return undefined;
        }
        if (isNaN(numberRight)) {
            return undefined;
        }
        if (numberLeft === numberRight) {
            // l
            return false;
        }
        if (numberLeft === Number.POSITIVE_INFINITY) {
            return false;
        }
        if (numberRight === Number.POSITIVE_INFINITY) {
            return true;
        }
        if (numberRight === Number.NEGATIVE_INFINITY) {
            return false;
        }
        if (numberLeft === Number.NEGATIVE_INFINITY) {
            return true;
        }
        return numberLeft < numberRight;
    } else {
        if (primitiveLeft.startsWith(primitiveRight)) {
            return false;
        }
        if (primitiveRight.startsWith(primitiveLeft)) {
            return true;
        }
        for (let k = 0; k < Math.min(primitiveLeft.length, primitiveRight.length); k++) {
            const m = primitiveLeft.charCodeAt(k);
            const n = primitiveRight.charCodeAt(k);
            if (m === n) {
                continue;
            }
            return m < n;
        }
        throw new Error("unreachable");
    }
}

function* equalsValue(ctx: Context, x: Value, y: Value): Generator<unknown, boolean | undefined> {
    if (typeof x === typeof y) {
        if (typeof x === "undefined") {
            return true;
        }
        if (x === null) {
            return true;
        }
        if (typeof x === "number" && typeof y === "number") {
            return x === y; // l
        }
        if (typeof x === "string" && typeof y === "string") {
            return x === y;
        }
        if (typeof x === "boolean" && typeof y === "boolean") {
            return x === y;
        }
        if (isObject(x) && isObject(y) && x === y) {
            return true;
        }
    }
    if (x === null && y === undefined) {
        return true;
    }
    if (x === undefined && y === null) {
        return true;
    }
    if (typeof x === "number" && typeof y === "string") {
        return yield* equalsValue(ctx, x, yield* toNumber(ctx, y));
    }
    if (typeof x === "string" && typeof y === "number") {
        return yield* equalsValue(ctx, yield* toNumber(ctx, x), y);
    }
    if (typeof x === "boolean") {
        return yield* equalsValue(ctx, yield* toNumber(ctx, x), y);
    }
    if (typeof y === "boolean") {
        return yield* equalsValue(ctx, x, yield* toNumber(ctx, y));
    }
    if ((typeof x === "string" || typeof x === "number") && isObject(y)) {
        return yield* equalsValue(ctx, x, yield* toPrimitive(ctx, y, "default"));
    }
    if (isObject(x) && (typeof y === "string" || typeof y === "number")) {
        return yield* equalsValue(ctx, yield* toPrimitive(ctx, x, "default"), y);
    }
    return false;
}

function* multiply(ctx: Context, left: Value, right: Value): Generator<unknown, Value> {
    return (yield* toNumber(ctx, left)) * (yield* toNumber(ctx, right));
}

function* divide(ctx: Context, left: Value, right: Value): Generator<unknown, Value> {
    return (yield* toNumber(ctx, left)) / (yield* toNumber(ctx, right));
}

function* modulo(ctx: Context, left: Value, right: Value): Generator<unknown, Value> {
    return (yield* toNumber(ctx, left)) % (yield* toNumber(ctx, right));
}

function* add(ctx: Context, left: Value, right: Value): Generator<unknown, Value> {
    // FIXME Date
    const primitiveLeft = yield* toPrimitive(ctx, left, "number");
    const primitiveRight = yield* toPrimitive(ctx, right, "number");
    if (typeof primitiveLeft === "string" || typeof primitiveRight === "string") {
        return (yield* toString(ctx, primitiveLeft)) + (yield* toString(ctx, primitiveRight));
    }
    return (yield* toNumber(ctx, primitiveLeft)) + (yield* toNumber(ctx, primitiveRight));
}

function* subtract(ctx: Context, left: Value, right: Value): Generator<unknown, Value> {
    return (yield* toNumber(ctx, left)) - (yield* toNumber(ctx, right));
}

function* leftShift(ctx: Context, left: Value, right: Value): Generator<unknown, Value> {
    return (yield* toNumber(ctx, left)) << (yield* toNumber(ctx, right));
}

function* signedRightShift(ctx: Context, left: Value, right: Value): Generator<unknown, Value> {
    return (yield* toNumber(ctx, left)) >> (yield* toNumber(ctx, right));
}

function* unsignedRightShift(ctx: Context, left: Value, right: Value): Generator<unknown, Value> {
    return (yield* toNumber(ctx, left)) >>> (yield* toNumber(ctx, right));
}

function* bitwiseAnd(ctx: Context, left: Value, right: Value): Generator<unknown, Value> {
    return (yield* toNumber(ctx, left)) & (yield* toNumber(ctx, right));
}

function* bitwiseOr(ctx: Context, left: Value, right: Value): Generator<unknown, Value> {
    return (yield* toNumber(ctx, left)) | (yield* toNumber(ctx, right));
}

function* bitwiseXor(ctx: Context, left: Value, right: Value): Generator<unknown, Value> {
    return (yield* toNumber(ctx, left)) ^ (yield* toNumber(ctx, right));
}

function* evaluateExpression(ctx: Context, expression: Expression): Generator<unknown, RefOrValue> {
    switch (expression.type) {
        case "literalExpression":
            return expression.value;
        case "thisExpression":
            return ctx.this;
        case "identifierExpression":
            return resolveIdentifier(ctx.scope, expression.name);
        case "groupingOperator":
            return yield* evaluateExpression(ctx, expression.expression);
        case "memberOperator": {
            const left = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.left));
            if ("name" in expression) {
                return {
                    baseObject: toObject(ctx.realm.intrinsics, left),
                    name: expression.name,
                    activation: false,
                };
            } else {
                const right = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.right));
                return {
                    baseObject: toObject(ctx.realm.intrinsics, left),
                    name: yield* toString(ctx, right),
                    activation: false,
                };
            }
        }
        case "newOperator": {
            const value = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.expression));
            let args: Value[] = [];
            if (expression.argumentList != null) {
                args = yield* evaluateList(ctx, expression.argumentList);
            }
            if (!isObject(value)) {
                throw new TypeError("!isObject");
            }
            const construct = value.internalProperties.construct;
            if (!construct) {
                throw new TypeError("not constructable");
            }
            const obj = yield* construct(ctx, args);
            if (!isObject(obj)) {
                throw new TypeError("[[Construct]] result is not a object");
            }
            return obj;
        }
        case "callOperator": {
            const valueRef = yield* evaluateExpression(ctx, expression.expression);
            const args = yield* evaluateList(ctx, expression.argumentList);
            const value = yield* referenceGetValue(ctx, valueRef);
            if (!isObject(value)) {
                throw new TypeError("not Object");
            }
            const call = value.internalProperties.call;
            if (call == null) {
                throw new TypeError("not callable");
            }
            let self = isReference(valueRef) && !valueRef.activation ? valueRef.baseObject : null;
            return yield* call(ctx, self, args);
        }
        case "postfixIncrementOperator": {
            const ref = yield* evaluateExpression(ctx, expression.expression);
            const value = yield* referenceGetValue(ctx, ref);
            const number = yield* toNumber(ctx, value);
            const computed = number + 1;
            yield* referencePutValue(ctx, ref, computed);
            return number;
        }
        case "postfixDecrementOperator": {
            const ref = yield* evaluateExpression(ctx, expression.expression);
            const value = yield* referenceGetValue(ctx, ref);
            const number = yield* toNumber(ctx, value);
            const computed = number - 1;
            yield* referencePutValue(ctx, ref, computed);
            return number;
        }
        case "deleteOperator": {
            const ref = yield* evaluateExpression(ctx, expression.expression);
            if (!isReference(ref)) {
                // newer ES returns true
                throw new ReferenceError("value can not be deleted");
            }
            if (!isObject(ref.baseObject)) {
                return true;
            }
            return deleteProperty(ref.baseObject, ref.name);
            // If Result(2) does not implement the internal [[Delete]] method, go to step 8 <= ?
        }
        case "voidOperator": {
            yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.expression));
            return undefined;
        }
        case "typeofOperator": {
            const ref = yield* evaluateExpression(ctx, expression.expression);
            if (isReference(ref)) {
                if (ref.baseObject == null) {
                    return "undefined";
                }
            }
            const value = yield* referenceGetValue(ctx, ref);
            const t = getType(value);
            if (t === "null") {
                return "object";
            }
            return t;
        }
        case "prefixIncrementOperator": {
            const ref = yield* evaluateExpression(ctx, expression.expression);
            const value = yield* referenceGetValue(ctx, ref);
            const number = yield* toNumber(ctx, value);
            const computed = number + 1;
            yield* referencePutValue(ctx, ref, computed);
            return computed;
        }
        case "prefixDecrementOperator": {
            const ref = yield* evaluateExpression(ctx, expression.expression);
            const value = yield* referenceGetValue(ctx, ref);
            const number = yield* toNumber(ctx, value);
            const computed = number - 1;
            yield* referencePutValue(ctx, ref, computed);
            return computed;
        }
        case "unaryPlusOperator": {
            const value = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.expression));
            const number = yield* toNumber(ctx, value);
            if (isNaN(number)) {
                return NaN;
            }
            return number;
        }
        case "unaryMinusOperator": {
            const value = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.expression));
            const number = yield* toNumber(ctx, value);
            if (isNaN(number)) {
                return NaN;
            }
            return -number;
        }
        case "bitwiseNotOperator": {
            const value = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.expression));
            const number = yield* toInt32(ctx, value);
            return ~number; // l
        }
        case "logicalNotOperator":
            return !toBoolean(yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.expression)));
        case "multiplyOperator": {
            const left = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.left));
            const right = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.right));
            return yield* multiply(ctx, left, right);
        }
        case "divideOperator": {
            const left = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.left));
            const right = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.right));
            return yield* divide(ctx, left, right);
        }
        case "moduloOperator": {
            const left = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.left));
            const right = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.right));
            return yield* modulo(ctx, left, right);
        }
        case "addOperator": {
            const left = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.left));
            const right = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.right));
            return yield* add(ctx, left, right);
        }
        case "subtractOperator": {
            const left = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.left));
            const right = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.right));
            return yield* subtract(ctx, left, right);
        }
        case "leftShiftOperator": {
            const left = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.left));
            const right = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.right));
            return yield* leftShift(ctx, left, right);
        }
        case "signedRightShiftOperator": {
            const left = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.left));
            const right = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.right));
            return yield* signedRightShift(ctx, left, right);
        }
        case "unsignedRightShiftOperator": {
            const left = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.left));
            const right = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.right));
            return yield* unsignedRightShift(ctx, left, right);
        }
        case "lessThanOperator": {
            const leftValue = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.left));
            const rightValue = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.right));
            const result = yield* compareValue(ctx, leftValue, rightValue);
            if (result === undefined) {
                return false;
            } else {
                return result;
            }
        }
        case "greaterThanOperator": {
            const leftValue = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.left));
            const rightValue = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.right));
            const result = yield* compareValue(ctx, rightValue, leftValue);
            if (result === undefined) {
                return false;
            } else {
                return result;
            }
        }
        case "lessThanOrEqualOperator": {
            const leftValue = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.left));
            const rightValue = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.right));
            const result = yield* compareValue(ctx, rightValue, leftValue);
            if (result === undefined || result) {
                return false;
            } else {
                return true;
            }
        }
        case "greaterThanOrEqualOperator": {
            const leftValue = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.left));
            const rightValue = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.right));
            const result = yield* compareValue(ctx, leftValue, rightValue);
            if (result === undefined || result) {
                return false;
            } else {
                return true;
            }
        }
        case "equalsOperator": {
            const leftValue = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.left));
            const rightValue = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.right));
            return yield* equalsValue(ctx, rightValue, leftValue);
        }
        case "doesNotEqualsOperator": {
            const leftValue = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.left));
            const rightValue = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.right));
            return !(yield* equalsValue(ctx, rightValue, leftValue));
        }
        case "bitwiseAndOperator": {
            const leftValue = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.left));
            const rightValue = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.right));
            return yield* bitwiseAnd(ctx, rightValue, leftValue);
        }
        case "bitwiseXorOperator": {
            const leftValue = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.left));
            const rightValue = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.right));
            return yield* bitwiseXor(ctx, rightValue, leftValue);
        }
        case "bitwiseOrOperator": {
            const leftValue = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.left));
            const rightValue = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.right));
            return yield* bitwiseOr(ctx, rightValue, leftValue);
        }
        case "logicalAndOperator": {
            const leftValue = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.left));
            if (!toBoolean(leftValue)) {
                return leftValue;
            }
            return yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.right));
        }
        case "logicalOrOperator": {
            const leftValue = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.left));
            if (toBoolean(leftValue)) {
                return leftValue;
            }
            return yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.right));
        }
        case "conditionalOperator": {
            const condition = yield* referenceGetValue(
                ctx,
                yield* evaluateExpression(ctx, expression.conditionExpression)
            );
            if (toBoolean(condition)) {
                return yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.thenExpression));
            } else {
                return yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.elseExpression));
            }
        }
        case "assignmentOperator": {
            switch (expression.operator) {
                case "=": {
                    const leftRef = yield* evaluateExpression(ctx, expression.left);
                    const rightRef = yield* evaluateExpression(ctx, expression.right);
                    const right = yield* referenceGetValue(ctx, rightRef);
                    yield* referencePutValue(ctx, leftRef, right);
                    return right;
                }
                default:
                    break;
            }
            const leftRef = yield* evaluateExpression(ctx, expression.left);
            const left = yield* referenceGetValue(ctx, leftRef);
            const rightRef = yield* evaluateExpression(ctx, expression.right);
            const right = yield* referenceGetValue(ctx, rightRef);
            switch (expression.operator) {
                case "*=": {
                    const result = yield* multiply(ctx, left, right);
                    yield* referencePutValue(ctx, leftRef, result);
                    return result;
                }
                case "/=": {
                    const result = yield* divide(ctx, left, right);
                    yield* referencePutValue(ctx, leftRef, result);
                    return result;
                }
                case "%=": {
                    const result = yield* modulo(ctx, left, right);
                    yield* referencePutValue(ctx, leftRef, result);
                    return result;
                }
                case "+=": {
                    const result = yield* add(ctx, left, right);
                    yield* referencePutValue(ctx, leftRef, result);
                    return result;
                }
                case "-=": {
                    const result = yield* subtract(ctx, left, right);
                    yield* referencePutValue(ctx, leftRef, result);
                    return result;
                }
                case "<<=": {
                    const result = yield* leftShift(ctx, left, right);
                    yield* referencePutValue(ctx, leftRef, result);
                    return result;
                }
                case ">>=": {
                    const result = yield* signedRightShift(ctx, left, right);
                    yield* referencePutValue(ctx, leftRef, result);
                    return result;
                }
                case ">>>=": {
                    const result = yield* unsignedRightShift(ctx, left, right);
                    yield* referencePutValue(ctx, leftRef, result);
                    return result;
                }
                case "&=": {
                    const result = yield* bitwiseAnd(ctx, left, right);
                    yield* referencePutValue(ctx, leftRef, result);
                    return result;
                }
                case "^=": {
                    const result = yield* bitwiseXor(ctx, left, right);
                    yield* referencePutValue(ctx, leftRef, result);
                    return result;
                }
                case "|=": {
                    const result = yield* bitwiseOr(ctx, left, right);
                    yield* referencePutValue(ctx, leftRef, result);
                    return result;
                }
                default:
                    const _: never = expression.operator;
                    throw new Error("unreachable");
            }
        }
        case "commaOperator": {
            yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.left));
            return yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, expression.right));
        }
    }
    throw new Error();
}

function* runVariableDeclaration(ctx: Context, declaration: VariableDeclaration): Generator<unknown, void> {
    if (declaration.initializer == null) {
        return;
    }
    const left = resolveIdentifier(ctx.scope, declaration.name);
    const rightRef = yield* evaluateExpression(ctx, declaration.initializer);
    const right = yield* referenceGetValue(ctx, rightRef);
    yield* referencePutValue(ctx, left, right);
}

function* runVariableStatement(ctx: Context, statement: VariableStatement): Generator<unknown, Completion> {
    for (const decl of statement.variableDeclarationList) {
        yield* runVariableDeclaration(ctx, decl);
    }
    return {
        type: "normalCompletion",
        hasValue: false,
    };
}

function* runExpressionStatement(ctx: Context, statement: ExpressionStatement): Generator<unknown, Completion> {
    const value = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, statement.expression));
    return {
        type: "normalCompletion",
        hasValue: true,
        value,
    };
}

function* runIfStatement(ctx: Context, statement: IfStatement): Generator<unknown, Completion> {
    const ref = yield* evaluateExpression(ctx, statement.expression);
    const value = yield* referenceGetValue(ctx, ref);
    if (toBoolean(value)) {
        return yield* runStatement(ctx, statement.thenStatement);
    } else if (statement.elseStatement != null) {
        return yield* runStatement(ctx, statement.elseStatement);
    }
    return {
        type: "normalCompletion",
        hasValue: false,
    };
}

function* runWhileStatement(ctx: Context, statement: WhileStatement): Generator<unknown, Completion> {
    let completion: Completion = {
        type: "normalCompletion",
        hasValue: false,
    };
    while (true) {
        const ref = yield* evaluateExpression(ctx, statement.expression);
        const value = yield* referenceGetValue(ctx, ref);
        if (!toBoolean(value)) {
            break;
        }
        const result = yield* runStatement(ctx, statement.statement);
        if (result.hasValue) {
            completion = {
                type: "normalCompletion",
                hasValue: true,
                value: result.value,
            };
        }
        if (result.type === "abruptCompletion" && result.cause === "break") {
            break;
        }
        if (result.type === "abruptCompletion" && result.cause === "continue") {
            continue;
        }
        if (result.type === "returnCompletion") {
            return result;
        }
    }
    return completion;
}

function* runForStatement(ctx: Context, statement: ForStatement): Generator<unknown, Completion> {
    let completion: Completion = {
        type: "normalCompletion",
        hasValue: false,
    };
    if (statement.initialization != null) {
        if (statement.initialization.type !== "variableStatement") {
            yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, statement.initialization));
        } else {
            yield* runVariableStatement(ctx, statement.initialization);
        }
    }
    while (true) {
        if (statement.condition != null) {
            if (!toBoolean(yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, statement.condition)))) {
                break;
            }
        }
        const result = yield* runStatement(ctx, statement.statement);
        if (result.hasValue) {
            completion = {
                type: "normalCompletion",
                hasValue: true,
                value: result.value,
            };
        }
        if (result.type === "abruptCompletion" && result.cause === "break") {
            break;
        }
        if (result.type === "returnCompletion") {
            return result;
        }
        if (statement.afterthought != null) {
            yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, statement.afterthought));
        }
    }
    return completion;
}

function* runForInStatement(ctx: Context, statement: ForInStatement): Generator<unknown, Completion> {
    if (statement.initialization.type === "variableDeclaration" && statement.initialization.initializer != null) {
        yield* runVariableDeclaration(ctx, statement.initialization);
    }
    let obj = toObject(
        ctx.realm.intrinsics,
        yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, statement.expression))
    );
    let completion: Completion = {
        type: "normalCompletion",
        hasValue: false,
    };
    const iterated = new Set<string>();
    while (true) {
        for (const [name, prop] of obj.properties) {
            if (prop.dontEnum) {
                continue;
            }
            if (iterated.has(name)) {
                continue;
            }
            iterated.add(name);
            if (statement.initialization.type === "variableDeclaration") {
                const ref = resolveIdentifier(ctx.scope, statement.initialization.name);
                yield* referencePutValue(ctx, ref, name);
            } else {
                const ref = yield* evaluateExpression(ctx, statement.initialization);
                yield* referencePutValue(ctx, ref, name);
            }
            const result = yield* runStatement(ctx, statement.statement);
            if (result.hasValue) {
                completion = {
                    type: "normalCompletion",
                    hasValue: true,
                    value: result.value,
                };
            }
            if (result.type === "abruptCompletion" && result.cause === "break") {
                break;
            }
            if (result.type === "returnCompletion") {
                return result;
            }
        }
        if (obj.internalProperties.prototype == null) {
            break;
        }
        obj = obj.internalProperties.prototype;
    }
    return completion;
}

function* runReturnStatement(ctx: Context, statement: ReturnStatement): Generator<unknown, Completion> {
    if (statement.expression == null) {
        return {
            type: "returnCompletion",
            hasValue: true,
            value: undefined,
        };
    }
    const value = yield* referenceGetValue(ctx, yield* evaluateExpression(ctx, statement.expression));
    return {
        type: "returnCompletion",
        hasValue: true,
        value,
    };
}

function* runWithStatement(ctx: Context, statement: WithStatement): Generator<unknown, Completion> {
    const ref = yield* evaluateExpression(ctx, statement.expression);
    const value = yield* referenceGetValue(ctx, ref);
    const object = toObject(ctx.realm.intrinsics, value);
    const withScope: Scope = {
        parent: ctx.scope,
        activation: false,
        object,
    };
    const withContext: Context = {
        scope: withScope,
        this: ctx.this,
        realm: ctx.realm,
    };
    return yield* runStatement(withContext, statement.statement);
}

function* runStatement(ctx: Context, statement: Statement): Generator<unknown, Completion> {
    switch (statement.type) {
        case "block":
            return yield* runBlock(ctx, statement);
        case "variableStatement":
            return yield* runVariableStatement(ctx, statement);
        case "emptyStatement":
            return yield* runEmptyStatement(ctx, statement);
        case "expressionStatement":
            return yield* runExpressionStatement(ctx, statement);
        case "ifStatement":
            return yield* runIfStatement(ctx, statement);
        case "whileStatement":
            return yield* runWhileStatement(ctx, statement);
        case "forStatement":
            return yield* runForStatement(ctx, statement);
        case "forInStatement":
            return yield* runForInStatement(ctx, statement);
        case "continueStatement":
            return {
                type: "abruptCompletion",
                cause: "continue",
                hasValue: false,
            };
        case "breakStatement":
            return {
                type: "abruptCompletion",
                cause: "break",
                hasValue: false,
            };
        case "returnStatement":
            return yield* runReturnStatement(ctx, statement);
        case "withStatement":
            return yield* runWithStatement(ctx, statement);
        default:
            const _: never = statement;
            throw new Error("unreachable " + statement);
    }
}

function defineVariable(ctx: Context, list: VariableDeclaration[]) {
    for (const decl of list) {
        if (!ctx.scope.object.properties.has(decl.name)) {
            ctx.scope.object.properties.set(decl.name, {
                readOnly: false,
                dontEnum: false,
                dontDelete: true,
                internal: false,
                value: undefined,
            });
        }
    }
}

function* newFunction(ctx: Context, parameters: string[], block: Block): Generator<unknown, InterpreterObject> {
    function* call(ctx: Context, self: InterpreterObject | null, args: Value[]): Generator<unknown, Value> {
        const scope = {
            parent: ctx.realm.globalScope,
            activation: true,
            object: newObject(ctx.realm.intrinsics.ObjectPrototype),
        };
        const context: Context = {
            scope,
            // The caller provides the this value. If the this value provided by the caller is not an object (including the case where it is null), then the this value is the global object.
            this: isObject(self) ? self : ctx.realm.globalObject,
            realm: ctx.realm,
        };
        const argumentsObject = newObject(ctx.realm.intrinsics.ObjectPrototype);
        argumentsObject.properties.set("callee", {
            readOnly: false,
            dontEnum: true,
            dontDelete: false,
            internal: false,
            value: func,
        });
        argumentsObject.properties.set("length", {
            readOnly: false,
            dontEnum: true,
            dontDelete: false,
            internal: false,
            value: args.length,
        });
        const iargToName = new Map<string, string>();
        const nameToIArg = new Map<string, string>();
        argumentsObject.internalProperties.get = function* (ctx, self, propertyName) {
            const name = iargToName.get(propertyName);
            if (name != null) {
                return scope.object.properties.get(name)?.value;
            }
            const prop = self.properties.get(propertyName);
            return prop?.value;
        };
        argumentsObject.internalProperties.put = function* (ctx, self, propertyName, value) {
            const name = iargToName.get(propertyName);
            if (name != null) {
                const prop = scope.object.properties.get(name);
                if (prop != null) prop.value = value;
                return;
            }
            defaultPutProperty(ctx, self, propertyName, value);
        };
        scope.object.properties.set("arguments", {
            readOnly: false,
            dontEnum: false,
            dontDelete: true,
            internal: false,
            value: argumentsObject,
        });
        for (let i = 0; i < parameters.length; i++) {
            scope.object.properties.set(parameters[i]!, {
                readOnly: false,
                dontEnum: false,
                dontDelete: true,
                internal: false,
                value: args[i],
            });
            nameToIArg.set(parameters[i]!, String(i));
        }
        // CreateMappedArgumentsObject
        for (let i = 0; i < args.length; i++) {
            argumentsObject.properties.set(String(i), {
                // newer ES: [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true
                // ES2: { DontEnum }
                readOnly: false,
                dontEnum: true,
                dontDelete: false,
                internal: false,
                value: args[i],
            });
        }
        for (const [name, iarg] of nameToIArg) {
            iargToName.set(iarg, name);
        }
        visitStatement(block, (statement) => {
            if (statement.type === "variableStatement") {
                defineVariable(context, statement.variableDeclarationList);
            }
            if (statement.type === "forInStatement") {
                if (statement.initialization?.type === "variableDeclaration") {
                    defineVariable(context, [statement.initialization]);
                }
            }
        });
        const completion = yield* runBlock(context, block);
        if (completion.type === "returnCompletion") {
            return completion.value;
        }
        return undefined;
    }
    const func = newNativeFunction(ctx.realm.intrinsics.FunctionPrototype, call, parameters.length);
    const prototype = newObject(ctx.realm.intrinsics.ObjectPrototype);
    func.properties.set("prototype", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: prototype,
    });
    prototype.properties.set("constructor", {
        readOnly: false,
        dontEnum: true,
        dontDelete: false,
        internal: false,
        value: func,
    });
    func.internalProperties.construct = function* construct(ctx, args) {
        const prototype = yield* getProperty(ctx, func, "prototype");
        if (!isObject(prototype)) {
            throw new Error("FIXME");
        }
        const self = newObject(prototype);
        yield* call(ctx, self, args);
        return self;
    };
    return func;
}

function findActivationObjectScope(scope: Scope): Scope | undefined {
    let s: Scope | undefined = scope;
    while (s != null) {
        if (s.activation) {
            return s;
        }
        s = s.parent;
    }
    return undefined;
}

function* defineFunction(ctx: Context, decl: FunctionDeclaration): Generator<unknown, void> {
    const func = yield* newFunction(ctx, decl.parameters, decl.block);
    yield* putProperty(ctx, findActivationObjectScope(ctx.scope)?.object ?? ctx.realm.globalObject, decl.name, func);
}

function createGlobalContext(): Context {
    const intrinsics = createIntrinsics();
    const globalObject = createGlobal(intrinsics);
    globalObject.properties.set("sleep", {
        readOnly: false,
        dontEnum: false,
        dontDelete: false,
        internal: false,
        value: newNativeFunction(
            intrinsics.FunctionPrototype,
            function* (ctx, _self, args) {
                const ms = yield* toNumber(ctx, args[0]);
                return yield new Promise((resolve) => {
                    setTimeout(() => resolve("slept"), ms);
                });
            },
            1
        ),
    });
    const globalScope: Scope = {
        parent: undefined,
        activation: false,
        object: globalObject,
    };
    const realm: Realm = {
        intrinsics,
        globalObject,
        globalScope,
    };
    const context: Context = {
        scope: globalScope,
        this: globalScope.object,
        realm: realm,
    };
    return context;
}

function* run(source: string, context: Context): Generator<unknown, Completion> {
    const program = parse(source);
    let completion: Completion = {
        type: "normalCompletion",
        hasValue: false,
    };
    for (const element of program.sourceElements) {
        if (element.type !== "functionDeclaration") {
            continue;
        }
        yield* defineFunction(context, element);
    }
    for (const element of program.sourceElements) {
        if (element.type === "functionDeclaration") {
            continue;
        }
        visitStatement(element, (statement) => {
            if (statement.type === "variableStatement") {
                defineVariable(context, statement.variableDeclarationList);
            }
            if (statement.type === "forInStatement") {
                if (statement.initialization?.type === "variableDeclaration") {
                    defineVariable(context, [statement.initialization]);
                }
            }
        });
    }
    for (const element of program.sourceElements) {
        if (element.type === "functionDeclaration") {
            continue;
        }
        const result = yield* runStatement(context, element);
        if (result.hasValue) {
            completion = result;
        }
    }
    return completion;
}

export async function runAsync(source: string): Promise<Completion> {
    const iter = run(source, createGlobalContext());
    let lastResult: any = undefined;
    while (true) {
        const { value, done } = iter.next(lastResult);
        if (value instanceof Promise) {
            lastResult = await value;
        } else {
            lastResult = undefined;
        }
        if (done) {
            return value;
        }
    }
}
