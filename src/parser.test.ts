import { expect, test } from "vitest";
import { tokenize, Token, parse, SourceElement, Expression, Statement, Program, runAsync } from ".";

function filterByValue(tokens: Iterable<Token>): (string | null | boolean | number)[] {
    return [...tokens].map((x) => x.value);
}

function error(func: () => void): any {
    try {
        func();
    } catch (e) {
        return e;
    }
}

test("tokenizer", () => {
    expect(filterByValue(tokenize("keyword"))).toStrictEqual(["keyword"]);
    expect(filterByValue(tokenize("abc.def"))).toStrictEqual(["abc", ".", "def"]);
    expect(filterByValue(tokenize("var a=2"))).toStrictEqual(["var", "a", "=", 2]);
    expect(filterByValue(tokenize("true"))).toStrictEqual([true]);
    expect(filterByValue(tokenize("false"))).toStrictEqual([false]);
    expect(filterByValue(tokenize("null"))).toStrictEqual([null]);
    expect(filterByValue(tokenize("$$"))).toStrictEqual(["$$"]);
    expect(filterByValue(tokenize("++a"))).toStrictEqual(["++", "a"]);
    expect(filterByValue(tokenize("\na++\n"))).toStrictEqual(["\n", "a", "++", "\n"]);
    expect(filterByValue(tokenize("$0_"))).toStrictEqual(["$0_"]);
    expect(filterByValue(tokenize("keyword /* comment */"))).toStrictEqual(["keyword"]);
    expect(filterByValue(tokenize("keyword  \u0009\r\n\u000B\u000Ctest"))).toStrictEqual([
        "keyword",
        "\n",
        "\n",
        "test",
    ]);
    // 3rd edition: Syntax Error
    // The source character immediately following a NumericLiteral must not be an IdentifierStart or DecimalDigit.
    expect(filterByValue(tokenize("3in"))).toStrictEqual([3, "in"]);
    expect(filterByValue(tokenize("0;"))).toStrictEqual([0, ";"]);
    expect(filterByValue(tokenize("-123"))).toStrictEqual(["-", 123]);
    expect(filterByValue(tokenize("+123"))).toStrictEqual(["+", 123]);
    expect(filterByValue(tokenize("0123"))).toStrictEqual([0o123]);
    expect(filterByValue(tokenize("07"))).toStrictEqual([0o7]);
    expect(filterByValue(tokenize("0x123"))).toStrictEqual([0x123]);
    expect(filterByValue(tokenize("0Xf"))).toStrictEqual([0xf]);
    expect(filterByValue(tokenize("0xF"))).toStrictEqual([0xf]);
    expect(filterByValue(tokenize("0X123"))).toStrictEqual([0x123]);
    expect(filterByValue(tokenize("123"))).toStrictEqual([123]);
    expect(filterByValue(tokenize("1"))).toStrictEqual([1]);
    expect(filterByValue(tokenize("123.1"))).toStrictEqual([123.1]);
    expect(filterByValue(tokenize("123.e1"))).toStrictEqual([123e1]);
    expect(filterByValue(tokenize(".123e1"))).toStrictEqual([0.123e1]);
    expect(error(() => filterByValue(tokenize("0x")))).toBeTruthy();
    expect(error(() => filterByValue(tokenize("@")))).toBeTruthy();
    expect(error(() => filterByValue(tokenize("1e")))).toBeTruthy();
    expect(error(() => filterByValue(tokenize("1e+")))).toBeTruthy();
    expect(filterByValue(tokenize(".e+1"))).toStrictEqual([".", "e", "+", 1]);
    expect(filterByValue(tokenize("."))).toStrictEqual(["."]);
    expect(filterByValue(tokenize("1e+2"))).toStrictEqual([1e2]);
    expect(filterByValue(tokenize("1e-2"))).toStrictEqual([1e-2]);
    expect(filterByValue(tokenize("0.123"))).toStrictEqual([0.123]);
    expect(filterByValue(tokenize("1."))).toStrictEqual([1]);
    expect(filterByValue(tokenize("12."))).toStrictEqual([12]);
    expect(filterByValue(tokenize(".1"))).toStrictEqual([0.1]);
    expect(filterByValue(tokenize(".12"))).toStrictEqual([0.12]);
    expect(filterByValue(tokenize("1e2"))).toStrictEqual([1e2]);
    expect(filterByValue(tokenize("1e2 13"))).toStrictEqual([1e2, 13]);
    expect(filterByValue(tokenize("0.e+2"))).toStrictEqual([0e2]);
    expect(filterByValue(tokenize("12e2"))).toStrictEqual([12e2]);
    expect(filterByValue(tokenize("1e23"))).toStrictEqual([1e23]);
    expect(filterByValue(tokenize("12e23"))).toStrictEqual([12e23]);
    expect(filterByValue(tokenize("e23"))).toStrictEqual(["e23"]);
    expect(filterByValue(tokenize(">>>= >>= >> >= >"))).toStrictEqual([">>>=", ">>=", ">>", ">=", ">"]);
    expect(filterByValue(tokenize(`''`))).toStrictEqual([""]);
    expect(filterByValue(tokenize(`""`))).toStrictEqual([""]);
    expect(filterByValue(tokenize(`"abc"`))).toStrictEqual(["abc"]);
    expect(filterByValue(tokenize(`"abc",'def'`))).toStrictEqual(["abc", ",", "def"]);
    expect(filterByValue(tokenize(String.raw`"\""`))).toStrictEqual(['"']);
    expect(filterByValue(tokenize(String.raw`'\''`))).toStrictEqual(["\'"]);
    expect(filterByValue(tokenize(String.raw`"abc\nm"`))).toStrictEqual(["abc\nm"]);
    expect(filterByValue(tokenize(String.raw`"abc\012v"`))).toStrictEqual(["abc\x0av"]);
    expect(filterByValue(tokenize(String.raw`"abc\x0av"`))).toStrictEqual(["abc\x0av"]);
    expect(filterByValue(tokenize(String.raw`"abc\u000av"`))).toStrictEqual(["abc\x0av"]);
    expect(error(() => filterByValue(tokenize('"')))).toBeTruthy();
    expect(error(() => filterByValue(tokenize('"aaa')))).toBeTruthy();
    expect(filterByValue(tokenize(String.raw`return 1`))).toStrictEqual(["return", 1]);
    expect(
        filterByValue(
            tokenize(String.raw`return
1`)
        )
    ).toStrictEqual(["return", "\n", 1]);
    expect(
        filterByValue(
            tokenize(String.raw`return/*
*/1`)
        )
    ).toStrictEqual(["return", "\n", 1]);
    expect(
        filterByValue(
            tokenize(String.raw`return/**
***
**/1`)
        )
    ).toStrictEqual(["return", "\n", 1]);
    expect(
        filterByValue(
            tokenize(String.raw`return//
1`)
        )
    ).toStrictEqual(["return", "\n", 1]);
    expect(filterByValue(tokenize("a = 1; a |= 4;"))).toStrictEqual(["a", "=", 1, ";", "a", "|=", 4, ";"]);
});

function omitPosition(p: any): any {
    if (p == null) {
        return p;
    }
    if (typeof p !== "object") {
        return p;
    }
    if (Array.isArray(p)) {
        return p.map(omitPosition);
    }
    const entries = Object.entries(p).filter(([k]) => k !== "start" && k !== "end");
    for (const kv of entries) {
        kv[1] = omitPosition(kv[1]);
    }
    return Object.fromEntries(entries);
}

function astToString(p: Program): any[] {
    return p.sourceElements.map(sourceElementToString);
}

function sourceElementToString(p: SourceElement): string {
    switch (p.type) {
        case "functionDeclaration":
            throw new Error();
        default:
            return statementToString(p);
    }
}

function statementToString(p: Statement): string {
    switch (p.type) {
        case "block":
        case "variableStatement":
        case "emptyStatement":
            throw new Error();
        case "expressionStatement":
            return `(expr ${expressionToString(p.expression)})`;
        case "ifStatement":
            if (p.elseStatement == null) {
                return `(if ${expressionToString(p.expression)} ${statementToString(p.thenStatement)})`;
            } else {
                return `(if ${expressionToString(p.expression)} ${statementToString(p.thenStatement)} ${statementToString(p.elseStatement)})`;
            }
        case "whileStatement":
            return `(while ${expressionToString(p.expression)} ${statementToString(p.statement)})`;
        case "forStatement":
        case "forInStatement":
        case "continueStatement":
        case "breakStatement":
        case "returnStatement":
        case "withStatement":
            throw new Error();
    }
}

function expressionToString(e: Expression): string {
    switch (e.type) {
        case "thisExpression":
            return "this";
        case "identifierExpression":
            return e.name;
        case "literalExpression":
            return String(e.value);
        case "groupingOperator":
            return `(${expressionToString(e.expression)})`;
        case "memberOperator":
            if ("name" in e) {
                return `(member-ident ${expressionToString(e.left)} ${e.name})`;
            }
            return `(member-expr ${expressionToString(e.left)} ${expressionToString(e.right)})`;
        case "newOperator":
            if (e.argumentList == null) {
                return `(new-no-args ${expressionToString(e.expression)})`;
            }
            if (e.argumentList.length === 0) {
                return `(new ${expressionToString(e.expression)})`;
            }
            return `(new ${expressionToString(e.expression)} ${e.argumentList.map(expressionToString).join(" ")})`;
        case "callOperator":
            if (e.argumentList.length === 0) {
                return `(call ${expressionToString(e.expression)})`;
            }
            return `(call ${expressionToString(e.expression)} ${e.argumentList.map(expressionToString).join(" ")})`;
        case "postfixIncrementOperator":
            return `(post-inc ${expressionToString(e.expression)})`;
        case "postfixDecrementOperator":
            return `(post-dec ${expressionToString(e.expression)})`;
        case "deleteOperator":
            return `(delete ${expressionToString(e.expression)})`;
        case "voidOperator":
            return `(void ${expressionToString(e.expression)})`;
        case "typeofOperator":
            return `(typeof ${expressionToString(e.expression)})`;
        case "prefixIncrementOperator":
            return `(pre-inc ${expressionToString(e.expression)})`;
        case "prefixDecrementOperator":
            return `(pre-dec ${expressionToString(e.expression)})`;
        case "unaryPlusOperator":
            return `(+ ${expressionToString(e.expression)})`;
        case "unaryMinusOperator":
            return `(- ${expressionToString(e.expression)})`;
        case "bitwiseNotOperator":
            return `(~ ${expressionToString(e.expression)})`;
        case "logicalNotOperator":
            return `(! ${expressionToString(e.expression)})`;
        case "multiplyOperator":
            return `(* ${expressionToString(e.left)} ${expressionToString(e.right)})`;
        case "divideOperator":
            return `(/ ${expressionToString(e.left)} ${expressionToString(e.right)})`;
        case "moduloOperator":
            return `(% ${expressionToString(e.left)} ${expressionToString(e.right)})`;
        case "addOperator":
            return `(+ ${expressionToString(e.left)} ${expressionToString(e.right)})`;
        case "subtractOperator":
            return `(- ${expressionToString(e.left)} ${expressionToString(e.right)})`;
        case "leftShiftOperator":
            return `(<< ${expressionToString(e.left)} ${expressionToString(e.right)})`;
        case "signedRightShiftOperator":
            return `(>> ${expressionToString(e.left)} ${expressionToString(e.right)})`;
        case "unsignedRightShiftOperator":
            return `(>>> ${expressionToString(e.left)} ${expressionToString(e.right)})`;
        case "lessThanOperator":
            return `(< ${expressionToString(e.left)} ${expressionToString(e.right)})`;
        case "greaterThanOperator":
            return `(> ${expressionToString(e.left)} ${expressionToString(e.right)})`;
        case "lessThanOrEqualOperator":
            return `(<= ${expressionToString(e.left)} ${expressionToString(e.right)})`;
        case "greaterThanOrEqualOperator":
            return `(>= ${expressionToString(e.left)} ${expressionToString(e.right)})`;
        case "equalsOperator":
            return `(== ${expressionToString(e.left)} ${expressionToString(e.right)})`;
        case "doesNotEqualsOperator":
            return `(!= ${expressionToString(e.left)} ${expressionToString(e.right)})`;
        case "bitwiseAndOperator":
            return `(& ${expressionToString(e.left)} ${expressionToString(e.right)})`;
        case "bitwiseXorOperator":
            return `(^ ${expressionToString(e.left)} ${expressionToString(e.right)})`;
        case "bitwiseOrOperator":
            return `(| ${expressionToString(e.left)} ${expressionToString(e.right)})`;
        case "logicalAndOperator":
            return `(&& ${expressionToString(e.left)} ${expressionToString(e.right)})`;
        case "logicalOrOperator":
            return `(|| ${expressionToString(e.left)} ${expressionToString(e.right)})`;
        case "conditionalOperator":
            return `(if-expr ${expressionToString(e.conditionExpression)} ${expressionToString(e.thenExpression)} ${expressionToString(e.elseExpression)})`;
        case "assignmentOperator":
            return `(${e.operator} ${expressionToString(e.left)} ${expressionToString(e.right)})`;
        case "commaOperator":
            return `(comma ${expressionToString(e.left)} ${expressionToString(e.right)})`;
    }
}

test("parser", () => {
    expect(astToString(parse("this"))).toStrictEqual(["(expr this)"]);
    expect(astToString(parse("a"))).toStrictEqual(["(expr a)"]);
    expect(astToString(parse("1"))).toStrictEqual(["(expr 1)"]);
    expect(astToString(parse("null"))).toStrictEqual(["(expr null)"]);
    expect(astToString(parse("'abc'"))).toStrictEqual(["(expr abc)"]);
    expect(astToString(parse("(1)"))).toStrictEqual(["(expr (1))"]);
    expect(astToString(parse("abc.def"))).toStrictEqual(["(expr (member-ident abc def))"]);
    expect(astToString(parse("abc.def.ghi"))).toStrictEqual(["(expr (member-ident (member-ident abc def) ghi))"]);
    expect(astToString(parse("abc[123]"))).toStrictEqual(["(expr (member-expr abc 123))"]);
    expect(astToString(parse("abc.def(1)"))).toStrictEqual(["(expr (call (member-ident abc def) 1))"]);
    expect(astToString(parse("abc[123](1)"))).toStrictEqual(["(expr (call (member-expr abc 123) 1))"]);
    expect(astToString(parse("(abc[123])(1)"))).toStrictEqual(["(expr (call ((member-expr abc 123)) 1))"]);
    expect(astToString(parse("abc.def[123](1)"))).toStrictEqual([
        "(expr (call (member-expr (member-ident abc def) 123) 1))",
    ]);
    expect(astToString(parse("new abc"))).toStrictEqual(["(expr (new-no-args abc))"]);
    expect(astToString(parse("new abc()"))).toStrictEqual(["(expr (new abc))"]);
    expect(astToString(parse("new new abc()"))).toStrictEqual(["(expr (new-no-args (new abc)))"]);
    expect(astToString(parse("new abc(1)"))).toStrictEqual(["(expr (new abc 1))"]);
    expect(astToString(parse("new new abc(1)(2)"))).toStrictEqual(["(expr (new (new abc 1) 2))"]);
    expect(astToString(parse("new abc.def"))).toStrictEqual(["(expr (new-no-args (member-ident abc def)))"]);
    expect(astToString(parse("new abc().def"))).toStrictEqual(["(expr (member-ident (new abc) def))"]);
    expect(astToString(parse("new abc(1).def(2)"))).toStrictEqual(["(expr (call (member-ident (new abc 1) def) 2))"]);
    expect(astToString(parse("new new abc(1).def(2)"))).toStrictEqual([
        "(expr (new (member-ident (new abc 1) def) 2))",
    ]);
    expect(astToString(parse("new (new abc(1).def(2))"))).toStrictEqual([
        "(expr (new-no-args ((call (member-ident (new abc 1) def) 2))))",
    ]);
    expect(astToString(parse("new (new abc(1).def(2))(3)"))).toStrictEqual([
        "(expr (new ((call (member-ident (new abc 1) def) 2)) 3))",
    ]);
    expect(astToString(parse("new abc[1]"))).toStrictEqual(["(expr (new-no-args (member-expr abc 1)))"]);
    expect(astToString(parse("new abc[1](2)"))).toStrictEqual(["(expr (new (member-expr abc 1) 2))"]);
    expect(astToString(parse("new abc[1](2)(3)"))).toStrictEqual(["(expr (call (new (member-expr abc 1) 2) 3))"]);
    expect(astToString(parse("new new abc[1](2)(3)"))).toStrictEqual(["(expr (new (new (member-expr abc 1) 2) 3))"]);
    expect(astToString(parse("(new abc)(1)"))).toStrictEqual(["(expr (call ((new-no-args abc)) 1))"]);
    expect(astToString(parse("new new abc"))).toStrictEqual(["(expr (new-no-args (new-no-args abc)))"]);
    expect(astToString(parse("abc()"))).toStrictEqual(["(expr (call abc))"]);
    expect(astToString(parse("abc(1)"))).toStrictEqual(["(expr (call abc 1))"]);
    expect(astToString(parse("abc(1, 2)"))).toStrictEqual(["(expr (call abc 1 2))"]);
    expect(astToString(parse("abc(1, (2, 3))"))).toStrictEqual(["(expr (call abc 1 ((comma 2 3))))"]);
    expect(astToString(parse("a++"))).toStrictEqual(["(expr (post-inc a))"]);
    expect(astToString(parse("abc--"))).toStrictEqual(["(expr (post-dec abc))"]);
    expect(astToString(parse("delete abc"))).toStrictEqual(["(expr (delete abc))"]);
    expect(astToString(parse("void abc"))).toStrictEqual(["(expr (void abc))"]);
    expect(astToString(parse("typeof abc"))).toStrictEqual(["(expr (typeof abc))"]);
    expect(astToString(parse("++abc"))).toStrictEqual(["(expr (pre-inc abc))"]);
    expect(astToString(parse("--abc"))).toStrictEqual(["(expr (pre-dec abc))"]);
    expect(astToString(parse("-abc"))).toStrictEqual(["(expr (- abc))"]);
    expect(astToString(parse("+abc"))).toStrictEqual(["(expr (+ abc))"]);
    expect(astToString(parse("~abc"))).toStrictEqual(["(expr (~ abc))"]);
    expect(astToString(parse("!abc"))).toStrictEqual(["(expr (! abc))"]);
    expect(astToString(parse("abc * def"))).toStrictEqual(["(expr (* abc def))"]);
    expect(astToString(parse("abc / def"))).toStrictEqual(["(expr (/ abc def))"]);
    expect(astToString(parse("abc % def"))).toStrictEqual(["(expr (% abc def))"]);
    expect(astToString(parse("abc + def"))).toStrictEqual(["(expr (+ abc def))"]);
    expect(astToString(parse("abc - def"))).toStrictEqual(["(expr (- abc def))"]);
    expect(astToString(parse("abc << def"))).toStrictEqual(["(expr (<< abc def))"]);
    expect(astToString(parse("abc >> def"))).toStrictEqual(["(expr (>> abc def))"]);
    expect(astToString(parse("abc >>> def"))).toStrictEqual(["(expr (>>> abc def))"]);
    expect(astToString(parse("abc < def"))).toStrictEqual(["(expr (< abc def))"]);
    expect(astToString(parse("abc > def"))).toStrictEqual(["(expr (> abc def))"]);
    expect(astToString(parse("abc <= def"))).toStrictEqual(["(expr (<= abc def))"]);
    expect(astToString(parse("abc >= def"))).toStrictEqual(["(expr (>= abc def))"]);
    expect(astToString(parse("abc == def"))).toStrictEqual(["(expr (== abc def))"]);
    expect(astToString(parse("abc != def"))).toStrictEqual(["(expr (!= abc def))"]);
    expect(astToString(parse("abc & def"))).toStrictEqual(["(expr (& abc def))"]);
    expect(astToString(parse("abc ^ def"))).toStrictEqual(["(expr (^ abc def))"]);
    expect(astToString(parse("abc | def"))).toStrictEqual(["(expr (| abc def))"]);
    expect(astToString(parse("abc && def"))).toStrictEqual(["(expr (&& abc def))"]);
    expect(astToString(parse("abc || def"))).toStrictEqual(["(expr (|| abc def))"]);
    expect(astToString(parse("1 ? 2 : 3"))).toStrictEqual(["(expr (if-expr 1 2 3))"]);
    expect(astToString(parse("abc = def"))).toStrictEqual(["(expr (= abc def))"]);
    expect(astToString(parse("abc = def /= 123"))).toStrictEqual(["(expr (= abc (/= def 123)))"]);
    expect(astToString(parse("abc |= 123"))).toStrictEqual(["(expr (|= abc 123))"]);
    expect(astToString(parse("abc, def"))).toStrictEqual(["(expr (comma abc def))"]);
    expect(astToString(parse("abc, def, ghi"))).toStrictEqual(["(expr (comma (comma abc def) ghi))"]);
    expect(astToString(parse("1+1"))).toStrictEqual(["(expr (+ 1 1))"]);
    expect(astToString(parse("1-1"))).toStrictEqual(["(expr (- 1 1))"]);
    expect(astToString(parse("1+2+3"))).toStrictEqual(["(expr (+ (+ 1 2) 3))"]);
    expect(astToString(parse("1+2+3+4"))).toStrictEqual(["(expr (+ (+ (+ 1 2) 3) 4))"]);
    expect(astToString(parse("1+1/3"))).toStrictEqual(["(expr (+ 1 (/ 1 3)))"]);
    expect(
        astToString(
            parse(String.raw`
1+1
2+2
`)
        )
    ).toStrictEqual(["(expr (+ 1 1))", "(expr (+ 2 2))"]);
    expect(
        astToString(
            parse(String.raw`a++
        b++
`)
        )
    ).toStrictEqual(["(expr (post-inc a))", "(expr (post-inc b))"]);
    expect(
        astToString(
            parse(String.raw`a
        ++b
`)
        )
    ).toStrictEqual(["(expr a)", "(expr (pre-inc b))"]);
    expect(astToString(parse(String.raw`a++;b`))).toStrictEqual(["(expr (post-inc a))", "(expr b)"]);
    expect(
        astToString(
            parse(String.raw`a++//
b`)
        )
    ).toStrictEqual(["(expr (post-inc a))", "(expr b)"]);
    expect(
        astToString(
            parse(String.raw`a++/*
*/b`)
        )
    ).toStrictEqual(["(expr (post-inc a))", "(expr b)"]);
    expect(error(() => parse(String.raw`a++b`))).toBeTruthy();
    expect(
        astToString(
            parse(String.raw`a = b + c
(d + e).print()`)
        )
    ).toStrictEqual(["(expr (= a (+ b (call (member-ident (call c (+ d e)) print)))))"]);
    expect(astToString(parse(String.raw`a = b + c(d + e).print()`))).toStrictEqual([
        "(expr (= a (+ b (call (member-ident (call c (+ d e)) print)))))",
    ]);
    expect(error(() => parse(String.raw`abc var abc`))).toBeTruthy();
    expect(error(() => parse(String.raw`void a = 1`))).toBeTruthy();
    expect(error(() => parse(String.raw`typeof a = 1`))).toBeTruthy();
    expect(
        omitPosition(
            parse(String.raw`
abc
var abc
                `)
        )
    ).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "expressionStatement",
                expression: {
                    type: "identifierExpression",
                    name: "abc",
                },
            },
            {
                type: "variableStatement",
                variableDeclarationList: [
                    {
                        type: "variableDeclaration",
                        name: "abc",
                        initializer: undefined,
                    },
                ],
            },
        ],
    } as Program);
    expect(
        omitPosition(
            parse(String.raw`
while (1) {
    console.log(1);
}
                `)
        )
    ).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "whileStatement",
                expression: { type: "literalExpression", value: 1 },
                statement: {
                    type: "block",
                    statementList: [
                        {
                            type: "expressionStatement",
                            expression: {
                                type: "callOperator",
                                expression: {
                                    type: "memberOperator",
                                    left: { type: "identifierExpression", name: "console" },
                                    name: "log",
                                },
                                argumentList: [{ type: "literalExpression", value: 1 }],
                            },
                        },
                    ],
                },
            },
        ],
    });
    expect(omitPosition(parse(String.raw`while (1)console.log(1)`))).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "whileStatement",
                expression: { type: "literalExpression", value: 1 },
                statement: {
                    type: "expressionStatement",
                    expression: {
                        type: "callOperator",
                        expression: {
                            type: "memberOperator",
                            left: { type: "identifierExpression", name: "console" },
                            name: "log",
                        },
                        argumentList: [{ type: "literalExpression", value: 1 }],
                    },
                },
            },
        ],
    } as Program);
    expect(omitPosition(parse(String.raw`with(1)console.log(1)`))).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "withStatement",
                expression: { type: "literalExpression", value: 1 },
                statement: {
                    type: "expressionStatement",
                    expression: {
                        type: "callOperator",
                        expression: {
                            type: "memberOperator",
                            left: { type: "identifierExpression", name: "console" },
                            name: "log",
                        },
                        argumentList: [{ type: "literalExpression", value: 1 }],
                    },
                },
            },
        ],
    } as Program);
    expect(omitPosition(parse(String.raw`break;continue;return 1;`))).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "breakStatement",
            },
            {
                type: "continueStatement",
            },
            {
                type: "returnStatement",
                expression: { type: "literalExpression", value: 1 },
            },
        ],
    } as Program);
    expect(
        omitPosition(
            parse(String.raw`return
1`)
        )
    ).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "returnStatement",
                expression: undefined,
            },
            {
                type: "expressionStatement",
                expression: { type: "literalExpression", value: 1 },
            },
        ],
    } as Program);
    expect(omitPosition(parse(String.raw`;;`))).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "emptyStatement",
            },
            {
                type: "emptyStatement",
            },
        ],
    } as Program);
    expect(omitPosition(parse(String.raw`var a = 1`))).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "variableStatement",
                variableDeclarationList: [
                    {
                        type: "variableDeclaration",
                        name: "a",
                        initializer: {
                            type: "literalExpression",
                            value: 1,
                        },
                    },
                ],
            },
        ],
    } as Program);
    expect(omitPosition(parse(String.raw`var a = 1, b = 2`))).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "variableStatement",
                variableDeclarationList: [
                    {
                        type: "variableDeclaration",
                        name: "a",
                        initializer: {
                            type: "literalExpression",
                            value: 1,
                        },
                    },
                    {
                        type: "variableDeclaration",
                        name: "b",
                        initializer: {
                            type: "literalExpression",
                            value: 2,
                        },
                    },
                ],
            },
        ],
    } as Program);
    expect(omitPosition(parse(String.raw`var a;`))).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "variableStatement",
                variableDeclarationList: [
                    {
                        type: "variableDeclaration",
                        name: "a",
                        initializer: undefined,
                    },
                ],
            },
        ],
    } as Program);
    expect(omitPosition(parse(String.raw`var a,b`))).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "variableStatement",
                variableDeclarationList: [
                    {
                        type: "variableDeclaration",
                        name: "a",
                        initializer: undefined,
                    },
                    {
                        type: "variableDeclaration",
                        name: "b",
                        initializer: undefined,
                    },
                ],
            },
        ],
    } as Program);
    expect(omitPosition(parse(String.raw`if (1);else;`))).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "ifStatement",
                expression: {
                    type: "literalExpression",
                    value: 1,
                },
                thenStatement: {
                    type: "emptyStatement",
                },
                elseStatement: {
                    type: "emptyStatement",
                },
            },
        ],
    } as Program);
    expect(omitPosition(parse(String.raw`if (1);`))).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "ifStatement",
                expression: {
                    type: "literalExpression",
                    value: 1,
                },
                thenStatement: {
                    type: "emptyStatement",
                },
                elseStatement: undefined,
            },
        ],
    } as Program);
    expect(omitPosition(parse(String.raw`for(;;);`))).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "forStatement",
                initialization: undefined,
                condition: undefined,
                afterthought: undefined,
                statement: {
                    type: "emptyStatement",
                },
            },
        ],
    } as Program);
    expect(omitPosition(parse(String.raw`for(var a;;);`))).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "forStatement",
                initialization: {
                    type: "variableStatement",
                    variableDeclarationList: [
                        {
                            type: "variableDeclaration",
                            name: "a",
                            initializer: undefined,
                        },
                    ],
                },
                condition: undefined,
                afterthought: undefined,
                statement: {
                    type: "emptyStatement",
                },
            },
        ],
    } as Program);
    expect(omitPosition(parse(String.raw`for(var a,b;;);`))).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "forStatement",
                initialization: {
                    type: "variableStatement",
                    variableDeclarationList: [
                        {
                            type: "variableDeclaration",
                            name: "a",
                            initializer: undefined,
                        },
                        {
                            type: "variableDeclaration",
                            name: "b",
                            initializer: undefined,
                        },
                    ],
                },
                condition: undefined,
                afterthought: undefined,
                statement: {
                    type: "emptyStatement",
                },
            },
        ],
    } as Program);
    expect(omitPosition(parse(String.raw`for(var a=1,b;;);`))).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "forStatement",
                initialization: {
                    type: "variableStatement",
                    variableDeclarationList: [
                        {
                            type: "variableDeclaration",
                            name: "a",
                            initializer: {
                                type: "literalExpression",
                                value: 1,
                            },
                        },
                        {
                            type: "variableDeclaration",
                            name: "b",
                            initializer: undefined,
                        },
                    ],
                },
                condition: undefined,
                afterthought: undefined,
                statement: {
                    type: "emptyStatement",
                },
            },
        ],
    } as Program);
    expect(omitPosition(parse(String.raw`for(var a=1,b;a>0;);`))).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "forStatement",
                initialization: {
                    type: "variableStatement",
                    variableDeclarationList: [
                        {
                            type: "variableDeclaration",
                            name: "a",
                            initializer: {
                                type: "literalExpression",
                                value: 1,
                            },
                        },
                        {
                            type: "variableDeclaration",
                            name: "b",
                            initializer: undefined,
                        },
                    ],
                },
                condition: {
                    type: "greaterThanOperator",
                    left: {
                        type: "identifierExpression",
                        name: "a",
                    },
                    right: {
                        type: "literalExpression",
                        value: 0,
                    },
                },
                afterthought: undefined,
                statement: {
                    type: "emptyStatement",
                },
            },
        ],
    } as Program);
    expect(omitPosition(parse(String.raw`for(var a=1,b;a>0;a++);`))).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "forStatement",
                initialization: {
                    type: "variableStatement",
                    variableDeclarationList: [
                        {
                            type: "variableDeclaration",
                            name: "a",
                            initializer: {
                                type: "literalExpression",
                                value: 1,
                            },
                        },
                        {
                            type: "variableDeclaration",
                            name: "b",
                            initializer: undefined,
                        },
                    ],
                },
                condition: {
                    type: "greaterThanOperator",
                    left: {
                        type: "identifierExpression",
                        name: "a",
                    },
                    right: {
                        type: "literalExpression",
                        value: 0,
                    },
                },
                afterthought: {
                    type: "postfixIncrementOperator",
                    expression: {
                        type: "identifierExpression",
                        name: "a",
                    },
                },
                statement: {
                    type: "emptyStatement",
                },
            },
        ],
    } as Program);
    expect(omitPosition(parse(String.raw`for(var a=1,b;a>0;a++,b++);`))).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "forStatement",
                initialization: {
                    type: "variableStatement",
                    variableDeclarationList: [
                        {
                            type: "variableDeclaration",
                            name: "a",
                            initializer: {
                                type: "literalExpression",
                                value: 1,
                            },
                        },
                        {
                            type: "variableDeclaration",
                            name: "b",
                            initializer: undefined,
                        },
                    ],
                },
                condition: {
                    type: "greaterThanOperator",
                    left: {
                        type: "identifierExpression",
                        name: "a",
                    },
                    right: {
                        type: "literalExpression",
                        value: 0,
                    },
                },
                afterthought: {
                    type: "commaOperator",
                    left: {
                        type: "postfixIncrementOperator",
                        expression: {
                            type: "identifierExpression",
                            name: "a",
                        },
                    },
                    right: {
                        type: "postfixIncrementOperator",
                        expression: {
                            type: "identifierExpression",
                            name: "b",
                        },
                    },
                },
                statement: {
                    type: "emptyStatement",
                },
            },
        ],
    } as Program);
    expect(omitPosition(parse(String.raw`for(a in b);`))).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "forInStatement",
                initialization: {
                    type: "identifierExpression",
                    name: "a",
                },
                expression: {
                    type: "identifierExpression",
                    name: "b",
                },
                statement: {
                    type: "emptyStatement",
                },
            },
        ],
    } as Program);
    expect(omitPosition(parse(String.raw`for(var a in b);`))).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "forInStatement",
                initialization: {
                    type: "variableDeclaration",
                    name: "a",
                    initializer: undefined,
                },
                expression: {
                    type: "identifierExpression",
                    name: "b",
                },
                statement: {
                    type: "emptyStatement",
                },
            },
        ],
    } as Program);
    expect(omitPosition(parse(String.raw`for(var a =1in b);`))).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "forInStatement",
                initialization: {
                    type: "variableDeclaration",
                    name: "a",
                    initializer: {
                        type: "literalExpression",
                        value: 1,
                    },
                },
                expression: {
                    type: "identifierExpression",
                    name: "b",
                },
                statement: {
                    type: "emptyStatement",
                },
            },
        ],
    } as Program);
    expect(omitPosition(parse(String.raw`function func(){}`))).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "functionDeclaration",
                name: "func",
                parameters: [] as string[],
                block: {
                    type: "block",
                    statementList: [] as Statement[],
                },
            },
        ],
    } as Program);
    expect(omitPosition(parse(String.raw`function func(a){}`))).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "functionDeclaration",
                name: "func",
                parameters: ["a"],
                block: {
                    type: "block",
                    statementList: [] as Statement[],
                },
            },
        ],
    } as Program);
    expect(omitPosition(parse(String.raw`function func(a, b){}`))).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "functionDeclaration",
                name: "func",
                parameters: ["a", "b"],
                block: {
                    type: "block",
                    statementList: [] as Statement[],
                },
            },
        ],
    } as Program);
    expect(
        omitPosition(
            parse(String.raw`function func(a, b){
a /= 2;
        }`)
        )
    ).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "functionDeclaration",
                name: "func",
                parameters: ["a", "b"],
                block: {
                    type: "block",
                    statementList: [
                        {
                            type: "expressionStatement",
                            expression: {
                                type: "assignmentOperator",
                                left: {
                                    type: "identifierExpression",
                                    name: "a",
                                },
                                operator: "/=",
                                right: {
                                    type: "literalExpression",
                                    value: 2,
                                },
                            },
                        },
                    ],
                },
            },
        ],
    } as Program);
    expect(
        parse(String.raw`anc/=2;function func(a, b){
anc /= 2;
        }`)
    ).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "expressionStatement",
                expression: {
                    type: "assignmentOperator",
                    left: {
                        type: "identifierExpression",
                        name: "anc",
                        start: { index: 0, line: 1, column: 1 },
                        end: { index: 2, line: 1, column: 3 },
                    },
                    operator: "/=",
                    right: {
                        type: "literalExpression",
                        value: 2,
                        start: { index: 5, line: 1, column: 6 },
                        end: { index: 5, line: 1, column: 6 },
                    },
                    start: { index: 0, line: 1, column: 1 },
                    end: { index: 5, line: 1, column: 6 },
                },
                start: { index: 0, line: 1, column: 1 },
                end: { index: 6, line: 1, column: 7 },
            },
            {
                type: "functionDeclaration",
                name: "func",
                parameters: ["a", "b"],
                block: {
                    type: "block",
                    statementList: [
                        {
                            type: "expressionStatement",
                            expression: {
                                type: "assignmentOperator",
                                left: {
                                    type: "identifierExpression",
                                    name: "anc",
                                    start: { index: 28, line: 2, column: 1 },
                                    end: { index: 30, line: 2, column: 3 },
                                },
                                operator: "/=",
                                right: {
                                    type: "literalExpression",
                                    value: 2,
                                    start: { index: 35, line: 2, column: 8 },
                                    end: { index: 35, line: 2, column: 8 },
                                },
                                start: { index: 28, line: 2, column: 1 },
                                end: { index: 35, line: 2, column: 8 },
                            },
                            start: { index: 28, line: 2, column: 1 },
                            end: { index: 36, line: 2, column: 9 },
                        },
                    ],
                    start: { index: 26, line: 1, column: 27 },
                    end: { index: 46, line: 3, column: 9 },
                },
                start: { index: 7, line: 1, column: 8 },
                end: { index: 46, line: 3, column: 9 },
            },
        ],
        start: { index: 0, line: 1, column: 1 },
        end: { index: 47, line: 3, column: 10 },
    });
});

test("interpreter", async () => {
    expect(await runAsync("1*2+3*4;")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1 * 2 + 3 * 4,
    });
    expect(await runAsync("1*(2+3)*4;")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1 * (2 + 3) * 4,
    });
    expect(await runAsync("1+'2';")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "12",
    });
    expect(await runAsync("'1'+'2';")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "12",
    });
    expect(await runAsync("'1'+2;")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "12",
    });
    expect(await runAsync("'10'*2;")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 20,
    });
    expect(await runAsync("NaN;")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: NaN,
    });
    expect(await runAsync("Object.length;")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync("this.Object.length;")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync("Object['length'];")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync("this.Object['length'];")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync("(new Object).constructor.length")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync("new Object().constructor.length")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync("String(11)")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "11",
    });
    expect(await runAsync("String()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "",
    });
    expect(await runAsync("Number(11)")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 11,
    });
    expect(await runAsync("Number()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    expect(await runAsync("Boolean()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: false,
    });
    expect(await runAsync("Boolean(true)")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(await runAsync("String.prototype + '1'")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "1",
    });
    expect(await runAsync("String.prototype.length")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    expect(await runAsync("String.prototype + 1")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "1",
    });
    expect(await runAsync("'1' + String.prototype")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "1",
    });
    expect(await runAsync("1 + String.prototype")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "1",
    });
    expect(await runAsync("String.prototype.valueOf()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "",
    });
    expect(await runAsync("String.prototype.toString()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "",
    });
    expect(await runAsync("new String('1')+1")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "11",
    });
    expect(await runAsync("new Number(1).valueOf()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync("new Number(16).toString()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "16",
    });
    expect(await runAsync("new Number(16).toString(16)")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "10",
    });
    expect(await runAsync("new Number(16).toString(Number.undef)")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "16",
    });
    expect(await runAsync("new Number(16).toString(2)")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "10000",
    });
    expect(await runAsync("new Number(16).toString(36)")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "g",
    });
    await expect(runAsync("Number(1)Number(2)")).rejects.toThrow();
    await expect(runAsync("new Number(16).toString(null)")).rejects.toThrow();
    await expect(runAsync("new Number(16).toString(1)")).rejects.toThrow();
    await expect(runAsync("new Number(16).toString(37)")).rejects.toThrow();
    expect(await runAsync("Number.prototype.valueOf()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    expect(await runAsync("'1'.toString()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "1",
    });
    expect(await runAsync("(1).toString()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "1",
    });
    await expect(runAsync("1.toString()")).rejects.toThrow();
    expect(await runAsync("1.0.toString()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "1",
    });
    expect(await runAsync("1e0.toString()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "1",
    });
    expect(await runAsync("true.toString()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "true",
    });
    expect(await runAsync("false.toString()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "false",
    });
    expect(await runAsync("Boolean(true).toString()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "true",
    });
    expect(await runAsync("Boolean(new Object())")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(await runAsync("Boolean.prototype.valueOf()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: false,
    });
    expect(await runAsync("new Object('1').valueOf()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "1",
    });
    expect(await runAsync("new Object('12345').length")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 5,
    });
    expect(await runAsync("new Object('1').toString()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "1",
    });
    expect(await runAsync("new Object('1') + '2'")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "12",
    });
    expect(await runAsync("new Object('1').valueOf()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "1",
    });
    expect(await runAsync("new Object(1).valueOf()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync("new Object(true).valueOf()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(await runAsync("new Object(new Object(true)).valueOf()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(await runAsync("null")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: null,
    });
    expect(await runAsync("Object.hoge")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: undefined,
    });
    expect(await runAsync("Object.prototype.toString()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "[object Object]",
    });
    await expect(runAsync("null.toString()")).rejects.toThrow();
    await expect(runAsync("new 1")).rejects.toThrow();
    await expect(runAsync("new 'a'")).rejects.toThrow();
    await expect(runAsync("new new Object()")).rejects.toThrow();
    expect(await runAsync("Object.prototype.constructor(1).valueOf()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync("Object.prototype.constructor(true).valueOf()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(await runAsync("Object.prototype.constructor('1').valueOf()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "1",
    });
    expect(await runAsync("String.prototype.constructor(1)")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "1",
    });
    expect(await runAsync("Number.prototype.constructor('1')")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync("Boolean.prototype.constructor(1)")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(await runAsync("Boolean.prototype.constructor(1)")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(await runAsync("new String.prototype.constructor(1).valueOf()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "1",
    });
    expect(await runAsync("new Number.prototype.constructor('1').valueOf()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync("new Boolean.prototype.constructor(1).valueOf()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(await runAsync("new Boolean.prototype.constructor.prototype.constructor(1).valueOf()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(await runAsync("new Boolean.prototype.constructor(1)")).toMatchObject({
        type: "normalCompletion",
        hasValue: true,
        value: {
            internalProperties: {
                value: true,
            },
        },
    });
    expect(await runAsync("new Number.prototype.constructor(1)")).toMatchObject({
        type: "normalCompletion",
        hasValue: true,
        value: {
            internalProperties: {
                value: 1,
            },
        },
    });
    expect(await runAsync("new String.prototype.constructor(1)")).toMatchObject({
        type: "normalCompletion",
        hasValue: true,
        value: {
            internalProperties: {
                value: "1",
            },
        },
    });
    expect(await runAsync("new Boolean(1)")).toMatchObject({
        type: "normalCompletion",
        hasValue: true,
        value: {
            internalProperties: {
                value: true,
            },
        },
    });
    expect(await runAsync("new Number(1)")).toMatchObject({
        type: "normalCompletion",
        hasValue: true,
        value: {
            internalProperties: {
                value: 1,
            },
        },
    });
    expect(await runAsync("new String(1)")).toMatchObject({
        type: "normalCompletion",
        hasValue: true,
        value: {
            internalProperties: {
                value: "1",
            },
        },
    });
    expect(await runAsync("a = 1")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync("a = 1; a")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync("Object = 1; Object")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync("Object.length = 123; Object.length")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync("Object.prototype.length = 123; Object.prototype.length")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 123,
    });
    expect(
        await runAsync(String.raw`
Object.prototype.hoge = 1;
o1 = new Object;
o1.hoge = 123;
o2 = new Object;
o2.hoge = o2.hoge + 1;
o2.hoge;
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 2,
    });
    expect(
        await runAsync(String.raw`
Object.prototype.hoge = 1;
o1 = new Object;
o1.hoge = 123;
o2 = new Object;
o2.hoge = o2.hoge + 1;
o1.hoge;
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 123,
    });
    expect(
        await runAsync(String.raw`
o1 = new Object;
Object.prototype.hoge = 1;
o1.hoge;
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
o1 = new Object;
Object.prototype.hoge = 1;
o1.hoge = 2;
Object.prototype.hoge;
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
Number.prototype.hoge = 1;
1.0.hoge = 123;
1.0.hoge = 1.0.hoge + 456;
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 457,
    });
    expect(
        await runAsync(String.raw`
            with (new Object) {
                hoge = 1;
            }
            hoge;
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
            o = new Object;
            o.a = 1;
            with (o) {
                a = 2;
            }
            o.a
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 2,
    });
    expect(
        await runAsync(String.raw`
            abc;
            var abc
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: undefined,
    });
    expect(
        await runAsync(String.raw`
            abc = 9;
            var abc
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 9,
    });
    expect(
        await runAsync(String.raw`
            abc = 9;
            var abc;
            abc;
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 9,
    });
    expect(
        await runAsync(String.raw`
            abc = 9;
            var abc = 10;
            abc;
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 10,
    });
    expect(
        await runAsync(String.raw`
            abc = 9;
            var abc = abc - 9;
            abc;
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    expect(
        await runAsync(String.raw`
            var abc = abc; abc;
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: undefined,
    });
    expect(await runAsync(String.raw`Function.prototype()`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: undefined,
    });
    /*expect(await runAsync(String.raw`Function.prototype.constructor.prototype()`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: undefined,
    });*/
    await expect(runAsync("new Object.prototype")).rejects.toThrow();
    await expect(runAsync("Object.prototype()")).rejects.toThrow();
    expect(await runAsync("new Object().toString")).toMatchObject({
        type: "normalCompletion",
        hasValue: true,
        value: {
            internalProperties: {
                class: "Function",
            },
        },
    });
    expect(
        await runAsync(String.raw`
        Function.prototype.hoge = 1
        new Object().toString.hoge
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        a = new Object().toString
        a()
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "[object Global]", // ????
    });
    expect(
        await runAsync(String.raw`
        if (1) 1; else 0;
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        if (1) 1;
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        if (0) 2; else 3;
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 3,
    });
    expect(
        await runAsync(String.raw`
        if (0) 1;
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: false,
    });
    expect(await runAsync(String.raw`1<2`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1 < 2,
    });
    expect(await runAsync(String.raw`1<1`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1 < 1,
    });
    expect(await runAsync(String.raw`NaN<NaN`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: NaN < NaN,
    });
    expect(await runAsync(String.raw`1>2`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1 > 2,
    });
    expect(await runAsync(String.raw`1>1`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1 > 1,
    });
    expect(await runAsync(String.raw`NaN>NaN`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1 > 1,
    });
    expect(await runAsync(String.raw`1>=2`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1 >= 2,
    });
    expect(await runAsync(String.raw`1>=1`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1 >= 1,
    });
    expect(await runAsync(String.raw`Infinity>=-Infinity`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: Infinity >= -Infinity,
    });
    expect(await runAsync(String.raw`Infinity>=Infinity`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: Infinity >= Infinity,
    });
    expect(await runAsync(String.raw`Infinity>=0`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: Infinity >= 0,
    });
    expect(await runAsync(String.raw`0>=Infinity`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0 >= Infinity,
    });
    expect(await runAsync(String.raw`0>=-Infinity`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0 >= -Infinity,
    });
    expect(await runAsync(String.raw`-Infinity>=0`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: -Infinity >= 0,
    });
    expect(await runAsync(String.raw`'A'>='B'`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "A" >= "B",
    });
    expect(await runAsync(String.raw`'aA'>='aB'`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "aA" >= "aB",
    });
    expect(await runAsync(String.raw`'A'>='A'`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "A" >= "A",
    });
    expect(await runAsync(String.raw`'A'<'A'`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "A" < "A",
    });
    expect(await runAsync(String.raw`'AB'<'AB'`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "AB" < "AB",
    });
    expect(await runAsync(String.raw`'C'>'B'`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "C" > "B",
    });
    expect(await runAsync(String.raw`'B'>'C'`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "B" > "C",
    });
    expect(await runAsync(String.raw`'AB'>'AB'`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "AB" > "AB",
    });
    expect(await runAsync(String.raw`1==1`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(await runAsync(String.raw`1==2`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: false,
    });
    expect(await runAsync(String.raw`1!=1`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: false,
    });
    expect(await runAsync(String.raw`1!=2`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(await runAsync(String.raw`NaN == NaN`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: false,
    });
    expect(await runAsync(String.raw`NaN != NaN`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(await runAsync(String.raw`true == 1`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(await runAsync(String.raw`true == 0`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: false,
    });
    expect(await runAsync(String.raw`true == 0`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: false,
    });
    expect(await runAsync(String.raw`Number.prototype == 0`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(await runAsync(String.raw`String.prototype == ""`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(await runAsync(String.raw`Boolean.prototype == false`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(await runAsync(String.raw`Object() == Object()`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: false,
    });
    expect(await runAsync(String.raw`Object == Object`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(
        await runAsync(String.raw`
        var a = new Object;
        var b = new Object;
        a == b`)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: false,
    });
    expect(
        await runAsync(String.raw`
        var a = Object();
        var b = Object();
        a == b`)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: false,
    });
    expect(
        await runAsync(String.raw`
        var a = new Object;
        var b = new Object(a);
        a == b`)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(
        await runAsync(String.raw`
        var a = Object();
        var b = Object(a);
        a == b`)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(await runAsync(String.raw`+1`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`-1`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: -1,
    });
    expect(await runAsync(String.raw`+""`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    expect(await runAsync(String.raw`-""`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: -0,
    });
    expect(await runAsync(String.raw`+"1"`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`-"1"`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: -1,
    });
    expect(
        await runAsync(String.raw`
        while (false) {
            Object.a();
        }
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: false,
    });
    expect(
        await runAsync(String.raw`
        a = 0
        while (a < 10) {
            a = a + 1
        }
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 10,
    });
    expect(
        await runAsync(String.raw`
        a = 0
        while (a < 10) {
            a = a + 1
            break;
        }
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        a = 0
        while (a < 10) {
            a = a + 1
            continue;
        }
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 10,
    });
    expect(
        await runAsync(String.raw`
            for (var i = 0; i < 10; i = i + 1) {}
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: false,
    });
    expect(
        await runAsync(String.raw`
            for (var i = 0; i < 10; i = i + 1) i;
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 9,
    });
    expect(
        await runAsync(String.raw`
            for (var i = 0; i < 10; i = i + 1) i = i + 2;
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 11,
    });
    expect(
        await runAsync(String.raw`
            for (var i = 0; i < 10; i = i + 1) {
                i;
                break;
            }
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    expect(
        await runAsync(String.raw`
            for (var i = 0; i < 10; i = i + 1) {
                i;
                continue;
            }
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 9,
    });
    expect(
        await runAsync(String.raw`
            for (;;) {
                1;
                break;
            }
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
            i = 0;
            for (;i = i + 1;) {
                if (i == 10) {
                    "ok";
                    break;
                }
            }
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "ok",
    });
    expect(
        await runAsync(String.raw`
            i = 0;
            for (;;i = i + 1) {
                if (i == 10) {
                    break;
                }
            }
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    expect(await runAsync(String.raw`0;while(0);`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    expect(await runAsync(String.raw`0;for(1;0;1);`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0, // V8, JSCore: undefined, Spidermonkey: 0, ???
    });
    expect(
        await runAsync(String.raw`
            for (i = 0;;) {
                i = i + 1;
                if (i == 10) {
                    break;
                }
            }
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 10,
    });
    expect(
        await runAsync(String.raw`
            o = new Object(); o.a = 1; for (var a in o) { a }
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "a",
    });
    expect(
        await runAsync(String.raw`
            Object.prototype.a = 2;
            o = new Object(); o.a = 1; for (var a in o) { a }
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "a",
    });
    expect(
        await runAsync(String.raw`
            Object.prototype.a = 2;
            o = new Object(); for (var a in o) { a }
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "a",
    });
    expect(
        await runAsync(String.raw`
            o = new Object();
            o.a = 1;
            for (o.a in o); o.a
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "a",
    });
    expect(await runAsync(String.raw`for (var a in Object);`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: false,
    });
    await expect(runAsync(String.raw`for (a=1 in Object);`)).rejects.toThrow();
    expect(await runAsync(String.raw`for (var a = 1 in Object); a`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`4&1`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    expect(await runAsync(String.raw`1&1`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`1&true`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`true&true`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`true&false`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    expect(await runAsync(String.raw`4|1`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 5,
    });
    expect(await runAsync(String.raw`1|1`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`1|true`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`true|true`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`true|false`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`(0xffffffff + 1) | 0`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    expect(await runAsync(String.raw`(0xffffffff + 1) & 0xffffffff`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    expect(await runAsync(String.raw`0x80000000|0`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: -2147483648,
    });
    expect(await runAsync(String.raw`-1 & ~0x80000000`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0x7fffffff,
    });
    expect(await runAsync(String.raw`~-1`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    expect(await runAsync(String.raw`!0`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(await runAsync(String.raw`1||0`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`''||'123'`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "123",
    });
    expect(await runAsync(String.raw`''&&'123'`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "",
    });
    expect(await runAsync(String.raw`'1'&&'123'`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "123",
    });
    expect(await runAsync(String.raw`var a = 0, b = 0; (a = 1) && (b = 2); a + b;`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 3,
    });
    expect(await runAsync(String.raw`var a = 0, b = 0; (a = 0) && (b = 2); a + b;`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    expect(await runAsync(String.raw`var a = 0, b = 0; (a = 1) || (b = 2); a + b;`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`var a = 0, b = 0; (a = 0) || (b = 2); a + b;`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 2,
    });
    expect(await runAsync(String.raw`true ? 'T' : 'F'`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "T",
    });
    expect(await runAsync(String.raw`!true ? 'T' : 'F'`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "F",
    });
    expect(await runAsync(String.raw`var a = 1, b = 1; true ? a = 'T' : b = 'F'; a + b`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "T1",
    });
    expect(await runAsync(String.raw`var a = 1, b = 1; !true ? a = 'T' : b = 'F'; a + b`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "1F",
    });
    expect(await runAsync(String.raw`a = 1; a *= 2;`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 2,
    });
    expect(await runAsync(String.raw`a = 6; a /= 2;`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 3,
    });
    expect(await runAsync(String.raw`a = 6; a %= 2;`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    expect(await runAsync(String.raw`a = 0; a += 1;`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`a = 0; a -= 1;`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: -1,
    });
    expect(await runAsync(String.raw`a = 1; a <<= 3;`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 8,
    });
    expect(await runAsync(String.raw`a = 8; a >>= 3;`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`a = 0xffffffff; a >>= 1;`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: -1,
    });
    expect(await runAsync(String.raw`a = 0xffffffff; a >>>= 1;`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0x7fffffff,
    });
    expect(await runAsync(String.raw`a = 1|4; a &= 4|8;`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 4,
    });
    expect(await runAsync(String.raw`a = 1; a |= 4;`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 5,
    });
    expect(await runAsync(String.raw`a = 1, a += 1, a += 2;`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 4,
    });
    expect(await runAsync(String.raw`a = 1; a++;`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`a = 1; a++; a`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 2,
    });
    expect(await runAsync(String.raw`a = 1; ++a;`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 2,
    });
    expect(await runAsync(String.raw`a = 1; ++a; a`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 2,
    });
    expect(await runAsync(String.raw`a = 1; a--;`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`a = 1; a--; a`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    expect(await runAsync(String.raw`a = 1; --a;`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    expect(await runAsync(String.raw`a = 1; --a; a`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    await expect(runAsync(String.raw`a++`)).rejects.toThrow();
    await expect(runAsync(String.raw`a--`)).rejects.toThrow();
    await expect(runAsync(String.raw`++a`)).rejects.toThrow();
    await expect(runAsync(String.raw`--a`)).rejects.toThrow();
    await expect(runAsync(String.raw`a+=1`)).rejects.toThrow();
    await expect(runAsync(String.raw`delete 1`)).rejects.toThrow();
    expect(await runAsync(String.raw`o = new Object; o.a = 1; delete o.a`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(await runAsync(String.raw`o = new Object; o.a = 1; delete o.a; o.a`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: undefined,
    });
    expect(await runAsync(String.raw`Object.prototype.a = 1; o = new Object; delete o.a`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(await runAsync(String.raw`Object.prototype.a = 1; o = new Object; delete o.a; o.a`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`delete abc`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(await runAsync(String.raw`var a = 1; delete a`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: false,
    });
    expect(await runAsync(String.raw`var a = 1; delete a; a`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`void (a = 1)`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: undefined,
    });
    expect(await runAsync(String.raw`void (a = 1); a`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`typeof 1`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "number",
    });
    expect(await runAsync(String.raw`typeof a`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "undefined",
    });
    await expect(runAsync(String.raw`typeof a.b`)).rejects.toThrow();
    expect(await runAsync(String.raw`typeof Object.a`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "undefined",
    });
    expect(await runAsync(String.raw`typeof 'a'`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "string",
    });
    expect(await runAsync(String.raw`typeof null`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "object",
    });
    expect(await runAsync(String.raw`typeof true`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "boolean",
    });
    expect(await runAsync(String.raw`typeof new Number()`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "object",
    });
    expect(await runAsync(String.raw`typeof Number`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "function",
    });
    expect(await runAsync(String.raw`typeof Object`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "function",
    });
    expect(await runAsync(String.raw`typeof Object.prototype`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "object",
    });
    expect(await runAsync(String.raw`typeof Object.prototype.constructor`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "function",
    });
    expect(await runAsync(String.raw`(a) = 1; a`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`(Object.a) = 1; Object.a`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
});
