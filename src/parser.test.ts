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
    expect(error(() => filterByValue(tokenize("'a\n'")))).toBeTruthy();
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
    expect(error(() => astToString(parse("def(1, 2, 3, )")))).toBeTruthy();
    expect(error(() => astToString(parse("new def(1, 2, 3, )")))).toBeTruthy();
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
    expect(astToString(parse(String.raw`a().hoge`))).toStrictEqual(["(expr (member-ident (call a) hoge))"]);
    expect(astToString(parse(String.raw`a()[1]`))).toStrictEqual(["(expr (member-expr (call a) 1))"]);
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
    expect(error(() => parse("break"))).toBeTruthy();
    expect(error(() => parse("continue"))).toBeTruthy();
    expect(error(() => parse("with(1){continue}"))).toBeTruthy();
    expect(error(() => parse("while(1){with(1){continue}}"))).toBeFalsy();
    expect(error(() => parse("while(1){with(1){if(0)continue;else continue;}}"))).toBeFalsy();
    expect(error(() => parse("for(;;){with(1){if(0)continue;else continue;}}"))).toBeFalsy();
    expect(error(() => parse("with(1){break}"))).toBeTruthy();
    expect(error(() => parse("while(1){with(1){break}}"))).toBeFalsy();
    expect(error(() => parse("while(1){with(1){if(0)break;else break;}}"))).toBeFalsy();
    expect(error(() => parse("for(;;){with(1){if(0)break;else break;}}"))).toBeFalsy();
    expect(error(() => parse("for(;;){for(;;){break;}break;}"))).toBeFalsy();
    expect(error(() => parse("for(;;){for(;;){continue;}continue;}"))).toBeFalsy();
    expect(omitPosition(parse(String.raw`while(1){break;continue;}`))).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "whileStatement",
                expression: {
                    type: "literalExpression",
                    value: 1,
                },
                statement: {
                    type: "block",
                    statementList: [
                        {
                            type: "breakStatement",
                        },
                        {
                            type: "continueStatement",
                        },
                    ],
                },
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
    expect(error(() => parse("return"))).toBeTruthy();
    expect(error(() => parse("function func(){return;}"))).toBeFalsy();
    expect(error(() => parse("function func(){with(1){for(;;)while(1)if(1)return;else return;}}"))).toBeFalsy();
    expect(
        omitPosition(
            parse(String.raw`function func(){return
1
            }`)
        )
    ).toStrictEqual({
        type: "program",
        sourceElements: [
            {
                type: "functionDeclaration",
                name: "func",
                parameters: [] as string[],
                block: {
                    type: "block",
                    statementList: [
                        {
                            type: "returnStatement",
                            expression: undefined,
                        },
                        {
                            type: "expressionStatement",
                            expression: { type: "literalExpression", value: 1 },
                        },
                    ],
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
    expect(omitPosition(parse(String.raw``))).toStrictEqual({
        type: "program",
        sourceElements: [] as SourceElement[],
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
    expect(await runAsync("4/2;")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 2,
    });
    expect(await runAsync("5%6;")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 5,
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
    expect(await runAsync("String(Object.undef)")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "undefined",
    });
    expect(await runAsync("String(null)")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "null",
    });
    expect(await runAsync("String(true)")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "true",
    });
    expect(await runAsync("String(false)")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "false",
    });
    expect(await runAsync("Number(Object.undef)")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: NaN,
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
    expect(await runAsync("new Boolean(1).valueOf()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(await runAsync("new Boolean().valueOf()")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: false,
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
    expect(await runAsync(String.raw`Function.prototype.constructor.prototype()`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: undefined,
    });
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
    expect(await runAsync(String.raw`1<=2`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1 <= 2,
    });
    expect(await runAsync(String.raw`1<=1`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1 <= 1,
    });
    expect(await runAsync(String.raw`2<=1`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 2 <= 1,
    });
    expect(await runAsync(String.raw`NaN<=1`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: false,
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
            o = new Object(); o.B = 1; for (var B in o) { B }
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "a",
    });
    expect(
        await runAsync(String.raw`
            o = new Object(); o.a = 1; for (var a in o) { break; }
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1, // V8, JSCore: undefined, Spidermonkey: 1, ???
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
    expect(await runAsync(String.raw`4^1`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 5,
    });
    expect(await runAsync(String.raw`1^1`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    expect(await runAsync(String.raw`1^true`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    expect(await runAsync(String.raw`true^true`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    expect(await runAsync(String.raw`true^false`)).toStrictEqual({
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
    expect(await runAsync(String.raw`1<<4`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 16,
    });
    expect(await runAsync(String.raw`16>>4`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`-1>>1`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: -1,
    });
    expect(await runAsync(String.raw`16>>>4`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`-1>>>1`)).toStrictEqual({
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
    expect(await runAsync(String.raw`a = 1|4|8; a ^= 4|16;`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 25,
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
    expect(
        await runAsync(String.raw`
        function hoge(){}
        hoge();
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: undefined,
    });
    expect(
        await runAsync(String.raw`
        function hoge(){return 1;}
        hoge();
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        function hoge(){return 1;}
        function hoge(){return 2;}
        hoge();
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 2,
    });
    expect(
        await runAsync(String.raw`
        a = 2
        function hoge(){var a = 1;}
        hoge();
        a
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 2,
    });
    expect(
        await runAsync(String.raw`
        function hoge(a){return a}
        hoge(1);
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        function hoge(a){return a}
        hoge();
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: undefined,
    });
    expect(
        await runAsync(String.raw`
        function hoge(a,a){return a}
        hoge(1);
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: undefined,
    });
    expect(
        await runAsync(String.raw`
        function hoge(a,a){return a}
        hoge(1, 2);
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 2,
    });
    expect(
        await runAsync(String.raw`
        function hoge(a,a){return a}
        hoge
    `)
    ).toMatchObject({
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
        function hoge(a,a){return a}
        hoge.length
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 2,
    });
    expect(
        await runAsync(String.raw`
        function hoge(a,a){return hoge.length}
        hoge()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 2,
    });
    expect(
        await runAsync(String.raw`
        function fib(n) {
            if (n == 0) {
                return 0;
            } else if (n == 1) {
                return 1;
            } else {
                return fib(n - 1) + fib(n - 2);
            }
        }
        fib(10);
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 55,
    });
    expect(
        await runAsync(String.raw`
        function A() {
            this.hoge = 1;
        }
        new A().hoge
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        function A() {
            this.hoge = 1;
        }
        function increment() {
            this.hoge++;
        }
        function AToString() {
            return "A: hoge=" + this.hoge;
        }
        A.prototype.increment = increment;
        A.prototype.toString = AToString;
        var a = new A();
        a.increment();
        a + ""
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "A: hoge=2",
    });
    expect(
        await runAsync(String.raw`
        function A() {
            this.hoge = 1;
        }
        function AToString() {
            return "A: hoge=" + this.hoge;
        }
        A.prototype.toString = AToString;
        var a = new A();
        String(a)
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "A: hoge=1",
    });
    expect(
        await runAsync(String.raw`
        function A() {
            this.hoge = 1;
        }
        function AToValueOf() {
            return this.hoge;
        }
        A.prototype.valueOf = AToValueOf;
        var a = new A();
        a.hoge = 2;
        a * 10
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 20,
    });
    expect(
        await runAsync(String.raw`
        function A() {
            this.hoge = 1;
        }
        function AToValueOf() {
            return this.hoge;
        }
        A.prototype.valueOf = AToValueOf;
        var a = new A();
        a + ""
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "1",
    });
    expect(
        await runAsync(String.raw`
        function hoge() { return 1; }
        hoge.prototype.constructor();
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        function hoge() { return 1; }
        hoge.tos = Object.prototype.toString;
        hoge.tos()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "[object Function]",
    });
    expect(
        await runAsync(String.raw`
        function hoge() { while (hoge) return 1; }
        hoge();
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        function hoge() { while (hoge) return 1; return 0; }
        hoge();
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        function hoge() { return 0; while (hoge) return 1; }
        hoge();
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    expect(
        await runAsync(String.raw`
        function hoge() { for (;;) return 1; }
        hoge();
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        function hoge() { return; }
        hoge();
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: undefined,
    });
    expect(
        await runAsync(String.raw`
        function hoge() { var o = new Object; o.a = 2; for (var a in o) return 1; }
        hoge();
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        Boolean(true)["a"]
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: undefined,
    });
    expect(
        await runAsync(String.raw`
        Function("return 1")()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        Function.length
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        Function()()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: undefined,
    });
    expect(
        await runAsync(String.raw`
        Function("")()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: undefined,
    });
    expect(
        await runAsync(String.raw`
        Function("a", "return a")(1)
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        Function("a", "b", "return a+b")(1, 2)
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 3,
    });
    expect(
        await runAsync(String.raw`
        Function("a,b", "c", "return a + b + c")(1, 2, 3)
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 6,
    });
    expect(
        await runAsync(String.raw`
        Function("a,\nb  ", "c", "return a + b + c")(1, 2, 3)
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 6,
    });
    expect(
        await runAsync(String.raw`
        Function("return 1")()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        new Function("return 1")()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        new Function("return 1").toString()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "Function", // FIXME
    });
    expect(
        await runAsync(String.raw`
        Function.prototype.constructor("return 1")()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        var b = 2;
        function a() {
            var b = 1;
            return new Function("return b");
        }
        a()();
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 2,
    });
    expect(
        await runAsync(String.raw`
        s = new Object;
        s.a = 1;
        with (s) {
            new Function("a = 123")();
        }
        s.a
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        function a() { this.func = new Function("return this"); }
        var i = new a();
        i.func() == i;
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(
        await runAsync(String.raw`
        var o = new Object;
        o.abc = 123;
        function func() { return this.abc; }
        o.func = func;
        with (o) {
            func();
        }
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 123,
    });
    expect(
        await runAsync(String.raw`
        var g = 123
        function b() { return this.g; }
        function a() { var f = b; var g = 'unused'; return f(); }
        a()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 123,
    });
    expect(
        await runAsync(String.raw`
        function a() { return arguments.callee == a; }
        a()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(
        await runAsync(String.raw`
        function a() { return arguments.length; }
        a()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    expect(
        await runAsync(String.raw`
        function a() { return arguments.length; }
        a(1)
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        function a(a1) { arguments[0] = 123; return a1; }
        a(1)
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 123,
    });
    expect(
        await runAsync(String.raw`
        function a(a1) { a1 = 123; return arguments[0]; }
        a(1)
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 123,
    });
    expect(
        await runAsync(String.raw`
        function a(a1) { var o = new Object; o[0] = 1; for (var i in o) return i;  return "?" }
        a(1)
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "0",
    });
    expect(
        await runAsync(String.raw`
        function a(a1) { for (var i in arguments) return i; return "DontEnum"; }
        a(1)
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "DontEnum",
    });
    expect(
        await runAsync(String.raw`
        function a(a1, a1) { return arguments[0]; }
        a(1, 2)
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        function a() { var v = 1; return delete v; }
        a()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: false,
    });
    expect(
        await runAsync(String.raw`
        function a() { var v = 1; delete v; return v; }
        a()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        function a() { var o = new Object; o.p = 1; with (o) return delete p; }
        a()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(
        await runAsync(String.raw`
        function a() { var p = 0; var o = new Object; o.p = 1; with (o) { delete p; return p; } }
        a()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    await expect(
        runAsync(String.raw`
        function a() { var o = new Object; o.p = 1; with (o) { delete p; return p; } }
        a()
    `)
    ).rejects.toThrow();
    expect(
        await runAsync(String.raw`
        function a(a1) { return delete a1; }
        a(1)
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: false,
    });
    expect(
        await runAsync(String.raw`
        function a(a1) { delete a1; return a1; }
        a(1)
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        function a(a1) { arguments.length = 0; return arguments.length; }
        a(1)
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    expect(
        await runAsync(String.raw`
        function a(a1) { arguments.length = 0; return arguments[0]; }
        a(1)
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        function a(a1) { return delete arguments.length; }
        a(1)
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(
        await runAsync(String.raw`
        function a(a1) { return delete arguments[0]; }
        a(1)
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(
        await runAsync(String.raw`
        function a(a1) { delete arguments[0]; return arguments[0]; }
        a(1)
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: undefined,
    });
    expect(
        await runAsync(String.raw`
        function a(a1) { delete arguments[0]; return a1; }
        a(1)
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        function a() { return arguments[0]; }
        a(1, 2)
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        function a() { var r = ""; for (var i in arguments) r += arguments[i]; return r; }
        a(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "", // DontEnum
    });
    expect(
        await runAsync(String.raw`
        function a(arguments) { return arguments; }
        a(1)
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        eval("1")
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        eval("var c = 1"); c;
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        eval("c = 1"); c;
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        a = new Object(); a.b = 1;with (a){eval("b");}
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        eval('a = new Object(); a.b = 1;with (a){eval("b");}')
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        a = 2; function f() { eval("var a = 1"); } a;
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 2,
    });
    expect(
        await runAsync(String.raw`
        function f() { eval("function g() {return 1;}"); return g(); } f();
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        function f() { eval("function g() {return 1;}"); return g(); } f(); typeof g;
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "undefined",
    });
    expect(
        await runAsync(String.raw`
        var d = 2; function f() { var d = 1; eval("function g() {return d;}"); return g(); } f();
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        // ES2: The scope chain is initialised to contain the activation object followed by the global object.
        value: 2,
    });
    expect(
        await runAsync(String.raw`
        eval(1)
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        o = new Object(); eval(o) == o;
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(
        await runAsync(String.raw`
        eval("")
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: undefined,
    });
    expect(
        await runAsync(String.raw`
        Array.prototype[4] = 1;
        Array.prototype.length;
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 5,
    });
    expect(await runAsync(String.raw`new Array(1, 2, 3)[1]`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 2,
    });
    expect(
        await runAsync(String.raw`
        var a = new Array(10);
        count = 0;
        for (var i in a)count++;
        count;
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    expect(
        await runAsync(String.raw`
        var a = new Array(10);
        a[1] = 1;
        count = 0;
        for (var i in a)count++;
        count;
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        var a = new Array(1, 2, 3);
        count = 0;
        for (var i in a)count++;
        count;
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 3,
    });
    expect(await runAsync(String.raw`new Array("100")[0]`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "100",
    });
    expect(await runAsync(String.raw`new Array(new Number(100)).length`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    await expect(runAsync(String.raw`new Array(NaN)`)).rejects.toThrow();
    expect(await runAsync(String.raw`new Array(100).length`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 100,
    });
    expect(
        await runAsync(String.raw`
        Array.prototype[0] = 1;
        var a = new Array;
        a[0]
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        Array.prototype[0] = 1;
        var a = new Array;
        a[1] = 2;
        var r = "";
        for (var b in a) {
            r += b;
        }
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "10",
    });
    expect(
        await runAsync(String.raw`
        var a = new Array(0, 1, 2);
        a.length = 0;
        a[0] + "," + a[1] + "," + a[2]
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "undefined,undefined,undefined",
    });
    expect(
        await runAsync(String.raw`
        var a = new Array(0, 1, 2);
        a.length = 1;
        a[0] + "," + a[1] + "," + a[2]
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "0,undefined,undefined",
    });
    expect(
        await runAsync(String.raw`
        var a = new Array(0, 1, 2);
        a.length = 2;
        a[0] + "," + a[1] + "," + a[2]
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "0,1,undefined",
    });
    expect(
        await runAsync(String.raw`
        var a = new Array(0, 1, 2);
        a.length = 3;
        a[0] + "," + a[1] + "," + a[2]
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "0,1,2",
    });
    expect(
        await runAsync(String.raw`
        var a = new Array(0, 1, 2);
        a.length = 4;
        a[0] + "," + a[1] + "," + a[2]
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "0,1,2",
    });
    expect(
        await runAsync(String.raw`
        var a = new Array(0, 1, 2);
        a.length = 1.1;
        a[0] + "," + a[1] + "," + a[2]
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "0,undefined,undefined", // newer ES does not allow non-integer length
    });
    expect(
        await runAsync(String.raw`
        var a = new Array(0, 1, 2);
        delete a[2];
        a.length
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 3,
    });
    expect(
        await runAsync(String.raw`
        var a = new Array(0, 1, 2);
        a.join()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "0,1,2",
    });
    expect(
        await runAsync(String.raw`
        var a = new Array(0, 1, 2);
        a.join("#")
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "0#1#2",
    });
    expect(
        await runAsync(String.raw`
        var a = new Array(0, 1, 2);
        a.join(null)
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "0null1null2",
    });
    expect(
        await runAsync(String.raw`
        var a = new Array(0, 1, 2);
        a.join(Object.undef)
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "0,1,2",
    });
    expect(
        await runAsync(String.raw`
        var a = new Array(0, 1, 2);
        a[0.1] = 999;
        a.join()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "0,1,2",
    });
    expect(
        await runAsync(String.raw`
        var a = new Array(0, 1, 2);
        a[-0] = 999;
        a.join()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "999,1,2",
    });
    expect(
        await runAsync(String.raw`
        var a = new Array(1);
        a[-0] = 999;
        a.join()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "999",
    });
    expect(
        await runAsync(String.raw`
        var a = new Array(1);
        a[-0] = 999;
        a.join()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "999",
    });
    expect(
        await runAsync(String.raw`
        a = new Array(1);
        a[0xffffffff] = 1;
        a.length`)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        a = new Array(1);
        a[0xfffffffe] = 1;
        a.length`)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0xffffffff,
    });
    expect(
        await runAsync(String.raw`
        a = new Array(1);
        a.length = 0xffffffff;
        a.length`)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0xffffffff,
    });
    await expect(
        runAsync(String.raw`
        a = new Array(1);
        a.length = 0x100000000;
        a.length`)
    ).rejects.toThrow();
    expect(
        await runAsync(String.raw`
        a = new Array(1);
        a[0x100000000] = 1;
        a.length = 0;
        a[0x100000000]`)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        v = new Object;
        function valueOf() { return 1; }
        v.valueOf = valueOf;
        a = new Array(0);
        a.length = v;
        a.length`)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        a = new Array(1);
        a[0] = 1;
        a[0xfffffffe] = 1;
        a[0xfffffffc] = 1;
        a.length = 1;
        a[0]`)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        var a = new Array(0, 1, 2);
        a.toString()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "0,1,2",
    });
    expect(
        await runAsync(String.raw`
        var a = Array.prototype.constructor(0, 1, 2);
        a.toString()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "0,1,2",
    });
    expect(
        await runAsync(String.raw`
        function f(a,b){}
        f.join = Array.prototype.join;
        f.join()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: ",",
    });
    expect(
        await runAsync(String.raw`
        var join = Array.prototype.join;
        join()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "", //
    });
    await expect(
        runAsync(String.raw`
        function f() {
            var join = Array.prototype.join;
            join();
        }
        f();
    `)
    ).rejects.toThrow();
    expect(
        await runAsync(String.raw`
        var a = new Array(1, 2, 3);
        a.reverse().join()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "3,2,1",
    });
    expect(
        await runAsync(String.raw`
        var a = new Array(1, 2);
        a.reverse().join()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "2,1",
    });
    expect(
        await runAsync(String.raw`
        var a = new Array(1);
        a[0] = 1;
        a.reverse().join()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "1",
    });
    expect(
        await runAsync(String.raw`
        var a = new Array();
        a.reverse().join()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "",
    });
    expect(
        await runAsync(String.raw`
        var a = new Array(1, 2);
        a.reverse() == a
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(
        await runAsync(String.raw`
        var a = new Object;
        a[0] = 1;
        a.length = 2;
        a.reverse = Array.prototype.reverse;
        a.reverse();
        a[1]
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        var a = new Object;
        a[0] = 1;
        a.length = 2;
        a.reverse = Array.prototype.reverse;
        a.reverse();
        var count = 0;
        for (var i in a) if (i != "length" && i != "reverse") count++;
        count
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        function a(a, a) {}
        Function.prototype[1] = 1;
        Function.prototype.reverse = Array.prototype.reverse;
        a.reverse();
        a[0];
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(
        await runAsync(String.raw`
        new Array(4, 3, 2).sort().join()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "2,3,4",
    });
    expect(
        await runAsync(String.raw`
        new Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11).sort().join()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "0,1,10,11,2,3,4,5,6,7,8,9",
    });
    expect(
        await runAsync(String.raw`
        function compare(a, b) { return a - b; }
        new Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11).sort(compare).join()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "0,1,2,3,4,5,6,7,8,9,10,11",
    });
    expect(
        await runAsync(String.raw`
        function compare(a, b) { return b - a; }
        new Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11).sort(compare).join()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "11,10,9,8,7,6,5,4,3,2,1,0",
    });
    expect(
        await runAsync(String.raw`
        var o = new Object;
        o[1] = 0;
        o.length = 2;
        o.sort = Array.prototype.sort;
        o.sort();
        o[0]
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    expect(
        await runAsync(String.raw`
        var o = new Object;
        o[1] = 0;
        o.length = 2;
        o.sort = Array.prototype.sort;
        o.sort();
        var success = true;
        for (var i in o) { if (i == "1") success = false; }
        success;
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(
        await runAsync(String.raw`
        var a = new Array;
        a.length = 100;
        a[100] = 0;
        a[0] = 1;
        function compare(a, b) { return a - b; }
        a.sort(compare);
        a.length = 2;
        a.join();
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "0,1",
    });
    expect(
        await runAsync(String.raw`
        var a = new Array;
        a.length = 100;
        a[100] = 0;
        a[0] = 1;
        function compare(a, b) { return a - b; }
        a.sort(compare);
        count = 0;
        for (var i in a) count++;
        count;
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 2,
    });
    expect(
        await runAsync(String.raw`
        var a = new Array;
        a.length = 100;
        a[0] = 1;
        a[1] = 2;
        a[100] = 100;
        a[99] = 99;
        function compare(a, b) { return a - b; }
        a.sort(compare);
        a.length = 4;
        a.join()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "1,2,99,100",
    });
    expect(
        await runAsync(String.raw`
        var a = new Array;
        a.length = 100;
        a[0] = 16;
        a[4] = 4;
        a[8] = 64;
        a[16] = 32;
        a[32] = 0;
        a[64] = 8;
        function compare(a, b) { return b - a; }
        a.sort(compare);
        a.length = 6;
        a.join()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "64,32,16,8,4,0",
    });
    expect(
        await runAsync(String.raw`
        var a = new Array;
        a.toString = Object.prototype.toString;
        a.toString()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "[object Array]",
    });
    expect(
        await runAsync(String.raw`
        Array.prototype[1] = 1;
        a = new Array(2);
        a[0] = 2;
        a.sort().join()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "1,2",
    });
    expect(
        await runAsync(String.raw`
        a = new Array(1, Object.undefined, 3, Object.undefined, Object.undefined, 2);
        a.length *= 2;
        a.sort();
        count = 0;
        for (var i in a)count++;
        count
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 6,
    });
    expect(
        await runAsync(String.raw`
        Array.prototype[1] = 1;
        a = new Array(2);
        a[0] = 2;
        Array.prototype.join()
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: ",1",
    });
    expect(
        await runAsync(String.raw`
        "hoge".charAt(1)
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "o",
    });
    expect(
        await runAsync(String.raw`
        a = new Object; a.charAt = String.prototype.charAt;
        a.charAt(1)
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "o",
    });
    expect(await runAsync(String.raw`"01234".charAt()`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "0",
    });
    expect(await runAsync(String.raw`"01234".charAt(Object.undef)`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "0",
    });
    expect(await runAsync(String.raw`"01234".charAt(null)`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "0",
    });
    expect(await runAsync(String.raw`"01234".charAt(NaN)`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "0",
    });
    expect(await runAsync(String.raw`String.prototype.charAt.length`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`"01234".charCodeAt(1)`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "1".charCodeAt(0),
    });
    expect(await runAsync(String.raw`"01234".charCodeAt(-1)`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: NaN,
    });
    expect(await runAsync(String.raw`"01234".charCodeAt(5)`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: NaN,
    });
    expect(await runAsync(String.raw`"01234".charCodeAt(Object.undef)`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "0".charCodeAt(0),
    });
    expect(await runAsync(String.raw`"01234".charCodeAt(null)`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "0".charCodeAt(0),
    });
    expect(await runAsync(String.raw`"01234".charCodeAt()`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "0".charCodeAt(0),
    });
    expect(await runAsync(String.raw`String.prototype.charCodeAt.length`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`"index".indexOf("dex")`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 2,
    });
    expect(await runAsync(String.raw`"index".indexOf("dex", 1)`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 2,
    });
    expect(await runAsync(String.raw`"index".indexOf("dex", 3)`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: -1,
    });
    expect(await runAsync(String.raw`"index".indexOf("dex", NaN)`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 2,
    });
    expect(await runAsync(String.raw`"indexundefined".indexOf()`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 5,
    });
    expect(await runAsync(String.raw`"indexnull".indexOf(null)`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 5,
    });
    expect(await runAsync(String.raw`String.prototype.indexOf.length`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`"indexnullnull".lastIndexOf(null)`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 9,
    });
    expect(await runAsync(String.raw`"indexnullnull".lastIndexOf(null, 8)`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 5,
    });
    expect(await runAsync(String.raw`String.prototype.lastIndexOf.length`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`"ABC".split().join()`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "ABC",
    });
    expect(await runAsync(String.raw`"ABC".split("").join()`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "A,B,C",
    });
    expect(await runAsync(String.raw`"ABC".split("B").join()`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "A,C",
    });
    expect(await runAsync(String.raw`String.prototype.split.length`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`"ABC".substring()`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "ABC",
    });
    expect(await runAsync(String.raw`"ABC".substring(10, 0)`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "ABC",
    });
    expect(await runAsync(String.raw`"ABC".substring(0, 2)`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "AB",
    });
    expect(await runAsync(String.raw`"ABC".substring(NaN, 1)`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "A",
    });
    expect(await runAsync(String.raw`"ABC".substring(1, NaN)`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "A",
    });
    expect(await runAsync(String.raw`String.prototype.substring.length`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 2,
    });
    expect(await runAsync(String.raw`"ABC".toLowerCase()`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "abc", // Unicode 2.0
    });
    expect(await runAsync(String.raw`"abc".toUpperCase()`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "ABC", // Unicode 2.0
    });
    expect(await runAsync(String.raw`String.prototype.toLowerCase.length`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`delete Math.E`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: false,
    });
    expect(await runAsync(String.raw`delete Math.sin`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(await runAsync(String.raw`Math.max(0, 1)`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await runAsync(String.raw`parseInt("10")`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 10,
    });
    expect(await runAsync(String.raw`parseInt("10", 16)`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 16,
    });
    expect(await runAsync(String.raw`isFinite(0)`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(await runAsync(String.raw`isFinite(Infinity)`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: false,
    });
    expect(await runAsync(String.raw`isNaN(0)`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: false,
    });
    expect(await runAsync(String.raw`isNaN(NaN)`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(await runAsync(String.raw`typeof Date()`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "string",
    });
    expect(await runAsync(String.raw`typeof new Date()`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "object",
    });
    expect(await runAsync(String.raw`typeof new Date().valueOf()`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "number",
    });
    expect(await runAsync(String.raw`Date.parse()`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: NaN,
    });
    expect(await runAsync(String.raw`typeof Date.parse("2025")`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "number",
    });
    expect(
        await runAsync(String.raw`
        Date.prototype.toS = Object.prototype.toString
        Date.prototype.toS()
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "[object Object]",
    });
    expect(
        await runAsync(String.raw`
        var d = new Date();
        Math.floor(d.valueOf() / 1000) == Math.floor(Date.parse(d) / 1000)
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(
        await runAsync(String.raw`new Date(new Date().setUTCFullYear(1970, 0, 1)).setUTCHours(0, 0, 0, 0);`)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    expect(
        await runAsync(String.raw`
        var d = new Date();
        d.setUTCFullYear(1970, 0, 1);
        d.setUTCHours(0, 0, 0, 0);
        `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 0,
    });
    expect(await runAsync(String.raw`typeof(new Date() + new Date())`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "string",
    });
    expect(await runAsync(String.raw`typeof(new Date() - new Date())`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "number",
    });
    expect(await runAsync(String.raw`typeof(-new Date())`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "number",
    });
    expect(await runAsync(String.raw`typeof(+new Date())`)).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "number",
    });
    expect(
        await runAsync(String.raw`
        var d = new Date();
        var x = d.valueOf();
        x == d`)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: false,
    });
    expect(
        await runAsync(String.raw`
        var d = new Date();
        var x = d.toString();
        x == d`)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(
        await runAsync(String.raw`
        var d = new Date();
        var y = d.valueOf();
        d == y`)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: false,
    });
    expect(
        await runAsync(String.raw`
        var d = new Date();
        var y = d.toString();
        d == y`)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: true,
    });
    expect(
        await runAsync(String.raw`
        sleep(1);
    `)
    ).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "slept",
    });
});
