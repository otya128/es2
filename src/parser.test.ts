import { expect, test } from "vitest";
import { tokenize, Token, parse, SourceElement, Expression, Statement, Program, Interpreter } from ".";

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
    expect(filterByValue(tokenize("0X123"))).toStrictEqual([0x123]);
    expect(filterByValue(tokenize("123"))).toStrictEqual([123]);
    expect(filterByValue(tokenize("1"))).toStrictEqual([1]);
    expect(filterByValue(tokenize("123.1"))).toStrictEqual([123.1]);
    expect(filterByValue(tokenize("123.e1"))).toStrictEqual([123e1]);
    expect(filterByValue(tokenize(".123e1"))).toStrictEqual([0.123e1]);
    expect(error(() => filterByValue(tokenize("@")))).toBeTruthy();
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
        case "unaryMiusOperator":
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
    expect(await new Interpreter().runAsync("1*2+3*4;")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1*2+3*4,
    });
    expect(await new Interpreter().runAsync("1*(2+3)*4;")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1*(2+3)*4,
    });
    expect(await new Interpreter().runAsync("1+'2';")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "12",
    });
    expect(await new Interpreter().runAsync("'1'+'2';")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "12",
    });
    expect(await new Interpreter().runAsync("'1'+2;")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: "12",
    });
    expect(await new Interpreter().runAsync("'10'*2;")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 20,
    });
    expect(await new Interpreter().runAsync("NaN;")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: NaN,
    });
    expect(await new Interpreter().runAsync("Object.length;")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await new Interpreter().runAsync("this.Object.length;")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await new Interpreter().runAsync("Object['length'];")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
    expect(await new Interpreter().runAsync("this.Object['length'];")).toStrictEqual({
        type: "normalCompletion",
        hasValue: true,
        value: 1,
    });
});
