import { expect, test } from "vitest";
import { tokenize, Token, parse, SourceElement, Expression, Statement, Program } from ".";

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

function astToString(p: Program): string[] {
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
        case "whileStatement":
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
});
