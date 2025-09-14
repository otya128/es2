import { test, expect } from "vitest";
import { Token, tokenize } from "./lexer";

test("lexer", () => {
    expect([
        ...tokenize(String.raw`interface a {}
// comment
/*
**/
        `),
    ]).toMatchObject([
        {
            type: "keyword",
            value: "interface",
        },
        {
            type: "identifier",
            value: "a",
        },
        {
            type: "punctuator",
            value: "{",
        },
        {
            type: "punctuator",
            value: "}",
        },
    ] as Token[]);
});
