import { test, expect } from "vitest";
import { ParameterDeclarations, parseSpecification, Specification } from "./parser";

test("interface", () => {
    expect(
        parseSpecification(String.raw`
interface test {
        attribute unsigned short test;
        readonly attribute unsigned long long test2, test3;
        attribute DOMString test4;
};
        `)
    ).toMatchObject({
        type: "specification",
        definitions: [
            {
                type: "interface",
                attribute: undefined,
                inheritanceInterfaces: [] as string[],
                name: "test",
                body: [
                    {
                        type: "attributeDeclaration",
                        isReadonly: false,
                        paramTypeSpec: "unsigned short",
                        declarators: [
                            {
                                type: "identifier",
                                value: "test",
                            },
                        ],
                    },
                    {
                        type: "attributeDeclaration",
                        isReadonly: true,
                        paramTypeSpec: "unsigned long long",
                        declarators: [
                            {
                                type: "identifier",
                                value: "test2",
                            },
                            {
                                type: "identifier",
                                value: "test3",
                            },
                        ],
                    },
                    {
                        type: "attributeDeclaration",
                        isReadonly: false,
                        paramTypeSpec: {
                            type: "scopedName",
                            names: ["DOMString"],
                        },
                        declarators: [
                            {
                                type: "identifier",
                                value: "test4",
                            },
                        ],
                    },
                ],
            },
        ],
    } as Specification);
});

test("opearation", () => {
    expect(
        parseSpecification(String.raw`
interface test {
        void test();
        oneway DOMString test2(inout unsigned long long param);
};
        `)
    ).toMatchObject({
        type: "specification",
        definitions: [
            {
                type: "interface",
                attribute: undefined,
                inheritanceInterfaces: [] as string[],
                name: "test",
                body: [
                    {
                        type: "operationDeclaration",
                        attribute: undefined,
                        typeSpec: "void",
                        name: "test",
                        parameters: [] as ParameterDeclarations,
                    },
                    {
                        type: "operationDeclaration",
                        attribute: "oneway",
                        typeSpec: {
                            type: "scopedName",
                            names: ["DOMString"],
                        },
                        name: "test2",
                        parameters: [
                            {
                                type: "parameterDeclaration",
                                attribute: "inout",
                                typeSpec: "unsigned long long",
                                declarator: {
                                    type: "identifier",
                                    value: "param",
                                },
                            },
                        ],
                    },
                ],
            },
        ],
    } as Specification);
});
