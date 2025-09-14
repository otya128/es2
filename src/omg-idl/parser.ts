import { Identifier, Position, SourceInfo, Token, Tokenizer } from "./lexer.js";

function tokenToString(token: Token) {
    return `${token.type}:${token.value} (${token.start.sourceInfo?.name ?? "unknown"}:${token.start.line}:${token.start.column})`;
}

// (1)
export type Specification = {
    type: "specification";
    definitions: Definition[];
    start: Position;
    end: Position;
};
// (2)
export type Definition = Interface;
// (7)
export type Interface = {
    type: "interface";
    attribute: undefined | "abstract" | "local";
    name: string;
    inheritanceInterfaces: string[];
    body: InterfaceBody;
    start: Position;
    end: Position;
};
// (8)
export type InterfaceBody = Export[];
// (9)
export type Export = AttributeDeclaration | OperationDeclaration;

// (12)
export type ScopedName = {
    type: "scopedName";
    names: string[];
};
// (44)
export type TypeSpec = SimpleTypeSpec | ConstrTypeSpec;
// (45)
export type SimpleTypeSpec = BaseTypeSpec | TemplateTypeSpec | ScopedName;
// (46)
export type BaseTypeSpec =
    | FloatingPointType
    | IntegerType
    | CharType
    | WideCharType
    | BooleanType
    | OctetType
    | AnyType
    | ObjectType
    | ValueBaseType;
// (47)
export type TemplateTypeSpec = SequenceType | StringType | WideStringType | FixedPointType;
// (48)
export type ConstrTypeSpec = StructType | UnionType | EnumType;
// (49)
export type Declarators = Declarator[];
// (50)
export type Declarator = SimpleDeclarator | ComplexDeclarator;
// (51)
export type SimpleDeclarator = Identifier;
// (52)
export type ComplexDeclarator = ArrayDeclarator;
// (53)
export type FloatingPointType = "float" | "double" | "long double";
// (54)
export type IntegerType = SignedInt | UnsignedInt;
// (55)
export type SignedInt = SignedShortInt | SignedLongInt | SignedLongLongInt;
// (56)
export type SignedShortInt = "short";
// (57)
export type SignedLongInt = "long";
// (58)
export type SignedLongLongInt = "long long";
// (59)
export type UnsignedInt = UnsignedShortInt | UnsignedLongInt | UnsignedLongLongInt;
// (60)
export type UnsignedShortInt = "unsigned short";
// (61)
export type UnsignedLongInt = "unsigned long";
// (62)
export type UnsignedLongLongInt = "unsigned long long";
// (63)
export type CharType = "char";
// (64)
export type WideCharType = "wchar";
// (65)
export type BooleanType = "boolean";
// (66)
export type OctetType = "octet";
// (67)
export type AnyType = "any";
// (68)
export type ObjectType = "Object";
// (69)
export type StructType = {
    type: "structType";
    name: string;
    memberList: MemberList;
    start: Position;
    end: Position;
};
// (70)
export type MemberList = Member[];
// (71)
export type Member = {
    type: "member";
    typeSpec: TypeSpec;
    declarators: Declarators;
    start: Position;
    end: Position;
};
// (72)
export type UnionType = {
    type: "union";
    name: string;
    switchTypeSpec: SwitchTypeSpec;
    switchBody: SwitchBody;
    start: Position;
    end: Position;
};
// (73)
export type SwitchTypeSpec = IntegerType | CharType | BooleanType | EnumType | ScopedName;
// (74)
export type SwitchBody = Case[];
// (75)
export type Case = {
    type: "case";
    caseLabels: CaseLabel[];
    elementSpec: ElementSpec;
    start: Position;
    end: Position;
};
// (76)
export type CaseLabel = {
    type: "caseLabel";
    // constExp?: ConstExp;
    start: Position;
    end: Position;
};
// (77)
export type ElementSpec = {
    type: "elementSpec";
    typeSpec: TypeSpec;
    declarator: Declarator;
    start: Position;
    end: Position;
};
// (78)
export type EnumType = {
    type: "enumType";
    name: string;
    enumerators: Enumerator[];
    start: Position;
    end: Position;
};
// (79)
export type Enumerator = Identifier;
// (80)
export type SequenceType = {
    type: "sequenceType";
    typeSpec: SimpleTypeSpec;
    size: number | undefined;
    start: Position;
    end: Position;
};
// (81)
export type StringType = {
    type: "stringType";
    size: number | undefined;
    start: Position;
    end: Position;
};
// (82)
export type WideStringType = {
    type: "wideStringType";
    size: number | undefined;
    start: Position;
    end: Position;
};
// (83)
export type ArrayDeclarator = {
    type: "arrayDeclarator";
    name: string;
    fixedArraySize: FixedArraySize;
};
// (84)
export type FixedArraySize = number;
// (85)
export type AttributeDeclaration = {
    type: "attributeDeclaration";
    isReadonly: boolean;
    paramTypeSpec: ParameterTypeSpec;
    declarators: SimpleDeclarator[];
    start: Position;
    end: Position;
};
// (87)
export type OperationDeclaration = {
    type: "operationDeclaration";
    attribute: OperationAttribute | undefined;
    typeSpec: OperationTypeSpec;
    name: string;
    parameters: ParameterDeclarations;
    // raises_expr
    // context_expr
    start: Position;
    end: Position;
};
// (88)
export type OperationAttribute = "oneway";
// (89)
export type OperationTypeSpec = ParameterTypeSpec | "void";
// (90)
export type ParameterDeclarations = ParameterDeclaration[];
// (91)
export type ParameterDeclaration = {
    type: "parameterDeclaration";
    attribute: ParameterAttribute;
    typeSpec: ParameterTypeSpec;
    declarator: SimpleDeclarator;
};
// (92)
export type ParameterAttribute = "in" | "out" | "inout";
// (93)
// raises_expr
// (94)
// context_expr
// (95)
export type ParameterTypeSpec = BaseTypeSpec | StringType | WideStringType | ScopedName;
// (96)
export type FixedPointType = {
    type: "fixedPointType";
    exponentSize: number;
    fractionSize: number;
    start: Position;
    end: Position;
};
// (98)
export type ValueBaseType = "ValueBase";

export function parseSpecification(source: string, sourceInfo?: SourceInfo): Specification {
    const tokenizer = new Tokenizer(source, sourceInfo);
    const begin = tokenizer.current;
    const definitions: Definition[] = [];
    while (tokenizer.current.type !== "end") {
        definitions.push(parseDefinition(tokenizer));
    }
    return {
        type: "specification",
        definitions,
        start: begin.start,
        end: tokenizer.current.end,
    };
}

export function parseDefinition(tokenizer: Tokenizer): Definition {
    const token = tokenizer.current;
    if (token.type === "keyword") {
        if (token.value === "interface" || token.value === "abstract" || token.value === "local") {
            return parseInterfaceDecl(tokenizer);
        }
    }
    throw new Error(`defnition: unexpected ${tokenToString(token)}`);
}

function parseInterfaceDecl(tokenizer: Tokenizer): Interface {
    const token = tokenizer.current;
    const attribute =
        token.type === "keyword" && (token.value === "abstract" || token.value === "local") ? token.value : undefined;
    if (attribute != null) {
        tokenizer.next();
    }
    const i = tokenizer.current;
    if (i.type !== "keyword" || i.value !== "interface") {
        throw new Error(`unexpected ${tokenToString(i)}`);
    }
    const name = tokenizer.next();
    if (name.type !== "identifier") {
        throw new Error(`unexpected ${tokenToString(name)}`);
    }
    const beginOrInheritance = tokenizer.next();
    if (beginOrInheritance.type === "punctuator" && beginOrInheritance.value === ";") {
        throw new Error(`not implemented ${tokenToString(beginOrInheritance)}`);
    }
    const inheritanceInterfaces: string[] = [];
    if (beginOrInheritance.type === "punctuator" && beginOrInheritance.value === ":") {
        const name = tokenizer.next();
        if (name.type !== "identifier") {
            throw new Error(`unexpected ${tokenToString(name)}`);
        }
        inheritanceInterfaces.push(name.value);
        while (true) {
            const t = tokenizer.next();
            if (t.type !== "punctuator" || t.value !== ",") {
                break;
            }
            const name = tokenizer.next();
            if (name.type !== "identifier") {
                throw new Error(`unexpected ${tokenToString(name)}`);
            }
            inheritanceInterfaces.push(name.value);
        }
    }
    const begin = tokenizer.current;
    if (begin.type !== "punctuator" || begin.value !== "{") {
        throw new Error(`unexpected ${tokenToString(begin)}`);
    }
    tokenizer.next();
    const body = parseInterfaceBody(tokenizer);
    const end = tokenizer.current;
    if (end.type !== "punctuator" || end.value !== "}") {
        throw new Error(`unexpected ${tokenToString(end)}`);
    }
    tokenizer.next();
    const semi = tokenizer.current;
    if (semi.type !== "punctuator" || semi.value !== ";") {
        throw new Error(`interface_decl: expected ; but ${tokenToString(semi)}`);
    }
    tokenizer.next();
    return {
        type: "interface",
        attribute,
        inheritanceInterfaces,
        name: name.value,
        body,
        start: token.start,
        end: tokenizer.prevPosition,
    };
}

function parseInterfaceBody(tokenizer: Tokenizer): Export[] {
    const exports: Export[] = [];
    while (true) {
        const token = tokenizer.current;
        if (token.type === "punctuator" && token.value === "}") {
            break;
        }
        exports.push(parseExport(tokenizer));
    }
    return exports;
}

function parseExport(tokenizer: Tokenizer): Export {
    const begin = tokenizer.current;
    if (begin.type === "keyword") {
        switch (begin.value) {
            case "readonly":
            case "attribute":
                return parseAttributeDeclaration(tokenizer);
            case "void":
            case "float":
            case "double":
            case "short":
            case "char":
            case "wchar":
            case "boolean":
            case "octet":
            case "any":
            case "Object":
            case "ValueBase":
            case "unsigned":
            case "long":
            case "string":
            case "wstring":
            case "oneway":
                return parseOperationDeclaration(tokenizer);
        }
    }
    if (begin.type === "identifier") {
        return parseOperationDeclaration(tokenizer);
    }
    throw new Error(`SyntaxError: export: unexpected ${tokenToString(begin)}`);
}

function parseAttributeDeclaration(tokenizer: Tokenizer): AttributeDeclaration {
    const begin = tokenizer.current;
    let isReadonly = false;
    if (begin.type === "keyword" && begin.value === "readonly") {
        isReadonly = true;
        tokenizer.next();
    }
    const attribute = tokenizer.current;
    if (attribute.type !== "keyword" || attribute.value !== "attribute") {
        throw new Error(`unexpected ${tokenToString(attribute)}`);
    }
    tokenizer.next();
    const paramTypeSpec = parseParamTypeSpec(tokenizer);
    const declarators: SimpleDeclarator[] = [];
    const decl = tokenizer.current;
    if (decl.type !== "identifier") {
        throw new Error(`attr_dcl: expected identifier but ${tokenToString(decl)}`);
    }
    declarators.push(decl);
    let comma = tokenizer.next();
    while (comma.type === "punctuator" && comma.value === ",") {
        const decl = tokenizer.next();
        if (decl.type !== "identifier") {
            throw new Error(`unexpected ${tokenToString(decl)}`);
        }
        declarators.push(decl);
        comma = tokenizer.next();
    }
    const semi = tokenizer.current;
    if (semi.type !== "punctuator" || semi.value !== ";") {
        throw new Error(`unexpected ${tokenToString(semi)}`);
    }
    tokenizer.next();
    return {
        type: "attributeDeclaration",
        isReadonly,
        paramTypeSpec,
        declarators,
        start: begin.start,
        end: tokenizer.prevPosition,
    };
}

function parseOperationDeclaration(tokenizer: Tokenizer): OperationDeclaration {
    const attr = tokenizer.current;
    const attribute = attr.type === "keyword" && attr.value === "oneway" ? attr.value : undefined;
    if (attribute != null) {
        tokenizer.next();
    }
    const voidOrType = tokenizer.current;
    const typeSpec =
        voidOrType.type === "keyword" && voidOrType.value === "void" ? voidOrType.value : parseParamTypeSpec(tokenizer);
    if (voidOrType.type === "keyword" && voidOrType.value === "void") {
        tokenizer.next();
    }
    const name = tokenizer.current;
    if (name.type !== "identifier") {
        throw new Error(`op_dcl: expected identifier but ${tokenToString(name)}`);
    }
    tokenizer.next();
    const parameters = parseParameterDecralations(tokenizer);
    const semi = tokenizer.current;
    if (semi.type !== "punctuator" || semi.value !== ";") {
        throw new Error(`unexpected ${tokenToString(semi)}`);
    }
    tokenizer.next();
    return {
        type: "operationDeclaration",
        attribute,
        typeSpec,
        name: name.value,
        parameters,
        start: attr.start,
        end: tokenizer.prevPosition,
    };
}

function parseParameterDecralations(tokenizer: Tokenizer): ParameterDeclarations {
    const begin = tokenizer.current;
    if (begin.type !== "punctuator" || begin.value !== "(") {
        throw new Error(`parameter_dcls: expected ( but ${tokenToString(begin)}`);
    }
    const paramOrEnd = tokenizer.next();
    if (paramOrEnd.type === "punctuator" && paramOrEnd.value === ")") {
        tokenizer.next();
        return [];
    }
    const decls: ParameterDeclarations = [];
    while (true) {
        const attr = tokenizer.current;
        if (attr.type !== "keyword" || (attr.value !== "in" && attr.value !== "out" && attr.value !== "inout")) {
            throw new Error(`param_attribute: expected in, out, or inout but ${tokenToString(attr)}`);
        }
        tokenizer.next();
        const typeSpec = parseParamTypeSpec(tokenizer);
        const name = tokenizer.current;
        if (name.type !== "identifier") {
            throw new Error(`param_attribute: expected identifier but ${tokenToString(name)}`);
        }
        decls.push({
            type: "parameterDeclaration",
            attribute: attr.value,
            typeSpec,
            declarator: name,
        });
        const commaOrEnd = tokenizer.next();
        if (commaOrEnd.type === "punctuator" && commaOrEnd.value === ",") {
            tokenizer.next();
            continue;
        }
        if (commaOrEnd.type === "punctuator" && commaOrEnd.value === ")") {
            tokenizer.next();
            break;
        }
        throw new Error(`param_attribute: expected , or ) but ${tokenToString(commaOrEnd)}`);
    }
    return decls;
}

function parseParamTypeSpec(tokenizer: Tokenizer): ParameterTypeSpec {
    const token = tokenizer.current;
    if (token.type === "keyword") {
        switch (token.value) {
            case "float":
            case "double":
            case "short":
            case "char":
            case "wchar":
            case "boolean":
            case "octet":
            case "any":
            case "Object":
            case "ValueBase":
                tokenizer.next();
                return token.value;
            case "unsigned": {
                const shortOrLong = tokenizer.next();
                if (shortOrLong.type !== "keyword") {
                    throw new Error();
                }
                const long = tokenizer.next();
                if (shortOrLong.value === "short") {
                    return "unsigned short";
                }
                if (shortOrLong.value !== "long") {
                    throw new Error();
                }
                if (long.type !== "keyword" || long.value !== "long") {
                    return "unsigned long";
                }
                tokenizer.next();
                return "unsigned long long";
            }
            case "long": {
                const long = tokenizer.next();
                if (long.type === "keyword" && long.value === "long") {
                    tokenizer.next();
                    return "long long";
                }
                if (long.type === "keyword" && long.value === "double") {
                    tokenizer.next();
                    return "long double";
                }
                return "long";
            }
            case "string":
                throw new Error("not impl");
            case "wstring":
                throw new Error("not impl");
        }
    } else if (token.type === "identifier") {
        let scoped = tokenizer.next();
        const names: string[] = [token.value];
        while (scoped.type === "punctuator" && scoped.value === "::") {
            const name = tokenizer.next();
            if (name.type !== "identifier") {
                throw new Error(`unexpected ${tokenToString(name)}`);
            }
            names.push(name.value);
            scoped = tokenizer.next();
        }
        return {
            type: "scopedName",
            names,
        };
    }
    throw new Error(`param_type_spec: unexpected ${tokenToString(token)}`);
}
