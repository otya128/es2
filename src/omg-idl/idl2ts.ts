import { Definition, ParameterTypeSpec, parseSpecification } from "./parser.js";
import fs from "node:fs";
import process from "node:process";

function convertParameter(t: ParameterTypeSpec, expr: string): string {
    if (typeof t === "object") {
        if (t.type === "scopedName") {
            if (t.names.join("::") === "DOMString") {
                return `yield* toString(ctx, ${expr}, caller)`;
            }
            return `unwrap(${expr})`;
        }
        throw new Error("unknown type");
    }
    switch (t) {
        case "boolean":
            return `toBoolean(${expr})`;
        case "float":
        case "double":
        case "long double":
        case "short":
        case "long":
        case "long long":
        case "unsigned short":
        case "unsigned long":
        case "unsigned long long":
            return `yield* toNumber(ctx, ${expr}, caller)`;
        case "char":
        case "wchar":
        case "octet":
        case "any":
        case "Object":
        case "ValueBase":
        default:
            throw new Error("unknown type");
    }
}

function toToplologicalSorted(definitions: Definition[]): Definition[] {
    function sort(def: Definition) {
        for (const i of def.inheritanceInterfaces) {
            const inherit = definitionMap.get(i);
            if (inherit == null) {
                throw new Error(`undefined interface ${inherit}`);
            }
            sort(inherit);
        }
        result.add(def);
    }
    const definitionMap = new Map<string, Definition>();
    for (const def of definitions) {
        definitionMap.set(def.name, def);
    }
    const result = new Set<Definition>();
    for (const def of definitionMap.values()) {
        sort(def);
    }
    return [...result];
}

export function idl2ts(definitions: Definition[], importPath: string, prefix?: string, preface?: string): string {
    prefix ??= "";
    let output = `// THIS IS A GENERATED FILE. DO NOT EDIT DIRECTLY.
import { Caller, Context, InterpreterObject, InterpreterTypeError, isPrimitive, newNativeFunction, newObject, toBoolean, toNumber, toString, Value } from "${importPath}";
${preface ?? ""}
export function define(context: Context, prototypes: Map<any, InterpreterObject>, map: WeakMap<any, InterpreterObject>) {\n`;
    definitions = toToplologicalSorted(definitions);
    for (const def of definitions) {
        output += `    const $${def.name}$prototype = newObject(${def.inheritanceInterfaces[0] != null ? `$${def.inheritanceInterfaces[0]}$prototype` : "context.realm.intrinsics.ObjectPrototype"});
    prototypes.set(${prefix}${def.name}.prototype, $${def.name}$prototype);
    $${def.name}$prototype.internalProperties.class = "${def.name}";
`;
        output += `    $${def.name}$prototype.internalProperties.get = function* $${def.name}$get(ctx, self, propertyName, caller) {
        if (!(self?.internalProperties.hostObjectValue instanceof ${prefix}${def.name})) {
            throw new InterpreterTypeError(\`${def.name}.prototype.\${propertyName}: Invalid call\`, ctx, caller);
        }
        switch (propertyName) {
`;
        for (const decl of def.body) {
            if (decl.type !== "attributeDeclaration") {
                continue;
            }
            for (const { value } of decl.declarators) {
                output += `            case "${value}":
                return wrap(prototypes, map, self.internalProperties.hostObjectValue.${value});
`;
            }
        }
        output += `        }
        return $${def.name}$prototype.properties.get(propertyName)?.value;
    };
`;
        output += `    function* $${def.name}$put(ctx: Context, self: InterpreterObject, propertyName: string, value: Value, caller: Caller) {
        if (!(self?.internalProperties.hostObjectValue instanceof ${prefix}${def.name})) {
            throw new InterpreterTypeError(\`${def.name}.prototype.\${propertyName}: Invalid call\`, ctx, caller);
        }
        switch (propertyName) {
`;
        for (const decl of def.body) {
            if (decl.type !== "attributeDeclaration") {
                continue;
            }
            for (const { value } of decl.declarators) {
                if (decl.isReadonly) {
                    output += `            case "${value}":
                return; // readonly
`;
                    continue;
                }
                output += `            case "${value}":
                self.internalProperties.hostObjectValue.${value} = ${convertParameter(decl.paramTypeSpec, "value")};
                return;
`;
            }
        }
        if (def.inheritanceInterfaces[0] != null) {
            output += `        }
        yield* $${def.inheritanceInterfaces[0]}$put(ctx, self, propertyName, value, caller);
        return;
    };
    $${def.name}$prototype.internalProperties.put = $${def.name}$put;
`;
        } else {
            output += `        }
        throw new InterpreterTypeError(\`${def.name}.prototype.\${propertyName}: Unknown property\`, ctx, caller);
    };
    $${def.name}$prototype.internalProperties.put = $${def.name}$put;
`;
        }
        for (const decl of def.body) {
            if (decl.type === "operationDeclaration") {
                output += `    $${def.name}$prototype.properties.set("${decl.name}", {
        readOnly: true,
        dontEnum: true,
        dontDelete: true,
        value: newNativeFunction(context.realm.intrinsics.FunctionPrototype, function* $${def.name}$prototype$${decl.name}(ctx, self, args, caller) {
            if (!(self?.internalProperties.hostObjectValue instanceof ${prefix}${def.name})) {
                throw new InterpreterTypeError("${def.name}.prototype.${decl.name}: Invalid call", ctx, caller);
            }
            return wrap(prototypes, map, self.internalProperties.hostObjectValue.${decl.name}(${decl.parameters
                .map((p, index) => convertParameter(p.typeSpec, `args[${index}]`))
                .join(", ")})${decl.typeSpec === "void" ? " as undefined" : ""});
        }, ${decl.parameters.length}),
    });
`;
            } else if (decl.type === "attributeDeclaration") {
                for (const { value } of decl.declarators) {
                    output += `    $${def.name}$prototype.properties.set("${value}", {
        readOnly: ${decl.isReadonly},
        dontEnum: true,
        dontDelete: true,
        value: undefined,
    });
`;
                }
            }
        }
    }
    output += "}\n";
    output += `
export function wrap(prototypes: Map<any, InterpreterObject>, map: WeakMap<any, InterpreterObject>, obj: any): Value {
    if (isPrimitive(obj)) {
        return obj;
    }
    if ("internalProperties" in obj && typeof obj.internalProperties.class === "string") {
        return obj as InterpreterObject;
    }
    const wrapperCache = map.get(obj);
    if (wrapperCache != null) {
        return wrapperCache;
    }
    const prototype = prototypes.get(Object.getPrototypeOf(obj));
    if (prototype == null) {
        throw new TypeError("unknown object: " + obj);
    }
    const wrapper = newObject(prototype);
    wrapper.internalProperties.hostObjectValue = obj;
    wrapper.internalProperties.class = prototype.internalProperties.class;
    if (prototype.internalProperties.put != null) {
        wrapper.internalProperties.put = prototype.internalProperties.put;
    }
    return wrapper;
}
`;
    return output;
}

if (import.meta.main) {
    const importPath = process.argv[2];
    const prefix = process.argv[3];
    const prefaceFile = process.argv[4];
    if (importPath == null || prefix == null || prefaceFile == null) {
        throw new Error("usage <importPath> <prefix> <prefaceFile> <output> <idl files...>");
    }
    const idlFiles = process.argv.slice(5).map((x) => parseSpecification(fs.readFileSync(x, "utf-8"), { name: x }));
    process.stdout.write(
        idl2ts(
            idlFiles.flatMap((x) => x.definitions),
            importPath,
            prefix,
            fs.readFileSync(prefaceFile === "-" ? process.stdin.fd : prefaceFile, "utf-8")
        )
    );
}
