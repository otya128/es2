import { Context, newNativeFunction, newObject, toNumber, Value } from "../../src/index";

export function setupConsole(context: Context, onLog: (value: Value) => void) {
    const consolePrototype = newObject(context.realm.intrinsics.ObjectPrototype);
    const consoleObject = newObject(consolePrototype);

    consoleObject.properties.set("log", {
        readOnly: false,
        dontEnum: false,
        dontDelete: false,
        value: newNativeFunction(
            context.realm.intrinsics.FunctionPrototype,
            function* log(_ctx, _, args) {
                for (const arg of args) {
                    onLog(arg);
                }
                return undefined;
            },
            0
        ),
    });

    context.realm.globalObject.properties.set("console", {
        readOnly: false,
        dontEnum: false,
        dontDelete: false,
        value: consoleObject,
    });
    context.realm.globalObject.properties.set("sleep", {
        readOnly: false,
        dontEnum: false,
        dontDelete: false,
        value: newNativeFunction(
            context.realm.intrinsics.FunctionPrototype,
            function* (ctx, _self, args) {
                const ms = yield* toNumber(ctx, args[0]);
                return yield new Promise((resolve) => {
                    setTimeout(() => resolve("slept"), ms);
                });
            },
            1
        ),
    });
}
