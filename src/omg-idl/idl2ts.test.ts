import { expect, test, vi } from "vitest";
import {
    Caller,
    Context,
    createGlobalContext,
    InterpreterObject,
    InterpreterTypeError,
    newNativeFunction,
    newObject,
    runInContext,
    toNumber,
    toString,
    Value,
} from "..";
import { idl2ts } from "./idl2ts";
import { parseSpecification } from "./parser";

idl2ts(
    parseSpecification(String.raw`
interface Test {
    void func(in DOMString param);
    attribute DOMString prop1;
};
interface Test2 : Test {
    void func2(in unsigned short param);
};
`).definitions,
    ".."
);

class Test {
    get prop1(): string {
        return "HOGE";
    }
    set prop1(_value: string) {}
    func(_param: string): void {}
}

class Test2 extends Test {
    func2(_param: number): void {}
}

test("idl2ts", async () => {
    const ctx = createGlobalContext();
    const prototypes = new Map<any, InterpreterObject>();
    define(ctx, prototypes);
    const test2 = new Test2();
    const func1Spy = vi.spyOn(test2, "func");
    func1Spy.mockImplementation(() => {});
    const func2Spy = vi.spyOn(test2, "func2");
    func2Spy.mockImplementation(() => {});
    const prop1SetSpy = vi.spyOn(test2, "prop1", "set");
    prop1SetSpy.mockImplementation(() => {});
    const prop1GetSpy = vi.spyOn(test2, "prop1", "get");
    prop1GetSpy.mockImplementation(() => "2");
    const ho = newObject(prototypes.get(Object.getPrototypeOf(test2))!);
    ho.internalProperties.hostObjectValue = test2;
    ho.internalProperties.class = "Test2";
    ho.internalProperties.put = ho.internalProperties.prototype?.internalProperties.put!;
    ctx.realm.globalObject.properties.set("test2", {
        dontEnum: false,
        dontDelete: false,
        readOnly: false,
        value: ho,
    });
    await runInContext(
        String.raw`
    test2.prop1 = test2.prop1 + 1;
    test2.func()
    test2.func2("1")
`,
        ctx
    );
    expect(func1Spy).toHaveBeenCalledOnce();
    expect(func1Spy).toHaveBeenCalledWith("undefined");
    expect(func2Spy).toHaveBeenCalledOnce();
    expect(func2Spy).toHaveBeenCalledWith(1);
    expect(prop1SetSpy).toHaveBeenCalledOnce();
    expect(prop1SetSpy).toHaveBeenCalledWith("21");
    expect(prop1GetSpy).toHaveBeenCalledWith();
    expect(prop1GetSpy).toHaveBeenCalledOnce();
});

function define(context: Context, prototypes: Map<any, InterpreterObject>) {
    const $Test$prototype = newObject(context.realm.intrinsics.ObjectPrototype);
    prototypes.set(Test.prototype, $Test$prototype);
    $Test$prototype.internalProperties.get = function* $Test$get(ctx, self, propertyName, caller) {
        if (!(self?.internalProperties.hostObjectValue instanceof Test)) {
            throw new InterpreterTypeError(`Test.prototype.${propertyName}: Invalid call`, ctx, caller);
        }
        switch (propertyName) {
            case "prop1":
                return self.internalProperties.hostObjectValue.prop1;
        }
        return $Test$prototype.properties.get(propertyName)?.value;
    };
    function* $Test$put(ctx: Context, self: InterpreterObject, propertyName: string, value: Value, caller: Caller) {
        if (!(self?.internalProperties.hostObjectValue instanceof Test)) {
            throw new InterpreterTypeError(`Test.prototype.${propertyName}: Invalid call`, ctx, caller);
        }
        switch (propertyName) {
            case "prop1":
                self.internalProperties.hostObjectValue.prop1 = yield* toString(ctx, value, caller);
                return;
        }
        throw new InterpreterTypeError(`Test.prototype.${propertyName}: Unknown property`, ctx, caller);
    }
    $Test$prototype.internalProperties.put = $Test$put;
    $Test$prototype.properties.set("func", {
        readOnly: true,
        dontEnum: true,
        dontDelete: true,
        value: newNativeFunction(
            context.realm.intrinsics.FunctionPrototype,
            function* $Test$prototype$func(ctx, self, args, caller) {
                if (!(self?.internalProperties.hostObjectValue instanceof Test)) {
                    throw new InterpreterTypeError("Test.prototype.func: Invalid call", ctx, caller);
                }
                return self.internalProperties.hostObjectValue.func(yield* toString(ctx, args[0], caller)) as undefined;
            },
            1
        ),
    });
    $Test$prototype.properties.set("prop1", {
        readOnly: false,
        dontEnum: true,
        dontDelete: true,
        value: undefined,
    });
    const $Test2$prototype = newObject($Test$prototype);
    prototypes.set(Test2.prototype, $Test2$prototype);
    $Test2$prototype.internalProperties.get = function* $Test2$get(ctx, self, propertyName, caller) {
        if (!(self?.internalProperties.hostObjectValue instanceof Test2)) {
            throw new InterpreterTypeError(`Test2.prototype.${propertyName}: Invalid call`, ctx, caller);
        }
        switch (propertyName) {
        }
        return $Test2$prototype.properties.get(propertyName)?.value;
    };
    function* $Test2$put(ctx: Context, self: InterpreterObject, propertyName: string, value: Value, caller: Caller) {
        if (!(self?.internalProperties.hostObjectValue instanceof Test2)) {
            throw new InterpreterTypeError(`Test2.prototype.${propertyName}: Invalid call`, ctx, caller);
        }
        switch (propertyName) {
        }
        yield* $Test$put(ctx, self, propertyName, value, caller);
        return;
    }
    $Test2$prototype.internalProperties.put = $Test2$put;
    $Test2$prototype.properties.set("func2", {
        readOnly: true,
        dontEnum: true,
        dontDelete: true,
        value: newNativeFunction(
            context.realm.intrinsics.FunctionPrototype,
            function* $Test2$prototype$func2(ctx, self, args, caller) {
                if (!(self?.internalProperties.hostObjectValue instanceof Test2)) {
                    throw new InterpreterTypeError("Test2.prototype.func2: Invalid call", ctx, caller);
                }
                return self.internalProperties.hostObjectValue.func2(
                    yield* toNumber(ctx, args[0], caller)
                ) as undefined;
            },
            1
        ),
    });
}
