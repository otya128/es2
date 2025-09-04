import React, { useState, useRef, useEffect } from "react";
import { createGlobalContext, runInContext, Value, isObject } from "../../src/index";
import { setupConsole } from "../utils/console";
import { ObjectInspector } from "./ObjectInspector";
import "./REPL.css";

interface REPLEntry {
    id: number;
    type: "input" | "output" | "error";
    content: string | Value;
    timestamp: Date;
}

export const REPL: React.FC = () => {
    const [entries, setEntries] = useState<REPLEntry[]>([]);
    const [input, setInput] = useState("");
    const [isRunning, setIsRunning] = useState(false);
    const [context] = useState(() => {
        const ctx = createGlobalContext();
        setupConsole(ctx, (value: Value) => {
            const newEntry: REPLEntry = {
                id: Date.now() + Math.random(),
                type: "output",
                content: value,
                timestamp: new Date(),
            };
            setEntries((prev) => [...prev, newEntry]);
        });
        return ctx;
    });

    const inputRef = useRef<HTMLTextAreaElement>(null);
    const outputRef = useRef<HTMLDivElement>(null);

    useEffect(() => {
        if (outputRef.current) {
            outputRef.current.scrollTop = outputRef.current.scrollHeight;
        }
    }, [entries]);

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        if (!input.trim() || isRunning) return;

        const inputEntry: REPLEntry = {
            id: Date.now(),
            type: "input",
            content: input,
            timestamp: new Date(),
        };

        setEntries((prev) => [...prev, inputEntry]);
        setIsRunning(true);

        try {
            const result = await runInContext(input, context);
            const value = result.hasValue ? result.value : undefined;

            const outputEntry: REPLEntry = {
                id: Date.now() + 1,
                type: "output",
                content: value,
                timestamp: new Date(),
            };

            setEntries((prev) => [...prev, outputEntry]);
        } catch (error) {
            const errorEntry: REPLEntry = {
                id: Date.now() + 1,
                type: "error",
                content: error instanceof Error ? error.message : String(error),
                timestamp: new Date(),
            };

            setEntries((prev) => [...prev, errorEntry]);
        } finally {
            setIsRunning(false);
        }

        setInput("");
    };

    const handleKeyDown = (e: React.KeyboardEvent) => {
        if (e.key === "Enter" && (e.ctrlKey || e.metaKey)) {
            e.preventDefault();
            handleSubmit(e as any);
        }
    };

    const clearHistory = () => {
        setEntries([]);
    };

    const getValueTypeClass = (value: Value): string => {
        if (value === null) return "repl-value-null";
        if (value === undefined) return "repl-value-undefined";
        if (typeof value === "number") return "repl-value-number";
        if (typeof value === "string") return "repl-value-string";
        if (typeof value === "boolean") return "repl-value-boolean";
        if (isObject(value)) return "repl-value-object";
        return "repl-value-default";
    };

    const renderPrimitiveValue = (value: Value): React.ReactNode => {
        const className = getValueTypeClass(value);

        if (value === null) {
            return <span className={className}>null</span>;
        }
        if (value === undefined) {
            return <span className={className}>undefined</span>;
        }
        if (typeof value === "string") {
            return <span className={className}>"{value}"</span>;
        }
        if (typeof value === "boolean") {
            return <span className={className}>{value ? "true" : "false"}</span>;
        }
        if (typeof value === "number") {
            return <span className={className}>{String(value)}</span>;
        }

        return <span className={className}>{String(value)}</span>;
    };

    const renderEntryContent = (entry: REPLEntry) => {
        if (entry.type === "input") {
            return <code className="repl-input-code">{entry.content as string}</code>;
        }

        if (entry.type === "error") {
            return <span className="repl-error">{entry.content as string}</span>;
        }

        // Output
        const value = entry.content as Value;
        if (isObject(value)) {
            return <ObjectInspector value={value} />;
        }
        return renderPrimitiveValue(value);
    };

    return (
        <div className="repl-container">
            <div className="repl-header">
                <h1>ES2 REPL</h1>
                <div className="repl-controls">
                    <button onClick={clearHistory} className="repl-clear-btn">
                        Clear History
                    </button>
                </div>
            </div>

            <div className="repl-output" ref={outputRef}>
                {entries.map((entry) => (
                    <div key={entry.id} className={`repl-entry repl-entry-${entry.type}`}>
                        <div className="repl-entry-indicator">
                            {entry.type === "input" ? "❯" : entry.type === "error" ? "✗" : "←"}
                        </div>
                        <div className="repl-entry-content">{renderEntryContent(entry)}</div>
                        <div className="repl-entry-timestamp">{entry.timestamp.toLocaleTimeString()}</div>
                    </div>
                ))}
                {isRunning && (
                    <div className="repl-entry repl-running">
                        <div className="repl-entry-indicator">⟳</div>
                        <div className="repl-entry-content">
                            <span className="repl-running-text">Running...</span>
                        </div>
                    </div>
                )}
            </div>

            <form onSubmit={handleSubmit} className="repl-input-form">
                <div className="repl-input-container">
                    <div className="repl-input-indicator">❯</div>
                    <textarea
                        ref={inputRef}
                        value={input}
                        onChange={(e) => setInput(e.target.value)}
                        onKeyDown={handleKeyDown}
                        placeholder="Enter JavaScript code... (Ctrl/Cmd + Enter to run)"
                        className="repl-input"
                        rows={3}
                        disabled={isRunning}
                    />
                    <button type="submit" disabled={!input.trim() || isRunning} className="repl-submit-btn">
                        {isRunning ? "Running..." : "Run"}
                    </button>
                </div>
                <div className="repl-hint">Tip: Press Ctrl/Cmd + Enter to execute, or click the Run button</div>
            </form>
        </div>
    );
};
