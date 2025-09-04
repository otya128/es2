import React, { useState } from "react";
import { Value, isObject } from "../../src/index";

interface ObjectInspectorProps {
    value: Value;
    level?: number;
}

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

export const ObjectInspector: React.FC<ObjectInspectorProps> = ({ value, level = 0 }) => {
    const [isExpanded, setIsExpanded] = useState(level < 2);

    if (!isObject(value)) {
        return renderPrimitiveValue(value);
    }

    const className = value.internalProperties.class;
    const properties = Array.from(value.properties.entries()).filter(([, prop]) => !prop.internal);

    if (properties.length === 0) {
        return (
            <span className="object-inspector-empty">
                {className} {"{}"}
            </span>
        );
    }

    return (
        <div className="object-inspector-container">
            <div
                className="object-inspector-header"
                onClick={() => setIsExpanded(!isExpanded)}
                style={{ cursor: "pointer" }}
            >
                <span className="object-inspector-toggle">{isExpanded ? "▼" : "▶"}</span>
                <span className="object-inspector-class">{className}</span>
                <span className="object-inspector-preview">{!isExpanded && `{${properties.length} properties}`}</span>
            </div>

            {isExpanded && (
                <div className="object-inspector-properties" style={{ marginLeft: "16px" }}>
                    {properties.map(([key, prop]) => (
                        <div key={key} className="object-inspector-property">
                            <span className="object-inspector-key">{key}:</span>
                            <span className="object-inspector-value">
                                <ObjectInspector value={prop.value} level={level + 1} />
                            </span>
                        </div>
                    ))}
                </div>
            )}
        </div>
    );
};
