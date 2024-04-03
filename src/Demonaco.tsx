import React, { JSX, useEffect } from 'react'
import { Editor } from "@monaco-editor/react";
import { useRef, useState } from "react";
import * as monaco_editor from 'monaco-editor';
import * as backend from './backend';
import * as fp_parser from './fp_parser';

const typecheck_delay = 500;
const typecheck_tick = 100;

export default function Demonaco(
    { start_program }:
        { start_program: string }
) {
    useEffect(() => {
        console.log("[demonaco]")
    })

    const [welltyped, set_welltyped] = useState(false);
    const [parsed_ast, set_parsed_ast] = useState<any>(undefined);
    const [evaluation, set_evaluation] = useState<string | undefined>(undefined);

    // def monaco_ref
    const monaco_ref = useRef<monaco_editor.editor.ICodeEditor>(null);

    // def typecheck_intervalId
    const typecheck_intervalId = useRef<number | undefined>(undefined);
    const time_since_change = useRef(0);
    const typechecked_since_change = useRef(false);

    // reset time_since_change, typechecked_since_change on editor change
    function onChangeEditor(value: string | undefined, ev: monaco_editor.editor.IModelContentChangedEvent) {
        time_since_change.current = 0;
        typechecked_since_change.current = false;
    }

    // init monaco_ref
    function onMountEditor(editor: monaco_editor.editor.ICodeEditor, monaco: typeof monaco_editor) {
        // init monaco
        monaco.languages.register(fp_language_extension_point);
        monaco.languages.setMonarchTokensProvider('fp', fp_monarch_langauge);
        monaco.editor.defineTheme('fp-theme', fp_theme);

        typecheck_intervalId.current = setInterval(() => {
            time_since_change.current += typecheck_tick;

            if (!typechecked_since_change.current && time_since_change.current > typecheck_delay) {
                typecheck(monaco);
                typechecked_since_change.current = true;
            }
        }, typecheck_tick);

        // @ts-ignore
        monaco_ref.current = editor;
        console.log("monaco_ref.current === null", monaco_ref.current === null);

        const urlParams = new URLSearchParams(window.location.search);
        const program_string = urlParams.get('program');
        if (program_string !== null) {
            if (monaco === null) throw new Error(`[onEditorDidMount] program_string !== null but monaco === null`)
            if (monaco_ref.current === null) throw new Error(`[onEditorDidMount] program_string !== null but monaco_ref.current === null`)
            editor.setValue(program_string)
        }
    }

    // when component dismounts, clear interval
    useEffect(() => () => {
        console.log("clearInterval(typecheck_intervalId)");
        clearInterval(typecheck_intervalId.current)
    }, []);

    function typecheck(monaco: typeof monaco_editor) {
        console.log("[typecheck]")
        // reset these so that on failure, doens't keep old state
        set_parsed_ast(undefined);
        set_welltyped(false);

        if (monaco === null) throw new Error(`[typecheck] monaco === null`)
        if (monaco_ref.current === null) throw new Error(`[typecheck] monaco_ref.current === null`)
        const value = monaco_ref.current.getValue();

        const parse_result = fp_parser.parse(value);

        const markers: monaco_editor.editor.IMarkerData[] = [];

        // parse errors
        for (const err of parse_result.errs) {
            const severity = monaco_editor.MarkerSeverity.Error;
            const message = err.toString();
            markers.push({
                severity, message,
                startLineNumber: err.pos.line, startColumn: err.pos.offset,
                endLineNumber: err.pos.line, endColumn: err.pos.offset + 1,
            });
        }

        if (parse_result.ast !== null) { set_parsed_ast(parse_result.ast); }

        if (parse_result.ast !== null) {
            const typecheck_errs: any[] = backend.typecheckTop((transformAst(parse_result.ast)));
            if (typecheck_errs.find((error) => error.severity == "error") === undefined) set_welltyped(true);
            for (const err of typecheck_errs) {
                markers.push({
                    severity:
                        (() => {
                            switch (err.severity) {
                                case 'warning': return monaco_editor.MarkerSeverity.Warning;
                                case 'error': return monaco_editor.MarkerSeverity.Error;
                                default: throw new Error(`unknown typecheck severity: ${err.severity}`);
                            }
                        })(),
                    message: err.msg,
                    startLineNumber: err.start_pos.line, startColumn: err.start_pos.offset,
                    endLineNumber: err.end_pos.line, endColumn: err.end_pos.offset,
                })
            }
        }
        monaco.editor.setModelMarkers(monaco_ref.current?.getModel() as monaco_editor.editor.ITextModel, 'owner', markers);
    }

    function run() {
        set_evaluation(undefined);

        if (parsed_ast === undefined || !welltyped) {
            set_evaluation("ill-typed");
            return;
        }
        let eval_result = undefined;
        // try {
        // console.log("before evaluate, the ast is: ", transformAst(parsed_ast));
        eval_result = backend.evaluate(transformAst(parsed_ast));
        // } catch (e) {
        // set_feedback(`evaluation error:\n${(e as Error).message}`);
        // }

        if (eval_result === undefined) return;
        set_evaluation(eval_result as string);
    }

    async function copy_encoding() {
        if (monaco_ref.current === null) throw new Error(`[copy_encoding] monaco_ref.current`)
        const value = monaco_ref.current.getValue();
        const escaped_value = encodeURIComponent(value);
        try {
            await navigator.clipboard.writeText(escaped_value);
            console.log("[copy_encoding] URI encoding copied to clipboard.")
        } catch (e) {
            console.error(`[copy_encoding] ${(e as Error).toString()}`)
        }
    }

    function renderControls(): JSX.Element {
        return (
            <div
                style={{
                    height: "1.5em",
                    padding: "0.5em",
                    display: "flex",
                    flexDirection: "row",
                    gap: "1em",
                    backgroundColor: "black",
                    color: "white",
                }}
            >
                <button onClick={run}>run</button>
                <div style={{ fontFamily: "monospace" }}>{evaluation ?? ""}</div>
            </div>
        )

    }

    return (
        <div style={{
            display: "flex",
            flexDirection: "column",
            height: "100%",
            boxShadow: "0 0 0 1px black"
        }}>
            <div
                style={{
                    flexGrow: 0,
                    flexShrink: 0,
                }}
            >
                {renderControls()}
            </div>
            <div
                style={{
                    flexGrow: 1,
                    flexShrink: 0,
                }}
            >
                <Editor
                    language="fp"
                    theme="fp-theme"
                    value={start_program}
                    onMount={onMountEditor}
                    onChange={onChangeEditor}
                    height="100%"
                    options={{
                        minimap: { enabled: false }
                    }}
                />
            </div>
        </div >
    )
}

const fp_keywords = [
    "fun",
    "let", "in",
    "if", "then", "else",
    "true", "false",
    "match", "with"
];

const fp_language_extension_point: monaco_editor.languages.ILanguageExtensionPoint =
    { id: "fp" }

const fp_monarch_langauge: monaco_editor.languages.IMonarchLanguage = {
    keywords: fp_keywords,
    tokenizer: {
        root: [
            [/@?[a-zA-Z][\w$]*/, {
                cases: {
                    '@keywords': 'keyword',
                    '@default': 'variable'
                }
            }],
            [/[0-9]/, 'number'],
            [/".*?"/, 'string'],
            [/\/\/.*$/, 'comment'],
        ],
    }
};

const fp_theme: monaco_editor.editor.IStandaloneThemeData = {
    base: 'vs',
    inherit: true,
    rules: [],
    colors: {}
};

type EditorMessage
    = { case: 'type-error', value: string }
    | { case: 'parse-error', value: string }
    | { case: 'parse-result', value: string }
    | { case: 'eval-result', value: string }
    | { case: 'eval-error', value: string }
    | { case: 'user-error', value: string }
    | { case: 'bug', value: string }
    | { case: 'info', value: string }

// @ts-ignore
function transformAst(ast) {
    // @ts-ignore
    var getChildren = function (obj) { // https://stackoverflow.com/questions/208016/how-to-list-the-properties-of-a-javascript-object
        if (!obj.kind) {
            throw "error: bad input"
        }
        var children = [];
        for (var key in obj) {
            if (key !== "kind" && key !== "start_pos" && key !== "end_pos" && key !== "data" && key !== "data2") {
                children.push(obj[key]);
            }
        }
        return children;
    }
    // @ts-ignore
    function getData(obj) {
        if (typeof obj === "string") {
            return obj
        } else if (obj.data) {
            return getData(obj.data)
        } else {
            return ""
        }
    }
    // @ts-ignore
    function getData2(obj) {
        if (typeof obj === "string") {
            return obj
        } else if (obj.data2) {
            return getData(obj.data2)
        } else {
            return ""
        }
    }
    return {
        label: ast.kind
        , dataa: getData(ast)
        , dataa2: getData2(ast)
        , kids: getChildren(ast).map(transformAst)
        , start_pos: ast.start_pos
        , end_pos: ast.end_pos
    }
}