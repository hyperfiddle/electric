"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.explain = void 0;
const language_1 = require("@codemirror/language");
function explain(state, explainf) {
    let cursor = language_1.syntaxTree(state).cursor();
    let errors = [];
    do {
        let { type, from, to } = cursor;
        if (type.name == "List") {
            let from_list = cursor.from;
            let to_list = cursor.to;
            let sexpr = state.doc.sliceString(from_list, to_list);
            let error = explainf(sexpr);
            if (error != null) {
                errors.push({
                    from: from,
                    to: to,
                    severity: "error",
                    source: "Spec",
                    message: error
                });
            }
        }
    } while (cursor.next());
    return errors;
}
exports.explain = explain;
