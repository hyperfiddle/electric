import {syntaxTree}  from "@codemirror/language";
import {EditorState} from "@codemirror/state";
import {Diagnostic}  from "@codemirror/lint";

export type Sexpr = string;
type Error = string;

type Explainer = (sexpr : Sexpr) => Error | null;

export function explain(state : EditorState, explainf : Explainer) : Diagnostic[] {
    let cursor = syntaxTree(state).cursor();
    let errors : Diagnostic[] = [];
    do {
        let {type, from, to} = cursor;
        if (type.name == "List") {
            let from_list = cursor.from;
            let to_list = cursor.to;
            let sexpr = state.doc.sliceString(from_list, to_list);
            let error = explainf(sexpr);
            if (error != null){
                errors.push({
                    from: from,
                    to: to,
                    severity: "error",
                    source: "Spec",
                    message: error
                });
            }
        }
    } while(cursor.next());
    return errors;
}
