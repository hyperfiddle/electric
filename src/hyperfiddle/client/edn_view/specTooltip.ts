import {hoverTooltip} from "@codemirror/tooltip";
import {syntaxTree} from "@codemirror/language";
import {Sexpr} from "./spec";
import {SyntaxNode} from "lezer-tree";
import { EditorState } from "@codemirror/state";

type SpecResolver = (form : Sexpr) => Sexpr | null;

function findNamespace(state : EditorState, startAt : SyntaxNode) : string | null {
    let cursor = startAt.cursor;
    cursor.parent();
    cursor.parent();
    if(cursor.type.name == "NamespacedMap"){
        cursor.next(); // KeywordPrefix
        let {from, to} = cursor;
        return state.doc.sliceString(from+1, to);
    }
    else return null;
}

function extractKeyword(state: EditorState, node : SyntaxNode) : string {
    let {from, to} = node;
    let kw = state.doc.sliceString(from, to);
    let parent = findNamespace(state, node);
    if (kw.includes("/")){
        return kw;
    } else if (parent != null){
        return parent + "/" + kw.substring(1); // drop ":"
    }
    else return kw;
}

export function specTooltip(resolve : SpecResolver){
    return hoverTooltip((view, pos, side) => {
        let node = syntaxTree(view.state).resolve(pos);
        let {name, from, to} = node;
        if (name == "Symbol" || name == "Keyword"){
            if (from == pos && side < 0 || to == pos && side > 0){
                return null;
            } else{
                let text : string;
                if(name == "Keyword"){
                    text = extractKeyword(view.state, node);
                } else {
                    text = view.state.doc.sliceString(from, to);
                }
                return {
                    pos: from,
                    end: to,
                    above: true,
                    create(_view) {
                        let spec = resolve(text) || "No spec found";
                        let dom = document.createElement("pre");
                        dom.textContent = spec;
                        dom.classList.add("hf-code-tooltip");
                        return {dom};
                    }
                }
            };
        }
        else{
            return null;
        }
    });
}
