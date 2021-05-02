import {hoverTooltip} from "@codemirror/tooltip";
import {syntaxTree} from "@codemirror/language";

type Symbol = string;
type Keyword = string;
type Speced = Symbol | Keyword;
type SpecedType = "Symbol" | "Keyword";
type Sexpr = string;

type SpecResolver = (sym : Speced, type : SpecedType) => Sexpr | null;

export function specTooltip(resolve : SpecResolver){
    return hoverTooltip((view, pos, side) => {
        let tree = syntaxTree(view.state);
        let astNode = tree.resolve(pos);
        if (astNode.name == "Symbol" || astNode.name == "Keyword"){
            let start = astNode.from;
            let end = astNode.to;
            let text = view.state.doc.sliceString(start, end);
            if (start == pos && side < 0 || end == pos && side > 0){
                return null;
            }
            else{
                return {
                    pos: astNode.from,
                    end: astNode.to,
                    above: true,
                    create(_view) {
                        let spec = resolve(text, <SpecedType> astNode.name);
                        if (spec == null){
                            spec = "No spec found";
                        }
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
