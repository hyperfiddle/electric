import {hoverTooltip} from "@codemirror/tooltip";
import {syntaxTree} from "@codemirror/language";

export const wordHover = hoverTooltip((view, pos, side) => {
    let tree = syntaxTree(view.state);
    let astNode = tree.resolve(pos);
    let start = astNode.from;
    let end = astNode.to;
    let text = view.state.doc.sliceString(start, end);
    if (start == pos && side < 0 || end == pos && side > 0)
        return null;
    else
        return {
            pos: astNode.from,
            end: astNode.to,
            above: true,
            create(_view) {
                let dom = document.createElement("div");
                dom.textContent = text + " : " + astNode.name;
                dom.classList.add("hf-code-tooltip");
                dom.classList.remove("cm-tooltip");
                return {dom};
            }
        };
});
