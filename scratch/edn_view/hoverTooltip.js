"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.wordHover = void 0;
const tooltip_1 = require("@codemirror/tooltip");
const language_1 = require("@codemirror/language");
exports.wordHover = tooltip_1.hoverTooltip((view, pos, side) => {
    let tree = language_1.syntaxTree(view.state);
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
                dom.style.padding = "0.25rem";
                return { dom };
            }
        };
});
