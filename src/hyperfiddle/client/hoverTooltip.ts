import {hoverTooltip} from "@codemirror/tooltip";

export const wordHover = hoverTooltip((view, pos, side) => {
    let astNode = view.state.tree.resolve(pos); // Not knows by type def, but it does exists.
    let start = astNode.from;
    let end = astNode.to;
    let text = view.state.doc.sliceString(start, end);
    let cursor = view.state.tree.cursor()
    do {
        console.log(`Node ${cursor.name} from ${cursor.from} to ${cursor.to}`)
    } while (cursor.next());
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
                return {dom};
            }
        };
});
