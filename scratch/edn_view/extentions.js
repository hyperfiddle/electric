"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.extentionsPlugin = exports.WidgetBuilder = exports.InputWidget = void 0;
const view_1 = require("@codemirror/view");
const language_1 = require("@codemirror/language");
const links_1 = require("./links");
;
class InputWidget extends view_1.WidgetType {
    constructor(id, value, onChange) {
        super();
        this.id = id;
        this.value = value;
        this.onChange = onChange;
        this.value = value;
        this.onChange = onChange;
    }
    eq(other) { return other.value == this.value; }
    toDOM() {
        let wrap = document.createElement("span");
        wrap.style.marginLeft = "0.5rem";
        let input = document.createElement("input");
        input.classList.add("hf-cm-input");
        input.value = this.value;
        input.type = "text";
        input.addEventListener("click", this.onClick);
        input.addEventListener("change", this.onChange);
        wrap.appendChild(input);
        return wrap;
    }
    onClick(ev) {
        console.log("Click", ev);
        let value = prompt();
        if (value)
            this.value = value;
        this.dispatchEvent(new Event("change"));
        return true;
    }
    updateDOM(input) {
        input.value = this.value;
        return true;
    }
    ignoreEvent() { return false; }
}
exports.InputWidget = InputWidget;
class WidgetBuilder {
    constructor(encodeSexp, parseTaggedValue) {
        this.encodeSexp = encodeSexp;
        this.parseTaggedValue = parseTaggedValue;
    }
}
exports.WidgetBuilder = WidgetBuilder;
function makeWidget(builder, text) {
    let [type, extention] = builder.parseTaggedValue(text);
    switch (type) {
        case "Link": {
            extention = extention;
            return new links_1.LinkWidget(builder.encodeSexp(extention.href), extention.value);
        }
        case "Input":
            {
                extention = extention;
                console.log(extention);
                return new InputWidget(extention.id, extention.value, extention.onChange);
            }
            ;
    }
    ;
}
function extentions(builder, view) {
    let widgets = [];
    for (let { from, to } of view.visibleRanges) {
        let tree = language_1.syntaxTree(view.state);
        let cursor = tree.cursor();
        do {
            let { type, from, to } = cursor;
            if (type.name == "Constructor") {
                cursor.prevSibling(); // go back one node to get the key position
                let prev_to = cursor.to;
                cursor.nextSibling(); // restore cursor position.
                let deco = view_1.Decoration.replace({
                    widget: makeWidget(builder, view.state.doc.sliceString(from, to)),
                    inclusive: true
                });
                widgets.push(deco.range(prev_to, to)); // push it as left as possible, next to the key
            }
        } while (cursor.next());
    }
    return view_1.Decoration.set(widgets);
}
function extentionsPlugin(builder) {
    return view_1.ViewPlugin.fromClass(class {
        constructor(view) {
            this.decorations = extentions(builder, view);
        }
        update(update) {
            if (update.docChanged || update.viewportChanged) {
                this.decorations = extentions(builder, update.view);
            }
        }
    }, {
        decorations: v => v.decorations,
        eventHandlers: {}
    });
}
exports.extentionsPlugin = extentionsPlugin;
