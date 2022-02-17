"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.LinkWidget = void 0;
const view_1 = require("@codemirror/view");
class LinkWidget extends view_1.WidgetType {
    constructor(href, value) {
        super();
        this.href = href;
        this.value = value;
    }
    eq(other) { return other.href == this.href; }
    toDOM() {
        let wrap = document.createElement("span");
        wrap.style.marginLeft = "0.5rem";
        let box = wrap.appendChild(document.createElement("a"));
        box.href = this.href;
        box.text = this.value;
        return wrap;
    }
    ignoreEvent() { return false; }
}
exports.LinkWidget = LinkWidget;
