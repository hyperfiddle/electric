import {WidgetType} from "@codemirror/view"

type Sexp = any;

export interface Link{
    href: Sexp;
    value: string;
}

export type SexpEncoder = (a : Sexp) => string;

export class LinkWidget extends WidgetType {
    constructor(readonly href: string,
                readonly value: string) {
        super();
    }

    eq(other: LinkWidget) { return other.href == this.href }

    toDOM() {
        let wrap = document.createElement("span");
        wrap.style.marginLeft = "0.5rem";
        let box = wrap.appendChild(document.createElement("a"));
        box.href = this.href;
        box.text = this.value;
        return wrap;
    }

    ignoreEvent() { return false }
}


