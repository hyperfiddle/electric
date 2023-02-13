import {EditorView, Decoration, ViewUpdate, ViewPlugin, DecorationSet, Range, WidgetType} from "@codemirror/view"
import {syntaxTree} from "@codemirror/language"
import {SexpEncoder, Link, LinkWidget} from "./links";

type UUID = string;

// TODO move
interface Input{
    id: UUID,
    value: string,
    onChange: (event: Event) => void
};

export class InputWidget extends WidgetType {
    constructor(readonly id: UUID,
                readonly value: string,
                readonly onChange: (event: Event) => void) {
        super();
        this.value = value;
        this.onChange = onChange;
    }

    eq(other: InputWidget) { return other.value == this.value }

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

    onClick(this: HTMLInputElement, ev : MouseEvent){
        console.log("Click", ev);
        let value = prompt();
        if (value) this.value = value;
        this.dispatchEvent(new Event("change"));
        return true;
    }

    updateDOM(input : HTMLInputElement){
        input.value = this.value;
        return true;
    }

    ignoreEvent() { return false }
}



type Extention = Link | Input;
type ExtentionType = "Link" | "Input";
type ExtentionParser = (s : string) => [ExtentionType, Extention];


export class WidgetBuilder{
    constructor(readonly encodeSexp : SexpEncoder,
                readonly parseTaggedValue : ExtentionParser){}
}

function makeWidget(builder : WidgetBuilder, text : string){
    let [type, extention] = builder.parseTaggedValue(text);
    switch (type){
        case "Link": {
            extention = extention as Link;
            return new LinkWidget(builder.encodeSexp(extention.href), extention.value);
        }
        case "Input": {
            extention = extention as Input;
            console.log(extention);
            return new InputWidget(extention.id, extention.value, extention.onChange)
        };
    };
}

function extentions(builder: WidgetBuilder, view: EditorView) {
    let widgets : Range<Decoration>[] = []
    for (let {from, to} of view.visibleRanges) {
        let tree = syntaxTree(view.state);
        let cursor = tree.cursor();
        do{
            let {type, from, to} = cursor;
            if (type.name == "Constructor") {
                cursor.prevSibling();    // go back one node to get the key position
                let prev_to = cursor.to;
                cursor.nextSibling();    // restore cursor position.
                let deco = Decoration.replace({
                    widget: makeWidget(builder, view.state.doc.sliceString(from, to)),
                    inclusive: true
                });
                widgets.push(deco.range(prev_to, to)) // push it as left as possible, next to the key
            }
        } while(cursor.next());
    }
    return Decoration.set(widgets)
}

export function extentionsPlugin (builder: WidgetBuilder) {
    return ViewPlugin.fromClass(class {
        decorations: DecorationSet

        constructor(view: EditorView) {
            this.decorations = extentions(builder, view);
        }

        update(update: ViewUpdate) {
            if (update.docChanged || update.viewportChanged){
                this.decorations = extentions(builder, update.view);
            }
        }
    }, {
        decorations: v => v.decorations,
        eventHandlers: {}
    })
}
