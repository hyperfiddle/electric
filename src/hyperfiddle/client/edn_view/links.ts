import {EditorView, Decoration, WidgetType, ViewUpdate, ViewPlugin, DecorationSet, Range} from "@codemirror/view"
import {syntaxTree} from "@codemirror/language"

type Sexp = any;

interface Link{
    href: Sexp;
    value: string;
}

type LinkParser = (s : string) => Link;
type SexpEncoder = (a : Sexp) => string;

class LinkWidget extends WidgetType {
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

function links(parseLink : LinkParser, encodeSexp : SexpEncoder,  view: EditorView) {
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
                let text = view.state.doc.sliceString(from, to); // get link edn text
                let link = parseLink(text);
                let deco = Decoration.replace({
                    widget: new LinkWidget(encodeSexp(link.href), link.value),
                    inclusive: true
                })
                widgets.push(deco.range(prev_to, to)) // push it as left as possible, next to the key
            }
        } while(cursor.next());
    }
    return Decoration.set(widgets)
}

export function linksPlugin (parseLink : LinkParser, encodeSexp : SexpEncoder) {
    return ViewPlugin.fromClass(class {
        decorations: DecorationSet

        constructor(view: EditorView) {
            this.decorations = links(parseLink, encodeSexp, view);
        }

        update(update: ViewUpdate) {
            if (update.docChanged || update.viewportChanged){
                this.decorations = links(parseLink, encodeSexp, update.view);
            }
        }
    }, {
        decorations: v => v.decorations
    })
}
