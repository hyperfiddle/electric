(ns contrib.element-syntax
  "Macro <% for an alternative shorthand element syntax."
  (:require [clojure.string :as str]
            [hyperfiddle.electric-dom2 :as dom]))

(defn element-class-names [elt]
  (map second (re-seq #"\.([^.#]+)" (name elt))))

(defn element-name [elt]
  (keyword (second (re-find #"^([^.#]+)" (name elt)))))

(defmacro <%
  "Create DOM element by giving a keyword name, optional attributes map and content.

  The keyword name can include hiccup-like class definitions. For example the keyword
  `:div.listing.text-xl` will create a div element with two classes (listing and text-xl).

  If the second parameter is a compile time map, it is expanded into an `dom/props` call.
  Any keys in the map that start with `:on-` will be turned into `dom/on` calls to register
  event handlers. If both the attributes map and the keyword contain classes, they are
  combined (keyword classes first).

  The rest of the parameters are the contents and other code passed into the body of
  `dom/element`. Compile time strings are wrapped `dom/text`, anything else is passed
  through as is.

  Full example:
  ```
  (<% :ul.my-listing
    (e/for [item (get-items)]
     (<% :li.my-listing.list-item
        {:on-click (e/fn [_] (js/alert (str \"you clicked: \" (:name item))))
         :class (when (= :warning (:type item)) \"red\")}
        (dom/text (:name item)))))
  ```

  "
  [elt & attrs-and-content]
  (let [[attrs content] (if (map? (first attrs-and-content))
                          [(first attrs-and-content) (rest attrs-and-content)]
                          [nil attrs-and-content])
        handlers (keep (fn [[key val]]
                         (when (str/starts-with? (str key) ":on-")
                           [(subs (str key) 4) val]))
                       (seq attrs))
        classes (element-class-names elt)
        attrs (cond-> (apply dissoc attrs (map first handlers))
                (seq classes)
                (update ::dom/class (fn [class]
                                      (let [class-names (str/join " " classes)]
                                        (if class
                                          `(str ~(str class-names " ") ~class)
                                          class-names)))))
        e (element-name elt)]
    `(dom/element
      ~e
      ~(when attrs
         `(dom/props ~attrs))
      ~@(for [[h b] handlers]
          `(dom/on ~h ~b))
      ~@(for [c content]
          (if (string? c)
            `(dom/text ~c)
            c)))))
