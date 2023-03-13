(ns contrib.element-syntax
  "Experimental electric-dom syntax contributed by @tatut. Unsupported, expect breaking changes,
  use at your own risk!"
  #?(:cljs (:require-macros [contrib.element-syntax :refer [<%]]))
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

(comment

  ; Gotchas:
  ; 1. (dom/style {:background-color "yellow"}) -- can be fixed
  ; 2. (dom/text x) needed for non-literal x -- hard to fix
  ; 3. overloaded semantics for string literals. What if we want to pass them out the return channel?

  (ns user.demo-4-chat-extended
    (:require
      contrib.str
      [contrib.element-syntax :refer [<%]]
      [hyperfiddle.electric :as e]
      [hyperfiddle.electric-dom2 :as dom]))

  #?(:clj (defonce !msgs (atom '())))
  (e/def msgs (e/server (reverse (e/watch !msgs))))

  #?(:clj (defonce !present (atom {}))) ; session-id -> user
  (e/def present (e/server (e/watch !present)))

  (e/defn Chat [username]
    (<% :p "Present: ")
    (<% :ul
      (e/server
        (e/for [[session-id username] present]
          (e/client
            (<% :li (dom/text username (str " (session-id: " session-id ")")))))))

    (<% :hr)
    (<% :ul
      (e/server
        (e/for [{:keys [::username ::msg]} msgs]
          (e/client
            (<% :li (<% :strong (dom/text username)) " " (dom/text msg))))))

    (<% :input
      {:props {:placeholder "Type a message"}
       :on-keydown (e/fn [e]
                     (when (= "Enter" (.-key e))
                       (when-some [v (contrib.str/empty->nil (-> e .-target .-value))]
                         (dom/style {:background-color "yellow"})
                         (e/server (swap! !msgs #(cons {::username username ::msg v} (take 9 %))))
                         (set! (.-value dom/node) ""))))}))

  (e/defn App []
    (e/client
      (<% :h1 "Multiplayer chat app with auth and presence")
      (let [session-id (e/server (get-in e/*http-request* [:headers "sec-websocket-key"]))
            username (e/server (get-in e/*http-request* [:cookies "username" :value]))]
        (if-not (some? username)
          (do (<% :p "Set login cookie here: " (<% :a {:href "/auth"} "/auth") " (blank password)")
              (<% :p "Example HTTP endpoint is here: "
                (<% :a {:href "https://github.com/hyperfiddle/electric/blob/master/src/hyperfiddle/electric_jetty_server.clj"}
                  "electric_jetty_server.clj")))
          (do
            (e/server
              (swap! !present assoc session-id username)
              (e/on-unmount #(swap! !present dissoc session-id)))
            (<% :p "Authenticated as: " (dom/text username))
            (Chat. username))))))

  )