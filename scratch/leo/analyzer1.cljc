(ns leo.analyzer1
  (:require
    [missionary.core :as m]
    [dustin.trace28 :refer [let-bindings translate-binding]]
    [minitest :refer [tests]]
    [clojure.string :as str]))

(defn analyze-let [ast {:keys [passives replayer-sym]}]
  (let [bindings     (let-bindings ast)
        syms         (distinct (map first bindings)) ;; shadowing
        new-bindings (mapcat (partial translate-binding passives replayer-sym) bindings)]
    (comment TODO body)
    {:syms syms
     :new-bindings new-bindings}))                          ;; change this form

(defn analyze-for [[key-fn [sym form] body] {:keys []}]

  )

(defn analyze-ast
  ([ast] (analyze-ast ast {}))
  ([ast opts]
   (case (first ast)
     :let (analyze-let ast opts)
     :for (analyze-for ast opts))))

(tests
  (def !a (atom nil))
  (def !b (atom nil))

  (analyze-ast
    [:let ['>a `(m/watch !a)
           '>b `(m/watch !b)
           '>c [:fmap `+ '>a '>b]]])
  :=
  {:paths [{:type :user :symbol '>a :form `(m/watch !a)}
           {:type :user :symbol '>b :form `(m/watch !b)}
           {:type :user :symbol '>c :form [:fmap `+ '>a '>b]}]}

  (def !c (atom #{}))

  (analyze-ast
    [:for ['>a :id `(m/watch !c)] (:name '>a)])
  :=
  {:paths [{:type :user :form `(m/watch !c)}
           {:type :diff :target 0 :symbol '>a :key-fn :id}
           {:type :user :parent 1 :form '(:name >a)}]}

  (reset! !c #{{:id :a :name "alice"}
               {:id :b :name "bob"}})

  {[0] #{}
   [1] [#{} #{}]}

  {[0]    #{{:id :a :name "alice"}
            {:id :b :name "bob"}}
   [1]    [#{:a :b} #{}]
   [1 :a] {:id :a :name "alice"}
   [1 :b] {:id :b :name "bob"}
   [2 :a] "alice"
   [2 :b] "bob"}


  )