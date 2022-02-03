(ns dustin.render1)


(comment

  (defn row-
    [<ctx> columns]
    (|> [<id> (df/map context/entity <ctx>)
         <cells> (->> <ctx>
                   (df/map context/focus*)
                   (df/map (fn [ctxs] (select-keys ctxs columns))))]
      (into
        [:tr {:key @<id>}]
        (map (fn [<ctx>] [cell- <ctx>])
          (vals (df/sequence <cells>))))))

  (defn table-
    [<ctx>]
    (|> [<columns> (->> <ctx> (df/map columns))
         <empty?> (->> <ctx> (df/map (comp empty? ::context/value)))
         <rows> (->> <ctx> (df/map context/focus*))]
      (df/consume println <ctx>)

      [:table
       [:thead {}
        (->> @<columns>
          (map (fn [v] [:td (str v)]))
          (apply (fn [tds] (into [:tr {}] tds))))]
       (when-not @<empty?>
         (into
           [:tbody {}]
           (map (fn [<ctx>] [row- <ctx> @<columns>]) (df/sequence <rows>))))]))

  (def table-
    (let [<columns> (->> <ctx> (df/map columns))
          <empty?> (->> <ctx> (df/map (comp empty? ::context/value)))
          <rows> (->> <ctx> (df/map context/focus*))]
      (reagent/create-class
        {:render ~@body
         :unmount #()
         :mount #()})))

  (defn table [<ctx>]
    (via
      (let [[columns, empty?, <row-ctxs>] ~[~(columns ~<ctx>)
                                            ~(empty? ~(::context/value ~<ctx>))
                                            (context/focus* ~<ctx>)]]
        [:table
         [:thead {}
          (->> columns
            (map (fn [v] [:td (str v)]))
            (apply (fn [tds] (into [:tr {}] tds))))]
         (when-not empty?
           (into
             [:tbody {}]
             (map (fn [<ctx>] (row- ~<ctx> columns)) (df/unsequence <row-ctxs>))))])))




  (eval-via monad    ; rewrite AST into "async/await" monadic forms
    (let [vdomStream (f ~database ~localStorage ~domInteractions)]
      (ReactDOM/render! (.getElementById "#root") ~vdomStream)))

  (def monad (reify
               Monad (bind [mv f] ...)
               Applicative (apply [af av] ...)
               Functor (fmap [f mv]) ...))





  (fmap #(ReactDom/render (.getElementByID "#root")) vdomStream)



  (eval (unquote-via '(println ~(inc 1)) identity))



  (do* (f ~x))
  (async (f (await x)))


  (do* (g ~(f ~x ~y)))
  (fmap g (fapply f x y))

  ;  g(await f(await x, await y)

  (alet [x' x
         y' y
         f' (f x' y')]
    (g f'))


  (do* (g ~(f ~x ~y)))
  ; vs
  (alet [x' x
         y' y
         f' f (x' y')]
    (g f'))


  (mlet [[x' y'] (alet [x' x y' y] [x' y'])
         f' (f x' y')]
    (g f'))


  (defn bind* [& args])


  (let [x 1 y 2 f inc g inc]
    (do* (g ~(f ~x ~y))))

  := (bind*
       x y [x' y']
       (f x' y') [f']
       (g f'))

  ; flipped to be more like mlet
  := (bind*
       [x' y'] x y                                          ; apply level
       [f'] (f x' y')                                       ; apply level
       (g f'))

  ; which is basically
  (mlet [[x' y'] (alet [x' x y' y] (pure [x y]))
         f' (f x' y')]
    (g f'))

  (require '[contrib.expr :refer :all])
  (require '[backtick :refer [template]])


  (defmacro do* [& xs]
    (letfn
      [(denote [binds x]
         (cond (form? x)
           (cond (= (first x) `unquote)
             (let [[_ v] x
                   [binds v] (denote binds v)]
               (template [(~@binds ~v [v#]) v#]))
             ()
             (reduce (fn [[binds acc] x']
                       (let [[binds x'] (denote binds x')]
                         (template [~binds (~@acc ~x')])))
               [binds ()] x))
           ()
           [binds x]))]
      (template (~@(denote '(bind*) xs)))))

  ; Make it work on streams, vdom in the stream
  ; React is only used for reconciler
  (ReactDOM/render (.getElementById "#root") ~vdomStream)
  )
