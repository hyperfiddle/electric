(ns hyperfiddle.electric.impl.ir-utils
  (:require [hyperfiddle.electric.impl.ir :as ir])
  #?(:clj (:import [clojure.lang IReduce])))

(defn reduce* [ir f init]
  (loop [ac init, todos (list ir)]
    (if (or (reduced? ac) (empty? todos))
      (unreduced ac)
      (let [[ir & todos] (seq todos)]
        (case (::ir/op ir)
          (::ir/literal ::ir/sub ::ir/global ::ir/source ::ir/def ::ir/eval ::ir/node ::ir/nop)
          (recur (f ac ir) todos)

          (::ir/target ::ir/input)
          (recur (f ac ir) (concat (::ir/deps ir) todos))

          (::ir/pub)
          (recur (f ac ir) (conj todos (::ir/inst ir) (::ir/init ir)))

          (::ir/constant ::ir/variable ::ir/output ::ir/lift)
          (recur (f ac ir) (conj todos (::ir/init ir)))

          (::ir/apply)
          (recur (f ac ir) (concat (list (::ir/fn ir)) (::ir/args ir) todos))

          (::ir/bind)
          (recur (f ac ir) (conj todos (::ir/inst ir)))

          (::ir/do)
          (recur (f ac ir) (concat (::ir/deps ir) (list (::ir/inst ir)) todos))

          #_else (throw (ex-info "what IR op is this?" {:inst ir})))))))

(defn ->reducible [ir]
  (reify IReduce
    (#?(:clj reduce :cljs -reduce) [_ f init] (reduce* ir f init))
    (#?(:clj reduce :cljs -reduce) [_ f] (reduce* ir f (f)))))

(defn unwrite [i]
  (let [ret (case (::ir/op i)
              ::ir/literal  (list 'ir/literal (::ir/value i))
              ::ir/sub      (list 'ir/sub (::ir/index i))
              ::ir/pub      (list 'ir/pub
                              (unwrite (::ir/init i)) (unwrite (::ir/inst i)))
              ::ir/constant (list 'ir/constant (unwrite (::ir/init i)))
              ::ir/target   (list 'ir/target (mapv unwrite (::ir/deps i)))
              ::ir/apply    (list* 'ir/apply (unwrite (::ir/fn i)) (map unwrite (::ir/args i)))
              ::ir/global   (list 'ir/global (::ir/name i))
              ::ir/variable (list 'ir/variable (unwrite (::ir/init i)))
              ::ir/source   (list 'ir/source)
              ::ir/input    (list 'ir/input (mapv unwrite (::ir/deps i)))
              ::ir/output   (list 'ir/output (unwrite (::ir/init i)))
              ::ir/def      (list 'ir/inject (::ir/slot i))
              ::ir/eval     (list 'ir/eval `'~(::ir/form i))
              ::ir/node     (list 'ir/node (::ir/slot i))
              ::ir/bind     (list 'ir/bind (::ir/slot i) (::ir/index i) (unwrite (::ir/inst i)))
              ::ir/lift     (list 'ir/lift (unwrite (::ir/init i)))
              ::ir/do       (list 'ir/do (mapv unwrite (::ir/deps i)) (unwrite (::ir/inst i)))
              ::ir/nop      (list 'ir/nop)
              #_else (throw (ex-info "what IR op is this?" {:inst i})))]
    (if-some [form (:hyperfiddle.electric.impl.lang/form i)]
      (list* (first ret) :form form (next ret))
      ret)))

(comment
  (run! (comp prn unwrite) (reduce conj (->reducible (ir/apply (ir/node 'x) (ir/literal 1) (ir/pub (ir/literal 2) (ir/sub 1)))))))
