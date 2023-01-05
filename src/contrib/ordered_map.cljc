(ns contrib.ordered-map
  (:require [hyperfiddle.rcf :refer [tests]]))

(defn remove-one [v coll]
  (let [index (count (take-while (complement (partial = v)) coll))]
    (if (= index (count coll))
      coll
      (into (subvec coll 0 index) (subvec coll (inc index))))))

#?(:cljs (declare PersistentOrderedMap))

#?(:cljs
   (deftype OrderedMapIter [m ^js order-iter]
     Object
     (hasNext [_] (.hasNext order-iter))
     (next [_] (let [k (.next order-iter)] (MapEntry. k (-lookup m k) nil)))
     (remove [_] (js/Error. "Unsupported operation")))

   :clj
   (deftype OrderedMapIter [m order-iter]
     java.util.Iterator
     (hasNext [_] (.hasNext order-iter))
     (next [_] (let [k (.next order-iter)] (clojure.lang.MapEntry. k (get m k))))
     (remove [_])))

#?(:cljs
   (deftype TransientOrderedMap [^:mutable ^boolean edit
                                 ^:mutable root
                                 ^:mutable order]
     Object
     (conj! [tcoll o]
       (if edit
         (cond
           (map-entry? o)
           (.assoc! tcoll (key o) (val o))
           (vector? o)
           (.assoc! tcoll (o 0) (o 1))
           :else
           (loop [es (seq o) tcoll tcoll]
             (if-let [e (first es)]
               (recur (next es)
                      (-assoc! tcoll (key e) (val e)))
               tcoll)))
         (throw (js/Error. "conj! after persistent"))))

     ICounted
     (-count [coll] (if edit (count root) (throw (js/Error. "count after persistent!"))))

     ILookup
     (-lookup [tcoll k] (-lookup root k))
     (-lookup [tcoll k not-found] (-lookup root k not-found))

     ITransientCollection
     (-conj! [tcoll val] (.conj! tcoll val))
     (-persistent! [tcoll]
       (if edit
         (do (set! edit nil)
             (PersistentOrderedMap. root order))
         (throw (js/Error. "persistent! called twice"))))

     ITransientAssociative
     (-assoc! [tcoll k v]
       (if edit
         (do (-assoc! root k v)
             (-conj! order k)
             tcoll)
         (throw (js/Error. "assoc! after persistent!"))))

     ITransientMap
     (-dissoc! [tcoll k]
       (if edit
         (if-not (contains? root k)
           tcoll
           (do (-dissoc! root k)
               (set! order (transient (remove-one k (persistent! order))))))
         (throw (js/Error. "dissoc! after persistent!"))))

     IFn
     (-invoke [tcoll key] (-lookup tcoll key))
     (-invoke [tcoll key not-found] (-lookup tcoll key not-found))))

#?(:cljs
   (deftype PersistentOrderedMap [backing-map order]
     Object
     (toString [coll]
       (str "{" (apply str
                       (drop-last
                         (interleave (map pr-str order)
                                     (repeat \space)
                                     (map #(pr-str (or (get backing-map %) nil)) order)
                                     (repeat ", ")))) "}"))
     (equiv [this other] (-equiv this other))

     ICloneable
     (-clone [_] (PersistentOrderedMap. backing-map order))

     IIterable
     (-iterator [coll] (OrderedMapIter. backing-map (-iterator order)))

     IWithMeta
     (-with-meta [coll new-meta]
       (if (identical? new-meta meta)
         coll (PersistentOrderedMap. (with-meta backing-map new-meta) order)))

     IMeta
     (-meta [coll] (meta backing-map))

     ICollection
     (-conj [coll entry]
       (if (vector? entry)
         (-assoc coll (-nth entry 0) (-nth entry 1))
         (loop [ret coll es (seq entry)]
           (if (nil? es)
             ret
             (let [e (first es)]
               (if (vector? e)
                 (recur (-assoc ret (-nth e 0) (-nth e 1))
                        (next es))
                 (throw (js/Error. "conj on a map takes map entries or seqables of map entries"))))))))

     IEmptyableCollection
     (-empty [coll] (-with-meta (.-EMPTY PersistentOrderedMap) meta))

     IEquiv
     (-equiv [coll other] (equiv-map coll other))

     IHash
     (-hash [coll] (hash backing-map))

     ISeqable
     (-seq [coll]
       (when-not (empty? backing-map)
         (map (fn [k] (MapEntry. k (-lookup coll k) nil)) order)))

     ICounted
     (-count [coll] (count order))

     ILookup
     (-lookup [coll k] (-lookup coll k nil))
     (-lookup [coll k not-found] (-lookup backing-map k not-found))

     IAssociative
     (-assoc [coll k v]
       (if (identical? (-lookup backing-map k) v)
         coll
         (PersistentOrderedMap. (-assoc backing-map k v) (conj order k))))

     (-contains-key? [coll k] (-contains-key? backing-map k))

     IFind
     (-find [coll k] (-find backing-map k))

     IMap
     (-dissoc [coll k]
       (if-not (-contains-key? backing-map k)
         coll
         (PersistentOrderedMap. (dissoc backing-map k) (remove-one k order))))

     IKVReduce
     (-kv-reduce [coll f init]
       (reduce (fn [acc [k v]] (apply f [acc k v])) init coll))

     IFn
     (-invoke [coll k] (-lookup coll k))
     (-invoke [coll k not-found] (-lookup coll k not-found))

     IEditableCollection
     (-as-transient [coll] (TransientOrderedMap. (js-obj) (transient backing-map) (transient order))))

   :clj
   (deftype PersistentOrderedMap [backing-map order]
     java.lang.Object
     (toString [_]
       (format "{%s}"
               (apply str
                      (drop-last
                        (interleave (map pr-str order)
                                    (repeat \space)
                                    (map #(pr-str (or (get backing-map %) nil)) order)
                                    (repeat ", "))))))

     clojure.lang.IPersistentMap
     (assoc [_ k v]
       (if (contains? backing-map k)
         (PersistentOrderedMap. (.assoc backing-map k v) order)
         (PersistentOrderedMap. (.assoc backing-map k v) (conj order k))))
     (assocEx [_ k v] (PersistentOrderedMap. (.assocEx backing-map k v) order))
     (without [coll k]
       (if-not (contains? backing-map k)
         coll (PersistentOrderedMap. (dissoc backing-map k) (remove-one k order))))

     clojure.lang.Associative
     (containsKey [_ k] (.containsKey backing-map k))
     (entryAt [_ k] (.entryAt backing-map k))

     java.lang.Iterable
     (iterator [_] (OrderedMapIter. backing-map (.iterator order)))

     clojure.lang.IPersistentCollection
     (count [_] (.count order))
     (cons [_ [k v]] (PersistentOrderedMap. (.cons backing-map [k v]) (.cons order k)))
     (empty [_] (PersistentOrderedMap. (empty backing-map) (empty order)))
     (equiv [_ o] (.equiv backing-map (.backing-map o)))

     clojure.lang.Seqable
     (seq [_] (when-not (empty? backing-map) (map #(.entryAt backing-map %) order)))

     clojure.lang.IKVReduce
     (kvreduce [coll f init] (reduce (fn [acc [k v]] (apply f [acc k v])) init coll))

     clojure.lang.ILookup
     (valAt [_ k] (.valAt backing-map k))
     (valAt [_ k not-found] (.valAt backing-map k not-found))

     clojure.lang.IObj
     (withMeta [_this m] (PersistentOrderedMap. (with-meta backing-map m) order))

     clojure.lang.IMeta
     (meta [_] (meta backing-map))))

#?(:clj (defmethod print-method PersistentOrderedMap [v ^java.io.Writer w] (.write w (.toString v))))

#?(:cljs (set! (.-EMPTY PersistentOrderedMap) (PersistentOrderedMap. {} []))
   :clj (do (-> (create-ns 'contrib.orderedmap.PersistentOrderedMap)
                (intern 'EMPTY (PersistentOrderedMap. {} [])))))

(defn ordered-map [& keyvals]
  (loop [in (seq keyvals)
         out-map (transient {})
         out-order (transient [])]
    (if in
      (recur (nnext in) (assoc! out-map (first in) (second in)) (conj! out-order (first in)))
      (let [om (persistent! out-map)
            oo (persistent! out-order)]
        (PersistentOrderedMap. om oo)))))

(defn with-order [backing-map order] (PersistentOrderedMap. backing-map order))

(tests "Ordered Map Construct"
  (seq (ordered-map)) := nil
  (def m (apply ordered-map (interleave (range 1 30) (range 1 30))))
  (get m 30) := nil
  (get m 1) := 1
  (get m nil) := nil
  (keys m) := (range 1 30)
  (vals m) := (range 1 30)
  (seq m) := (map vector (range 1 30) (range 1 30)))


(tests "Ordered Map Assoc"
  (def m (apply ordered-map (interleave (range 1 30) (range 1 30))))
  (get (assoc m 30 30) 30) := 30
  (get (assoc m 1 :a) 1) := :a
  (first m) := [1 1]
  (get (assoc m nil 1) nil) := 1
  (keys (assoc m 30 30)) := (range 1 31)
  (vals (assoc m 30 30)) := (range 1 31)
  (seq (assoc m 30 30)) := (map vector (range 1 31) (range 1 31)))

(tests "Ordered Map Dissoc"
  (def m (apply ordered-map (interleave (range 1 30) (range 1 30))))
  (get (dissoc m 29) 29) := nil
  (get (dissoc m 1) 1) := nil
  (get (dissoc (assoc m nil :asdf) nil) nil) := nil
  (keys (dissoc m 29)) := (range 1 29)
  (vals (dissoc m 29)) := (range 1 29)
  (seq (dissoc m 29)) := (map vector (range 1 29) (range 1 29)))

(tests "Ordered Map reduce-kv"
  (def m (apply ordered-map (interleave (range 1 30) (range 1 30))))
  (reduce-kv (fn [acc k v] (conj acc k)) [] m) := (vec (range 1 30)))

#?(:clj
   (tests "Ordered Map Iterator"
     (def m (apply ordered-map (interleave (range 1 30) (range 1 30))))
     (def iter (.iterator m))

     (reduce (fn [acc kv]
               (if (and (.hasNext iter)
                        (= (.next iter) kv))
                 true (reduced false)))
             true m)
     := true))
