(ns hyperfiddle.electric.impl.runtime-de
  (:require [hyperfiddle.incseq :as i]
            [missionary.core :as m])
  #?(:clj (:import (clojure.lang IFn IDeref))))

(deftype Peer [step done defs ^objects state]
  IFn
  (#?(:clj invoke :cljs -invoke) [_]
    (prn :cancel-peer)

    )
  IDeref
  (#?(:clj deref :cljs -deref) [_]
    (prn :transfer-peer)

    ))

(defn invariant [x] (m/cp x))

(deftype Pure [values]
  IFn
  (#?(:clj invoke :cljs -invoke) [_ step done]
    ((apply i/fixed (map invariant values)) step done)))

(def pure (comp ->Pure vector))

(defn error [^String msg]
  #?(:clj (Error. msg)
     :cljs (js/Error. msg)))

(deftype Failer [done e]
  IFn
  (#?(:clj invoke :cljs -invoke) [_])
  IDeref
  (#?(:clj deref :cljs -deref) [_]
    (done) (throw e)))

(deftype Unbound [k]
  IFn
  (#?(:clj invoke :cljs -invoke) [_ step done]
    (step) (->Failer done (error (str "Unbound electric var lookup - " (pr-str k))))))

(deftype Cdef [frees nodes calls result build])

(def cdef ->Cdef)

(deftype Ctor [^Peer peer key idx ^objects free env])

(defn bind [^Ctor ctor k v]
  (->Ctor (.-peer ctor) (.-key ctor) (.-idx ctor) (.-free ctor)
    (assoc (.-env ctor) k v)))

(defn bind-args [^Ctor ctor & args]
  (reduce (partial apply bind) ctor (eduction (map-indexed vector) args)))

(defn ctor-peer
  "Returns the peer of given constructor."
  {:tag Peer}
  [^Ctor ctor]
  (.-peer ctor))

(defn ctor-cdef
  "Returns the cdef of given constructor."
  {:tag Cdef}
  [^Ctor ctor]
  (((.-defs (ctor-peer ctor)) (.-key ctor)) (.-idx ctor)))

;; TODO local?
(deftype Frame [parent call-id ^Ctor ctor ^objects signals]
  IFn
  (#?(:clj invoke :cljs -invoke) [_ step done]
    (let [cdef (ctor-cdef ctor)]
      ((aget signals
         (+ (count (.-nodes cdef))
           (count (.-calls cdef))))
       step done))))

(defn frame-ctor
  "Returns the constructor of given frame."
  {:tag Ctor}
  [^Frame frame]
  (.-ctor frame))

(deftype Node [^Frame frame id]
  IFn
  (#?(:clj invoke :cljs -invoke) [_ step done]
    ((aget (.-signals frame) id) step done)))

(deftype Call [^Frame frame id]
  IFn
  (#?(:clj invoke :cljs -invoke) [_ step done]
    (let [cdef (ctor-cdef (frame-ctor frame))]
      ((aget (.-signals frame)
         (+ (count (.-nodes cdef)) id))
       step done))))

(defn make-frame [^Frame frame call-id ctor]
  (let [cdef (ctor-cdef ctor)
        length (+ (count (.-nodes cdef))
                 (count (.-calls cdef)))
        signals (object-array (inc length))
        frame (->Frame frame call-id ctor signals)]
    (aset signals length ((.-build cdef) frame)) frame))

(defn define-node
  "Defines signals node id for given frame."
  [^Frame frame id incseq]
  (let [signals (.-signals frame)]
    (when-not (nil? (aget signals id))
      (throw (error "Can't redefine signal node.")))
    (aset signals id (m/signal i/combine incseq)) nil))

(defn define-call
  "Defines call site id for given frame."
  [^Frame frame id incseq]
  (let [signals (.-signals frame)
        slot (-> (.-nodes (ctor-cdef (frame-ctor frame)))
               (count) (+ id))]
    (when-not (nil? (aget signals slot))
      (throw (error "Can't redefine call site.")))
    (aset signals slot
      (m/signal i/combine
        (i/latest-product
          (fn [ctor]
            (when-not (instance? Ctor ctor)
              (throw (error (str "Not a constructor - " (pr-str ctor)))))
            (when-not (identical? (ctor-peer (frame-ctor frame)) (ctor-peer ctor))
              (throw (error "Can't call foreign constructor.")))
            (make-frame frame id ctor)) incseq))) nil))

(defn define-free
  "Defines free variable id for given constructor."
  [^Ctor ctor id incseq]
  (let [free (.-free ctor)]
    (when-not (nil? (aget free id))
      (throw (error "Can't redefine free variable.")))
    (aset free id incseq) nil))

(defn frame-parent
  "Returns the parent frame of given frame if not root, nil otherwise."
  {:tag Frame}
  [^Frame frame]
  (.-parent frame))

(defn frame-call-id
  "Returns the call id of given frame."
  [^Frame frame]
  (.-call-id frame))

(defn frame-call-count
  "Returns the call count of given frame."
  [^Frame frame]
  (.-calls (ctor-cdef (frame-ctor frame))))

(defn lookup
  "Returns the value associated with given key in the dynamic environment of given frame."
  ([^Frame frame key]
   (lookup frame key (->Unbound key)))
  ([^Frame frame key nf]
   (loop [frame frame]
     (if-some [s ((.-env (frame-ctor frame)) key)]
       s (if-some [p (frame-parent frame)]
           (recur p) nf)))))

(defn make-ctor
  "Returns a fresh constructor for cdef coordinates key and idx."
  [^Frame frame key idx]
  (let [^Peer peer (ctor-peer (frame-ctor frame))
        ^Cdef cdef (((.-defs peer) key) idx)]
    (->Ctor peer key idx (object-array (.-frees cdef)) {})))

(defn node
  "Returns the signal node id for given frame."
  [^Frame frame id]
  (->Node frame id))

(defn free
  "Returns the free variable id for given frame."
  [^Frame frame id]
  (aget (.-free (frame-ctor frame)) id))

(defn call
  "Returns the call site id for given frame."
  [^Frame frame id]
  (->Call frame id))

(def join i/latest-concat)

(def ap (partial i/latest-product (fn [f & args] (apply f args))))

(def fixed-signals (comp (partial m/signal i/combine) i/fixed))

(defn drain [incseq]
  (m/ap
    (m/amb (i/empty-diff 0)
      (do (m/?> incseq) (m/amb)))))

(def peer-slot-input 0)
(def peer-slot-store 1)
(def peer-slots 2)

(defn context-input-notify [^Peer peer done?]
  ;; TODO
  )

(defn peer "
Returns a peer definition from given definitions and main key.
" [defs main & args]
  (fn [msgs]
    (fn [step done]
      (let [state (object-array peer-slots)
            peer (->Peer step done defs state)]
        (aset state peer-slot-store {})
        (aset state peer-slot-input
          ((m/stream (m/observe msgs))
           #(context-input-notify peer false)
           #(context-input-notify peer true)))

        ((->> args
           (into {} (map-indexed (fn [i arg] [i (pure arg)])))
           (->Ctor peer main 0 (object-array 0))
           (make-frame nil 0)
           (m/signal i/combine)
           (m/reduce (fn [_ x] (prn :output x)) nil))
         #(prn :success %) #(prn :failure %))

        peer))))

;; local only
(defn root-frame [defs main]
  (->> (->Ctor (->Peer nil nil defs nil)
         main 0 (object-array 0) {})
    (make-frame nil 0)
    (m/signal i/combine)))

(def ^{::type ::node, :doc "for loop/recur impl"} rec)

#?(:clj
   (def arg-sym
     (map (comp symbol
            (partial intern *ns*)
            (fn [i]
              (with-meta (symbol (str "%" i))
                {::type ::node})))
       (range))))
