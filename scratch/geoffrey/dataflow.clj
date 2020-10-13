(ns hyperfiddle.dataflow
  {:clj-kondo/config '{:lint-as {hyperfiddle.klass/deftype+ clojure.core/deftype}}}
  (:refer-clojure :exclude [update into val count apply get aget])
  (:require [clojure.string :as str]
            [hyperfiddle.array :as arr :refer [length aget apush acopy]]
            [hyperfiddle.klass :refer [deftype+]]))

;;; Protocols
;; Because Clojure’s deftype expects methods to be implemented from a protocol
(do ; for folding
  (declare on) ;; from Origin

  (defprotocol IPut
    (put [this value] [this node value]))

  (defprotocol IEnd
    (end [this]))

  (defprotocol IOutput
    (init [this])
    (off [this]))

  (defprotocol IRun
    (run [_] [_ node]))

  (defprotocol IFlow
    (-all [_ f])
    (add [_ node])
    (into [_ node f val])
    (-on-error [_ e])
    (clear [_ node])
    (update [_ node])
    (attach [_ na nb])
    (detach [_ na nb]))

  (defprotocol IPush ; boilderplate, just ignore it
    (ok [_])
    (active [_])
    (joins [_])
    (push [_])
    (link [_ node])
    (unlink [_])))

;;; NodeDef
;; GT the NodeDef values essentially define a live AST of what should be done
;; GG clojure doesn’t have ADTs, so we assume these types are grouped in some way
(deftype From [on off])
(deftype Into [f]) ; terminal node
(deftype ApplyN [f])

;;; Action
;; GG same here, no ADTs so we assume these are grouped
(deftype Val [val])
(deftype Err [e])
(deftype End [])

;;; Nodes
(deftype View [flow node])

(deftype Input [flow node]
  IPut
  (put [_ value] (put flow node (->Val value)))
  IEnd
  (end [_] (put flow node (->End))))

;;; Output
(deftype Output [flow node]
  IOutput
  (init [_] (update flow node)) ; Indicate someone is listening
  (off [_] (put flow node (->End)))) ; Indicate stopped listening

;;; Flow singleton

(def count (volatile! 0))
(defn- id [] (vswap! count inc))

;;; Push

(declare get-val get-error get-frame set-frame set-lock set-next set-prev)

(defn- node-run [frame flow this def on]
  (when-not (= frame (get-frame flow)) ; already ran this node?
    (set-frame flow (int (get-frame flow))) ; mark ran
    (when (active this) ; skip the work, nobody is listening
      (try
        (condp some [(type def)]
          #{From} nil
          #{Into, ApplyN}
          (do
            (cond
              (every? ok on) ; all dependencies are already propagated, check for ends
              ,, (condp = (type def)
                   Into   (into flow this (.f def) (get-val (aget on 0)))
                   ApplyN (put this (->Val (clojure.core/apply (.f def) (map get-val on))))
                   nil)
              (some #(.ended %) on) ; if my inbound edges are ended, end me
              ,, (put this (->End)))
            (doseq [x (filter get-error on)]
              (throw x))))
        (catch Error e
          (put this e))))))

(defn- unlink*
  [prev next this]
  (when-not (nil? prev) (set-next prev next))
  (when-not (nil? next) (set-prev next prev))
  (let [n next]
    (set-prev this nil)
    (set-next this nil)
    n))

(deftype+ Push [def ;; NodeDef<A>
                on  ;; Null<Array<Push<Dynamic>>>
                ^:get ^:set ^:unsynchronized-mutable to  ;; Null<Array<Push<Dynamic>>
                                                     ^int id ;; int
                ^:get ^:set ^:unsynchronized-mutable ^int rank ;; int
                            ^:unsynchronized-mutable ^int frame ;; int
                ^:get ^:set ^:unsynchronized-mutable ^boolean queued ;; boolean
                ^:get ^:set ^:unsynchronized-mutable val ;; Null<A>
                ^:get       ^:unsynchronized-mutable error ;; Null<Dynamic>
                            ^:unsynchronized-mutable ^boolean ended ;; boolean
                ^:get ^:set ^:unsynchronized-mutable next  ;; Null<Push<Dynamic>>
                ^:get ^:set ^:unsynchronized-mutable prev  ;; Null<Push<Dynamic>>
                ]
  IPush
  (ok [_] (and val (not ended)))
  (active [_] (and (not ended) (or to (instance? Into def))))
  (joins [_] (and on (> (length on) 1)))
  (push [this] ; Plan out the order of the computation
        (when to
          (reduce (fn [n x] ; outgoing edges run after this runs
                    (if (get-queued x)
                      n ; continue
                      (do
                        (link n x) ; link each node as part of propagation? this
                                   ; happens in a deterministic order
                        x)))
                  this
                  to)))
  (link [this node] ; Order this node next (splice here)
        (when-not (= node next) ; already in the right place
          (unlink node) ; remove it from its current order so we can put it sooner e.g. a diamond
          (set-next node next)
          (set-prev node this)
          (set! next node)))
  (unlink [this]
          (unlink* prev next this))
  IRun
  (run [this flow] ; run this layer of the applicative functor and push effect forward
    (node-run frame flow this def on))
  IPut
  (put [this a]
       (let [prev val] ; prevent loosing ref to previous val
         (condp = (type a)
           Val (set! val (.val a)) ; memory
           Err (set! error (.e a))
           End (set! ended (boolean true))
           (throw (ex-info "No matching clause "{:val val})))
         (when-not (= a prev) ; Plan this node's outbound edges (they will be run at Flow/put level)
           (push this)))))

(defn- ->Push
  ([def]
   (->Push nil def))
  ([nodes def]
   (Push. def ;; NodeDef<A>
          (when nodes (acopy nodes))  ;; Null<Array<Push<Dynamic>>>
          nil  ;; Null<Array<Push<Dynamic>>
          (id)
          0
          0
          false
          nil ;; Null<A>
          nil ;; Null<Dynamic>
          false
          nil  ;; Null<Push<Dynamic>>
          nil  ;; Null<Push<Dynamic>>
          )))

;;; Flow

(defn- flow-update
  [a this]
  (when (active a)
    (set-rank a 0)
    (doseq [x (.on a)] ; inbound edges
      (attach this x a) ; setup reverse links
      (when (> (get-rank x) (get-rank a))
        (set-rank a (get-rank x))))
    (when (joins a)
      (set-rank a (inc (get-rank a))) ; run after all dependencies
      )))

(defn- run* [lock frame queue this]
  (when-not lock
    (set-lock this true)
    (set-frame this (inc frame))
    (try ; Flow owns the queue. Flow chooses what order the nodes will run in and runs them in that order.
      (loop [rank 0] ; for ordering dependencies
        (when (< rank (length queue))
          (doseq [node (aget queue rank)]
            (run node this)) ; compute the node and plan what happens next
          (doseq [node (aget queue rank)] ; execute the plan we just planned
            (loop [node (unlink node)] ; already computed this one
              (when node ; the plan is a linked list
                (cond
                  (= (get-rank node) rank) (run node this) ; compute the node and propagate forward
                  (> (get-rank node) rank) (add this node)) ; queue it for when this rank runs
                (recur (unlink node))))) ; done with this node, continue next
          (doseq [node (aget queue rank)]
            (clear this node)) ; mark join nodes as not-ok, but why?
          #_(arr/clear (aget queue rank)) ; empty this layer of queue
          (recur (inc rank))))
      (catch Error e
        (set-lock this false) ; duplicated here as a safty net if ever `-on-error` throws
        (-on-error this e))
      (finally
        (set-lock this false)))))

(deftype+ Flow [      ^:set ^:unsynchronized-mutable ^boolean lock  ;; boolean
                ^:get ^:set ^:unsynchronized-mutable ^int frame ;; int
                queue]
  IPut
  (put [this node a]
       (put node a)
       (let [node (unlink node)]
         (when node
           (add this node))
         (run this)))
  IFlow
  (-all [this f] ; batch put in one frame
        (set-lock this true)
        (try
          (f)
          (catch Throwable e
            (-on-error this e))
          (finally
            (set-lock this false)
            (run this))))
  (add [_ node] ; queue
       (when-not (get-queued node)
         (while (<= (length queue) (get-rank node))
           (apush queue (arr/array)))
         (-> (aget queue (get-rank node))
             (apush node))
         (set-queued node true)))
  (into [_ node f val] (f val))
  (-on-error [_ e]
             (when e
               ;; this should probably throw or notify the userland.
               ;; Without breaking the execution OFC
               (prn e)))
  (clear [this node]
         (when-not (get-queued node) ; ?
           (set-queued node false)
           (when-not (some joins (get-to node)) ; join nodes only
             (set-val node nil)) ; mark not ok, but why?
           (doseq [x (.on node)] ; propagate backwards
             (clear this x))))
  (update [this a] ; rename "init"?
          (flow-update a this))
  (attach [this a b] ; set reverse links
          ;; reverse links aren’t set until someone is listening… which is now.
          (if-not (get-to a) ; first listener
            (do
              (set-to a (doto (arr/array) (apush b))) ; set the first backlink
              (condp = (type (.def a))
                From (when-let [on (.on (.def a))] ; if it’s the origin, fire lifecycle there.
                       (on))
                nil))
            (when-not (arr/contains? (get-to a) b) ; DG it’s a set ; GG It’s an array, lookup by indexOf
              (apush (get-to a) b)))
          (update this a))
  (detach [this a b]
          (when (and (get-to a) (arr/aremove (get-to a) b))
            (when-not (get-to a) ; no more left
              (condp = (type (.def a))
                From (when-let [off (.off (.def a))] ; lifecycle
                       (off))
                (doseq [x (.on a)] ; traverse backwards until we find the source
                  (detach this x a))))))
  IRun
  (run [this] ; run queue until empty
    (run* lock frame queue this)))

(defn- ->Flow [] (Flow. false 0 (arr/array)))

;;; Origin

;; public API singleton
(def ^:private main (volatile! nil))

(defn- get [] (or @main (vreset! main (->Flow))))

(defn- all [f] (-all (get) f))

(defn- input
  ([] (input nil))
  ([f] ; `f` is the lifecycle fn
   (let [end*  (volatile! nil)
         input (volatile! nil)
         on    #(and f (vreset! end* (f))) ; f can return end continuation
         off   #(when-not (nil? @end*)
                  (end @input)
                  (vreset! end* nil)) ; lifecycle state
         ]
     (vreset! input (->Input (get) (->Push (->From on off)))))))

(defn- on [view f] ; terminal node
  ;; f is an effect callback
  (doto (->Output (get) (->Push (arr/array (.node view)) (->Into f)))
    init ; propagate that someone is listening
    ))

(defn- apply [ns f]
  ;; set the inbound edges
  (->View (get) (->Push (acopy (map #(.node %) ns))
                        (->ApplyN f))))

;;; ----------

(comment
  (do
    (def email-input (input))
    (def parts (apply [email-input] #(str/split % #"@")))
    (def name (apply [parts] first))
    (def domain (apply [parts] last))
    (def reconstructed (apply [name domain] #(str %1 "@" %2)))
    (def printer (on reconstructed println))
    (put email-input "bob@example.com")
    (put email-input "alice@example.com")
    (off printer)
    (put email-input "john@example.com")
    (def printer (on reconstructed println))
    (put email-input "john2@example.com")
    (off printer)
    ))
