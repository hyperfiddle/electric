(declare focus, focus*, focus-in)
(defn eldest
  [ctx key]
  (when ctx
    (or (eldest (::parent ctx) key)
      (get ctx key))))
(defn youngest
  [ctx key]
  (when ctx
    (if (contains? ctx key)
      (get ctx key)
      (recur (::parent ctx) key))))
(defn ancestory
  [ctx key]
  (loop [ctx ctx
         hier []]
    (if-let [val (key ctx)]
      (recur (::parent ctx) (conj hier val))
      hier)))
(defn identifier?
  [{:keys [::schema ::attribute]}]
  (or
    (= :db/id attribute)
    (= :db/ident attribute)
    (-> schema (get attribute) :db/unqiue (= :db.unique/identity))))
(defn entity
  [ctx]
  (condp = (::level ctx)
    :find/rel-entry (some entity (focus* ctx))
    :find/tuple (some entity (vals (focus* ctx)))
    :find/scalar (when (identifier? ctx) (::value ctx))))
(defn with-entity
  [ctx]
  (assoc ctx ::entity (entity ctx)))
(defmulti focus
  (fn [ctx element]
    [(::level ctx)
     (cond
       (int? element) :focus/index
       (keyword? element) :focus/attribute)]))
(defmethod focus [:find/rel :focus/index]
  [ctx index]
  {::value (-> ctx ::value (nth index))
   ::level :find/rel-entry
   ::parent ctx})
(defmethod focus [:find/coll :focus/index]
  [ctx index]
  (with-entity
    {::value (-> ctx ::value (nth index))
     ::level :find/tuple
     ::parent ctx}))
(defmethod focus [:find/tuple :focus/attribute]
  [{:keys [::value ::schema] :as ctx} attribute]
  {::value (-> value (get attribute))
   ::attribute attribute
   ::level :find/scalar
   ::schema (->> schema (get attribute))
   ::parent ctx})
(defmethod focus :default
  [ctx element]
  (throw
    (ex-info "Invalid focus level / element combination"
      {::level (::level ctx)
       ::element (cond
                   (int? element) :focus/index
                   (keyword? element) :focus/attribute)})))
(defn focus-in
  [ctx path]
  (loop [ctx ctx
         path path]
    (if (empty? path)
      ctx
      (recur (focus ctx (first path)) (rest path)))))
(defmulti focus*
  (fn [ctx]
    (::level ctx)))
(defmethod focus* :find/rel
  [ctx]
  (map (fn [i] (focus ctx i)) (-> ctx ::value count range)))
(defmethod focus* :find/rel-entry
  [ctx]
  (map (fn [i] (focus ctx i)) (-> ctx ::value count range)))
(defmethod focus* :find/coll
  [ctx]
  (map (fn [i] (focus ctx i)) (-> ctx ::value count range)))
(defmethod focus* :find/tuple
  [ctx]
  (into {} (map (fn [a] [a (focus ctx a)]) (-> ctx ::value keys))))
(defmethod focus* :find/scalar
  [ctx]
  (throw (ex-info "Can't focus* a scalar" {::value (::value ctx)})))
(defmethod focus* :default
  [ctx]
  (throw (ex-info "Unexpected Level" {::context ctx})))
(defn safe-deref
  [any]
  (when any @any))
(defn ctx0-adapter
  [{rt :runtime pid :partition-id :as ctx0}]
  {::runtime rt
   ::partition-id pid
   ::level :find/coll
   ::fiddle (safe-deref (:hypercrud.browser/fiddle ctx0))
   ::value (safe-deref (:hypercrud.browser/result ctx0))
   ::schema (.-schema-by-attr (safe-deref (runtime/get-schema+ rt pid "$")))})