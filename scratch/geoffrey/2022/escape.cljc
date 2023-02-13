(ns geoffrey.escape
  (:require [cljs.compiler])
  (:import [java.util Formatter]))

;; From cljs.compiler These two account for more than 20% of compilation time
;; because they allocate a lot a put pressure on the gc.

(def formatter (new Formatter))

(defn format1 [fmt x]
  (String/format fmt (to-array [x])))

(defn- escape-char [^Character c]
  (let [cp #?(:clj (.hashCode c)
              :cljs (gstring/hashCode c))]
    (case cp
                                        ; Handle printable escapes before ASCII
      34 "\\\""
      92 "\\\\"
                                        ; Handle non-printable escapes
      8 "\\b"
      12 "\\f"
      10 "\\n"
      13 "\\r"
      9 "\\t"
      (if (< 31 cp 127)
        c                               ; Print simple ASCII characters
        #?(:clj (format1 "\\u%04X" cp)   ; Any other character is Unicode
           :cljs (let [unpadded (.toString cp 16)
                       pad      (subs "0000" (.-length unpadded))]
                   (str "\\u" pad unpadded)))))))

#?(:clj (def sb-pool (volatile! (clojure.lang.PersistentQueue/EMPTY))))

#?(:clj
   (defn aquire-sb [cnt]
     (dosync
       (if-let [sb (peek @sb-pool)]
         (do (vswap! sb-pool pop)
             sb)
         (new StringBuilder cnt)))))

(.toString
  (doto (new StringBuilder 5)
    (.append "hello")
    (.setLength 0)
    (.append "worldworld")
    ))

#?(:clj
   (defn release-sb [^StringBuilder sb]
     (.setLength sb 0)
     (vswap! sb-pool conj sb)
     #_(tap> (count @sb-pool))))

(alter-meta! #'cljs.compiler/escape-char dissoc :private)

(def sb (new StringBuilder))

(defn escape-string [^CharSequence s]
  (let [len (.length s)]
    (if (= 0 len)
      ""
      (let [up  (unchecked-add-int len -1)
            sb  (new StringBuilder len)]
        (loop [i 0]
          (.append sb (escape-char (.charAt s i)))
          (when (< i up)
            (recur (unchecked-add-int i 1))))
        (let [s (.toString sb)]
          ;; (.setLength sb 0)
          s)))))

(defonce escape-string* @#'cljs.compiler/escape-string)
(defonce emit* @#'cljs.compiler/emit*)

(defmacro multi->case [sym]
  (let [ms-sym (gensym "ms")
        ks     (keys (methods @(resolve sym)))]
    `(let [multi#    ~sym
           ms#       (methods multi#)
           dispatch# (.-dispatchFn multi#)]
      (fn [x#]
        (let [val# (dispatch# x#)]
          ((get ms# val# multi#) x#))))))


(comment


  (intern 'cljs.compiler 'escape-string escape-string*)

  (intern 'cljs.compiler 'emit* (multi->case emit*) )
  (intern 'cljs.compiler 'emit* emit* )
  )

