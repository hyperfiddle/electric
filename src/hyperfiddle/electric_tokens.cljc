(ns hyperfiddle.electric-tokens
  (:require [clojure.datafy :refer [datafy]]
            [hyperfiddle.electric3 :as e]))

(defn- fn-name [f]
  (when (fn? f)
    #?(:cljs (.-name f)
       :clj (-> f class .getName ((requiring-resolve 'clojure.main/demunge))))))

(def ansi-color-no-color 0)
(def ansi-color-256 {:ansi.color-256/red 196, :ansi.color-256/green 46, :ansi.color-256/orange 202}) ; use with "\033[38;5;<color>m<TEXT>\033[0m"
(def diff-color {::added :ansi.color-256/green, ::retracted :ansi.color-256/red, ::changed :ansi.color-256/orange})

(defn- colorize [color string]
  #?(:clj (str "\033[38;5;" (ansi-color-256 color ansi-color-no-color) "m" string "\033[0m")
     :cljs string #_(str "\u001B[38;5;" (ansi-color-256 color ansi-color-no-color) "m" string "\u001B[m")))

(defn- ansi-bold [string] #?(:clj (str "\033[1m" string "\033[0m")
                            :cljs string #_(str "\u001B[1m" string "\u001B[m")))

(defn token-trace
  ([t] (token-trace 0 t))
  ([level t]
   (if-not t
     "nil\n"
     (let [{:keys [::e/name ::e/children ::e/diff ::e/hash]} (datafy t)]
       (apply str (cond-> (or name (fn-name t)) (not (e/token? t)) (ansi-bold))
         " "
         (colorize (diff-color diff) (some-> diff clojure.core/name not-empty))
         (cond (= ::changed diff) (str " #" hash)
               (and t (not (e/token? t))) (str " #" (clojure.core/hash t)))
         "\n"
         (map (fn [child]
                (let [level (inc level)]
                  (str (apply str (repeat (max 0 (dec level)) "│  ")) "├─ "
                    (token-trace level child))))
           children))))))

(defn- token-diff-1 [ta tb]
  (cond (= ta tb) nil ; account for nil
        (nil? ta) ::added
        (nil? tb) ::retracted
        ()        ::changed))

(defn- pad [v n coll] (concat coll (repeat n v)))

(defn- pad-colls [coll-a coll-b]
  (let [ca (count coll-a)
        cb (count coll-b)]
    (cond (= ca cb) [coll-a coll-b]
          (< ca cb) [(pad nil (- cb ca) coll-a) coll-b]
          (> ca cb) [coll-a (pad nil (- ca cb) coll-b)])))

(defn token-diff [ta tb]
  (let [diff (token-diff-1 ta tb)]
    (case diff
      ::added (vary-meta tb assoc ::e/diff diff)
      ::retracted nil
      ::changed (-> (vary-meta tb update ::e/children #(apply map token-diff (pad-colls (::e/children (meta ta)) %)))
                  (vary-meta assoc ::e/diff diff))
      (if (e/token? ta)
        (-> (vary-meta ta update ::e/children #(apply map token-diff (pad-colls % (::e/children (meta tb)))))
          (vary-meta assoc ::e/diff diff))
        ta))))

(comment
  (def left (e/->Token "left1"
              (e/->Token "left1.1")
              (e/->Token "left1.2")))

  (def right-1 (e/->Token "right1.1" #(prn "hello")))
  (def right-2 (e/->Token "right1.2"))

  (def right (e/->Token "right" right-1 right-2))

  (def ta
    (e/->Token "root"
      left
      right))

  (def tb
    (e/->Token "root"
      left
      (e/->Token "right"
        (e/->Token "right1.3")
        right-2)
      ))
  (datafy ta)

  (println (token-trace ta))
  (println (token-trace tb))

  (println (token-trace (token-diff ta ta)))
  (println (token-trace (token-diff ta tb)))

  )
