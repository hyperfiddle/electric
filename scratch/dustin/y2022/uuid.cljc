(ns contrib.uuid
  (:require clojure.string
            contrib.data
            [hyperfiddle.rcf :refer [tests]]))

(tests
  "#uuid works out of the box"
  (def x #?(:clj (java.util.UUID/fromString "2e50ddcb-082a-4e50-b671-f603b6345519")
            :cljs (cljs.core/uuid "2e50ddcb-082a-4e50-b671-f603b6345519")))
  (uuid? x) := true
  (str x) := "2e50ddcb-082a-4e50-b671-f603b6345519"
  (pr-str x) := "#uuid \"2e50ddcb-082a-4e50-b671-f603b6345519\""

  (= x #uuid "2e50ddcb-082a-4e50-b671-f603b6345519") := true
  (= x #?(:cljs (cljs.reader/read-string "#uuid \"2e50ddcb-082a-4e50-b671-f603b6345519\"")
          :clj (clojure.core/read-string "#uuid \"2e50ddcb-082a-4e50-b671-f603b6345519\""))) := true)

; Below changeset is from Karl 2018 - it may have been fixed, so saving as scrap.
; Possibly related:
; https://clojure.atlassian.net/browse/CLJS-3370
; https://clojure.atlassian.net/browse/CLJS-3130

(def uuid-regex-pattern #"^([0-9a-fA-F]{1,8})-([0-9a-fA-F]{1,4})-([0-9a-fA-F]{1,4})-([0-9a-fA-F]{1,4})-([0-9a-fA-F]{1,12})$")

; for consistency with java.util.UUID/fromString
; such that: (= (str (UUID/fromString s)) (str (read-uuid s)))
#?(:cljs
   (defn read-uuid [s]
     (if-let [[_ & groups] (re-find uuid-regex-pattern s)]
       (->> (map clojure.string/lower-case groups)
            #_(map #(cuerdas.core/pad %2 {:length %1 :padding "0"}) [8 4 4 4 12])
            (map #(contrib.data/padl-str %1 "0" %2) [8 4 4 4 12])
            (clojure.string/join "-")
            (uuid))
       (throw (ex-info (str "Invalid UUID string: " s) {})))))

#?(:cljs
   (tests
     (read-uuid "2e50ddcb-82a-4e50-b671-f603b6345519") := #uuid"2e50ddcb-082a-4e50-b671-f603b6345519"
     (read-uuid "00000000-0000-0000-0000-000000000000") := #uuid"00000000-0000-0000-0000-000000000000"
     (read-uuid "0-0-0-0-0") := #uuid"00000000-0000-0000-0000-000000000000"))