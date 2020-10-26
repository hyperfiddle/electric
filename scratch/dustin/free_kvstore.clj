(ns dustin.free-kvstore)


(comment

  ; free monad study

  ; http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html
  ; https://typelevel.org/cats/datatypes/freemonad.html#:~:text=A%20free%20monad%20is%20a,provide%20a%20practical%20way%20to%3A&text=retarget%20a%20computation%20to%20another%20interpreter%20using%20natural%20transformations
  ; https://stackoverflow.com/questions/13352205/what-are-free-monads

  ; these are data, not evaluated
  ; ... this is a functor ?
  '(put "a" 2)
  '(get "a")
  '(delete "a")
  '(update "a" inc)


  (mlet [_ (put "a" 2)
         _ (update "a" inc)
         n (get "a")
         _ (delete "a")]
    (return n))

  ; This looks like a monadic flow. However, it just builds a recursive data structure representing the sequence of operations.

  (defn apply [fa]
    (match fa
      (put ?k ?v)
      (get ?k) ...
      (delete ?k) ...
      (update ?k ?f) ...
      )
    )

  (defrecord KVStore []
    Do-via
    (resolver-for [_]
      {:KVStore.get    (fn [k]   (get *this k))
       :KVStore.put    (fn [k v] (set! *this (assoc *this k v)))
       :KVStore.update (fn [k f] (set! *this (update *this k)))
       :KVStore.delete (fn [k]   (set! *this (dissoc *this k)))
       }))

  (via (KVStore.)
    (let [_ (! :KVStore.put "a" 2)
          _ (! :KVStore.update "a" inc)
          n (! :KVStore.get "a")
          _ (! :KVStore.delete "a")]
      n))

  (via (KVStore.)
    ; rewrite into above
    (mlet [_ (put "a" 2)
           _ (update "a" inc)
           n (get "a")
           _ (delete "a")]
      n))

  ; every single form should be lifted????
  ; embedding clojure with the command language

  (via (KVStore.)
    ; rewrite each form into ! ???
    (let [_ (put "a" 2)
          _ (update "a" inc)
          n (get "a")
          _ (delete "a")]
      n))

  (via (KVStore.)
    (let [_ (put "a" 2)
          _ (update "a" inc)
          n (get "a")
          _ (delete "a")]
      n))

  (via ()
    (! let [_ (! put "a" 2)
            _ (! update "a" inc)
            n (! get "a")
            _ (! delete "a")]
      n))

  )