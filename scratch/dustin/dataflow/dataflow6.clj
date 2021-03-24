(ns dustin.dataflow6)



; trying to understand physical types that the compiler produces

; biz effect
(defn submissions! [x] :! a -> Flow b
  (m/via m/cpu (d/q ...)))
:= (Flow [{:db/id 1 :name x}])

; submissions :: a -> m b
; x :: m a
; x >>= submissions!


; wrong type
(defn submissions! [x] :! a -> Flow b
  (m/signal! (m/cp ... [{:db/id 1 :name x}])))

:= (Flow [{:db/id 1 :name x}])


; submissions! :: m a -> m b
; x :: m a
; submissions! (extend x)

; (extend x) :: m (m a)
; incr-call submissions (extend x) :: m b
; incr-call :: m (m a) -> m (m a -> m b) -> m b
; incr-call :: m (m a) -> m (m a -> m b) -> m b

; its close to pass by reference

; without extend

; submissions! :: m a -> m b
; x :: m a

; incr-call submissions x :: m b
; incr-call :: (m a) -> m (m a -> m b) -> m b


; incr-call :: (m a) -> m (m a -> m b) -> m b
; (>>=)     :: (m a) ->   (  a -> m b) -> m b
; (<*>)     :: (m a) -> m (  a ->   b) -> m b
; fmap      :: (m a) ->   (  a ->   b) -> m b

; https://wiki.haskell.org/If-then-else