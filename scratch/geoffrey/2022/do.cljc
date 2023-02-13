

(def !x (atom 0))

;; Current
(p/run (let [x ~(m/watch !x)]
         (do (prn x)
             x)))

;; Alternative
(tests
 (p/run (! (let [x ~(m/watch !x)]
             (do (prn x)
                 nil))))

 ;; prints 0
 % := nil
 (swap! !x inc)

 ;; prints 1
 % := nil)


;; Should there be a dependency between the statements of the do and the result of the do?
;; Currently there is.

;; Which way is the more RT?



;; Do desugares to :

(do a b c)
(latest {} a
        (latest {} b c))

({} 1 2) := 2
(get {} 1 2)

(latest last [a b c])


;; Example with let
(cc/do a b c) => (cc/let [_ a, _ b] c)

(cc/let [a 1
         _ (prn "hello")
         b (+ a 1)]
  b)

(p/let [a 1
        _ (prn "hello") ;; Should photon add implicit do?
        b (+ a 1)]
  b)

(p/let [a 1
        _ ~(m/cp (prn "hello"))
        b (+ a 1)]
  b)


getLine :: IO String
putStrLn :: String -> IO ()
>>= :: m a -> (a -> m a) -> m a

do
 x <- getLine
 putStrLn x

;; Macroexpanded
getLine >>= putStrLn

(tests

 (def !x (atom 0))
 (def !y (atom :a))

 (let [_ (prn @!x) ;; first
       _ (prn @!y) ;; second
       ]
   nil)

 (p/run (! (let [_ (prn ~(m/watch !x)) ;; first
                 _ (prn ~(m/watch !y)) ;; second
                 ]
             nil)))

 ;; prints 0
 ;; prints :a
 % := nil

 (reset! !x 1)


 ;; prints 1
 ;; prints :a
 % := nil

  --- OR ---

 ;; prints 1
 

 )



(tests
;; sequencing do
 (def !x (atom 0))
 (p/run (! (let [x ~(m/watch !x)] ;; server
             ~@;; client
               (try
                 (do (if (odd? x) (throw …) (prn x))
                     nil)
                 (catch :default _
                   :err)))))
 ;; prints 0
 % := nil

 (swap! !x inc)

 % := :err

 (swap! !x inc)

 ;; prints 2
 % := nil)

(tests
;; ? do
 (def !x (atom 0))
 (p/run (! (let [x ~(m/watch !x)] ;; server
             ~@;; client
               (try
                 (do (if (odd? x) (throw …) (prn x))
                     nil)
                 (catch :default _
                   :err)))))
 ;; prints 0
 % := nil

 (swap! !x inc)

 % := :err

 (swap! !x inc)

 ;; prints 2
 % := nil
 
 )


 (tests
;; ? do
  (def !x (atom 0))
  (p/run (! (let [x ~(m/watch !x)] ;; server
              ~@;; client
                (do (if (odd? x) (throw …) (prn x))
                    nil))))
 ;; prints 0
  % := nil

  (swap! !x inc)

 ;; prints 1
  % := ::rcf/timeout)
 

(tests
 (def !x (atom 0))
 (p/run (! (let [x ~(m/watch !x)] ;; server
             ~@ ;; client
             (p/let [_ (prn x)]
               nil)
             )))

 
 (tests
  ;; ? do
  (def !x (atom 0))
  (p/run (! (let [x ~(m/watch !x)] ;; server
              ~@;; client
                (try
                  (do (if (= 0 (mod x 3)) (dom/text x) (throw …))
                      (dom/text "nil"))
                  (catch :default _
                    (dom/text "failure"))))))
 ;; mounts (dom/text 0) on dom
 ;; mounts (dom/text "nil") on dom
  % := "nil"

  (swap! !x inc)

  ;; set (dom/text 1)
  % := ::rcf/timeout

  (swap! !x inc)

 ;; remove (dom/text 0) from dom
 ;; remove (dom/text "nil") from dom
 ;; mounts (dom/text "failure") on dom
  % := "failure"

  (swap! !x inc)

  ;; remove (dom/text "failure") from dom
  ;; mount (dom/text 2) on dom
  ;; mount (dom/text "nil") on dom

  % := "nil"
  

;;   (swap! !x inc)

;;  ;; remove (dom/text 2) from dom
;;  ;; remove (dom/text "nil") from dom
;;  ;; mounts (dom/text "failure") on dom
;;  % := "failure"

  
  )



(p/run (p/client (! (p/server (p/client 1)))))


(p/run (p/client (! (p/server (do (p/client 1))))))


(p/client (p/client 1))


(def !x (atom true))
(p/run (p/$ (if ~(m/watch !x) >x >y)))
(swap! !x identity)