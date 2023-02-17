(ns geoffrey.ffi
  (:require
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-impl.compiler :as c]
   [hyperfiddle.photon-impl.runtime :as r]
   [hyperfiddle.rcf :as rcf :refer [with tap % tests]]))

(p/def Foo (p/fn [] (prn 1)))

(p/def Bar (p/fn [] (Foo.)))

(set! *print-namespace-maps* false)

(def _foo (c/analyze {} `(p/fn [] ::a)))

(first _foo)

(r/emit nil (first _foo))

(p/run (p/fn [] (Foo.))) 

(p/local (p/fn [] :geoffrey.ffi/a)) 

(binding [c/*env* {}]
  (macroexpand '(p/fn [] :a)))


((p/local (p/fn [] (Foo.))) 
 (fn [___350784__auto__])
 (fn [___350784__auto__]))



(rcf/enable! true)

(def program (p/pair
       (r/peer
         3
         []
         0
         0
         2
         0
         0
         0
         (fn [G__355843-frame G__355843-vars]
           (let [G__355843-pub-0 (r/signal
                                   (r/constant
                                     G__355843-frame
                                     0
                                     (r/constructor
                                       []
                                       [0]
                                       1
                                       0
                                       1
                                       0
                                       0
                                       0
                                       (fn 
                                         [G__355843-frame G__355843-vars]
                                         (r/check-failure
                                           '{:hyperfiddle.photon-impl.ir/op
                                             :hyperfiddle.photon-impl.ir/constant,
                                             :hyperfiddle.photon.debug/name
                                             nil,
                                             :hyperfiddle.photon.debug/args
                                             [],
                                             :hyperfiddle.photon.debug/type
                                             :reactive-fn,
                                             :hyperfiddle.photon.debug/meta
                                             {:line 8}}
                                           (r/latest-apply
                                             '#:hyperfiddle.photon.debug{:type
                                                                         :apply,
                                                                         :name
                                                                         {}}
                                             (r/pure '{})
                                             (r/latest-apply
                                               '{:hyperfiddle.photon-impl.ir/op
                                                 :hyperfiddle.photon-impl.ir/global,
                                                 :hyperfiddle.photon.debug/meta
                                                 {},
                                                 :hyperfiddle.photon.debug/type
                                                 :apply,
                                                 :hyperfiddle.photon.debug/name
                                                 p/-check-fn-arity!}
                                               (r/pure p/-check-fn-arity!)
                                               (r/pure 'nil)
                                               (r/pure '0)
                                               (r/dynamic
                                                 G__355843-frame
                                                 0
                                                 '{:hyperfiddle.photon-impl.ir/op
                                                   :hyperfiddle.photon-impl.ir/node,
                                                   :hyperfiddle.photon.debug/name
                                                   c/%arity,
                                                   :hyperfiddle.photon.debug/scope
                                                   :dynamic}))
                                             (let 
                                                 [G__355843-pub-0
                                                  (r/signal
                                                    (r/constant
                                                      G__355843-frame
                                                      0
                                                      (r/constructor
                                                        []
                                                        []
                                                        0
                                                        0
                                                        0
                                                        0
                                                        0
                                                        0
                                                        (fn 
                                                          [G__355843-frame
                                                           G__355843-vars]
                                                          (r/check-failure
                                                            '#:hyperfiddle.photon-impl.ir{:op
                                                                                          :hyperfiddle.photon-impl.ir/constant}
                                                            (r/latest-apply
                                                              '{:hyperfiddle.photon-impl.ir/op
                                                                :hyperfiddle.photon-impl.ir/global,
                                                                :hyperfiddle.photon.debug/meta
                                                                {},
                                                                :hyperfiddle.photon.debug/type
                                                                :apply,
                                                                :hyperfiddle.photon.debug/name
                                                                prn}
                                                              (r/pure prn)
                                                              (r/pure
                                                                '1)))))))]
                                                 (r/latest-apply
                                                   '#:hyperfiddle.photon.debug{:type
                                                                               :apply,
                                                                               :name
                                                                               {}}
                                                   (r/pure '{})
                                                   G__355843-pub-0
                                                   (let 
                                                       [G__355843-prev
                                                        (aget G__355843-vars 1)]
                                                       (aset
                                                         G__355843-vars
                                                         (int 1)
                                                         G__355843-pub-0)
                                                       (let 
                                                           [G__355843-res
                                                            (let 
                                                                [G__355843-pub-1
                                                                 (r/signal
                                                                   (aget
                                                                     G__355843-vars
                                                                     (int 1)))]
                                                                (r/latest-apply
                                                                  '#:hyperfiddle.photon.debug{:type
                                                                                              :apply,
                                                                                              :name
                                                                                              {}}
                                                                  (r/pure '{})
                                                                  G__355843-pub-1
                                                                  (let 
                                                                      [G__355843-pub-2
                                                                       (r/signal
                                                                         (r/pure '0))]
                                                                      (r/latest-apply
                                                                        '#:hyperfiddle.photon.debug{:type
                                                                                                    :apply,
                                                                                                    :name
                                                                                                    {}}
                                                                        (r/pure '{})
                                                                        G__355843-pub-2
                                                                        (let 
                                                                            [G__355843-prev
                                                                             (aget
                                                                               G__355843-vars
                                                                               0)]
                                                                            (aset
                                                                              G__355843-vars
                                                                              (int 0)
                                                                              G__355843-pub-2)
                                                                            (let 
                                                                                [G__355843-res
                                                                                 (r/variable
                                                                                   G__355843-frame
                                                                                   G__355843-vars
                                                                                   0
                                                                                   0
                                                                                   G__355843-pub-1)]
                                                                                (aset
                                                                                  G__355843-vars
                                                                                  (int 0)
                                                                                  G__355843-prev)
                                                                                G__355843-res))))))]
                                                           (aset
                                                             G__355843-vars
                                                             (int 1)
                                                             G__355843-prev)
                                                           G__355843-res))))))))))]
             (let [G__355843-prev (aget G__355843-vars 2)]
               (aset G__355843-vars (int 2) G__355843-pub-0)
               (let [G__355843-res (r/constant
                                     G__355843-frame
                                     1
                                     (r/constructor
                                       []
                                       [0]
                                       1
                                       0
                                       1
                                       0
                                       0
                                       0
                                       (fn 
                                         [G__355843-frame G__355843-vars]
                                         (r/check-failure
                                           '{:hyperfiddle.photon-impl.ir/op
                                             :hyperfiddle.photon-impl.ir/constant,
                                             :hyperfiddle.photon.debug/name
                                             nil,
                                             :hyperfiddle.photon.debug/args
                                             [],
                                             :hyperfiddle.photon.debug/type
                                             :reactive-fn,
                                             :hyperfiddle.photon.debug/meta
                                             {}}
                                           (r/latest-apply
                                             '#:hyperfiddle.photon.debug{:type
                                                                         :apply,
                                                                         :name
                                                                         {}}
                                             (r/pure '{})
                                             (r/latest-apply
                                               '{:hyperfiddle.photon-impl.ir/op
                                                 :hyperfiddle.photon-impl.ir/global,
                                                 :hyperfiddle.photon.debug/meta
                                                 {},
                                                 :hyperfiddle.photon.debug/type
                                                 :apply,
                                                 :hyperfiddle.photon.debug/name
                                                 p/-check-fn-arity!}
                                               (r/pure p/-check-fn-arity!)
                                               (r/pure 'nil)
                                               (r/pure '0)
                                               (r/dynamic
                                                 G__355843-frame
                                                 0
                                                 '{:hyperfiddle.photon-impl.ir/op
                                                   :hyperfiddle.photon-impl.ir/node,
                                                   :hyperfiddle.photon.debug/name
                                                   c/%arity,
                                                   :hyperfiddle.photon.debug/scope
                                                   :dynamic}))
                                             (let 
                                                 [G__355843-pub-1
                                                  (r/signal
                                                    (r/constant
                                                      G__355843-frame
                                                      0
                                                      (r/constructor
                                                        []
                                                        [2]
                                                        1
                                                        0
                                                        0
                                                        0
                                                        0
                                                        0
                                                        (fn 
                                                          [G__355843-frame
                                                           G__355843-vars]
                                                          (r/check-failure
                                                            '#:hyperfiddle.photon-impl.ir{:op
                                                                                          :hyperfiddle.photon-impl.ir/constant}
                                                            (let 
                                                                [G__355843-pub-1
                                                                 (r/signal
                                                                   (r/dynamic
                                                                     G__355843-frame
                                                                     0
                                                                     '{:hyperfiddle.photon-impl.ir/op
                                                                       :hyperfiddle.photon-impl.ir/node,
                                                                       :hyperfiddle.photon.debug/name
                                                                       Foo,
                                                                       :hyperfiddle.photon.debug/scope
                                                                       :dynamic}))]
                                                                (r/latest-apply
                                                                  '#:hyperfiddle.photon.debug{:type
                                                                                              :apply,
                                                                                              :name
                                                                                              {}}
                                                                  (r/pure '{})
                                                                  G__355843-pub-1
                                                                  (let 
                                                                      [G__355843-pub-2
                                                                       (r/signal
                                                                         (r/pure
                                                                           '0))]
                                                                      (r/latest-apply
                                                                        '#:hyperfiddle.photon.debug{:type
                                                                                                    :apply,
                                                                                                    :name
                                                                                                    {}}
                                                                        (r/pure
                                                                          '{})
                                                                        G__355843-pub-2
                                                                        (let 
                                                                            [G__355843-prev
                                                                             (aget
                                                                               G__355843-vars
                                                                               0)]
                                                                            (aset
                                                                              G__355843-vars
                                                                              (int
                                                                                0)
                                                                              G__355843-pub-2)
                                                                            (let 
                                                                                [G__355843-res
                                                                                 (r/variable
                                                                                   G__355843-frame
                                                                                   G__355843-vars
                                                                                   0
                                                                                   0
                                                                                   G__355843-pub-1)]
                                                                                (aset
                                                                                  G__355843-vars
                                                                                  (int
                                                                                    0)
                                                                                  G__355843-prev)
                                                                                G__355843-res)))))))))))]
                                                 (r/latest-apply
                                                   '#:hyperfiddle.photon.debug{:type
                                                                               :apply,
                                                                               :name
                                                                               {}}
                                                   (r/pure '{})
                                                   G__355843-pub-1
                                                   (let 
                                                       [G__355843-prev
                                                        (aget G__355843-vars 1)]
                                                       (aset
                                                         G__355843-vars
                                                         (int 1)
                                                         G__355843-pub-1)
                                                       (let 
                                                           [G__355843-res
                                                            (let 
                                                                [G__355843-pub-2
                                                                 (r/signal
                                                                   (aget
                                                                     G__355843-vars
                                                                     (int 1)))]
                                                                (r/latest-apply
                                                                  '#:hyperfiddle.photon.debug{:type
                                                                                              :apply,
                                                                                              :name
                                                                                              {}}
                                                                  (r/pure '{})
                                                                  G__355843-pub-2
                                                                  (let 
                                                                      [G__355843-pub-3
                                                                       (r/signal
                                                                         (r/pure '0))]
                                                                      (r/latest-apply
                                                                        '#:hyperfiddle.photon.debug{:type
                                                                                                    :apply,
                                                                                                    :name
                                                                                                    {}}
                                                                        (r/pure '{})
                                                                        G__355843-pub-3
                                                                        (let 
                                                                            [G__355843-prev
                                                                             (aget
                                                                               G__355843-vars
                                                                               0)]
                                                                            (aset
                                                                              G__355843-vars
                                                                              (int 0)
                                                                              G__355843-pub-3)
                                                                            (let 
                                                                                [G__355843-res
                                                                                 (r/variable
                                                                                   G__355843-frame
                                                                                   G__355843-vars
                                                                                   0
                                                                                   0
                                                                                   G__355843-pub-2)]
                                                                                (aset
                                                                                  G__355843-vars
                                                                                  (int 0)
                                                                                  G__355843-prev)
                                                                                G__355843-res))))))]
                                                           (aset
                                                             G__355843-vars
                                                             (int 1)
                                                             G__355843-prev)
                                                           G__355843-res)))))))))]
                 (aset G__355843-vars (int 2) G__355843-prev)
                 G__355843-res)))))
       (r/peer
         0
         []
         0
         0
         0
         2
         0
         0
         (fn [G__355844-frame G__355844-vars]
           (do
             (r/target
               G__355844-frame
               0
               (r/constructor
                 []
                 []
                 0
                 1
                 0
                 1
                 0
                 0
                 (fn [G__355844-frame G__355844-vars]
                   (do
                     (r/target
                       G__355844-frame
                       0
                       (r/constructor
                         []
                         []
                         0
                         0
                         0
                         0
                         0
                         0
                         (fn [G__355844-frame G__355844-vars] nil)))
                     (do
                       (r/source G__355844-frame G__355844-vars 0 0)
                       nil)))))
             (do
               (r/target
                 G__355844-frame
                 1
                 (r/constructor
                   []
                   []
                   0
                   1
                   0
                   1
                   0
                   0
                   (fn [G__355844-frame G__355844-vars]
                     (do
                       (r/target
                         G__355844-frame
                         0
                         (r/constructor
                           []
                           []
                           0
                           1
                           0
                           0
                           0
                           0
                           (fn [G__355844-frame G__355844-vars]
                             (do
                               (r/source G__355844-frame G__355844-vars 0 0)
                               nil))))
                       (do
                         (r/source G__355844-frame G__355844-vars 0 0)
                         nil)))))
               nil))))))

(def two (r/pure 2))
(def prn* (r/pure prn))

;; (defn foo []  (p/run (prn 2)))
(defn foo []  ((p/pair
                 (r/peer
                   0
                   []
                   0
                   0
                   0
                   0
                   0
                   0
                   (fn [G__356471-frame G__356471-vars]
                     (r/latest-apply
                       '{:hyperfiddle.photon-impl.ir/op
                         :hyperfiddle.photon-impl.ir/global,
                         :hyperfiddle.photon.debug/meta {},
                         :hyperfiddle.photon.debug/type :apply,
                         :hyperfiddle.photon.debug/name prn}
                       prn*
                       two)))
                 (r/peer 0 [] 0 0 0 0 0 0 (fn [G__356472-frame G__356472-vars] nil)))
               (fn [___350784__auto__])
               (fn [___350784__auto__])) )

(defn f [G__356563-frame]
  (r/signal
    (r/constant
      G__356563-frame
      0
      (r/constructor
        []
        []
        0
        0
        0
        0
        0
        0
        (fn 
          [G__356563-frame
           G__356563-vars]
          (r/check-failure
            '#:hyperfiddle.photon-impl.ir{:op
                                          :hyperfiddle.photon-impl.ir/constant}
            (r/latest-apply
              '{:hyperfiddle.photon-impl.ir/op
                :hyperfiddle.photon-impl.ir/global,
                :hyperfiddle.photon.debug/meta
                {},
                :hyperfiddle.photon.debug/type
                :apply,
                :hyperfiddle.photon.debug/name
                prn}
              (r/pure prn)
              (r/pure '1))))))))

(defn program []
  ((p/pair
     (r/peer
       2
       []
       1
       0
       1
       0
       0
       0
       (fn [G__356568-frame G__356568-vars]
         (let [G__356568-pub-0 (r/signal
                                 (r/constant
                                   G__356568-frame
                                   0
                                   (r/constructor
                                     []
                                     [0]
                                     1
                                     0
                                     1
                                     0
                                     0
                                     0
                                     (fn 
                                       [G__356568-frame G__356568-vars]
                                       (r/check-failure
                                         '{:hyperfiddle.photon-impl.ir/op
                                           :hyperfiddle.photon-impl.ir/constant,
                                           :hyperfiddle.photon.debug/name
                                           nil,
                                           :hyperfiddle.photon.debug/args
                                           [],
                                           :hyperfiddle.photon.debug/type
                                           :reactive-fn,
                                           :hyperfiddle.photon.debug/meta
                                           {}}
                                         (r/latest-apply
                                           '#:hyperfiddle.photon.debug{:type
                                                                       :apply,
                                                                       :name
                                                                       {}}
                                           (r/pure '{})
                                           (r/latest-apply
                                             '{:hyperfiddle.photon-impl.ir/op
                                               :hyperfiddle.photon-impl.ir/global,
                                               :hyperfiddle.photon.debug/meta
                                               {},
                                               :hyperfiddle.photon.debug/type
                                               :apply,
                                               :hyperfiddle.photon.debug/name
                                               p/-check-fn-arity!}
                                             (r/pure p/-check-fn-arity!)
                                             (r/pure 'nil)
                                             (r/pure '0)
                                             (r/dynamic
                                               G__356568-frame
                                               0
                                               '{:hyperfiddle.photon-impl.ir/op
                                                 :hyperfiddle.photon-impl.ir/node,
                                                 :hyperfiddle.photon.debug/name
                                                 c/%arity,
                                                 :hyperfiddle.photon.debug/scope
                                                 :dynamic}))
                                           (let 
                                               [G__356568-pub-0
                                                (f G__356568-frame)]
                                             (r/latest-apply
                                               '#:hyperfiddle.photon.debug{:type
                                                                           :apply,
                                                                           :name
                                                                           {}}
                                               (r/pure '{})
                                               G__356568-pub-0
                                               (let 
                                                   [G__356568-prev
                                                    (aget G__356568-vars 1)]
                                                 (aset
                                                   G__356568-vars
                                                   (int 1)
                                                   G__356568-pub-0)
                                                 (let 
                                                     [G__356568-res
                                                      (let 
                                                          [G__356568-pub-1
                                                           (r/signal
                                                             (aget
                                                               G__356568-vars
                                                               (int 1)))]
                                                        (r/latest-apply
                                                          '#:hyperfiddle.photon.debug{:type
                                                                                      :apply,
                                                                                      :name
                                                                                      {}}
                                                          (r/pure '{})
                                                          G__356568-pub-1
                                                          (let 
                                                              [G__356568-pub-2
                                                               (r/signal
                                                                 (r/pure '0))]
                                                            (r/latest-apply
                                                              '#:hyperfiddle.photon.debug{:type
                                                                                          :apply,
                                                                                          :name
                                                                                          {}}
                                                              (r/pure '{})
                                                              G__356568-pub-2
                                                              (let 
                                                                  [G__356568-prev
                                                                   (aget
                                                                     G__356568-vars
                                                                     0)]
                                                                (aset
                                                                  G__356568-vars
                                                                  (int 0)
                                                                  G__356568-pub-2)
                                                                (let 
                                                                    [G__356568-res
                                                                     (r/variable
                                                                       G__356568-frame
                                                                       G__356568-vars
                                                                       0
                                                                       0
                                                                       G__356568-pub-1)]
                                                                  (aset
                                                                    G__356568-vars
                                                                    (int 0)
                                                                    G__356568-prev)
                                                                  G__356568-res))))))]
                                                   (aset
                                                     G__356568-vars
                                                     (int 1)
                                                     G__356568-prev)
                                                   G__356568-res))))))))))]
           (r/latest-apply
             '#:hyperfiddle.photon.debug{:type :apply, :name {}}
             (r/pure '{})
             G__356568-pub-0
             (let [G__356568-pub-1 (r/signal (r/pure '0))]
               (r/latest-apply
                 '#:hyperfiddle.photon.debug{:type :apply, :name {}}
                 (r/pure '{})
                 G__356568-pub-1
                 (let [G__356568-prev (aget G__356568-vars 0)]
                   (aset G__356568-vars (int 0) G__356568-pub-1)
                   (let [G__356568-res (r/variable
                                         G__356568-frame
                                         G__356568-vars
                                         0
                                         0
                                         G__356568-pub-0)]
                     (aset G__356568-vars (int 0) G__356568-prev)
                     G__356568-res))))))))
     (r/peer
       0
       []
       0
       1
       0
       1
       0
       0
       (fn [G__356569-frame G__356569-vars]
         (do
           (r/target
             G__356569-frame
             0
             (r/constructor
               []
               []
               0
               1
               0
               1
               0
               0
               (fn [G__356569-frame G__356569-vars]
                 (do
                   (r/target
                     G__356569-frame
                     0
                     (r/constructor
                       []
                       []
                       0
                       0
                       0
                       0
                       0
                       0
                       (fn [G__356569-frame G__356569-vars] nil)))
                   (do
                     (r/source G__356569-frame G__356569-vars 0 0)
                     nil)))))
           (do (r/source G__356569-frame G__356569-vars 0 0) nil)))))
   identity
   identity))

(p/local (new (p/fn [] 1))) 
(p/local (p/fn [] (prn 1))) 

(p/def dynamic :dynamic)

(p/local (do 1 2)) 

(tests
  (with
    #_(foo)
    (program)

    #_(p/run (p/fn [] (prn 1))) 


    #_((p/local (p/fn [] (prn 1)))
       (fn [___350784__auto__])
       (fn [___350784__auto__])))






  %)
