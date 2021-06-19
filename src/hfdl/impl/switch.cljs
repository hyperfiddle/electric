(ns hfdl.impl.switch
  (:require [hfdl.impl.util :as u]))

(declare more! cancel transfer)

(deftype Switch [notifier terminator iterator sampler ready operational token]
  IFn
  (-invoke [s] (cancel s))
  IDeref
  (-deref [s] (transfer s)))

(deftype Target [iterator])

(defn cancel [^Switch s]
  (when-some [t (.-token s)]
    (set! (.-token s) nil) (t)))

(defn swap-token! [^Switch s token]
  (if (nil? (.-token s))
    (token) (set! (.-token s) token)))

(defn transfer [^Switch s]
  @(if-some [in (.-sampler s)]
     (let [it (.-iterator in)]
       (set! (.-sampler s) it))
     (.-iterator s)))

(defn next! [^Switch s it in]
  (when (set! (.-operational s) (not (.-operational s)))
    (if (nil? (u/get-set in iterator (u/get-set s iterator it)))
      (if-some [x (.-sampler s)]
        (do (set! (.-sampler s) in)
            (if (ifn? x)
              (do ((.-notifier s)) (x))
              (let [it (.-iterator x)]
                (it) (try @it (catch :default _)))))
        (do (set! (.-sampler s) in)
            ((.-notifier s)) (more! s)))
      (more! s))))

(defn more! [^Switch s]
  (when (set! (.-ready s) (not (.-ready s)))
    (when (.-operational s)
      (if-some [t (.-terminator s)]
        (if-some [x (.-sampler s)]
          (do (set! (.-iterator s) (if (ifn? x) x (.-iterator x)))
              (set! (.-sampler s) nil)) (t))
        (let [it (.-iterator s)
              in (->Target nil)]
          (->> (@it
                 #(if-some [it (.-iterator in)]
                    (if (identical? it (.-sampler s))
                      (do (set! (.-sampler s) in)
                          ((.-notifier s)))
                      (if (identical? it (.-iterator s))
                        ((.-notifier s))
                        (try @it (catch :default _))))
                    (next! s it in))
                 #(if-some [it (.-iterator in)]
                    (if (identical? it (.-sampler s))
                      (do (set! (.-sampler s) nil)
                          (swap-token! s (.-iterator s)))
                      (if (identical? it (.-iterator s))
                        ((.-terminator s))
                        (more! s)))
                    (next! s it (set! (.-iterator in) in))))
            (set! (.-iterator s))
            (swap-token! s))
          (next! s it in))))))

(defn switch [f]
  (fn [n t]
    (let [s (->Switch n nil nil nil true true nil)]
      (->> (f (partial more! s)
             #(do (set! (.-terminator s) t)
                  (more! s)))
        (set! (.-iterator s))
        (set! (.-token s)))
      (more! s) s)))