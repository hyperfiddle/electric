
; Hypothesis: in Photon, recursion is an explicit choice about the lifecycle

; Program FOo1
(p/defn Foo1 "reactive recursion (in linear space)" [n]
  #_(new (m/observe (fn [!] (println 'mount) (! nil) #(println 'unmount))))
  (dom/div n)
  (if (pos? n)
    (identity (Foo. (dec n))) ; chain of frames. goto named recursion point Foo1
    n))

(Foo1. 2) := ; mount is called 3 times

(p/defn Foo2 "reactive iteration (in constant space)" [n]
  #_(new (m/observe (fn [!] (println 'mount) (! nil) #(println 'unmount))))
  (dom/div n)
  (if (pos? n)
    (recur (dec n)) ; goto implicit recursion point Foo1
    n))

; := 0 after n+1 frames, and one div that has been updated n+1 times
(Foo1. 2) := ; mount is called once

(p/defn Foo3 "reactive iteration" [n]
  (loop [n n]
    #_(new (m/observe (fn [!] (println 'mount) (! nil) #(println 'unmount))))
    (dom/div n)
    (if (pos? n)
      (recur (dec n)) ; goto implicit recursion point - loop
      n)))

; We agree Foo2 and Foo3, if it works, under the definition below, have the same meaning and likely same implementation.

; We propose defined behavior of Foo1 and Foo2, and the meaning is not the same.
; D: I agree
; L: I agree because it's a definition

; L: If we decide this is a good idea, we can materialize it in an implementation.
; Does L think it's a good idea that these two programs having distinct meaning is a good idea? L: Maybe – it is an interesting idea
; Does L think it's a good name*? L: No, because it brings confusion regarding Clojure's loop/recur

; What does L think should be the syntax for an interative recursion that reuses the Photon frame?
; Proposal of Leo: with-cycle, which Dustin thinks has syntax problems and Dustin questions if they are essential?
; Proposal of Dustin: loop/recur, which Leo thinks is an interesting idea but implementing the idea
;   under the loop/recur special form is confusing.

; D: I am not interested in the name, I am interested in the structure. Leo: me too.
; What's interesting about loop/recur structure is:


; Q: Does reactive iteration, in Clojure under loop/recur syntax, have a parallel symmetrical semantics in Photon reactive context?
; L: I'm not interested in this question, because we still need to udnerstand if different forms of recursion having distinct meaning
; is a good idea ...

; L: What I'm interested in is: is reactive iteration with loop recur syntax, superior to reactive iteration by with-cycle?



; L: Is non-tail recursion (computational structure) allowed? D: I don't know
; L: especially with the recur syntax? ... L: I don't care about the name
; L: Do we agree,


; L: non-tail recursion, like below, should not be allowed in Photon, because simultaneous reactive
; recur has no meaning (or Leo doesn't know what it means).
; L: My insight is, I think the tail recursion constraint is enough to prevent simultaneous recursion.
(p/run (loop [a true]
         [(recur a)
          (recur (not a))]))
:= [[[[] []] [[] []]] [[[] []] [[] []]]] ; cantor set

(p/run (loop [a true]
         [(recur identity)
          (recur not)]))
; concurrent txns are serialized and compose - (comp not identity)
:= false


; D: if a recursion contains a new, it has two possible meanings, what should we do?
; we agree both have meanings
; two forms of recursion - recursion by name and recursion by recur operator

; L: I reject the idea of an operator `recur` with two possible meanings
; L: The way I would tackle

