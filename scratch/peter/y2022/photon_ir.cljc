(ns peter.y2022.photon-ir)

;; an instruction is a vector starting with a keyword, the instruction type.
;; The rest of the vector depends on the instruction.
;; An instruction can have child instructions.
;; The available types:
;; - `:literal` - takes 1 arg, produces a pure value
;; - `:nop` - noop, takes no args
;; - `:global` - takes 1 arg as a keyword and resolves the keyword in the global environment
;; - `:apply` - concurrent application, 1st arg is an inst for a single function, the rest are
;;              also insts that should be passed to the fn. It then combines all of these into
;;              1 inst, run them concurrently and return the result of applying the fn on the args.
;; - `:pub` - "publisher", takes an inst and a continuation. It evaluates the inst and makes its result
;;            available in the continuation (like `let`).
;; - `:sub` - "subscriber", takes an integer, which is the De Bruijn index (https://en.wikipedia.org/wiki/De_Bruijn_index)
;;            of a `:pub` and subscribes to it, returning the value.
;; - `:variable` - takes 1 inst, is monadic join (takes a flow of flows and returns a flow).
;; - `:source` - takes a continuation. It is emitted when the remote peer has a `:variable`.
;;               It defines a mount point for `p/fn`.
;; - `:constant` - takes an inst and returns a constant flow that runs this inst. Basically it's `p/fn`.
;; - `:target` - takes a sequence of insts that must be run on request from the remote peer.
;; - `:input` - takes no args, defines a value that will be sent by the remote peer.
;; - `:output` - takes 2 args, 1 inst and 1 continuation, runs the inst and sends the result to the remote peer.
;;               Then it runs the continuation and returns its result.
;; - `:node` - takes an integer which identifies a Var to look up and returns a subscription to the current
;;             binding of the Var.
;; - `:bind` - takes an integer identifying a Var, another integer identifying a publisher and a continuation.
;;             It runs the continuation with the Var bound to the publisher.
