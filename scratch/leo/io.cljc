(ns leo.io)

"
## Transit limitations
* limited sharing support
* suboptimal for binary data, e.g numbers
* offline only, unsuitable for large data structures
* no easy way to leverage static data structure knowledge, extensions work but require to change the codebase
(typically switch from heterogenous maps to defrecords)

## Instruction set
pop : pull top stack value and emit it
dup : duplicate top stack value
rot : read next byte, parse it as unsigned integer, rotate this number of items on top of stack.
int8 : read next byte, parse it as signed integer and push it on stack.
int16 : read next 2 bytes, ...
int32 : read next 4 bytes, ...
int64 : read next 8 bytes, ...
float32 : read next 4 bytes, ...
float64 : read next 8 bytes, ...
string : read next byte, parse it as unsigned integer, read this number of characters and push this string.
static : read next byte, parse it as unsigned integer, lookup index in constant pool and push value on top of stack.
apply : read next byte, parse it as unsigned integer, pull this number of items from the stack and run them as a sexp.
chunk : read next byte, parse it as unsigned integer, read this number of bytes and push this byte array.
"

(defn unit [x]
  {:op :unit :value x})

(defn primitive [x]
  {:op :primitive :type x})

(defn variant [s & ts]
  {:op :variant
   :switch s
   :types ts})

(defn product [c & args]
  (let [[ts ps] (apply map vector (partition-all 2 args))]
    {:op    :product
     :ctor  c
     :types ts
     :projs ps}))

(defn coll [ctor t]
  {:op :collection
   :ctor ctor
   :type t})

(defn lookup [k]
  {:op :lookup :key k})

(defn Vec [V]
  (coll vector V))

(defn Set [V]
  (coll hash-set V))

(defn Map [K V]
  (coll (partial into {})
    (product vector
      K key
      V val)))


(def standard
  {::boolean             (variant (fn [x t f] (case x true t false f))
                           (unit true) (unit false))
   ::integer             (variant
                           (fn [n i8 i16 i32 i64]
                             (if (neg? n)
                               (cond
                                 (<= -128 n) i8
                                 (<= -32768 n) i16
                                 (<= -2147483648 n) i32
                                 (<= -9223372036854775808 n) i64)
                               (cond
                                 (< n 128) i8
                                 (< n 32768) i16
                                 (< n 2147483648) i32
                                 (< n 9223372036854775808) i64)))
                           (primitive :int8) (primitive :int16) (primitive :int32) (primitive :int64))
   ::unqualified-symbol  (product symbol
                           (primitive :string) name)
   ::qualified-symbol    (product symbol
                           (primitive :string) namespace
                           (primitive :string) name)
   ::symbol              (variant (fn [s us qs] (case (namespace s) nil us qs))
                           (lookup ::unqualified-symbol) (lookup ::qualified-symbol))
   ::unqualified-keyword (product keyword
                           (primitive :string) name)
   ::qualified-keyword   (product keyword
                           (primitive :string) namespace
                           (primitive :string) name)
   ::keyword             (variant (fn [k uk qk] (case (namespace k) nil uk qk))
                           (lookup ::unqualified-keyword) (lookup ::qualified-keyword))
   ::any                 (variant
                           (fn [x n b st sy k i v s m]
                             (cond
                               (nil? x) n
                               (boolean? x) b
                               (string? x) st
                               (symbol? x) sy
                               (keyword? x) k
                               (integer? x) i
                               (sequential? x) v
                               (set? x) s
                               (map? m) m))
                           (unit nil) (lookup ::boolean)
                           (primitive :string) (lookup ::symbol) (lookup ::keyword) (lookup ::integer)
                           (Vec (lookup ::any)) (Set (lookup ::any)) (Map (lookup ::any) (lookup ::any)))})
