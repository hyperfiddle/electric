(ns leo.io)

"
## Transit limitations
* limited sharing support
* suboptimal for binary data, e.g numbers (to be confirmed)
* offline only, unsuitable for large data structures
* no easy way to leverage static data structure knowledge, extensions work but require to change the codebase
(typically switch from heterogenous maps to defrecords)
* no easy way to leverage history

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

(defn Primitive [t]
  {:op :primitive
   :type t})

(defn Unit [x]
  {:op :unit
   :value x})

(defn Product [f & args]
  (let [[ts ps] (apply map vector (partition-all 2 args))]
    {:op    :product
     :ctor  f
     :types ts
     :projs ps}))

(defn Variant [f & ts]
  {:op :variant
   :switch f
   :types ts})

(defn Collection [t f]
  {:op :collection
   :ctor f
   :type t})

(defn Int8  [] [Primitive :int8])
(defn Int16 [] [Primitive :int16])
(defn Int32 [] [Primitive :int32])
(defn Int64 [] [Primitive :int64])
(defn Str   [] [Primitive :string])

(defn integer-check [n i8 i16 i32 i64]
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

(defn Int []
  [Variant integer-check [Int8] [Int16] [Int32] [Int64]])

(defn Nil [] [Unit nil])
(defn True [] [Unit true])
(defn False [] [Unit false])

(defn boolean-check [x t f]
  (if x t f))

(defn Bool []
  [Variant boolean-check [True] [False]])

(defn Vec [T]
  [Collection T vector])

(defn Set [T]
  [Collection T hash-set])

(defn entries [& es]
  (into {} es))

(defn Map [K V]
  [Collection
   [Product vector
    K       key
    V       val]
   entries])

(defn ns-check [x u q]
  (case (namespace x) nil u q))

(defn UnqualifiedSymbol []
  [Product symbol
   [Str] name])

(defn QualifiedSymbol []
  [Product symbol
   [Str] namespace
   [Str] name])

(defn Symbol []
  [Variant ns-check
   [UnqualifiedSymbol]
   [QualifiedSymbol]])

(defn UnqualifiedKeyword []
  [Product keyword
   [Str] name])

(defn QualifiedKeyword []
  [Product keyword
   [Str] namespace
   [Str] name])

(defn Keyword []
  [Variant ns-check
   [UnqualifiedKeyword]
   [QualifiedKeyword]])

(defn any-check [x n b st sy k i v s m]
  (cond
    (nil? x) n
    (boolean? x) b
    (string? x) st
    (symbol? x) sy
    (keyword? x) k
    (integer? x) i
    (sequential? x) v
    (set? x) s
    (map? x) m))

(defn Any []
  [Variant any-check
   [Nil] [Bool] [Str] [Symbol] [Keyword] [Int] [Vec [Any]] [Set [Any]] [Map [Any] [Any]]])

(defn Any []
  (Variant any-check
    [Nil] [Bool] [Str] [Symbol] [Keyword] [Int] (Vec (Any)) [Set [Any]] [Map [Any] [Any]]))

(comment

  (defn nil-check [x n o]
    (case x nil n o))

  (defn IntList []
    [Variant nil-check
     [Nil]
     [Product   vector
      [Int]     first
      [IntList] second]])

  (defn List [T]
    [Variant nil-check
     [Nil]
     [Product  vector
      T        first
      [List T] second]])

  (def IntList* [List [Int]])

  )

(comment

  {:name "alice" :address {:user/city "Baltimore" :user/state "MD"} :phone 0123}

  (defn person [name address phone]
    {:name name :address address :phone phone})

  (defn address [city state]
    {:user/city city :user/state state})

  (defn StaticMap [& types+keys]
    (into [Product hash-map] types+keys))

  (defn Address []
    [Product address
     [Str] :user/city
     [Str] :user/state])

  (defn Address* []
    [StaticMap
     [Str] :user/city
     [Str] :user/state])

  (defn Person []
    [Product person
     [Str] :name
     [Address] :address
     [Int] :phone])

  )
