(ns hyperfiddle.common.routes)

(def ROUTES ["/" [#_["/auth" ::auth]
                  [[:sexpr ".*"] ::sexpr]]])
