(comment
  (defprotocol Router
    (push-state [this])
    (replace-state [this])
    (anchor [this]))

  ; Leo does this read/write interface connect with what you are designing?
  ; (I do not know if the following pattern is good or bad)
  (defn router [encode decode]
    (let [!path (m/mbx)
          >route (m/zip decode (path> !path))
          !route (reify Router
                   (push-state [this route] (pushState! !path (encode route)))
                   (replace-state [this route] (replaceState! !path (encode route)))
                   (anchor [this route] (ui/element dom/a {::dom/href (encode route) ; middle click
                                                           ::ui/click-event (p/fn [e]
                                                                              (.preventDefault e)
                                                                              (pushState! !path (encode route)))} label)))]
      [!route >route]))

  (defn router []
    (let [!path (m/mbx)]
      [!path (path> !path)]))
  )