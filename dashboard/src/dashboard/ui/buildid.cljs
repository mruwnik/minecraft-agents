(ns dashboard.ui.buildid
  "The page reloads itself when the server's build id changes (the server was rebuilt and restarted).")

(defn observe
  "known is the id the page first saw (nil before), incoming the id in the latest /api/state (nil if absent).
  Returns {:known id-to-keep :reload? bool}."
  [known incoming]
  (cond
    (nil? incoming) {:known known :reload? false}
    (nil? known) {:known incoming :reload? false}
    :else {:known known :reload? (not= known incoming)}))
