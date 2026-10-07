(ns engine.path.offsets
  "The shared vanilla offset hash (engine/js/offsets.mjs; the per-tick physics wrapper uses it too), loaded from the engine root:
  engine.main sets it from --engine-root, else the working directory."
  (:require ["module" :refer [createRequire]]))

(defonce ^:private engine-root (atom nil))

(defn set-root! [root] (reset! engine-root root))

(defn root [] (or @engine-root (js/process.cwd)))

(def ^:private loaded (atom nil)) ;; [root module]: the planner asks per cell

(defn offsets []
  (let [r (root)
        [loaded-root module] @loaded]
    (if (= r loaded-root)
      module
      (let [module ((createRequire (str r "/")) "./js/offsets.mjs")]
        (reset! loaded [r module])
        module))))
