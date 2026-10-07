(ns engine.path.offsets-test
  "engine.path.offsets: the shared offsets module loads from the engine root, not from wherever the process was started."
  (:require [cljs.test :refer [deftest is]]
            ["os" :as os]
            ["path" :as path]
            [engine.path.offsets :as offsets]))

(deftest loads-from-the-engine-root-when-cwd-differs
  (let [root (js/process.cwd)]
    (try
      (offsets/set-root! root)
      (.chdir js/process (os/tmpdir))
      (is (= 0.25 (.-bamboo ^js (.-OFFSET_MAX ^js (offsets/offsets)))))
      (is (= (path/resolve root) (path/resolve (offsets/root))))
      (finally
        (.chdir js/process root)
        (offsets/set-root! nil)))))
