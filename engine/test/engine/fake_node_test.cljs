(ns engine.fake-node-test
  (:require [cljs.test :refer [deftest is]]
            ["os" :as os]
            ["path" :as path]
            [engine.fake.node :as node]))

(deftest engine-root-is-the-nearest-ancestor-holding-the-js-modules
  (let [root (js/process.cwd)]
    (is (= root (node/engine-root (.join path root "test" "engine") "/nowhere")))
    (is (= "/nowhere" (node/engine-root (os/tmpdir) "/nowhere")))))
