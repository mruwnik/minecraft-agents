(ns dashboard.ui.map-test
  (:require [cljs.test :refer [deftest is]]
            [dashboard.ui.map :as map]))

(deftest overview-path-cache-key-covers-geometry-inputs
  (let [index {[-1 2] 1234}
        key (map/coverage-path-key "claude" index 0.5)]
    (is (map/coverage-path-cache-matches? key (map/coverage-path-key "claude" index 0.5)))
    (is (not (map/coverage-path-cache-matches? key (map/coverage-path-key "other" index 0.5))))
    (is (not (map/coverage-path-cache-matches? key (map/coverage-path-key "claude" index 0.6))))
    (let [replacement (into {} [[[-1 2] 1234]])]
      (is (= index replacement))
      (is (not (map/coverage-path-cache-matches? key (map/coverage-path-key "claude" replacement 0.5)))))))

(deftest failed-image-retry-is-scoped-to-the-failed-attempt
  (let [failed #js {} replacement #js {}]
    (is (map/retry-same-image? failed failed))
    (is (not (map/retry-same-image? replacement failed)))))

(deftest unmount-releases-the-retained-overview-path
  (let [before @map/coverage-path-cache]
    (reset! map/coverage-path-cache {:path #js {}})
    (map/release-terrain-cache!)
    (is (nil? @map/coverage-path-cache))
    (reset! map/coverage-path-cache before)))

(deftest coverage-path-is-built-once-per-key
  (let [before @map/coverage-path-cache
        calls (atom 0)
        index {[0 0] 1 [1 0] 2}
        original (aget js/globalThis "Path2D")]
    (aset js/globalThis "Path2D" (fn [] (this-as o (swap! calls inc) (aset o "rect" (fn [& _])) o)))
    (try
      (reset! map/coverage-path-cache nil)
      (let [p1 (map/coverage-path "w" index 1)
            p2 (map/coverage-path "w" index 1)
            p3 (map/coverage-path "w" {[0 0] 1} 1)]
        (is (identical? p1 p2) "same key returns the cached path")
        (is (not (identical? p1 p3)) "a different index rebuilds")
        (is (= 2 @calls)))
      (finally (aset js/globalThis "Path2D" original) (reset! map/coverage-path-cache before)))))
