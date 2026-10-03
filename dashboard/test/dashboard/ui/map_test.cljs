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
  (reset! map/coverage-path-cache {:path #js {}})
  (map/release-terrain-cache!)
  (is (nil? @map/coverage-path-cache)))
