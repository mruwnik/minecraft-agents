(ns dashboard.ui.tiles-events-test
  (:require [cljs.test :refer [deftest are]]
            [dashboard.ui.tiles-events :as te]))

(deftest merge-index-cases
  (are [held world entries expected] (= expected (te/merge-index held world entries))
    nil "w" [[0 0 5]] {[0 0] 5}
    {:requested "w" :index {[0 0] 5 [1 1] 6}} "w" [[0 0 9] [2 2 7]] {[0 0] 9 [1 1] 6 [2 2] 7}
    {:requested "other" :index {[0 0] 5}} "w" [[3 3 1]] {[3 3] 1}
    {:requested "w" :index {[0 0] 5}} "w" [] {[0 0] 5}))

(deftest fetch-fx-cases
  (are [db expected] (= expected (te/fetch-fx db))
    {} nil
    {:state {}} nil
    {:world "w a"} {:fetch-json {:key :tiles :url "/api/tiles/w%20a" :on-ok [:tiles/ok "w a"] :on-err [:tiles/err]}}
    {:world "w" :tiles {:requested "w" :at 50000}} {:fetch-json {:key :tiles :url "/api/tiles/w?since=40000" :on-ok [:tiles/ok "w"] :on-err [:tiles/err]}}))
