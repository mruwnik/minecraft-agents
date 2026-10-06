(ns engine.result-test
  (:require [cljs.test :refer [deftest is]]
            [jobs.lib.result :as r]))

(defn fake [] (let [a (atom nil)] [{:result #(reset! a %)} a]))

(deftest stop-sets-stopped-result-and-returns-done
  (let [[c a] (fake)]
    (is (= :done (r/stop! c :no-tree "no tree in sight")))
    (is (= {:status :stopped :reason :no-tree :text "no tree in sight"} @a))))

(deftest stop-carries-cause-cell-and-extra-keys
  (let [[c a] (fake)
        cause {:child :walk :reason :blocked :text "x"}]
    (r/stop! c :walk-failed "t" :cause cause :cell [1 2 3] :got 2)
    (is (= {:status :stopped :reason :walk-failed :text "t" :cause cause :cell [1 2 3] :got 2} @a))))

(deftest stop-omits-nil-cause
  (let [[c a] (fake)]
    (r/stop! c :r "t" :cause nil)
    (is (not (contains? @a :cause)))))

(deftest cause-of-nests-a-child-result
  (is (= {:child :go :reason :no-path :text "boom"}
         (r/cause-of :go {:status :stopped :reason :no-path :text "boom" :junk 1})))
  (is (= {:child :go :reason :no-path}
         (r/cause-of :go {:status :stopped :reason :no-path})))
  (is (= {:child :go :reason :no-path :cause {:child :walk :reason :x}}
         (r/cause-of :go {:status :stopped :reason :no-path :cause {:child :walk :reason :x}}))))

(deftest cause-of-nil-result
  (is (= {:child :go :reason :unknown} (r/cause-of :go nil))))

(deftest finish-sets-done-result
  (let [[c a] (fake)]
    (is (= :done (r/finish! c {:got 3})))
    (is (= {:status :done :got 3} @a))))

(deftest finish-without-data
  (let [[c a] (fake)]
    (r/finish! c)
    (is (= {:status :done} @a))))
