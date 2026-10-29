(ns world-test.item-costs-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.item-costs :as ic]))

(def seq-no (atom 0))

(defn line
  ([job t kind data] (line job t kind data :debug))
  ([job t kind data level]
   (pr-str (cond-> {:seq (swap! seq-no inc) :generation-id "g" :time-ms t :source (if job :job :body) :kind kind :data data :level level}
             job (assoc :context {:job-id job})))))

(defn spans [lines] (ic/spans (keep ic/scan-line lines)))

(deftest a-gather-costs-the-seconds-it-held-the-body-and-gets-the-items-picked-up-meanwhile
  (is (= {["g" "j1/gather-raw_iron"] {:item "raw_iron" :s 50 :got 3}
          ["g" "j1/gather-raw_iron/fetch/gather-coal"] {:item "coal" :s 10 :got 1}}
         (spans [(line "j1/gather-raw_iron" 0 :child_started {})
                 (line nil 5000 :picked-up {:item "raw_iron" :count 2})
                 (line "j1/gather-raw_iron/fetch/gather-coal" 10000 :planned {})
                 (line nil 15000 :picked-up {:item "coal" :count 1})
                 (line "j1/gather-raw_iron" 20000 :mine.done {:got 0})
                 (line "j9" 30000 :cut {:by :hostile-near})
                 (line "j1/gather-raw_iron" 330000 :child_started {})
                 (line nil 340000 :picked-up {:item "raw_iron" :count 1})
                 (line nil 350000 :offline {})
                 (line "j1/gather-raw_iron" 950000 :child_ended {})]))
      "a reflex's and an offline body's seconds are not the gather's; a child gather's seconds are its parent's too"))

(deftest a-gather-logged-at-info-alone-counts-not
  (is (= {} (spans [(line "j1/gather-coal" 0 :stair.step {} :info) (line "j1/gather-coal" 9000 :mine.done {:got 0} :info)]))
      "pickups are debug lines: a body logging at info shows none"))

(deftest a-gather-read-again-keeps-its-longer-span
  (is (= {:item-costs {:a {:s 9 :got 1} :b {:s 1 :got 1}}}
         (ic/fold {:item-costs {:a {:s 9 :got 1}}} {:a {:s 4 :got 1} :b {:s 1 :got 1}}))))

(deftest a-prior-counts-as-many-items-as-the-gathers-rates-spread-allows
  (let [rows (fn [gathers] (into {} (map-indexed (fn [i [s got]] [i {:item "dirt" :s s :got got}])) gathers))]
    (is (nil? (ic/prior-items (rows [[10 1] [10 1] [10 1]]))) "no spread beyond the counts' own: nothing to fit")
    (is (< (ic/prior-items (rows [[100 0] [100 40]])) (ic/prior-items (rows [[100 15] [100 25]])))
        "the wider the gathers' rates spread, the fewer items the prior counts as")))
