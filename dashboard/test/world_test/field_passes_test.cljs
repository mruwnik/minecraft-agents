(ns world-test.field-passes-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.field-passes :as f]))

(defn line [t id kind data]
  (pr-str {:time-ms t :generation-id "g" :source :job :kind kind :context {:job-id id} :data data}))

(defn done [t id steps field]
  (line t id :farm-tend.done {:steps steps :field field :text "farm tend done: x"}))

(deftest a-pass-books-its-step-childrens-seconds-and-units
  (let [rows (f/passes (keep f/scan-line [(line 0 "j1/harvest" :child_started {:slot :harvest})
                                          (line 30000 "j1/harvest" :child_ended {:slot :harvest})
                                          (line 31000 "j1/plant" :child_started {:slot :plant})
                                          (line 33000 "j1/plant" :child_ended {:slot :plant})
                                          (done 34000 "j1" {:harvest {:cut 4} :plant {:planted 2}} {:crops 8 :bare 1 :untilled 1})]))]
    (is (= [{:fields [{:cells 10 :ripe 4 :sowable 4}] :steps {:harvest {:s 30 :units 4} :plant {:s 2 :units 2}}}] rows))))

(deftest one-ending-logged-twice-counts-once
  (let [ended (line 34500 "j1" :child_ended {:ending {:kind :done :data {:steps {:harvest {:cut 1}} :field {:crops 2} :text "farm tend done: 2"}}})]
    (is (= 1 (count (f/passes (keep f/scan-line [(done 34000 "j1" {:harvest {:cut 1}} {:crops 2}) ended])))))))

(deftest the-prior-is-fitted-per-field-and-the-work-per-step
  (let [r (f/report [{:fields [{:cells 10 :ripe 1 :sowable 5} {:cells 10 :ripe 3 :sowable 1}] :steps {:till {:s 3 :units 2}}}
                     {:fields [{:cells 10 :ripe 2 :sowable 3}] :steps {:till {:s 5 :units 4}}}])]
    (is (= 3 (get-in r [:prior :fields])))
    (is (< 0.15 (let [[a b] (get-in r [:prior :ripe])] (/ a (+ a b))) 0.25) "the mean of the shares, 0.2")
    (is (= {:n 2 :fixed-s 1 :per-unit-s 1} (get-in r [:work :till])))))
