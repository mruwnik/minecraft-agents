(ns world-test.wall-places-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.wall-places :as w]))

(defn ev [t kind job data]
  (pr-str {:time-ms t :generation-id "g" :kind kind :context {:job-id job :reflex-id :hostile-near} :data data}))

(defn choice [t hostiles] (ev t :retreat.choice "j1" {:pos {:x 0.5 :y 64 :z 0.5} :hostiles hostiles}))

(defn place [t status & [reason]] (ev t :done "j1/plug/walls/fill/place" (cond-> {:name "place" :status status} reason (assoc :reason reason))))

(deftest an-attempt-is-classed-by-the-flee-choice-before-it
  (let [rows (w/scan [(choice 0 ["zombie 7 [2 64 1]" "skeleton 8 [1 64 0]"])
                      (ev 100 :child_started "j1/plug/walls" {:slot :walls})
                      (place 200 "placed") (place 900 "placed")
                      (ev 1000 :child_ended "j1/plug/walls" {:slot :walls :status :done})])]
    (is (= {["g" "j1/plug/walls"] {:t 100 :way :walls :class :beside :placed [200 900] :mob? false :end :done :done? true}} rows)))
  (is (= :clear (:class (val (first (w/scan [(choice 0 ["zombie 7 [3 64 0]" "skeleton 8 [1 64 0]"])
                                             (ev 100 :child_started "j1/plug/walls" {})
                                             (ev 200 :child_ended "j1/plug/walls" {:status :done})])))))
      "a melee mob three cells off, or a shooter beside, stands beside no side cell")
  (is (nil? (:class (val (first (w/scan [(choice 0 ["zombie 7 [1 64 0]"])
                                         (ev 5000 :child_started "j1/plug/walls" {})
                                         (ev 5100 :child_ended "j1/plug/walls" {:status :done})])))))
      "a choice older than choice-ms says nothing of where the mobs stand"))

(deftest a-failure-counts-for-a-mob-only-when-a-mob-caused-it
  (let [end (fn [& lines] (val (first (w/scan (concat [(choice 0 ["zombie 7 [1 64 1]"]) (ev 100 :child_started "j1/plug/walls" {})] lines)))))]
    (is (:mob? (end (ev 200 :child_ended "j1/plug/walls" {:status :stopped :reason :hostile-in-cell}))))
    (is (:mob? (end (place 150 "failed" "Server refused (entities near the cell: player#1, zombie#2)")
                    (ev 200 :child_ended "j1/plug/walls" {:status :stopped :reason :failed}))))
    (is (not (:mob? (end (place 150 "failed" "Server refused (entities near the cell: player#1)")
                         (ev 200 :child_ended "j1/plug/walls" {:status :stopped :reason :no-block})))))))

(deftest a-pillar-is-classed-by-a-mob-in-or-beside-the-body-cell-and-fails-for-it-by-its-refused-place
  (let [pillar (fn [hostiles end] (val (first (w/scan [(choice 0 hostiles) (ev 100 :child_started "j1/pillar" {:slot :pillar})
                                                       (ev 200 :child_ended "j1/pillar" end)]))))]
    (is (= {:way :pillar :class :beside :mob? true :done? false}
           (select-keys (pillar ["zombie 7 [1 64 1]"] {:status :stopped :reason :place-failed}) [:way :class :mob? :done?])))
    (is (= :clear (:class (pillar ["zombie 7 [2 64 0]"] {:status :done})))
        "a mob two cells off is beside a side cell, not the body's")))

(deftest the-report-gives-the-gap-and-each-class-failures-for-mobs
  (is (= {:gap {:mean 0.5 :n 3} :beside {:k 1 :n 2} :clear {:k 0 :n 1} :pillar {:beside {:k 1 :n 1} :clear {:k 0 :n 0}}}
         (w/report {:p {:way :pillar :class :beside :placed [] :done? false :mob? true}
                    :a {:class :beside :placed [0 400 1000] :done? true :mob? false}
                    :b {:class :beside :placed [] :done? false :mob? true}
                    :c {:class :clear :placed [0 500] :done? true :mob? false}
                    :d {:class :clear :placed [] :done? false :mob? false}}))
      "an attempt that failed for want of blocks says nothing of mobs"))
