(ns world-test.wall-places-test
  (:require [cljs.test :refer [deftest is]]
            [jobs.lib.cost.danger :as danger]
            [world-test.wall-places :as w]))

(defn ev [t kind job data]
  (pr-str {:time-ms t :generation-id "g" :kind kind :context {:job-id job :reflex-id :hostile-near} :data data}))

(defn choice [t hostiles & [pos]] (ev t :retreat.choice "j1" {:pos (or pos {:x 0.5 :y 64 :z 0.5}) :hostiles hostiles}))

(defn place [t status & [reason]] (ev t :done "j1/plug/walls/fill/place" (cond-> {:name "place" :status status} reason (assoc :reason reason))))

(deftest an-attempt-is-classed-by-the-flee-choice-before-it
  (let [rows (w/scan [(choice 0 ["zombie 7 [2 64 1]" "skeleton 8 [1 64 0]"])
                      (ev 100 :child_started "j1/plug/walls" {:slot :walls})
                      (place 200 "placed") (place 900 "placed")
                      (ev 1000 :child_ended "j1/plug/walls" {:slot :walls :status :done})])]
    (is (= {["g" "j1/plug/walls"] {:t 100 :end-t 1000 :way :walls :arrive-s (/ (- (js/Math.hypot 2 1) 1.4) (danger/chase-speed "zombie"))
                                   :class :reached :placed [200 900] :mob? false :end :done :done? true}} rows)))
  (is (= :clear (:class (val (first (w/scan [(choice 0 ["zombie 7 [6 64 0]" "skeleton 8 [1 64 0]"])
                                             (ev 100 :child_started "j1/plug/walls" {})
                                             (ev 600 :child_ended "j1/plug/walls" {:status :done})])))))
      "a melee mob six cells off comes after half a second of walls; a shooter strikes no blow")
  (is (= :reached (:class (val (first (w/scan [(choice 0 ["zombie 7 [6 64 0]"])
                                               (ev 100 :child_started "j1/plug/walls" {})
                                               (ev 9100 :child_ended "j1/plug/walls" {:status :done})])))))
      "the same mob comes before nine seconds of walls are up")
  (is (nil? (:class (val (first (w/scan [(choice 0 ["zombie 7 [1 64 0]"])
                                         (ev 5000 :child_started "j1/plug/walls" {})
                                         (ev 5100 :child_ended "j1/plug/walls" {:status :done})])))))
      "a choice older than choice-ms says nothing of where the mobs stand"))

(deftest the-arrival-is-the-engines-from-the-bodys-exact-place
  (let [at {:x 0.9 :y 64 :z 0.1}
        rows (w/scan [(choice 0 ["zombie 7 [3 64 0]"] at) (ev 100 :child_started "j1/plug/walls" {}) (ev 200 :child_ended "j1/plug/walls" {:status :done})])]
    (is (= (danger/arrival-s "zombie" {:x 3.5 :y 64 :z 0.5} at) (:arrive-s (val (first rows)))))))

(deftest a-failure-counts-for-a-mob-only-when-a-mob-caused-it
  (let [end (fn [& lines] (val (first (w/scan (concat [(choice 0 ["zombie 7 [1 64 1]"]) (ev 100 :child_started "j1/plug/walls" {})] lines)))))]
    (is (:mob? (end (ev 200 :child_ended "j1/plug/walls" {:status :stopped :reason :hostile-in-cell}))))
    (is (:mob? (end (place 150 "failed" "Server refused (entities near the cell: player#1, zombie#2)")
                    (ev 200 :child_ended "j1/plug/walls" {:status :stopped :reason :failed}))))
    (is (not (:mob? (end (place 150 "failed" "Server refused (entities near the cell: player#1)")
                         (ev 200 :child_ended "j1/plug/walls" {:status :stopped :reason :no-block})))))))

(defn begun [t class & [at]] (ev t :retreat.pillar "j1" {:class class :at (or at [0 64 0])}))

(deftest a-pillar-is-classed-by-the-engines-retreat-pillar-and-fails-for-a-mob-only-with-mob-evidence
  (let [pillar (fn [end] (val (first (w/scan [(choice 0 ["zombie 7 [1 64 1]"]) (begun 50 :clear)
                                              (ev 100 :child_started "j1/pillar" {:slot :pillar}) (ev 200 :child_ended "j1/pillar" end)]))))
        refused (fn [mob] {:status :stopped :reason :place-failed :ending {:data (cond-> {:reason :place-failed} mob (assoc :mob mob))}})]
    (is (= {:way :pillar :class :clear :situation ["g" [0 64 0]] :mob? true :done? false}
           (select-keys (pillar (refused "zombie")) [:way :class :situation :mob? :done?]))
        "the engine's class, not one guessed from the choice's floored cells")
    (is (false? (:mob? (pillar (refused nil)))) "a refusal with no hostile near says nothing of mobs")
    (is (:mob? (pillar {:status :stopped :reason :mob-in-cell})))
    (is (:mob? (pillar {:status :stopped :reason :off-column :ending {:data {:mob "zombie"}}})) "a knockback with a hostile near")
    (is (nil? (:class (val (last (sort-by key (w/scan [(begun 0 :beside) (ev 100 :child_started "j1/pillar" {}) (ev 150 :child_ended "j1/pillar" {:status :done})
                                                       (ev 9000 :child_started "j1/pillar" {}) (ev 9100 :child_ended "j1/pillar" {:status :done})]))))))
        "a retreat.pillar is read by the one pillar after it")
    (is (= {["g" "j1/pillar" 100] :beside ["g" "j1/pillar" 5100] :clear}
           (update-vals (w/scan [(begun 0 :beside) (ev 100 :child_started "j1/pillar" {}) (ev 200 :child_ended "j1/pillar" {:status :stopped :reason :mob-in-cell})
                                 (begun 5000 :clear [2 64 0]) (ev 5100 :child_started "j1/pillar" {}) (ev 5200 :child_ended "j1/pillar" {:status :done})])
                        :class))
        "each pillar of the one job is a row")))

(deftest the-report-counts-one-pillar-per-situation-the-first
  (is (= {:beside {:k 1 :n 2} :clear {:k 0 :n 1}}
         (:pillar (w/report {:a {:way :pillar :t 0 :class :beside :situation [:g [0 64 0]] :mob? true :done? false}
                             :b {:way :pillar :t 1 :class :beside :situation [:g [0 64 0]] :mob? true :done? false}
                             :c {:way :pillar :t 2 :class :beside :situation [:g [0 64 0]] :done? true :mob? false}
                             :d {:way :pillar :t 3 :class :clear :situation [:g [0 64 0]] :done? true :mob? false}
                             :e {:way :pillar :t 0 :class :beside :done? true :mob? false}})))
      "the retries at one cell and class after a failure are left out; a row of an older log, with no situation, counts"))

(deftest the-report-gives-the-gap-and-each-class-failures-for-mobs
  (is (= {:gap {:mean 0.5 :n 3} :reached {:k 1 :n 2} :clear {:k 0 :n 1} :pillar {:beside {:k 1 :n 1} :clear {:k 0 :n 0}}}
         (w/report {:p {:way :pillar :class :beside :placed [] :done? false :mob? true}
                    :a {:class :reached :placed [0 400 1000] :done? true :mob? false}
                    :b {:class :reached :placed [] :done? false :mob? true}
                    :c {:class :clear :placed [0 500] :done? true :mob? false}
                    :d {:class :clear :placed [] :done? false :mob? false}}))
      "an attempt that failed for want of blocks says nothing of mobs"))

(deftest a-row-classed-before-arrival-gives-way-to-its-rescan
  (let [old {:t 0 :class :clear :done? true}
        new {:t 0 :end-t 900 :arrive-s 1 :class :reached :done? true}
        kept {:t 5 :end-t 900 :arrive-s nil :class :clear :done? true}]
    (is (= {:wall-places {:a new :b kept}} (w/fold {:wall-places {:a old :b kept}} {:a new :b (assoc kept :class :reached)}))
        "a row with :end-t stays; one without is rescanned")))

(deftest an-attempt-with-an-open-count-is-classed-by-the-seconds-to-place-every-cell
  (let [class-with (fn [open end]
                     (:class (val (first (w/scan [(choice 0 ["zombie 7 [9 64 0]"])
                                                  (ev 100 :child_started "j1/plug/walls" {})
                                                  (ev 150 :walls.open "j1/plug/walls" {:open open})
                                                  (ev end :child_ended "j1/plug/walls" {:status :done})])))))]
    (is (= :reached (class-with 8 300)) "an attempt that ended early (200 ms) still needed 8 placements, longer than the mob's arrival")
    (is (= :clear (class-with 1 300)) "one cell is placed before the mob arrives")))
