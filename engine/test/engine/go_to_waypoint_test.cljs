(ns engine.go-to-waypoint-test
  "go-to's waypoints toward a far column whose ground is only guessed (jobs.movement.go-to.column/waypoint)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.go-to-column-test :refer [slope stub-ctx]]
            [engine.go-to-support :refer [hiding]]
            [engine.fake :as fake]
            [engine.go-to-test :refer [go! at]]
            [engine.settings :as settings]
            [engine.memory :as mem]
            [engine.test-util :as tu :refer [box]]
            [jobs.lib.near :as near]
            [jobs.movement.go-to :as go-to]
            [jobs.movement.go-to.tuning :as gts]
            [jobs.movement.go-to.aim :as gaim]
            [jobs.movement.go-to.walk :as gwalk]
            [jobs.movement.go-to.column :as column]
            [jobs.movement.go-to.result :as end]))

(defn across [{:keys [x z]} [gx gz]] (js/Math.hypot (- gx x) (- gz z)))

(defn end-pos [moved] (:to (last moved)))

;; flat ground at feet 64 (x -2..30, z -6..6), a cliff at x 31 up to a plateau at feet 84 (x 31..70); a ramp beside the
;; line, z 4..6, climbs from feet 64 at x 30 to feet 84 at x 50
(def cliff-with-ramp
  (merge (box -2 63 -6 30 63 6 "stone")
         (box 31 63 -6 70 83 3 "stone")
         (into {} (for [x (range 31 71)] (box x 63 4 x (+ 63 (min 20 (- x 30))) 6 "stone")))))

(deftest an-off-line-seen-stand-nearer-the-column-is-a-waypoint-when-the-line-gives-none
  ;; at the cliff foot: the plateau top over the line (z 0..3) is out of sight, so the line gives no stand nearer x 60
  (let [p (hiding cliff-with-ramp (fn [[x _ z]] (and (> x 30) (<= z 3))))
        way (column/waypoint (column/seen-kind-at p) 60 0 [30 64 0])]
    (is (some? way) "the ramp")
    (is (<= (across way [60 0]) 28) (pr-str way))
    (is (>= (:z way) 4) (str "on the ramp: " (pr-str way)))))

(deftest an-unseen-head-over-seen-air-counts-open
  ;; the waypoint's feet cell is seen air over seen ground, its head cell never seen (a seen slope's air): it stands;
  ;; with the feet cell unseen too it does not
  (let [land (slope 64 1)
        head-hidden (hiding land (fn [[x y _]] (or (> x 44) (and (= x 44) (= y 79)))))
        feet-hidden (hiding land (fn [[x y _]] (or (> x 44) (and (= x 44) (#{78 79} y)))))]
    (is (= {:x 44 :y 78 :z 0} (column/waypoint (column/seen-kind-at head-hidden) 60 0 [0 64 0])))
    (is (not= 44 (:x (column/waypoint (column/seen-kind-at feet-hidden) 60 0 [0 64 0]))))))

(deftest a-column-across-an-uncrossable-gap-gives-up-with-a-reason-after-a-few-walks
  ;; flat ground to x 20, nothing at x 21..29 (a gap no walk crosses), the column x 60 on land at feet 84 beyond it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (merge (box -2 63 -3 20 63 3 "stone") (box 30 63 -3 70 83 3 "stone"))
              {:keys [out eng ticks]} (await (go! {:blocks blocks :light [15 0] :self {:pos {:x 0 :y 64 :z 0}}}
                                                  {:pos [60 0] :range 0}))
              moved (map :data (mem/entries (mem/view (:store eng)) :moved))
              at-edge (count (drop-while #(< (:x (:to %)) 19) moved))]
          (is (false? (:arrived @out)) (pr-str @out))
          (is (keyword? (:why @out)) (pr-str @out))
          (is (< ticks 30) "the call ends")
          (is (<= 18 (:x (end-pos moved)) 20) "the waypoints led to the gap's edge")
          ;; from the walk to the edge on: it, a walk per way-look, the re-aim, the column looks, the blocked tries
          (is (<= at-edge (+ 1 (count (gts/way-looks)) 1 gaim/max-column-looks (gts/max-blocked)))
              (pr-str (map (juxt (comp :x :to) :status) moved))))))))

(defn ^:async waypoint-walk!
  "walk-waypoint! from x 0 toward the column x 60 with near/walk-round! stubbed: the body ends at x to, the walk says
  result/status, and the pose callback sees x 5 (a pose of the walk). The memory after it, and the poses recorded."
  [to result status & [init]]
  (let [p (tu/fake {:self {:pos [0.5 64 0.5]} :blocks (box -2 63 -3 70 63 3 "stone")})
        mem* (atom (or init {}))
        poses (atom [])
        c (stub-ctx p mem* {:pos [60 0] :range 0})]
    (with-redefs [near/walk-round! (fn [c _ _ _]
                                     (when-let [f (:on-pose c)] (f {:x 5 :y 64 :z 0}) (swap! poses conj 5))
                                     (swap! (fake/state p) assoc-in [:self :pos] [to 64 0.5])
                                     (js/Promise.resolve {:result result :status status :to {:x to :y 64 :z 0}}))]
      (await (gwalk/walk-waypoint! c {:x 60 :y 64 :z 0} {:x 20 :y 64 :z 0} {})))
    {:mem @mem* :poses @poses}))

(deftest a-waypoint-walk-keeps-the-approach-and-a-short-one-is-a-miss
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [mem poses]} (await (waypoint-walk! 20.5 {:status :arrived} "arrived"))]
          (is (= [5] poses) "the walk records the approach (approach-recorder)")
          (is (< (:last-dist mem) 41) (pr-str mem))
          (is (nil? (:waypoint-misses mem)) "20 blocks nearer: no miss"))
        (let [{:keys [mem]} (await (waypoint-walk! 1.6 {:status :arrived} "arrived"))]
          (is (= 1 (:waypoint-misses mem)) "1 block nearer is less than the 2-block gain: a miss"))))))

(deftest a-refused-waypoint-walk-is-named-by-the-give-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [mem]} (await (waypoint-walk! 0.5 {:status :no-path :reason :needs-health} "blocked"))]
          (is (= :needs-health (:waypoint-why mem)))
          (is (re-find #"waypoint walk toward it failed: needs-health"
                       (end/give-up-words [60 64 0] (end/give-up-fields {:status :no-path :reason :goal-not-standable
                                                                          :waypoint-why (:waypoint-why mem)})))))))))

(deftest a-waypoint-walk-that-went-well-clears-an-earlier-failure-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [mem]} (await (waypoint-walk! 20.5 {:status :arrived} "arrived" {:waypoint-why :needs-health}))]
          (is (nil? (:waypoint-why mem))))))))

(deftest a-stand-already-walked-to-is-no-waypoint-again
  (let [p (hiding cliff-with-ramp (fn [[x _ z]] (and (> x 30) (<= z 3))))
        ka (column/seen-kind-at p)
        way (column/waypoint ka 60 0 [30 64 0])
        again (column/waypoint ka 60 0 [30 64 0] 2 #{[(:x way) (:y way) (:z way)]})]
    (is (not= way again))))

(deftest the-off-line-cone-is-a-setting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (hiding cliff-with-ramp (fn [[x _ z]] (and (> x 30) (<= z 3))))
              narrow (await (settings/with-settings {:jobs.movement.go-to.tuning/off-line-cone 5}
                              #(column/waypoint (column/seen-kind-at p) 60 0 [30 64 0])))]
          (is (nil? narrow)))))))
