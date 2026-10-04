(ns view.schedule-test
  "The hub's render scheduling and stream replay (ported from test/view-web-hub.test.mjs)."
  (:require [clojure.test :refer [deftest are is]]
            [view.schedule :as s]))

(defn entry
  ([id due-at] (entry id due-at true))
  ([id due-at visible] #js {:id id :dueAt due-at :visible visible}))

(defn raf-entry [id due-at visible] #js {:id id :dueAt due-at :visible visible :raf true :fps "raf"})

(deftest due-scenes-cases
  (are [entries now max expected] (= expected (vec (s/due-scenes (into-array entries) now max)))
    ;; only visible scenes whose time has come
    [(entry "a" 100) (entry "b" 100 false) (entry "c" 300)] 200 js/Infinity ["a"]
    ;; longest overdue first
    [(entry "a" 150) (entry "b" 50) (entry "c" 100)] 200 js/Infinity ["b" "c" "a"]
    ;; ties keep their order
    [(entry "a" 10) (entry "b" 10) (entry "c" 10)] 20 js/Infinity ["a" "b" "c"]
    ;; at most max
    [(entry "a" 1) (entry "b" 2) (entry "c" 3)] 10 2 ["a" "b"]
    ;; due exactly now
    [(entry "a" 200)] 200 js/Infinity ["a"]
    [] 200 js/Infinity []
    ;; a raf target is always due and goes first, outside max
    [(entry "a" 1) (raf-entry "big" 9999 true) (entry "b" 2)] 10 1 ["big" "a"]
    ;; an invisible raf target is not due
    [(raf-entry "big" 0 false)] 10 js/Infinity []))

(defn near? [a b] (< (js/Math.abs (- a b)) 1e-9))

(deftest next-due-at-cases
  (are [due-at now fps expected] (near? expected (s/next-due-at due-at now fps))
    ;; one period after it was due
    1000 1010 6 (+ 1000 (/ 1000 6))
    ;; a little late still keeps the cadence
    1000 1100 6 (+ 1000 (/ 1000 6))
    ;; more than a period behind restarts from now
    1000 1300 6 (+ 1300 (/ 1000 6))
    ;; raf is due again at the next frame
    1000 1016 "raf" 1016))

;; 60 Hz animation frames for `seconds`; each frame renders what is due, at most `per-frame`; returns the targets, each with
;; its render times in .-renders, and the number of renders of each frame
(defn simulate [{:keys [scenes fps seconds per-frame]}]
  (let [state (into-array (map (fn [{:keys [id visible start]}] #js {:id id :visible visible :dueAt (or start 0) :renders #js []}) scenes))
        frame-ms (/ 1000 60)]
    (loop [now 0 counts []]
      (if (>= now (* seconds 1000))
        {:targets (vec state) :counts counts}
        (let [due (s/due-targets state now per-frame)]
          (doseq [^js t due]
            (.push (.-renders t) now)
            (set! (.-dueAt t) (s/next-due-at (.-dueAt t) now fps)))
          (recur (+ now frame-ms) (conj counts (alength due))))))))

(defn rate [^js t seconds] (/ (alength (.-renders t)) seconds))

(deftest eleven-visible-scenes-at-6-fps-get-6-fps-never-more-than-2-per-frame
  (let [{:keys [targets counts]} (simulate {:scenes (map (fn [i] {:id (str "s" i) :visible true :start (* i (/ 1000 6 11))}) (range 11))
                                            :fps 6 :seconds 10 :per-frame 2})]
    (is (every? #(< (js/Math.abs (- (rate % 10) 6)) 0.3) targets))
    (is (<= (apply max counts) 2))))

(deftest invisible-scenes-never-render-and-a-scene-that-becomes-visible-renders-at-once
  (let [{[on off] :targets} (simulate {:scenes [{:id "on" :visible true} {:id "off" :visible false}] :fps 6 :seconds 2 :per-frame 2})]
    (is (zero? (alength (.-renders ^js off))))
    (is (>= (alength (.-renders ^js on)) 11))
    (is (= ["off"] (vec (s/due-scenes #js [#js {:id "off" :visible true :dueAt 5}] 1000000))))))

(deftest eleven-scenes-that-all-start-due-at-once-settle-to-6-fps
  (let [{:keys [targets]} (simulate {:scenes (map (fn [i] {:id (str "s" i) :visible true}) (range 11)) :fps 6 :seconds 10 :per-frame 2})]
    (is (every? #(>= (rate % 10) 5.8) targets))))

;; 60 Hz frames for `seconds` through plan-frame: one raf target "big" (big-ms per render) and `cards` cards at 6 fps
;; (card-ms per render); returns the targets and each frame's cost
(defn simulate-plan [{:keys [cards big-ms card-ms seconds budget-ms with-big] :or {seconds 10 budget-ms 8 with-big true}}]
  (let [targets (into-array
                 (concat (when with-big [#js {:id "big" :raf true :fps "raf" :visible true :dueAt 0 :cost big-ms :renders #js []}])
                         (map (fn [i] #js {:id (str "c" i) :raf false :fps 6 :visible true :dueAt (* i (/ 1000 6 cards)) :cost card-ms :renders #js []})
                              (range cards))))]
    (loop [n 0 costs []]
      (if (>= n (* seconds 60))
        {:targets (vec targets) :frame-costs costs}
        (let [now (/ (* n 1000) 60)
              frame (volatile! 0)]
          (s/plan-frame targets now budget-ms
                        (fn [^js t]
                          (.push (.-renders t) now)
                          (set! (.-dueAt t) (s/next-due-at (.-dueAt t) now (.-fps t)))
                          (vswap! frame + (.-cost t))
                          (.-cost t)))
          (recur (inc n) (conj costs @frame)))))))

(deftest plan-frame-the-raf-target-renders-every-frame-and-the-cards-keep-6-fps
  (are [args]
       (let [{[big & cards] :targets} (simulate-plan args)]
         (and (= 600 (alength (.-renders ^js big)))
              (every? #(>= (rate % 10) 5.8) cards)))
    ;; cheap big view
    {:cards 11 :big-ms 3 :card-ms 2}
    ;; big view with a few cards
    {:cards 4 :big-ms 8 :card-ms 2}))

(deftest plan-frame-cards-stay-within-the-budget-next-to-the-big-view
  (is (<= (apply max (:frame-costs (simulate-plan {:cards 11 :big-ms 3 :card-ms 2}))) (+ 8 2))))

(deftest plan-frame-without-the-big-view-the-cards-use-the-same-budget
  (let [{:keys [frame-costs targets]} (simulate-plan {:cards 11 :big-ms 0 :card-ms 2 :with-big false})]
    (is (<= (apply max frame-costs) (+ 8 2)))
    (is (every? #(>= (rate % 10) 5.8) targets))))

(deftest plan-frame-a-big-view-that-eats-the-frame-slows-the-cards-but-starves-nobody
  (let [{[big & cards] :targets} (simulate-plan {:cards 11 :big-ms 12 :card-ms 2})]
    (is (= 600 (alength (.-renders ^js big))))
    (is (every? #(>= (rate % 10) 3.5) cards))))

(deftest plan-frame-an-invisible-raf-target-does-not-render-and-the-cards-are-unaffected
  (is (= ["c"] (vec (s/plan-frame #js [#js {:id "big" :raf true :fps "raf" :visible false :dueAt 0}
                                       #js {:id "c" :raf false :fps 6 :visible true :dueAt 0}]
                                  5 8 (constantly 1))))))

(deftest plan-frame-over-budget-renders-one-overdue-card-per-frame-and-no-merely-due-one
  (are [cards expected]
       (= expected (vec (s/plan-frame (into-array (cons #js {:id "big" :raf true :fps "raf" :visible true :dueAt 0} cards))
                                      1000 8 (fn [^js t] (if (.-raf t) 20 1)))))
    ;; 6 fps: more than 500 / 6 ms late is overdue
    [#js {:id "a" :fps 6 :visible true :dueAt 100} #js {:id "b" :fps 6 :visible true :dueAt 200}] ["big" "a"]
    [#js {:id "a" :fps 6 :visible true :dueAt 950}] ["big"]))

;; the hub's replay of the last pose and hud of each agent to scenes added after the stream opened
(defn pose-of [agent n] #js {:agent agent :pose #js {:n n}})
(defn replayed [^js cache agent]
  (let [got (volatile! [])]
    (.replay cache agent (fn [event data] (vswap! got conj [event data])))
    @got))

(deftest a-scene-added-for-an-already-streamed-agent-is-fed-the-latest-pose-and-hud
  (let [^js cache (s/event-cache)
        latest (pose-of "Bob" 2)
        hud #js {:agent "Bob" :hud 7}]
    (.record cache "pose" (pose-of "Bob" 1))
    (.record cache "pose" latest)
    (.record cache "hud" hud)
    (.record cache "column" #js {:agent "Bob" :cx 0 :cz 0})
    (is (= [["pose" latest] ["hud" hud]] (replayed cache "Bob")))))

(deftest replay-is-per-agent-and-every-scene-of-one-agent-gets-it
  (let [^js cache (s/event-cache)
        bob (pose-of "Bob" 1)]
    (.record cache "pose" bob)
    (.record cache "pose" (pose-of "Ann" 5))
    (is (= [] (replayed cache "Cy")))
    (is (= [["pose" bob]] (replayed cache "Bob")))
    (is (= [["pose" bob]] (replayed cache "Bob")))))

(deftest a-scene-closed-and-re-added-is-fed-the-last-pose-at-once
  (let [^js cache (s/event-cache)
        bob (pose-of "Bob" 1)]
    (.record cache "pose" bob)
    (.keepOnly cache ["Bob"])
    (is (= [["pose" bob]] (replayed cache "Bob")))))

(deftest keep-only-forgets-the-agents-a-reopened-stream-no-longer-carries-and-only-those
  (let [^js cache (s/event-cache)
        ann (pose-of "Ann" 5)]
    (.record cache "pose" (pose-of "Bob" 1))
    (.record cache "hud" #js {:agent "Bob" :hud 1})
    (.record cache "pose" ann)
    (.keepOnly cache ["Ann"])
    (is (= [] (replayed cache "Bob")))
    (is (= [["pose" ann]] (replayed cache "Ann")))))

(deftest stream-key-cases
  (are [agents radii expected] (= expected (js->clj (s/stream-key agents radii)))
    ["w/b" "w/a" "w/b"] [2 3 1] {"key" "w/a,w/b|3" "agents" ["w/a" "w/b"] "radius" 3}
    [] [] {"key" "|0" "agents" [] "radius" 0}))

(deftest percentile-cases
  (are [values p expected] (= expected (s/percentile (into-array values) p))
    [] 0.5 nil
    [3 1 2] 0.5 2
    [3 1 2] 0.95 3
    [5] 0 5))
