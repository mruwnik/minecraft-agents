(ns engine.way-log-test
  "What jobs.lib.walk.way-log logs of a priced decision and of its ways later: the counts as priced, the light as perceived,
  each way read once its cells are in memory, the ways dropped past the cap said."
  (:require [cljs.test :refer [deftest is]]
            [engine.memory :as mem]
            [engine.way-on-test :as wt]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.walk.way-log :as way-log]
            [jobs.lib.walk.world :as wworld]))

(defn stub
  "A ctx whose body memory is an engine.memory store and whose events go to the atom seen ([kind data])."
  [seen]
  (let [store (atom {:data {} :now (constantly 0) :world-time (constantly 0)})]
    {:primitives #js {}
     :view #(mem/view store)
     :remember (fn [kind data policy] (mem/write! store kind data policy))
     :forget (fn [kind pred] (mem/forget-where! store kind pred))
     :emit (fn [kind _ data] (swap! seen conj [kind data]))}))

(defn corridor
  "A corridor along x at z 0 (y 64 and 65) in rock, seen from x -12 to 12, open where open-x?."
  [open-x?]
  (wt/known-of (fn [[x y z]] (not (and (= z 0) (#{64 65} y) (open-x? x)))) {} [-12 60 -8 12 68 8]))

(defn with-body
  "Calls f with the body's block memory known and the light at its feet light ({:light n :lit? bool}, nil no light data)."
  [known light f]
  (with-redefs [wworld/memory-sections (constantly known)
                sh-sense/feet (constantly {:x 0 :y 64 :z 0})
                sh-sense/cell-light (constantly (:light light))
                sh-sense/lit-here? (constantly (:lit? light))]
    (f)))

(defn pending [c] (mapv (comp :way :data) (mem/entries ((:view c)) :way-pending)))

(def ways [[8 64 0 0] [-8 64 0 1]])

(deftest a-decision-logs-the-counts-it-priced-and-each-way-is-read-once-its-cells-are-seen
  (let [seen (atom []) c (stub seen) scan {:k 1 :n 2 :ways ways}]
    (with-body wt/open-corridor {:light 15 :lit? true}
      (fn []
        (way-log/log-priced! c scan)
        (way-log/log-priced! c scan)
        (way-log/resolve! c)))
    (is (= [[:way-on.priced (assoc scan :set :lit)] [:way-on.priced (assoc scan :set :lit)]] @seen)
        "the counts as given, not scanned again; the corridor's ends are not seen yet")
    (is (= ways (pending c)) "a way already waiting is not kept twice")
    (reset! seen [])
    (with-body (corridor #(<= -12 % 8)) nil #(way-log/resolve! c))
    (with-body (corridor #(<= -12 % 8)) nil #(way-log/resolve! c))
    (is (= [[:way-on.seen {:way [8 64 0 0] :on false}] [:way-on.seen {:way [-8 64 0 1] :on true}]] @seen)
        "rock east of x 8, open west: each read once")
    (is (= [] (pending c)))))

(deftest past-the-cap-the-oldest-waiting-ways-are-dropped-and-said
  (let [seen (atom []) c (stub seen)
        way #(vector (+ 100 %) 64 0 0)]  ; far past the seen box: never read
    (with-body wt/open-corridor {:light 15 :lit? true}
      (fn [] (doseq [i (range 0 (+ way-log/pending-cap 2) 6)] (way-log/log-priced! c {:k 0 :n 0 :ways (mapv way (range i (+ i 6)))}))))
    (is (= [[:way-on.censored {:way (way 0) :why :evicted}] [:way-on.censored {:way (way 1) :why :evicted}]]
           (filter #(= :way-on.censored (first %)) @seen))
        "66 ways listed, 64 kept: the two oldest go as the last six come")
    (is (= way-log/pending-cap (count (pending c))))
    (is (= (way 2) (first (pending c))))))

(deftest the-setting-is-the-light-the-body-perceives-at-its-feet
  (let [setting (fn [light] (let [seen (atom [])]
                              (with-body wt/open-corridor light #(way-log/log-priced! (stub seen) {:k 0 :n 0 :ways []}))
                              (:set (second (first @seen)))))]
    (is (= :unknown (setting {:light nil :lit? true})) "no light data: unknown, not a guess")
    (is (= :dark (setting {:light 0 :lit? false})))
    (is (= :lit (setting {:light 15 :lit? true}))))
  (let [seen (atom [])]
    (with-body wt/open-corridor nil #(way-log/log-priced! (stub seen) nil))
    (is (= [] @seen) "a decision that priced no way counts logs none")))
