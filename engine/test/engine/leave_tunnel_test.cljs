(ns engine.leave-tunnel-test
  "jobs.access.leave-tunnel: the mouth as a pure function, then whole ways out after a real tunnel in the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [jobs.lib.ledger :as ledger]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.job-api :as job-api]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as world]
            [jobs.access.leave-tunnel :as leave-tunnel]))

(def tunnel-job 'jobs.access.tunnel)
(def job 'jobs.access.leave-tunnel)

;; ---------------------------------------------------------------- the mouth, pure

(defn world-of
  "A block-at: air for the cells in air-cells and above y 64, stone elsewhere."
  [air-cells]
  (fn [[_ y _ :as cell]] (if (or (contains? air-cells cell) (> y 64)) "air" "stone")))

(defn dug-of [cells] (mapv (fn [cell] {:cell cell :block "stone"}) cells))

(defn mouth-of [entry cells air-cells]
  (set (leave-tunnel/mouth (dug-of cells) entry (world-of (into (set cells) air-cells)))))

(deftest the-mouth-is-the-open-ground-level-cells-beside-open-air
  (are [entry cells air-cells expected] (= expected (mouth-of entry cells air-cells))
    ;; flat ground: both cells of a short cut
    [0 65 0] [[0 64 -1] [0 64 -2]] #{} #{[0 64 -1] [0 64 -2]}
    ;; a stair down four from [0 65 0] north: the three cells at ground level, the rest is under rock
    [0 65 0] [[0 64 -1] [0 64 -2] [0 63 -2] [0 64 -3] [0 63 -3] [0 62 -3] [0 63 -4] [0 62 -4] [0 61 -4]] #{}
    #{[0 64 -1] [0 64 -2] [0 64 -3]}
    ;; a deep pocket beside a dug cell under the entry floor is not the mouth
    [0 65 0] [[0 64 -1] [0 60 -5]] #{[0 60 -6]} #{[0 64 -1]}))

(deftest a-stair-up-into-a-cliff-has-the-face-for-a-mouth
  (let [cliff (fn [[x y z]] (if (and (< z 0) (<= 50 y 80)) "stone" (if (< y 60) "stone" "air")))
        cells [[0 61 -1] [0 62 -1] [0 63 -1] [0 62 -2] [0 63 -2] [0 64 -2] [0 63 -3] [0 64 -3]]
        block-at (fn [cell] (if (some #{cell} cells) "air" (cliff cell)))]
    (is (= #{[0 61 -1] [0 62 -1] [0 63 -1]} (set (leave-tunnel/mouth (dug-of cells) [0 60 0] block-at))))))

;; ---------------------------------------------------------------- the escape, pure

(deftest escape-attempts-go-back-along-the-heading-then-the-others-then-override-zones
  (are [heading ignore? expected] (= expected (mapv (juxt :heading :ignore-zones?) (leave-tunnel/escape-attempts heading ignore?)))
    :east false [[:west false] [:north false] [:east false] [:south false]
                 [:west true] [:north true] [:east true] [:south true]]
    :north true [[:south true] [:north true] [:east true] [:west true]]))

(deftest the-escape-stair-runs-opposite-to-the-tunnels-own-stair
  (are [line-dir expected] (= expected (leave-tunnel/escape-stair {:dir line-dir} 65))
    :down {:dir :up :y 65}
    :up {:dir :down :y 65}))

(deftest only-an-access-reason-warrants-the-override
  (are [results expected] (= expected (leave-tunnel/zones-blocked? results))
    [{:reason :zone} {:reason :no-floor}] true
    [{:reason :footprint}] true
    [{:reason :no-floor} {:reason :dig-failed}] false
    [] false))

;; ---------------------------------------------------------------- the fake world

(defn stone
  "Stone over x0..x1, y0..y1, z0..z1, as fake blocks."
  [x0 x1 y0 y1 z0 z1]
  (into {} (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))] [(str x "," y "," z) "stone"])))

(def eight-down (assoc (stone -12 12 50 64 -3 3) "6,57,0" "iron_ore"))
(def entry [-3 65 0])
(def mouth #{[-2 64 0] [-1 64 0] [0 64 0]})
(def drops {"wall_torch" "torch" "stone" "cobblestone"})

(defn inventory [& {:keys [torches cobble] :or {torches 8 cobble 10}}]
  (cond-> [{:name "iron_pickaxe" :count 1} {:name "torch" :count torches}]
    (pos? cobble) (conj {:name "cobblestone" :count cobble})))

(defn keep-body-put!
  "The fake's collect moves the body onto the drop; the real one picks it up from where it stands."
  [p]
  (let [w (.-world p)
        st (fake/state p)]
    (.override w "collect" (fn ^:async f [token a impl]
                             (let [at (get-in @st [:self :pos])
                                   r (await (impl token a))]
                               (swap! st assoc-in [:self :pos] at)
                               r)))))

(defn setup
  "An engine over the fake world; a recording parent runs the tunnel as a child, calls between with the body, then runs
  leave-tunnel with its result and leave-args, keeping both results in :tun and :out. :p and :dir restart over an
  earlier engine's world and state."
  [spec targs largs & {:keys [dir p between]}]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p' (or p (tu/fake (merge {:self {:pos {:x 0 :y 65 :z 0}} :drops drops} (dissoc spec :zones))))
        tun (atom :not-done)
        out (atom :not-done)
        w (world/of-data {} {} (get spec :zones []))
        parent {:check (fn [c] (if-let [t (:tunnel (ctx/mem c))]
                                 (boolean (ctx/check-child c :kid job (assoc largs :tunnel t)))
                                 true))
                :round (fn ^:async recording-round [c]
                         (if-let [t (:tunnel (ctx/mem c))]
                           (let [r (await (ctx/call-child c :kid job (assoc largs :tunnel t)))]
                             (when (= :done r) (reset! out (ctx/child-result c :kid)))
                             r)
                           (let [r (await (ctx/call-child c :in tunnel-job targs))]
                             (when (= :done r)
                               (let [res (ctx/child-result c :in)]
                                 (reset! tun res)
                                 (when between (between p'))
                                 (ctx/update-mem! c assoc :tunnel (select-keys res [:line :dug :torches]))))
                             :continue)))}
        eng (core/create {:primitives p' :jobs (assoc registry/jobs 'recording-parent parent)
                          :triggers triggers/all :dir (or dir (tu/tmp-dir)) :now #(deref clock) :world w
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (let [id (when-not p
               (keep-body-put! p')
               (core/submit! eng '(recording-parent) {}))]
      {:eng eng :p p' :clock clock :seen seen :tun tun :out out :id id})))

(defn waiting
  "The reason the recording parent waits with (job.waiting), or nil."
  [{:keys [eng id]}]
  (:waiting (job-api/summary eng id)))

(defn ^:async ticks-while! [{:keys [eng clock]} more?]
  (loop [i 0]
    (when (and (< i 600) (seq (:list (core/state eng))) (more?))
      (swap! clock + 500)
      (await (core/tick! eng))
      (recur (inc i)))))

(defn ^:async run-out! [s] (await (ticks-while! s (constantly true))) s)

(defn block-at [p [x y z]] (.-name (.blockAt p #js {:x x :y y :z z})))
(defn feet [p] (let [pos (.-pos (.self p))] (mapv js/Math.floor [(.-x pos) (.-y pos) (.-z pos)])))
(defn carried [p item]
  (reduce + (map #(.-count %) (filter #(= item (.-name %)) (array-seq (.-inventory (.self p)))))))
(defn calls-of [p kind]
  (mapv #(js->clj (.-pos (.-args %)) :keywordize-keys true) (filter #(= kind (.-name %)) (.-calls (.-world p)))))
(defn events-of [{:keys [seen]} kind] (filter #(= kind (:kind %)) @seen))
(defn the-ledger [eng] (ledger/open-entries (mem/view (:store eng))))

(deftest every-torch-comes-back-and-the-mouth-is-sealed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p tun out] :as s} (await (run-out! (setup {:blocks eight-down :inventory (inventory)}
                                                                {:target [6 57 0]} {})))
              res @out]
          (is (= 2 (count (:torches @tun))) "two torches went in")
          (is (= :done (:status res)))
          (is (= :sealed (:reason res)))
          (is (= (set (map :cell (:torches @tun))) (set (:taken res))))
          (is (every? #(= "air" (block-at p (:cell %))) (:torches @tun)))
          (is (= 8 (carried p "torch")) "the torches are back")
          (is (= entry (feet p)))
          (is (= mouth (set (:filled res))))
          (is (every? #(= "cobblestone" (block-at p %)) mouth))
          (is (= 7 (carried p "cobblestone")))
          (is (empty? (:left res)))
          (is (empty? (:open res)))
          (is (= [] (the-ledger eng)))
          (is (= 1 (count (events-of s :leave-tunnel.done)))))))))

(deftest nothing-to-fill-with-leaves-the-mouth-open-but-the-torches-taken
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as s} (await (run-out! (setup {:blocks eight-down :inventory (inventory :cobble 0)}
                                                        {:target [6 57 0]} {})))
              res @out]
          (is (= :open (:reason res)))
          (is (= :done (:status res)))
          (is (= mouth (set (map :cell (:open res)))))
          (is (every? #(= :no-blocks (:reason %)) (:open res)))
          (is (empty? (:filled res)))
          (is (= 8 (carried p "torch")))
          (is (= entry (feet p)))
          (is (= 1 (count (events-of s :leave-tunnel.open)))))))))

(deftest a-zone-that-allows-only-digging-leaves-its-mouth-cell-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zone {:name "keep" :min [-1 64 0] :max [-1 64 0] :allow #{:dig}}
              {:keys [p out]} (await (run-out! (setup {:blocks eight-down :inventory (inventory) :zones [zone]}
                                                  {:target [6 57 0]} {})))
              res @out]
          (is (= :open (:reason res)))
          (is (= [{:cell [-1 64 0] :reason :zone}] (:open res)))
          (is (= #{[-2 64 0] [0 64 0]} (set (:filled res))))
          (is (not-any? #(= {:x -1 :y 64 :z 0} %) (calls-of p "place")) "never placed in the zone")
          (is (= "air" (block-at p [-1 64 0]))))))))

(deftest the-spare-items-are-filled-with-last
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [inv [{:name "iron_pickaxe" :count 1} {:name "torch" :count 8} {:name "cobblestone" :count 10} {:name "dirt" :count 5}]
              {:keys [p out]} (await (run-out! (setup {:blocks eight-down :inventory inv} {:target [6 57 0]}
                                                  {:spare ["cobblestone"]})))]
          (is (= :sealed (:reason @out)))
          (is (= 2 (carried p "dirt")) "the other building block first")
          (is (= 10 (carried p "cobblestone")) "the spare untouched while another is carried"))))))

(deftest a-restart-on-the-way-out-finishes-without-digging-or-placing-twice
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              spec {:blocks eight-down :inventory (inventory)}
              s (setup spec {:target [6 57 0]} {} :dir dir)]
          (await (ticks-while! s #(not (and (map? @(:tun s)) (= 7 (carried (:p s) "torch"))))))
          (is (= 7 (carried (:p s) "torch")) "the first torch is back")
          (let [again (await (run-out! (setup spec {:target [6 57 0]} {} :dir dir :p (:p s))))
                p (:p s)
                digs (calls-of p "dig")
                places (calls-of p "place")
                torch-cells (set (map :cell (:torches @(:tun s))))
                dug-cells (set (map :cell (:dug @(:tun s))))]
            (is (= :sealed (:reason @(:out again))))
            (is (= 8 (carried p "torch")))
            (is (= entry (feet p)))
            (is (every? (fn [[cell n]] (= n (+ (if (torch-cells cell) 1 0) (if (dug-cells cell) 1 0))))
                        (frequencies (map (juxt :x :y :z) digs)))
                "each cell dug once as stone and once as a torch, as often as it was either")
            (is (= (count places) (count (distinct places))) "no cell placed twice")
            (is (= [] (the-ledger (:eng again))))))))))

(defn block-stair!
  "Stone in the middle of the stair, as a cave-in leaves it."
  [p]
  (doseq [k [[4 58 0] [4 59 0]]] (swap! (fake/state p) assoc-in [:blocks k] "stone")))

(defn drop-pickaxe!
  "The stair is blocked and the pickaxe is gone: nothing can be dug out."
  [p]
  (block-stair! p)
  (swap! (fake/state p) update :inventory #(vec (remove (fn [i] (= "iron_pickaxe" (:name i))) %))))

(deftest a-broken-stair-is-no-trap-the-body-digs-its-own-way-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as s} (await (run-out! (setup {:blocks eight-down :inventory (inventory)}
                                                              {:target [6 57 0]} {} :between block-stair!)))
              res @out]
          (is (= :done (:status res)))
          (is (= 1 (count (events-of s :leave-tunnel.escape))))
          (is (= :sealed (:reason res)))
          (is (>= (second (feet p)) 65) "the body stands at the entry's height or above")
          (is (= 0 (count (events-of s :leave-tunnel.stopped))))
          (is (every? #(#{"torch" "wall_torch"} (block-at p (:cell %))) (the-ledger (:eng s)))
              "no ledger entry for a torch the escape stair dug through"))))))

(deftest a-body-without-a-pickaxe-cannot-dig-out-and-waits-for-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (run-out! (setup {:blocks eight-down :inventory (inventory)}
                                                        {:target [6 57 0]} {} :between drop-pickaxe!)))]
          (is (= :not-done @out) "no result: the escape's stair declined")
          (is (= :no-tool (:reason (waiting s))) "the parent waits with the stair's reason")
          (is (= "pickaxe" (:tool (waiting s))))
          (is (= 0 (count (events-of s :leave-tunnel.escape))))
          (is (= 0 (count (events-of s :leave-tunnel.stopped)))))))))

(deftest a-waiting-escape-runs-no-rounds-and-resumes-when-a-pickaxe-is-given
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock p out] :as s} (setup {:blocks eight-down :inventory (inventory)}
                                                     {:target [6 57 0]} {} :between drop-pickaxe!)
              rounds #(count (filter (fn [e] (= :round_started (:kind e))) @(:seen s)))
              tick-n! (fn ^:async tick-n [n] (dotimes [_ n] (swap! clock + 500) (await (core/tick! eng))))]
          (await (ticks-while! s #(not (waiting s))))
          (await (tick-n! 2))
          (let [n (rounds)]
            (await (tick-n! 15))
            (is (= n (rounds)) "parked: no rounds while the escape's stair would decline")
            (is (= 1 (count (filter #(= :waiting (:kind %)) @(:seen s)))) "told once")
            (fake/add-item! p "iron_pickaxe" 1)
            (await (run-out! s))
            (is (= :done (:status @out)) "resumes once the pickaxe is carried")))))))

(deftest a-tunnel-result-without-a-line-is-bad-args
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              [seen sink] (tu/legacy-capture-sink)
              p (tu/fake {:self {:pos {:x 0 :y 65 :z 0}} :inventory (inventory)})
              out (atom :not-done)
              parent {:check (constantly true)
                      :round (fn ^:async recording-round [c]
                               (let [r (await (ctx/call-child c :kid job {:tunnel {}}))]
                                 (when (= :done r) (reset! out (ctx/child-result c :kid)))
                                 r))}
              eng (core/create {:primitives p :jobs (assoc registry/jobs 'recording-parent parent)
                                :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock) :world (world/of-data {} {} [])
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (core/submit! eng '(recording-parent) {})
          (dotimes [_ 5] (swap! clock + 500) (await (core/tick! eng)))
          (is (= [:stopped :bad-args] ((juxt :status :reason) @out)))
          (is (= 1 (count (filter #(= :leave-tunnel.stopped (:kind %)) @seen)))))))))
