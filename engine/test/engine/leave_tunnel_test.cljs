(ns engine.leave-tunnel-test
  "jobs.access.leave-tunnel: the mouth as a pure function, then whole ways out after a real tunnel in the fake world."
  (:require [engine.child-ends :as ce]
            [clojure.string :as str]
            [cljs.test :refer [deftest is are async]]
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

(deftest the-way-out-is-complete-when-no-torch-stands-and-the-mouth-is-shut
  (let [line {:entry [0 65 0] :heading :north :dir :down :steps 2 :run 0 :stand [0 63 -2] :target [0 63 -3]}
        tunnel {:line line :dug (dug-of [[0 64 -1] [0 63 -2]]) :torches [{:cell [0 64 -1] :site 0}]}
        world (fn [named] (fn [[_ y _ :as cell]] (get named cell (if (> y 64) "air" "stone"))))
        shut (world {[0 64 -1] "cobblestone" [0 63 -2] "air"})]
    (is (= :sealed (leave-tunnel/reached shut tunnel {})) "torch cell filled, mouth shut")
    (is (nil? (leave-tunnel/reached (world {[0 64 -1] "wall_torch"}) tunnel {})) "a torch stands")
    (is (nil? (leave-tunnel/reached (world {[0 64 -1] "air"}) tunnel {})) "the mouth is open")
    (is (nil? (leave-tunnel/reached (world {[0 64 -1] "air"}) tunnel {:open [{:cell [0 64 -1] :reason :no-blocks}]}))
        "an open cell booked with its reason is not sealed")
    (is (nil? (leave-tunnel/reached (fn [cell] (when (not= cell [0 65 0]) "stone")) tunnel {})) "the entry is not seen")
    (is (nil? (leave-tunnel/reached (fn [cell] (when (not= cell [0 64 -1]) "stone")) tunnel {})) "a torch cell not seen is not known")
    (is (= :sealed (leave-tunnel/reached (fn [cell] (when (not= cell [0 64 -1]) "stone")) tunnel {:taken [[0 64 -1]]})) "but one taken is")))

;; ---------------------------------------------------------------- the escape, pure

(deftest escape-attempts-go-back-along-the-heading-then-the-others-and-override-zones-only-when-asked
  (are [heading ignore? expected] (= expected (mapv (juxt :heading :ignore-zones?) (leave-tunnel/escape-attempts heading ignore?)))
    :east false [[:west false] [:north false] [:east false] [:south false]]
    :north true [[:south true] [:north true] [:east true] [:west true]]))

(deftest the-escape-stair-runs-opposite-to-the-tunnels-own-stair
  (are [line-dir expected] (= expected (leave-tunnel/escape-stair {:dir line-dir} 65))
    :down {:dir :up :y 65}
    :up {:dir :down :y 65}))

;; ---------------------------------------------------------------- the fake world

(defn stone
  "Stone over x0..x1, y0..y1, z0..z1, as fake blocks."
  [x0 x1 y0 y1 z0 z1]
  (into {} (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))] [(str x "," y "," z) "stone"])))

(def eight-down (assoc (stone -12 12 50 64 -3 3) "6,57,0" "iron_ore"))
(def entry [-3 65 0])
(def mouth #{[-2 64 0] [-1 64 0] [0 64 0]})
(def sixteen-down
  "A stair long enough for two torches."
  (assoc (stone -14 12 40 64 -3 3) "6,49,0" "iron_ore"))
(def entry16 [-11 65 0])
(def mouth16 #{[-10 64 0] [-9 64 0] [-8 64 0]})
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
  [spec targs largs & {:keys [dir p between jobs]}]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p' (or p (tu/fake (merge {:self {:pos {:x 0 :y 65 :z 0}} :drops drops} (dissoc spec :zones :zones-after))))
        tun (atom :not-done)
        out (atom :not-done)
        w (world/of-data {} {} (get spec :zones []))
        parent {:round (fn ^:async recording-round [c]
                         (if-let [t (:tunnel (ctx/mem c))]
                           (let [r (await (ctx/call c :kid job (assoc largs :tunnel t)))]
                             (if (= :continue r) r (do (reset! out (tu/outcome r)) (ctx/end c :done nil))))
                           (let [r (await (ctx/call c :in tunnel-job targs))]
                             (when-not (= :continue r)
                               (let [res (tu/outcome r)]
                                 (reset! tun res)
                                 (when between (between p'))
                                 (when-let [z (:zones-after spec)] (world/set-zones! w z))
                                 (ctx/update-mem! c assoc :tunnel (select-keys res [:line :dug :torches]))))
                             :continue)))}
        eng (core/create {:primitives p' :jobs (merge registry/jobs {'recording-parent parent} jobs)
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
        (let [{:keys [eng p tun out] :as s} (await (run-out! (setup {:blocks sixteen-down :inventory (inventory)}
                                                                {:target [6 49 0]} {})))
              res @out]
          (is (= 2 (count (:torches @tun))) "two torches went in")
          (is (= :done (:status res)))
          (is (= :sealed (:reason res)))
          (is (= (set (map :cell (:torches @tun))) (set (:taken res))))
          (is (every? #(= "air" (block-at p (:cell %))) (:torches @tun)))
          (is (= 8 (carried p "torch")) "the torches are back")
          (is (= entry16 (feet p)))
          (is (= mouth16 (set (:filled res))))
          (is (every? #(= "cobblestone" (block-at p %)) mouth16))
          (is (= 7 (carried p "cobblestone")))
          (is (empty? (:left res)))
          (is (empty? (:open res)))
          (is (= [] (the-ledger eng))))))))

(deftest each-torch-is-dug-through-the-dig-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (await (run-out! (setup {:blocks eight-down :inventory (inventory)} {:target [6 57 0]} {})))]
          (is (pos? (count (calls-of p "dig"))) "at least one torch is dug")
          (is (= (count (calls-of p "dig")) (count (ce/ends s :dug)))))))))

(deftest each-mouth-cell-is-filled-through-the-place-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run-out! (setup {:blocks eight-down :inventory (inventory)} {:target [6 57 0]} {})))]
          (is (= (count mouth) (count (ce/ends s :placed)))))))))

(deftest nothing-to-fill-with-leaves-the-mouth-open-but-the-torches-taken
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as s} (await (run-out! (setup {:blocks eight-down :inventory (inventory :cobble 0)}
                                                        {:target [6 57 0]} {})))
              res @out]
          (is (= :open (:reason res)))
          (is (not= :done (:status res)) "an open mouth is not done")
          (is (= mouth (set (map :cell (:open res)))))
          (is (every? #(= :no-blocks (:reason %)) (:open res)))
          (is (empty? (:filled res)))
          (is (= 8 (carried p "torch")))
          (is (= entry (feet p)))
          (is (str/starts-with? (:text res) "leave-tunnel open:")))))))

(defn obtain-stub
  "jobs.items.obtain giving 10 cobblestone to the body in the atom; calls are recorded."
  [calls body]
  (assoc (select-keys (get registry/jobs 'jobs.items.obtain) [:ways :args])
         :round (fn ^:async obtain [c]
                  (swap! calls conj (:args c))
                  (fake/add-item! @body "cobblestone" 10)
                  (ctx/end c :done nil {:item "cobblestone" :count 10}))))

(deftest no-blocks-to-seal-with-are-fetched-by-default-and-the-mouth-is-shut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom [])
              body (atom nil)
              s (setup {:blocks eight-down :inventory (inventory :cobble 0)} {:target [6 57 0]} {}
                       :jobs {'jobs.items.obtain (obtain-stub calls body)})
              _ (reset! body (:p s))
              {:keys [out]} (await (run-out! s))]
          (is (= 1 (count @calls)) "one fetch for all the open cells")
          (is (= [:done :sealed] ((juxt :status :reason) @out)))
          (is (= mouth (set (:filled @out)))))))))

(deftest no-blocks-to-seal-with-and-fetch-off-leave-the-mouth-open-without-a-fetch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom [])
              body (atom nil)
              s (setup {:blocks eight-down :inventory (inventory :cobble 0)} {:target [6 57 0]} {:fetch false}
                       :jobs {'jobs.items.obtain (obtain-stub calls body)})
              _ (reset! body (:p s))
              {:keys [out]} (await (run-out! s))]
          (is (empty? @calls))
          (is (= :open (:reason @out))))))))

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

;; The tunnel's round runs first (the parent yields once), then leave-tunnel's; the nth dig of leave-tunnel never
;; happens, as the body stops mid-way at a restart.
(defn ^:async stall-dig! [{:keys [eng clock p]} n]
  (swap! clock + 500)
  (await (core/tick! eng))
  (let [k (atom 0)
        world (.-world p)]
    (.override world "dig" (fn [token args impl]
                             (if (= n (swap! k inc))
                               (js/Promise. (fn [_ _]))
                               (impl token args))))
    (swap! clock + 500)
    (await (js/Promise.race #js [(core/tick! eng)
                                 (js/Promise. (fn [ok] (let [poll (fn poll [] (if (>= @k n) (ok) (js/setTimeout poll 10)))] (poll))))]))
    (.override world "dig" nil)))

(deftest a-restart-on-the-way-out-finishes-without-digging-or-placing-twice
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              spec {:blocks sixteen-down :inventory (inventory)}
              s (setup spec {:target [6 49 0]} {} :dir dir)]
          (await (stall-dig! s 2))
          (is (= 7 (carried (:p s) "torch")) "the first torch is back")
          (let [again (await (run-out! (setup spec {:target [6 49 0]} {} :dir dir :p (:p s))))
                p (:p s)
                digs (calls-of p "dig")
                places (calls-of p "place")
                torch-cells (set (map :cell (:torches @(:tun s))))
                dug-cells (set (map :cell (:dug @(:tun s))))]
            (is (= :sealed (:reason @(:out again))))
            (is (= 8 (carried p "torch")))
            (is (= entry16 (feet p)))
            (is (= 1 (reduce + (map (fn [[cell n]] (- n (+ (if (torch-cells cell) 1 0) (if (dug-cells cell) 1 0))))
                                    (frequencies (map (juxt :x :y :z) digs)))))
                "each cell dug once as stone and once as a torch; only the cut dig is asked twice")
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
          (is (= [] (the-ledger (:eng s))) "every torch was taken or destroyed: no entry is left"))))))

(defn creeper!
  "A blast: the stair is blocked and the torches in the middle of the tunnel are gone."
  [p]
  (block-stair! p)
  (let [blocks (:blocks @(fake/state p))
        torches (take 2 (filter #(#{"torch" "wall_torch"} (blocks %)) (keys blocks)))]
    (doseq [t torches]
      (swap! (fake/state p) assoc-in [:blocks t] "air"))))

(deftest a-creeper-blast-leaves-no-ledger-entry-for-a-torch-it-destroyed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (run-out! (setup {:blocks eight-down :inventory (inventory)}
                                                              {:target [6 57 0]} {} :between creeper!)))]
          (is (= :done (:status @out)))
          (is (= [] (the-ledger (:eng s))) "every torch was taken or destroyed: no entry is left"))))))

(deftest an-escape-never-overrides-zones-unless-the-caller-asks-and-stops-when-all-are-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zone {:name "keep" :min [-12 50 -3] :max [12 64 3] :allow #{:walk}}
              {:keys [out] :as s} (await (run-out! (setup {:blocks eight-down :inventory (inventory) :zones [zone]}
                                                          {:target [6 57 0] :ignore-zones? true} {} :between block-stair!)))
              escapes (events-of s :leave-tunnel.escape)]
          (is (= [:stopped :walk-failed] ((juxt :status :reason) @out)) "every heading refused: the escapes are used up")
          (is (= :nothing-possible (get-in @out [:ending :kind])))
          (is (= [:refused :refused :refused :refused] (map :reason (:escape @out))))
          (is (not-any? :ignore-zones? escapes) "no attempt ran with :ignore-zones?"))))))

(deftest a-zone-refusing-only-the-first-escape-heading-leaves-by-another
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zone {:name "keep" :min [-12 50 -3] :max [4 64 3] :allow #{:walk}}
              {:keys [out] :as s} (await (run-out! (setup {:blocks eight-down :inventory (inventory) :zones-after [zone]}
                                                          {:target [6 57 0]} {} :between block-stair!)))
              escapes (events-of s :leave-tunnel.escape)]
          (is (= :open (:reason @out)) "not parked on the refused heading: out, with the zoned mouth open")
          (is (= 1 (count escapes)))
          (is (not= :west (:heading (first escapes)))))))))

(deftest an-escape-that-respects-zones-is-info
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run-out! (setup {:blocks eight-down :inventory (inventory)}
                                        {:target [6 57 0]} {} :between block-stair!)))
              escapes (events-of s :leave-tunnel.escape)]
          (is (= [[:info false]] (map (juxt :level :ignore-zones?) escapes))))))))

(deftest a-nil-read-does-not-forget-a-standing-torch
  (let [tunnel {:torches [{:cell [1 64 0]} {:cell [2 64 0]} {:cell [3 64 0]}]}
        l [{:cell [1 64 0]} {:cell [2 64 0]} {:cell [3 64 0]}]
        reads {[1 64 0] nil [2 64 0] "air" [3 64 0] "wall_torch"}]
    (is (= [[2 64 0]] (leave-tunnel/gone-cells l tunnel reads)))))

(deftest a-body-without-a-pickaxe-cannot-dig-out-and-ends-saying-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (run-out! (setup {:blocks eight-down :inventory (inventory)}
                                                        {:target [6 57 0]} {:fetch false} :between drop-pickaxe!)))]
          (is (= [:stopped :no-tool] ((juxt :status :reason) @out)) "the escape's stair declined: the job ends with its reason")
          (is (= :condition-failed (get-in @out [:ending :kind])))
          (is (= "pickaxe" (:tool (:cause (:ending @out)))))
          (is (nil? (waiting s)))
          (is (= 0 (count (events-of s :leave-tunnel.escape)))))))))

(deftest a-tunnel-result-without-a-line-is-bad-args
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              [seen sink] (tu/legacy-capture-sink)
              p (tu/fake {:self {:pos {:x 0 :y 65 :z 0}} :inventory (inventory)})
              out (atom :not-done)
              parent (tu/recording-parent job {:tunnel {}} out)
              eng (core/create {:primitives p :jobs (assoc registry/jobs 'recording-parent parent)
                                :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock) :world (world/of-data {} {} [])
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (core/submit! eng '(recording-parent) {})
          (await (tu/run-ticks eng clock 5 500))
          (is (= [:stopped :bad-args] ((juxt :status :reason) @out)))
          (is (= :impossible (get-in @out [:ending :kind]))))))))

(deftest a-missing-pickaxe-is-fetched-for-the-escape-with-fetch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom [])
              body (atom nil)
              stub {:ways [:chest :craft :gather]
                    :round (fn ^:async get-tool [c]
                             (swap! calls conj (:args c))
                             (fake/add-item! @body "iron_pickaxe" 1)
                             (ctx/end c :done nil {:tool "iron_pickaxe"}))
                    :args {:block {:default nil} :item {:default nil} :kind {:default nil} :how {:default nil} :depth {:default nil}
                           :minutes {:default nil} :fail-minutes {:default nil} :chain {:default []} :explore {:default true}}}
              s (setup {:blocks eight-down :inventory (inventory)} {:target [6 57 0]} {:fetch true}
                       :between drop-pickaxe! :jobs {'jobs.items.get-tool stub})
              _ (reset! body (:p s))
              {:keys [out]} (await (run-out! s))]
          (is (= 1 (count @calls)))
          (is (= :done (:status @out))))))))

(deftest a-pickaxe-that-cannot-be-fetched-has-the-escape-dug-by-hand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom [])
              stub {:ways [:chest :craft :gather]
                    :round (fn ^:async get-tool [c]
                             (swap! calls conj (:args c))
                             (ctx/end c :condition-failed :no-source))
                    :args {:block {:default nil} :item {:default nil} :kind {:default nil} :how {:default nil} :depth {:default nil}
                           :minutes {:default nil} :fail-minutes {:default nil} :chain {:default []} :explore {:default true}}}
              s (setup {:blocks eight-down :inventory (inventory)} {:target [6 57 0]} {:fetch true}
                       :between drop-pickaxe! :jobs {'jobs.items.get-tool stub})
              {:keys [out]} (await (run-out! s))]
          (is (= 1 (count @calls)) "asked once, not again")
          (is (= :done (:status @out))))))))

(deftest a-default-call-fetches-a-missing-pickaxe-for-the-escape
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom [])
              body (atom nil)
              stub {:ways [:chest :craft :gather]
                    :round (fn ^:async get-tool [c]
                             (swap! calls conj (:args c))
                             (fake/add-item! @body "iron_pickaxe" 1)
                             (ctx/end c :done nil {:tool "iron_pickaxe"}))
                    :args {:block {:default nil} :item {:default nil} :kind {:default nil} :how {:default nil} :depth {:default nil}
                           :minutes {:default nil} :fail-minutes {:default nil} :chain {:default []} :explore {:default true}}}
              s (setup {:blocks eight-down :inventory (inventory)} {:target [6 57 0]} {}
                       :between drop-pickaxe! :jobs {'jobs.items.get-tool stub})
              _ (reset! body (:p s))
              {:keys [out]} (await (run-out! s))]
          (is (= 1 (count @calls)))
          (is (= :done (:status @out))))))))

(deftest fetch-false-opts-out-and-never-calls-get-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom [])
              body (atom nil)
              stub {:ways [:chest :craft :gather]
                    :round (fn ^:async get-tool [c]
                             (swap! calls conj (:args c))
                             (fake/add-item! @body "iron_pickaxe" 1)
                             (ctx/end c :done nil {:tool "iron_pickaxe"}))
                    :args {:block {:default nil} :item {:default nil} :kind {:default nil} :how {:default nil} :depth {:default nil}
                           :minutes {:default nil} :fail-minutes {:default nil} :chain {:default []} :explore {:default true}}}
              s (setup {:blocks eight-down :inventory (inventory)} {:target [6 57 0]} {:fetch false}
                       :between drop-pickaxe! :jobs {'jobs.items.get-tool stub})
              _ (reset! body (:p s))
              {:keys [out]} (await (run-out! s))]
          (is (empty? @calls))
          (is (= [:stopped :no-tool] ((juxt :status :reason) @out))))))))

(deftest one-call-takes-the-torches-back-and-seals
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock out]} (setup {:blocks eight-down :inventory (inventory)} {:target [6 57 0]} {})]
          (swap! clock + 500)
          (await (core/tick! eng))
          (is (= :not-done @out) "the first round is the tunnel's, the second leave-tunnel's")
          (swap! clock + 500)
          (await (core/tick! eng))
          (is (= :sealed (:reason @out)) "one round takes every torch and seals the mouth"))))))
