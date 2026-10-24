(ns engine.explore-search-test
  "jobs.explore.search against the fake world, with a notes store over a temp world folder."
  (:require [cljs.test :refer [deftest is are async]]
            ["fs" :as fs]
            ["path" :as path]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.entity-observations :as obs]
            [engine.events :as events]
            [engine.expr :as expr]
            [engine.shelter-support :as st]
            [engine.memory :as mem]
            [engine.notes :as notes]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.threats :as threats]
            [jobs.lib.world-files :as world]
            [jobs.lib.child :as child]
            [plan.shape :as shape]
            [jobs.explore.search :as search]
            [jobs.lib.shelter.sense :as sh-sense]))

(def ground
  "Grass at y 63 under every column within r of the origin (r a little past the search's :max-distance): every leg the
  patterns choose from the origin stands there, and the planner has a floor to walk over between them."
  (memoize (fn [r] (tu/box (- r) 63 (- r) r 63 r "grass_block"))))

(defn setup
  "An engine over the fake world spec (ground of radius ground-r added, default 64) with a notes store for body Fake in a temp world folder; a :wait act moves the clock by its ms."
  [spec & [ground-r]]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        dir (tu/tmp-dir)
        p (tu/fake (update (dissoc spec :cone? :zones :plans :seen-r) :blocks #(merge (ground (or ground-r 36)) %)))
        _ (.override (.-world p) "wait" (fn [token a impl] (swap! clock + (.-ms a)) (impl token a)))
        p (if (:cone? spec)
            (let [w (perception/wrap p (perception/create (fake-raw/create p) {:radius (if (number? (:cone? spec)) (:cone? spec) 16) :ray-deg 2}))]
              (.setOwner p "Fake")
              w)
            (let [p (tu/seeing-all p)]
              ;; the body has seen only what lies near it, unless the case says it saw farther earlier
              (aset p "seenBlocks" (fn [q] (.blocks p (doto (js/Object.assign #js {} q) (aset "radius" (min (.-radius q) (:seen-r spec 24)))))))
              p))
        store (notes/open {:world-dir dir :body "Fake" :emit (fn [_])})
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :world (assoc (world/of-data (:plans spec {}) {} (:zones spec [])) :notes store)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock :dir dir :store store}))

(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))

(defn ^:async run-until-done
  "Tick 700 ms apart until the list is empty or n ticks have run."
  [{:keys [eng clock] :as s} n]
  (loop [i 0]
    (when (and (< i n) (not (finished? s)))
      (swap! clock + 700)
      (await (core/tick! eng))
      (recur (inc i))))
  s)

(defn ^:async search!
  [args spec & [n before]]
  (let [s (setup spec (some-> (:max-distance args) (+ 8)))]
    (when before (before s))
    (core/submit! (:eng s) (list 'jobs.explore.search (child/search-args args)) {})
    (await (run-until-done s (or n 400)))))

(defn event-of [{:keys [seen]} kind] (first (filter #(= kind (:kind %)) @seen)))
(defn ended-of
  "The job's ending event."
  [s]
  (some #(when (#{:completed :stopped} (:kind %)) %) @(:seen s)))
(defn found-of
  "The ending's data when it found the targets."
  [s]
  (let [d (ended-of s)] (when (and (= :completed (:kind d)) (= :found (:reason d))) d)))
(defn not-found-of
  "The ending's data when it did not find them."
  [s]
  (let [d (ended-of s)] (when (and d (not (found-of s))) d)))
(defn walks
  "The targets of the walks go-to made, in order (each walk writes a :moved entry)."
  [{:keys [eng]}]
  (mapv (comp :target :data) (mem/entries (mem/view (:store eng)) :moved)))
(defn file-notes [{:keys [dir]}]
  (:value (notes/parse-file (fs/readFileSync (path/join dir "notes" "Fake.edn") "utf8"))))
(defn write-other! [{:keys [dir]} body ns]
  (fs/mkdirSync (path/join dir "notes") #js {:recursive true})
  (fs/writeFileSync (path/join dir "notes" (str body ".edn")) (notes/render body ns)))

(def diamond {"28,64,3" "diamond_block"})

;; ------------------------------------------------------------------ pure

(deftest the-spiral-rings-the-origin-at-spacing-within-the-distance
  (is (= [[-16 -16] [0 -16] [16 -16] [16 0] [16 16] [0 16] [-16 16] [-16 0]]
         (search/spiral-points [0 0] 16 23)))
  (is (= [[0 -16] [16 0] [0 16] [-16 0]] (search/spiral-points [0 0] 16 20)))
  (is (= 24 (count (search/spiral-points [100 100] 16 46)))))

(deftest outward-goes-ahead-first-then-to-the-sides
  (is (= [[0 -16] [8 -12] [-8 -12] [12 -4] [-12 -4] [16 0] [-16 0]]
         (search/outward-points [0.5 0.5] :north 16)))
  (is (= [16 0] (first (search/outward-points [0 0] :east 16))))
  (is (= [11 11] (first (search/outward-points [0 0] :south-east 16)))))

(defn column [m] (fn [[x y z]] (if (contains? m :unloaded) nil (get m [x y z] "air"))))

(deftest a-leg-stands-on-the-ground-not-the-canopy-and-never-in-the-unknown
  (are [m expected] (= expected (search/stand-cell (column m) 5 5 64))
    {[5 63 5] "grass_block"} {:y 64}
    {[5 70 5] "stone" [5 69 5] "stone"} {:y 71}
    {[5 66 5] "oak_leaves" [5 63 5] "dirt"} {:y 64}
    {[5 63 5] "dirt" [5 64 5] "wheat"} {:y 64}
    {[5 63 5] "water"} {:fail :wet}
    {[5 63 5] "dirt" [5 64 5] "oak_log" [5 65 5] "oak_log" [5 66 5] "oak_leaves"} {:fail :no-surface}
    {[5 43 5] "dirt"} {:y 44}
    {[5 66 5] "jungle_leaves" [5 30 5] "dirt"} {:y 31}
    {[5 66 5] "jungle_leaves" [5 20 5] "dirt"} {:fail :no-surface}
    {} {:fail :no-surface}
    {:unloaded true} {:fail :not-loaded}))

(deftest a-column-out-of-view-is-unseen-and-the-search-steps-toward-it
  (let [seen (fn [[x y z]] (cond (> x 8) :unseen (= y 63) "grass_block" :else "air"))]
    (is (= {:fail :unseen} (search/stand-cell seen 20 0 64)))
    (is (= [6 64 0] (search/step-toward seen [0 64 0] 20 0)) "12 is out of view, so the 6 block step")
    (is (= [6 64 0] (search/step-toward (fn [_] :unseen) [0 64 0] 20 0)) "all unseen: 6 blocks at feet height")
    (is (nil? (search/step-toward (fn [_] :unseen) [0 64 0] 3 0)))))

(deftest ore-beyond-the-view-is-walked-toward-and-found-after-looking
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :max-distance 20} {:blocks {"13,64,0" "diamond_block"} :cone? 8}))
              e (found-of s)]
          (is (= nil (not-found-of s)))
          (is (= [{:what "diamond_block" :pos [13 64 0]}] (:found e)))
          (is (seq (walks s))))))))

;; ------------------------------------------------------------------ the job

(defn hear-cow-until-north!
  "The cows are out of sight (entities() drops passives) and heard as heardMobs rows until the body stands at z <= -2."
  [^js p]
  (let [orig (.-entities p)
        sees? #(<= (.-z (.-pos (.self p))) -2)]
    (set! (.-entities p) (fn [opts] (let [all (.call orig p opts)] (if (sees?) all (.filter all #(not= "passive" (.-kind %)))))))
    (set! (.-heardMobs p)
          (fn [_]
            (let [me (.-pos (.self p)) from {:x (.-x me) :y (.-y me) :z (.-z me)}]
              (into-array (when-not (sees?)
                            (for [^js e (array-seq (.call orig p #js {:radius 24 :kind "passive"}))
                                  :let [pos (.-pos e)]]
                              (clj->js (assoc (obs/rough-hearing from {:x (.-x pos) :y (.-y pos) :z (.-z pos)})
                                              :id (.-id e) :name (.-name e) :kind "passive" :distance (.-distance e)))))))))))

(deftest an-animal-heard-but-not-seen-is-walked-toward-before-any-leg
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "cow"}
                                {:entities [{:id 3 :uuid "u3" :name "cow" :kind "passive" :pos {:x 0.5 :y 64 :z -8.5}}]}
                                400 #(hear-cow-until-north! (:p %))))]
          (is (= "cow" (:what (first (:found (found-of s))))) "the cow was found by stepping toward the sound")
          (is (zero? (:legs (:coverage (found-of s)))) "no leg was walked"))))))

(deftest a-target-in-sight-is-found-without-walking
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block"} {:blocks {"10,64,0" "diamond_block"}}))]
          (is (finished? s))
          (is (= :found (:reason (found-of s))))
          (is (= [{:what "diamond_block" :pos [10 64 0]}] (:found (found-of s))))
          (is (= [] (walks s))))))))

(def two-diamonds {"10,64,0" "diamond_block" "0,64,12" "diamond_block"})

(deftest a-target-in-an-avoided-column-is-not-found
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :avoid [[10 0]]} {:blocks two-diamonds}))]
          (is (= [[0 64 12]] (map :pos (:found (found-of s))))))))))

(deftest a-target-in-a-foreign-zone-is-found-only-without-takeable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [vault {:name "vault" :min [8 60 -2] :max [12 70 2] :owner "Miles"}
              spec {:blocks two-diamonds :zones [vault]}
              any (await (search! {:target "diamond_block"} spec))
              own (await (search! {:target "diamond_block" :takeable true} spec))]
          (is (= [[10 64 0]] (map :pos (:found (found-of any)))))
          (is (= [[0 64 12]] (map :pos (:found (found-of own))))))))))

(deftest a-target-out-of-sight-is-walked-to-found-and-noted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block"} {:blocks diamond}))
              ns (file-notes s)]
          (is (finished? s))
          (is (= :found (:reason (found-of s))))
          (is (= [[28 64 3]] (map :pos (:found (found-of s)))))
          (is (pos? (count (walks s))))
          (is (some #(= [:seen "diamond_block" [28 64 3] "Fake"] ((juxt :kind :what :pos :by) %)) ns))
          (is (some #(= [:searched ["diamond_block"]] ((juxt :kind :what) %)) ns))
          (is (nil? (not-found-of s))))))))

(deftest a-seen-note-lasts-the-seen-ttl-setting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (settings/with-settings {:jobs.explore.search/seen-ttl-s 100}
                 (fn ^:async short-ttl []
                   (let [s (await (search! {:target "diamond_block"} {:blocks diamond}))
                         n (first (filter #(= :seen (:kind %)) (file-notes s)))]
                     (is (= 100000 (- (:until n) (:t n))))))))))))

(deftest a-searched-note-lasts-a-day-and-keeps-legs-off-farm-animal-ground-only-the-short-animal-ttl
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [notes-of (fn ^:async notes-of* [target] (let [s (await (search! {:target target :settings {:jobs.explore.search/max-legs 0}} {:blocks (ground 36)}))]
                                              (first (filter #(= :searched (:kind %)) (file-notes s)))))
              animal (await (notes-of "cow"))
              block (await (notes-of "diamond_block"))
              p (tu/seeing-all (tu/fake {:blocks (ground 36)}))
              leg (fn [age-s] (:leg (settings/with-settings {:jobs.lib.cost.find/leg-spacing 4 :jobs.explore.search/max-distance 8}
                                      #(search/choose-leg {:args {:pattern :spiral} :primitives p :view (constantly {:now 1000000})} {:origin [0 64 0]} ["cow"]
                                                          [{:kind :searched :what ["cow"] :pos [0 64 0] :r 200 :t (- 1000000 (* 1000 age-s))}]))))]
          (is (= 86400000 (- (:until animal) (:t animal))) "its looks still tell the chance of a find for a day")
          (is (= 86400000 (- (:until block) (:t block))))
          (is (nil? (leg 60)) "ground searched for cows a minute ago is skipped")
          (is (some? (leg 300)) "five minutes on it is walked again: animals wander"))))))

(deftest the-whole-search-is-one-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:blocks diamond})]
          (core/submit! (:eng s) (list 'jobs.explore.search {:target "diamond_block"}) {})
          (swap! (:clock s) + 700)
          (await (core/tick! (:eng s)))
          (is (finished? s) "legs walked and the ore found in one round")
          (is (= :found (:reason (found-of s)))))))))

(deftest an-entity-target-is-found-and-noted-by-its-uuid
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target ["cow" "pig"]}
                                {:entities [{:id 3 :uuid "u3" :name "cow" :kind "passive" :pos {:x 30.5 :y 64 :z 0.5}}]}))]
          (is (= [{:what "cow" :pos [30 64 0] :id "u3" :entity-id 3}] (:found (found-of s))))
          (is (some #(= [:seen "cow" "u3"] ((juxt :kind :what :id) %)) (file-notes s))))))))

(deftest skip-ids-match-the-numeric-entity-id-and-the-uuid
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [spec {:entities [{:id 3 :uuid "u3" :name "cow" :kind "passive" :pos {:x 6.5 :y 64 :z 6.5}}]}
              kept (await (search! {:target "cow" :skip-ids [4] :max-distance 20 :settings {:jobs.explore.search/max-legs 0}} spec))
              by-number (await (search! {:target "cow" :skip-ids [3] :max-distance 20 :settings {:jobs.explore.search/max-legs 0}} spec))
              by-uuid (await (search! {:target "cow" :skip-ids ["u3"] :max-distance 20 :settings {:jobs.explore.search/max-legs 0}} spec))]
          (is (= 1 (count (:found (found-of kept)))))
          (is (= :stopped (:kind (not-found-of by-number))))
          (is (= :stopped (:kind (not-found-of by-uuid)))))))))

(deftest a-cow-in-a-fenced-pen-of-another-is-found-only-without-takeable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [ground (into {} (for [x (range -4 14) z (range -4 14)] [(str x ",63," z) "stone"]))
              ring (into {} (for [x (range 3 10) z (range 3 10) :when (or (#{3 9} x) (#{3 9} z))] [(str x ",64," z) "oak_fence"]))
              spec {:blocks (merge ground ring)
                    :entities [{:id 3 :uuid "u3" :name "cow" :kind "passive" :pos {:x 6.5 :y 64 :z 6.5}}]}
              any (await (search! {:target "cow" :max-distance 20 :settings {:jobs.explore.search/max-legs 0}} spec))
              own (await (search! {:target "cow" :takeable true :max-distance 20 :settings {:jobs.explore.search/max-legs 0}} spec))]
          (is (= [[6 64 6]] (map :pos (:found (found-of any)))))
          (is (= :stopped (:kind (not-found-of own))) "the penned cow is not counted"))))))

(deftest a-sheep-in-a-foreign-zone-is-found-only-without-takeable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [ground (into {} (for [x (range -4 14) z (range -4 14)] [(str x ",63," z) "stone"]))
              pen {:name "kettricken-pen" :min [3 60 3] :max [9 70 9] :owner "Kettricken"}
              spec {:blocks ground :zones [pen]
                    :entities [{:id 3 :uuid "u3" :name "sheep" :kind "passive" :pos {:x 6.5 :y 64 :z 6.5}}]}
              any (await (search! {:target "sheep" :max-distance 20 :settings {:jobs.explore.search/max-legs 0}} spec))
              own (await (search! {:target "sheep" :takeable true :max-distance 20 :settings {:jobs.explore.search/max-legs 0}} spec))]
          (is (= [[6 64 6]] (map :pos (:found (found-of any)))))
          (is (= :stopped (:kind (not-found-of own))) "the sheep in another's zone is not counted")
          (is (= ["sheep"] (:saw (first (filter #(= :searched (:kind %)) (file-notes own)))))
              "its look still saw a sheep: the ground is not searched in vain (cost/look-chance)"))))))

(deftest a-target-seen-beyond-the-scan-radius-is-found-without-a-leg
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :scan-radius 10 :max-distance 40} {:blocks diamond :seen-r 64}))]
          (is (= [{:what "diamond_block" :pos [28 64 3]}] (:found (found-of s))))
          (is (= [] (walks s))))))))

(deftest a-target-seen-beyond-the-distance-bound-is-not-taken
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :scan-radius 10 :max-distance 20 :settings {:jobs.explore.search/max-legs 1}} {:blocks diamond :seen-r 64}))]
          (is (nil? (found-of s)))
          (is (= [] (:found (not-found-of s)))))))))

(deftest a-dense-target-out-of-range-beside-the-body-does-not-hide-one-in-range
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :scan-radius 10 :max-distance 10 :settings {:jobs.explore.search/max-legs 1}}
                                {:blocks (merge (tu/box 11 64 -4 28 64 4 "diamond_block") {"0,80,0" "diamond_block"}) :seen-r 64}))]
          (is (= [{:what "diamond_block" :pos [0 80 0]}] (:found (found-of s)))))))))

(deftest nothing-within-the-distance-ends-not-found-with-the-coverage
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :max-distance 20} {:blocks {"45,64,3" "diamond_block"}}))
              e (not-found-of s)]
          (is (finished? s))
          (is (= :stopped (:kind e)))
          (is (= :distance (:why e)))
          (is (= 4 (get-in e [:coverage :legs])))
          (is (= 5 (get-in e [:coverage :scans])))
          (is (<= 14 (get-in e [:coverage :farthest]) 16) "the body stops within 2 of the leg 16 out (go-to ends on any cell within its range, so 14 or 15)")
          (is (re-find #"5 looks of 24 blocks round" (:text e)) "the text names how far each look reached")
          (is (= [] (:found e))))))))

(deftest the-leg-and-time-bounds-end-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [legs (await (search! {:target "diamond_block" :settings {:jobs.explore.search/max-legs 2}} {}))
              timed (await (search! {:target "diamond_block" :spend {:job-time 2}} {} nil
                                    (fn [{:keys [p clock]}]
                                      (.override (.-world p) "look"
                                                 (fn ^:async f [token args impl]
                                                   (swap! clock + 1000)
                                                   (await (impl token args)))))))]
          (is (= [:legs 2] ((juxt :why #(get-in % [:coverage :legs])) (not-found-of legs))))
          (is (= :job-time (:reason (event-of timed :stopped))) "the engine's time limit ends it"))))))

(deftest a-priced-search-ends-at-the-first-walk-that-costs-more-than-is-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :worth 12 :settings {:jobs.explore.search/max-legs 1}} {}))
              e (not-found-of s)]
          (is (= :price (:why e)) "the price ends it, not the legs setting")
          (is (= 3 (get-in e [:coverage :legs])) "three 16-block walks (their quoted seconds, under 4 s each) fit in 12 s, a fourth does not")
          (is (re-find #"the next walk costs \d+ s, more than the \d+ s left" (:text e))))))))

(deftest a-priced-animal-search-over-ground-searched-in-vain-stops-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [marks (fn [what] (for [[x z] (search/spiral-points [0 0] 16 128)]
                                 {:kind :searched :what [what] :pos [x 64 z] :r 24 :by "Other" :t 1 :until 9e15}))
              sheep (not-found-of (await (search! {:target "sheep" :worth 1000 :max-distance 32} {} 400 #(write-other! % "Other" (marks "sheep")))))
              fresh (not-found-of (await (search! {:target "sheep" :worth 1000 :max-distance 32 :use-notes false} {} 400 #(write-other! % "Other" (marks "sheep")))))
              cows (await (search! {:target "sheep" :worth 1000 :max-distance 32} {} 400 #(write-other! % "Other" (marks "cow"))))]
          (is (= [:price 0] ((juxt :why #(get-in % [:coverage :legs])) sheep)) "no sheep for 128 blocks round: a look here is not worth a walk")
          (is (= [:price 0] ((juxt :why #(get-in % [:coverage :legs])) fresh)) "older looks price the chance under :use-notes false too")
          (is (re-find #"at a \d+\.\d% chance a look there finds one" (:text sheep)))
          (is (seq (walks cows)) "ground searched for cows says nothing of sheep"))))))

(deftest a-priced-ore-search-is-priced-by-the-rock-in-sight-and-on-grass-walks-no-leg
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [iron {:target ["iron_ore" "deepslate_iron_ore"] :worth 1000 :max-distance 32}
              grass (not-found-of (await (search! iron {})))
              rock (await (search! iron {:blocks (tu/box -40 63 -40 40 63 40 "stone")}))]
          (is (= [:price 0] ((juxt :why #(get-in % [:coverage :legs])) grass)) "no rock seen holds iron: no look pays for a walk")
          (is (seq (walks rock)) "stone round the body may hold iron: the legs are walked"))))))

(deftest a-cheap-find-far-off-is-not-searched-for-and-a-dear-one-is
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cheap (await (search! {:target "diamond_block" :worth 5 :max-distance 50} {:blocks diamond}))
              dear (await (search! {:target "diamond_block" :worth 600 :max-distance 50} {:blocks diamond}))]
          (is (nil? (found-of cheap)) "28 blocks off is past what 5 s pays for")
          (is (= [{:what "diamond_block" :pos [28 64 3]}] (:found (found-of dear))))))))) 

(defn ^:async search-quoted!
  "A priced search whose go-to quotes are answered by (quote-for n args) for the nth quote asked; {:state s :asked [args ...]}."
  [quote-for]
  (let [asked (atom [])
        arity "cljs$core$IFn$_invoke$arity$3"
        real (aget ctx/quote arity)]
    (aset ctx/quote arity (fn [_ _ args] (js/Promise.resolve (quote-for (count (swap! asked conj args)) args))))
    (let [state (try (await (search! {:target "diamond_block" :worth 12 :max-distance 20} {}))
                     (finally (aset ctx/quote arity real)))]
      {:state state :asked @asked})))

(deftest a-leg-whose-walk-likely-ends-in-death-is-priced-against-the-survived-share-of-the-worth
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [legs (fn ^:async l [chance]
                     (let [{:keys [state]} (await (search-quoted! (fn [_ _] {:uses {:job-time [3 3 9]} :risk {:death-chance chance}})))]
                       ((juxt :why #(get-in % [:coverage :legs])) (not-found-of state))))]
          (is (not= [:price 0] (await (legs 0))) "a 3 s walk is under the 12 s worth")
          (is (= [:price 0] (await (legs 0.9))) "a body that comes back one time in ten gets 1.2 s of it"))))))

(deftest a-leg-with-no-route-fails-alone-and-the-next-is-tried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [state asked]} (await (search-quoted! (fn [_ _] {:uses {:job-time [##Inf ##Inf ##Inf]}})))
              e (not-found-of state)]
          (is (not= :price (:why e)) "a leg no route reaches is not a price over the worth")
          (is (< 1 (count (distinct (map :pos asked)))) "the next frontier is tried"))))))

(deftest a-quote-search-that-has-not-ended-is-asked-again-next-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [state asked]} (await (search-quoted! (fn [n _] (if (= 1 n)
                                                                         {:uses {:job-time [0 1 ##Inf]} :estimate true}
                                                                         {:uses {:job-time [1 1 1]}}))))]
          (is (= (:pos (first asked)) (:pos (second asked))) "the same leg is quoted again, not walked or priced on an estimate")
          (is (not= :price (:why (not-found-of state)))))))))

(deftest the-spiral-legs-nearest-the-body-come-first
  (let [c {:args {:pattern :spiral} :primitives #js {:self (fn [] #js {:pos #js {:x 42.5 :y 0 :z 0.5}})}}
        m {:origin [0 0 0]}
        order (settings/with-settings {:jobs.explore.search/max-distance 64} #(search/candidates c m))]
    (is (= [48 0] (first order)) "straight on from the body, not the origin's ring 1 first")
    (is (= (count (search/spiral-points [0 0] 16 64)) (count order)))))

(deftest ground-another-body-searched-for-these-targets-is-skipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [marks (for [[x z] (search/spiral-points [0 0] 16 23)]
                      {:kind :searched :what ["diamond_block"] :pos [x 64 z] :r 24 :by "Other" :t 1 :until 9e15})
              s (await (search! {:target "diamond_block" :max-distance 23} {} 400 #(write-other! % "Other" marks)))
              e (not-found-of s)]
          (is (= [] (walks s)))
          (is (= 8 (get-in e [:coverage :skipped])))
          (is (= :distance (:why e))))))))

(deftest a-spot-walked-toward-twice-without-a-look-is-not-walked-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [marks (for [[x z] (search/spiral-points [0 0] 16 23)]
                      {:kind :leg-tried :what ["diamond_block"] :pos [x 64 z] :n 2 :by "Other" :t 1 :until 9e15})
              s (await (search! {:target "diamond_block" :max-distance 23} {} 400 #(write-other! % "Other" marks)))]
          (is (= [] (walks s)))
          (is (= :distance (:why (not-found-of s)))))))))

(deftest a-walk-start-is-noted-for-the-next-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :max-distance 23} {} 400))]
          (is (some #(= :leg-tried (:kind %)) (file-notes s))))))))

(deftest an-outward-search-goes-on-past-searched-ground-ahead
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [marks (for [[x z] (search/outward-points [0.5 0.5] :north 16)]
                      {:kind :searched :what ["diamond_block"] :pos [x 64 z] :r 3 :by "Other" :t 1 :until 9e15})
              s (await (search! {:target "diamond_block" :pattern :outward :heading :north :max-distance 64} {} 400 #(write-other! % "Other" marks)))
              e (not-found-of s)]
          (is (pos? (count (walks s))) "the body walked on instead of ending at once")
          (is (every? #(< (:z %) -16) (walks s)) "past the searched legs, further north")
          (is (pos? (get-in e [:coverage :legs]))))))))

(deftest ground-searched-for-other-names-is-not-skipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [marks [{:kind :searched :what ["oak_log"] :pos [0 64 -16] :r 24 :by "Other" :t 1 :until 9e15}]
              s (await (search! {:target "diamond_block" :max-distance 20} {} 400 #(write-other! % "Other" marks)))]
          (is (= 4 (count (walks s)))))))))

(deftest a-target-another-body-noted-counts-unless-told-not-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [seen [{:kind :seen :what "diamond_block" :pos [28 64 3] :by "Other" :t 1 :until 9e15}]
              s (await (search! {:target "diamond_block"} {:cone? 8 :blocks diamond} 400 #(write-other! % "Other" seen)))
              own (await (search! {:target "diamond_block" :use-notes false :max-distance 20} {:cone? 8 :blocks diamond} 400
                                  #(write-other! % "Other" seen)))]
          (is (= [{:what "diamond_block" :pos [28 64 3] :noted true :by "Other"}] (:found (found-of s))))
          (is (= [] (walks s)))
          (is (= :stopped (:kind (not-found-of own)))))))))

(deftest a-noted-animal-in-the-bodys-own-zone-is-no-find-one-outside-is-and-an-old-note-is-no-find
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [note (fn [pos t] [{:kind :seen :what "cow" :id "u1" :pos pos :by "Other" :t t :until 9e15}])
              farm {:name "farm" :owner "Fake" :min [0 60 0] :max [10 70 10]}
              run! (fn ^:async f [pos t] (await (search! {:target "cow" :max-distance 20 :settings {:jobs.explore.search/max-legs 0}} {:zones [farm]} 20
                                                         #(write-other! % "Other" (note pos t)))))
              penned (await (run! [3 64 3] (js/Date.now)))
              wild (await (run! [12 64 12] (js/Date.now)))
              old (await (run! [12 64 12] 1))]
          (is (= :stopped (:kind (not-found-of penned))) "the penned cow is not counted")
          (is (re-find #"12,64,12" (str (:text (found-of wild)))) "a fresh note counts and the text says where")
          (is (= [{:x 12 :y 64 :z 12}] (walks old)) "walked to the old note") (is (= :stopped (:kind (not-found-of old))) "a note older than the stale setting is looked up again, not trusted"))))))

(deftest legs-that-cannot-be-stood-on-or-walked-are-recorded-and-skipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :max-distance 20 :spend {:wall-time 60}}
                                {:unloaded ["0,63,-16"] :unreachable ["16,64,0"]}))
              failed (get-in (event-of s :stopped) [:coverage :failed])]
          (is (= #{{:pos [16 64 0] :reason :unreachable}} (set failed)))
          (is (not-any? #(= {:x 0 :z -16} (select-keys % [:x :z])) (walks s)) "an unloaded column is never walked to"))))))

(def ring-1-cells (mapv (fn [[x z]] (str x ",63," z)) (search/spiral-points [0 0] 16 23)))

(deftest a-search-started-before-the-ground-around-loaded-waits-for-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:unloaded ring-1-cells} 28)]
          (core/submit! (:eng s) '(jobs.explore.search {:target "diamond_block" :settings {:jobs.explore.search/max-distance 20}}) {})
          (await (tu/run-ticks (:eng s) (:clock s) 8 700))
          (is (not (finished? s)) "still waiting for the chunks")
          (is (= [] (walks s)))
          (swap! (fake/state (:p s)) assoc :unloaded #{})
          (await (run-until-done s 400))
          (is (= 4 (count (walks s))))
          (is (= :distance (:why (not-found-of s)))))))))

(deftest a-far-centre-whose-ground-is-unloaded-is-walked-toward-not-waited-for
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells (mapv (fn [[x z]] (str x ",63," z)) (search/spiral-points [300 0] 16 23))
              s (setup {:unloaded cells} 28)]
          (core/submit! (:eng s) '(jobs.explore.search {:target "diamond_block" :center [300 64 0] :settings {:jobs.explore.search/max-distance 20}}) {})
          (await (tu/run-ticks (:eng s) (:clock s) 8 700))
          (is (seq (walks s)) "a walk started")
          (is (every? #(> (:x %) 200) (walks s)) "toward the centre"))))))

(deftest a-body-whose-own-ground-is-unloaded-waits-instead-of-walking-toward-far-columns
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells (into ["0,63,0"] (map (fn [[x z]] (str x ",63," z))) (search/spiral-points [0 0] 16 100))
              s (setup {:unloaded cells} 28)]
          (core/submit! (:eng s) '(jobs.explore.search {:target "diamond_block" :settings {:jobs.explore.search/max-distance 100}}) {})
          (await (tu/run-ticks (:eng s) (:clock s) 8 700))
          (is (not (finished? s)) "still waiting for its own chunks")
          (is (= [] (walks s))))))))

(deftest ground-that-never-loads-waits-until-the-call-time-is-spent
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :max-distance 20 :spend {:wall-time 60}} {:unloaded ring-1-cells}))]
          (is (= :wall-time (:reason (event-of s :stopped))) "no clock of its own: the engine's wall-time limit ends the wait")
          (is (= "stopped: wall-time" (:text (ended-of s))))
          (is (= [] (walks s))))))))

;; a leg whose goal lies by a mob the body would flee is not walked: go-to gives it up (:goal-danger :end), it is recorded
;; :goal-dangerous and does not count as a stuck walk
(deftest a-leg-by-a-dangerous-mob-is-recorded-and-not-walked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zombie {:key "z1" :name "zombie" :pos {:x 17 :y 64 :z 1}}
              s (with-redefs [threats/sensed-mobs (constantly [zombie])]
                  (await (search! {:target "diamond_block" :max-distance 20} {})))
              e (not-found-of s)]
          (is (= :distance (:why e)) (pr-str e))
          (is (some #{{:pos [16 64 0] :reason :goal-dangerous}} (get-in e [:coverage :failed])) (pr-str (:coverage e)))
          (is (not-any? #(= {:x 16 :z 0} (select-keys % [:x :z])) (walks s)) "the leg by the zombie is never walked to")
          (is (= 3 (count (walks s)))))))))

;; a step toward an unseen column whose step point lies by a dangerous mob is given up once: that column is tried, the
;; search goes on with others and never gives the same step up again
(deftest a-dangerous-step-toward-an-unseen-column-is-given-up-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zombie {:key "z1" :name "zombie" :pos {:x 1 :y 64 :z -7}}
              far? (fn [pos] (or (> (js/Math.abs (.-x pos)) 8) (> (js/Math.abs (.-z pos)) 8)))
              blind-far (fn [{:keys [p]}]
                          (let [at (.-sensedAt p)]
                            (aset p "sensedAt" (fn [pos] (if (far? pos) #js {:unknown true} (at pos))))))
              s (with-redefs [threats/sensed-mobs (constantly [zombie])]
                  (await (search! {:target "diamond_block" :max-distance 20} {} 80 blind-far)))
              given-up (filterv #(= :goal-dangerous (:reason %)) (get-in (or (not-found-of s) (found-of s)) [:coverage :failed]))]
          (is (finished? s) "the search ends")
          (is (= [{:pos [0 64 -6] :reason :goal-dangerous}] given-up) (pr-str (not-found-of s) (walks s)))
          (is (not-any? #(= {:x 0 :z -6} (select-keys % [:x :z])) (walks s)) "the step by the zombie is never walked to"))))))

(deftest a-step-toward-an-unseen-column-that-fails-is-not-repeated
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blind-far (fn [{:keys [p]}]
                          (let [at (.-sensedAt p)]
                            (aset p "sensedAt" (fn [pos] (if (or (> (js/Math.abs (.-x pos)) 8) (> (js/Math.abs (.-z pos)) 8)) #js {:unknown true} (at pos))))))
              s (await (search! {:target "diamond_block" :max-distance 20}
                                {:self {:pos [0 80 0]} :blocks (tu/box -2 99 -2 8 99 2 "stone")} 60 blind-far))
              e (not-found-of s)]
          (is (finished? s) "the search ends within 60 rounds, not at the time bound")
          (is (not= :time (:why e)))
          (let [ps (map :pos (get-in e [:coverage :failed]))]
            (is (= (count ps) (count (set ps))) "a column whose step failed is not stepped toward again")))))))

(deftest a-body-under-rock-never-steps-into-unseen-rock-toward-an-unseen-column
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blind-far (fn [{:keys [p]}]
                          (let [at (.-sensedAt p)]
                            (aset p "sensedAt" (fn [pos] (if (or (> (js/Math.abs (.-x pos)) 8) (> (js/Math.abs (.-z pos)) 8)) #js {:unknown true} (at pos))))))
              pocket (merge (tu/box -12 70 -12 12 120 12 "stone") (tu/box 0 80 0 0 81 0 "air"))
              s (await (search! {:target "rail" :max-distance 20} {:self {:pos [0.5 80 0.5]} :blocks pocket} 60 blind-far))]
          (is (finished? s))
          (is (empty? (walks s)) "no go-to toward a point inside rock"))))))

(deftest the-last-leg-before-stuck-digs-out-of-a-sealed-pocket
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [room (apply dissoc (tu/box -3 64 -3 3 66 3 "dirt") (for [x (range -2 3) z (range -2 3) y [64 65]] (str x "," y "," z)))
              s (await (search! {:target "diamond_block" :max-distance 23} {:blocks room} 200))]
          (is (some #(= :go-to.escalated (:kind %)) @(:seen s))))))))

(deftest an-interrupted-walk-is-not-a-verdict-on-its-leg
  (are [res expected] (= expected (search/interrupted? res))
    {:reason :cut-again} true
    {:reason :unreachable :why :moved-while-searching} true
    {:reason :unreachable :why :goal-not-standable} false
    {:arrived true} false))

(deftest a-plain-search-still-ends-at-the-timeout
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom nil)
              s (with-redefs [child/call-walk* (fn [_ _ _] (swap! @clock + 5000) (js/Promise.resolve :continue))]
                  (await (search! {:target "diamond_block" :max-distance 20 :spend {:job-time 60}} {} 400 #(reset! clock (:clock %)))))
              e (event-of s :limit_reached)]
          (is (finished? s))
          (is (= :job-time (:line (:cause e)))))))))

(deftest three-failed-legs-in-a-row-end-it-stuck
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :max-distance 23} {:unreachable (mapv (fn [[x z]] (str x ",64," z)) (search/spiral-points [0 0] 16 23))}))
              e (not-found-of s)]
          (is (= :stuck (:why e)))
          (is (= [[0 64 -16] [16 64 0] [0 64 16]] (mapv :pos (get-in e [:coverage :failed])))
              "each next leg lies another way: one within 60 degrees of a failed leg's way is held back"))))))

(deftest legs-refused-by-the-calls-floor-end-stuck-saying-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [refused {:kind :condition-failed :cause {:floor :health :min 10} :data {}}
              s (with-redefs [child/call-walk* (fn [_ _ _] (js/Promise.resolve refused))]
                  (await (search! {:target "diamond_block" :max-distance 23} {:self {:pos {:x 0 :y 64 :z 0} :health 8}})))
              e (not-found-of s)]
          (is (= :stuck (:why e)))
          (is (re-find #"every walk was refused: the body's health 8 is under the call's floor 10" (:text e)))
          (is (not (re-find #"skipped" (:text e)))))))))

;; live (Tamsin #68): the night reflex sealed the body in a pit; every leg's walk was blocked and three failed legs ended the search
(def night-pit
  (merge (into {} (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]] y [64 65]] [(str dx "," y "," dz) "dirt"]))
         {"0,66,0" "dirt"}))

(deftest a-body-sealed-in-its-night-shelter-walks-legs-and-lets-go-to-price-them
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :max-distance 23}
                                {:time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks night-pit}
                                40
                                #(mem/write! (:store (:eng %)) :shelter {:pos {:x 0 :y 64 :z 0} :roof {:x 0 :y 66 :z 0} :state :built} {:cap 10 :ttl st/day-ms})))]
          (is (seq (walks s)) "no flat dark hold: each leg's go-to prices its own route"))))))

(deftest a-leg-given-up-for-the-sealed-night-shelter-waits-for-light-not-stuck
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [kept {:kind :nothing-possible :cause {:reason :unreachable :why :seal-kept}
                    :data {:inside-own-shelter {:x 0 :y 64 :z 0} :arrived false}}
              calls (atom 0)
              s (with-redefs [child/call-walk* (fn [_ _ _] (swap! calls inc) (js/Promise.resolve kept))]
                  (await (search! {:target "diamond_block" :max-distance 23}
                                  {:time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks night-pit :light {[0 64 0] [0 0] [0 65 0] [0 0]}}
                                  40
                                  #(mem/write! (:store (:eng %)) :shelter {:pos {:x 0 :y 64 :z 0} :roof {:x 0 :y 66 :z 0} :state :built} {:cap 10 :ttl st/day-ms}))))]
          (is (not (finished? s)) "a kept seal is no failed leg")
          (is (= 1 @calls) "go-to is not asked again while the night keeps the seal")
          (is (some #(and (= :waiting (:kind %)) (= :dark (:reason %))) @(:seen s))))))))

;; live (Juniper, card e51d1fe9): three legs east all failed at the same rim and the search ended stuck; a leg that lies
;; the way a leg just failed from here waits until a leg arrives, so the next leg goes another way
(deftest a-leg-the-way-a-leg-just-failed-waits-for-one-that-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :max-distance 23} {:unreachable ["-16,64,-16" "0,64,-16" "16,64,-16"]}))
              e (not-found-of s)]
          (is (= :distance (:why e)) (pr-str e))
          (is (= #{[-16 64 -16] [0 64 -16] [16 64 -16]} (set (map :pos (get-in e [:coverage :failed])))) "the held leg is walked later, after an arrival"))))))

;; only held legs left (all candidates lie the way the one failed leg went): they are tried, so the search ends stuck
;; only after its failures in a row
(deftest only-held-legs-left-are-tried-after-one-failed-leg
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [marks (for [[x z] [[16 -16] [16 0] [16 16] [0 16] [-16 16]]]
                      {:kind :searched :what ["diamond_block"] :pos [x 64 z] :r 3 :by "Other" :t 1 :until 9e15})
              s (await (search! {:target "diamond_block" :max-distance 23} {:unreachable ["-16,64,-16"]} 400 #(write-other! % "Other" marks)))
              e (not-found-of s)]
          (is (= :distance (:why e)) (pr-str e))
          (is (some #(= {:x 0 :z -16} (select-keys % [:x :z])) (walks s)) "the held legs are walked after the one failure"))))))

(deftest outward-walks-along-the-heading
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :pattern :outward :heading :east :max-distance 40} {}))]
          (is (= [16 30] (take 2 (map :x (walks s)))) "the second leg is 16 further than where the body stopped, 2 short of the first")
          (is (every? #(<= (js/Math.hypot (:x %) (:z %)) 40) (walks s)) "no leg beyond :max-distance")
          (is (= :distance (:why (not-found-of s)))))))))

(deftest a-heading-alone-searches-outward-that-way
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :heading :west :max-distance 40} {}))]
          (is (seq (walks s)))
          (is (every? #(<= (:x %) 0) (walks s)) "no leg east of the origin"))))))

(deftest legs-radiate-from-the-center-when-one-is-given
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [args {:target "diamond_block" :pattern :outward :heading :west :max-distance 40}
              own (await (search! args {}))
              centred (await (search! (assoc args :center [30 64 0]) {}))]
          (is (seq (walks centred)))
          (is (not= (walks own) (walks centred)) "the legs are laid out from the centre, not from the body"))))))

(deftest a-center-beyond-the-distance-from-the-body-is-walked-to-and-searched-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :max-distance 40 :center [90 64 0]}
                                {:blocks (merge (tu/box 40 63 -30 130 63 30 "grass_block") {"96,64,2" "diamond_block"})}))]
          (is (= nil (not-found-of s)))
          (is (= [{:what "diamond_block" :pos [96 64 2]}] (:found (found-of s)))))))))

(deftest an-unknown-pattern-or-heading-is-refused-at-submit
  (is (re-find #":pattern must be one of :outward, :spiral, got :zigzag"
               (expr/problem registry/jobs '(jobs.explore.search {:target "diamond_block" :pattern :zigzag}))))
  (is (re-find #":heading must be one of :east, :north, :north-east, :north-west, :south, :south-east, :south-west, :west, got :up"
               (expr/problem registry/jobs '(jobs.explore.search {:target "diamond_block" :pattern :outward :heading :up})))))

(deftest a-call-without-a-target-can-never-run
  (with-redefs [ctx/frozen-call (constantly nil)]
    (is (= [:never :bad-args] ((juxt first #(:reason (nth % 2))) (search/no-target {:args {:target nil}}))))
    (is (= [:never :bad-args] ((juxt first #(:reason (nth % 2))) (search/no-target {:args {:target []}}))))
    (is (= :ok (first (search/no-target {:args {:target "diamond_block"}}))))))

(deftest blocks-in-the-footprint-of-the-bodys-own-plan-are-no-find
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (shape/with-author {:id "hut" :parts [{:id "post" :cells [[10 64 0]] :want "oak_log"}]} "Fake")
              s (await (search! {:target "oak_log"} {:blocks {"10,64,0" "oak_log" "0,64,12" "oak_log"} :plans {"hut" plan}}))]
          (is (= [[0 64 12]] (map :pos (:found (found-of s))))))))))

(deftest a-killed-animal-noted-close-by-is-no-find
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [note [{:kind :seen :what "cow" :id "dead" :pos [3 64 3] :by "Fake" :t 1 :until 9e15}]
              s (await (search! {:target "cow" :max-distance 20 :settings {:jobs.explore.search/max-legs 0}} {} 20 #(write-other! % "Other" note)))]
          (is (nil? (found-of s)) (pr-str (:found (ended-of s)))))))))

(deftest ore-the-body-has-not-seen-is-neither-found-nor-noted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :max-distance 20 :spend {:wall-time 60}} {:blocks {"10,64,0" "diamond_block"}}
                                400 (fn [{:keys [p]}] (tu/blind p))))
              e (event-of s :stopped)]
          (is (= [] (:found e)))
          (is (not-any? #(= :seen (:kind %)) (file-notes s))))))))

(deftest a-scan-turns-its-head-so-ore-behind-the-body-is-found
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :max-distance 20 :settings {:jobs.explore.search/max-legs 0}}
                                {:blocks {"-4,64,0" "diamond_block"} :cone? true}))
              e (found-of s)]
          (is (= 1 (count (:found e))))
          (is (some #(= :seen (:kind %)) (file-notes s))))))))

(deftest a-scan-notes-ore-it-last-saw-until-a-look-corrects-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :max-distance 20 :settings {:jobs.explore.search/max-legs 0}}
                                {:blocks {"0,64,5" "diamond_block"} :cone? true}
                                400 (fn [{:keys [p]}]
                                      ;; mined once the body saw it, before sense reads memory: the body cannot know
                                      (let [seen-blocks (aget p "seenBlocks")]
                                        (aset p "seenBlocks" (fn [q]
                                                               (let [r (seen-blocks q)]
                                                                 (when (some #(= "diamond_block" (.-name %)) r)
                                                                   (fake/set-block! p {:x 0 :y 64 :z 5} "air"))
                                                                 r)))))))]
          (is (some? (found-of s)) "no sight pass corrected it: memory still says ore")
          (is (some #(= :seen (:kind %)) (file-notes s))))))))

(deftest a-name-with-many-sightings-does-not-fill-the-scan-cap-for-the-others
  (let [p (tu/seeing-all (tu/fake {:blocks (merge (tu/box 1 64 1 10 64 7 "hay_block") {"14,64,0" "diamond_block"})}))
        seen (set (map :what (search/sense p ["hay_block" "diamond_block"] 20)))]
    (is (= #{"hay_block" "diamond_block"} seen))))

(deftest one-choice-reads-a-bounded-number-of-stand-cells-and-says-more-is-left
  (let [reads (atom 0)
        p (tu/fake {})
        counting (js/Proxy. p #js {:get (fn [t k] (if (= "blockAt" k)
                                                   (fn [pos] (swap! reads inc) (.blockAt t pos))
                                                   (let [v (aget t k)] (if (fn? v) (.bind v t) v))))})
        c {:args {:pattern :spiral} :primitives counting}
        r (settings/with-settings {:jobs.lib.cost.find/leg-spacing 4 :jobs.explore.search/max-distance 128}
            #(search/choose-leg c {:origin [0 64 0]} ["diamond_block"] []))]
    (is (nil? (:leg r)))
    (is (:more r))
    (is (< @reads 5000))))

(deftest a-search-over-ground-with-no-surface-takes-several-steps-and-ends-not-found-distance
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {} 0)]
          (core/submit! (:eng s) '(jobs.explore.search {:target "diamond_block" :settings {:jobs.explore.search/max-distance 40 :jobs.lib.cost.find/leg-spacing 4}}) {})
          (await (run-until-done s 400))
          (let [e (not-found-of s)]
            (is (<= (get-in e [:coverage :scans]) 2) "a step that only goes on choosing points does not look again")
            (is (= :distance (:why e)))
            (is (> (count (get-in e [:coverage :failed])) 100))))))))

(def sealed-room
  "Stone x 2..30, z 8..24, y 64..70 with a 3x3x2 room x 15..17, z 15..17 (the body in it) and ore buried at 22,64,16 and 16,64,21."
  (merge (tu/box 2 64 8 30 70 24 "stone") (tu/box 15 64 15 17 65 17 "air")
         {"22,64,16" "iron_ore" "16,64,21" "iron_ore"}))

(deftest buried-ore-is-not-found-from-inside-a-sealed-room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "iron_ore" :count 1 :max-distance 8 :settings {:jobs.lib.cost.find/leg-spacing 4} :scan-radius 16 :spend {:job-time 40}}
                                {:blocks sealed-room :cone? true :self {:pos {:x 16.5 :y 64 :z 16.5}}} 60))]
          (is (nil? (found-of s)) (pr-str (:found (found-of s)))))))))

(deftest use-notes-false-ignores-searched-notes-older-than-the-job
  (let [p (tu/seeing-all (tu/fake {:blocks (ground 36)}))
        note (fn [t] {:kind :searched :what ["diamond_block"] :pos [0 64 0] :r 200 :t t})
        choose (fn [use-notes? ns]
                 (settings/with-settings {:jobs.lib.cost.find/leg-spacing 4 :jobs.explore.search/max-distance 8}
                   #(search/choose-leg {:args {:pattern :spiral :use-notes use-notes?} :primitives p} {:origin [0 64 0] :t0 100} ["diamond_block"] ns)))]
    (is (nil? (:leg (choose true [(note 50)]))) "default: an old note covers the ground")
    (is (some? (:leg (choose false [(note 50)]))) "fresh look: an old note is ignored")
    (is (nil? (:leg (choose false [(note 150)]))) "a note this job made still covers")))

(deftest a-leg-for-farm-animals-never-stands-under-rock-but-one-for-ore-may
  (let [p (tu/seeing-all (tu/fake {:blocks (ground 36)}))
        choose (fn [names] (with-redefs [sh-sense/sky-light-at (constantly 0)]
                             (settings/with-settings {:jobs.lib.cost.find/leg-spacing 4 :jobs.explore.search/max-distance 8}
                               #(search/choose-leg {:args {:pattern :spiral} :primitives p} {:origin [0 64 0]} names []))))]
    (is (nil? (:leg (choose ["cow" "pig"]))))
    (is (every? #(= :underground (:reason %)) (:failed (choose ["cow" "pig"]))))
    (is (some? (:leg (choose ["diamond_block"]))))))

(deftest a-leg-for-farm-animals-never-stands-in-a-pit-below-the-feet-but-one-for-ore-may
  (let [p (tu/seeing-all (tu/fake {:blocks (tu/box -36 45 -36 36 45 36 "grass_block") :self {:pos {:x 0.5 :y 64 :z 0.5}}}))
        choose (fn [names] (settings/with-settings {:jobs.lib.cost.find/leg-spacing 4 :jobs.explore.search/max-distance 8}
                             #(search/choose-leg {:args {:pattern :spiral} :primitives p} {:origin [0 64 0]} names [])))]
    (is (nil? (:leg (choose ["chicken"]))))
    (is (every? #(= :underground (:reason %)) (:failed (choose ["chicken"]))))
    (is (some? (:leg (choose ["diamond_block"]))))))

(deftest a-note-of-a-block-the-body-now-sees-as-something-else-is-not-a-finding
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [seen [{:kind :seen :what "diamond_block" :pos [0 64 5] :by "Other" :t 1 :until 9e15}]
              s (await (search! {:target "diamond_block" :max-distance 20 :settings {:jobs.explore.search/max-legs 0}} {:cone? true} 60 #(write-other! % "Other" seen)))]
          (is (nil? (found-of s)) "the cell is in view and holds air: the note is out of date")
          (is (= :stopped (:kind (not-found-of s)))))))))

(defn looks [{:keys [p]}] (count (filter #(= "look" (.-name %)) (.-calls (.-world p)))))

;; a body in water does not turn to look round (it sinks and drowns while it does): it glances at what it faces and walks on
(deftest a-body-in-water-does-not-look-round-where-it-is
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :max-distance 20} {:self {:pos [0.5 64 0.5] :inWater true}}))
              e (not-found-of s)]
          (is (finished? s))
          (is (zero? (looks s)) (pr-str e)))))))

(deftest a-target-already-seen-is-found-without-looking-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [rays (atom nil)
              stats #(perception/stats (aget (:p %) "perception"))
              s (await (search! {:target "diamond_block" :max-distance 20} {:blocks {"0,64,5" "diamond_block"} :cone? true} 60
                                (fn [s] (perception/pass! (aget (:p s) "perception")) (reset! rays (:rays (stats s))))))]
          (is (= [{:what "diamond_block" :pos [0 64 5]}] (:found (found-of s))))
          (is (= 0 (looks s)))
          (is (= @rays (:rays (stats s))) "no sight pass cast a ray"))))))

;; walks that fail a block from where the body last looked round: it looks round once per spot, not on every cell
(deftest a-scan-within-a-few-blocks-of-the-last-look-round-does-not-look-round-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [prims (atom nil)
              walk (fn [_ _ _]
                     (swap! (fake/state @prims) update-in [:self :pos 0] inc)
                     (js/Promise.resolve {:kind :nothing-possible :cause :unreachable :data {}}))
              s (with-redefs [child/call-walk* walk]
                  (await (search! {:target "diamond_block" :max-distance 23} {} 60 #(reset! prims (:p %)))))
              e (not-found-of s)]
          (is (= :stuck (:why e)))
          (is (= 3 (get-in e [:coverage :scans])))
          (is (= 8 (looks s)) "one look round (four headings, level and down) at the first spot only"))))))

(deftest done-carries-the-found-and-coverage-for-a-limit-ending
  (let [c {:primitives (tu/fake {}) :root "j1" :slots [] :args {:count 2}
           :view (fn [] {:now 0 :data {:entries {:job/j1 [{:t 0 :data {:found [{:what "x" :pos [1 2 3]}] :legs 3 :scans 2 :farthest 40}}]}}})}
        [_ _ d] (search/done? c)]
    (is (= {:done 1 :left 1} (select-keys d [:done :left])))
    (is (= {:legs 3 :scans 2 :failed [] :skipped 0 :farthest 40} (get-in d [:result :coverage])))
    (is (= [{:what "x" :pos [1 2 3]}] (get-in d [:result :found])))))

(deftest a-patch-search-names-no-animal-so-it-is-not-surface-only
  (are [names surface?] (= surface? (search/surface-only? names))
    [] false
    ["cow" "pig"] true
    ["cow" "iron_ore"] false))
