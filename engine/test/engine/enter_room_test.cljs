(ns engine.enter-room-test
  "jobs.survival.enter-room: walk into the home (or stay in the roofed room the body is in), shut its open door, fill its
  doorway; end for what it cannot do."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.events :as events]
            [jobs.lib.world-files :as ew]
            [engine.triggers :as triggers]
            [engine.hut-shelter-test :refer [hut-world door-states door-open?]]
            [engine.memory :as mem]
            [jobs.lib.ledger :as ledger]
            [engine.registry :as registry]
            [engine.shelter-support :as st]
            [jobs.lib.walk-risk :as risk]
            [engine.ctx :as ctx]
            [jobs.lib.cost :as cost]
            [engine.flee-home-test :as fh]
            [jobs.lib.reach :as reach]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.rooms :as room-lib]
            [jobs.survival.enter-room :as enter-room]
            [engine.test-util :as tu]))

(def open-door-states (update-vals door-states #(assoc % :open true)))

(defn ^:async enter!
  "Run (jobs.survival.enter-room args) over world, with :home at home when given: {:eng :p :seen}."
  [world args & [home]]
  (let [{:keys [eng p seen] :as s} (st/setup (merge {:time st/night} world))]
    (when home (mem/write! (:store eng) :home {:pos home} mem/place-policy))
    (core/submit! eng (list 'jobs.survival.enter-room args) {})
    (await (tu/run-until-empty eng 30))
    s))

(deftest a-body-in-the-hut-shuts-its-open-door
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]} (await (enter! (assoc (hut-world {:x 5 :y 64 :z 0} {}) :states open-door-states) {}))]
          (is (false? (door-open? p)) "the door is shut")
          (is (empty? (st/calls p "place")) "nothing placed")
          (is (empty? (st/ended-because seen :door-open))))))))

(deftest a-door-that-stays-open-is-named-and-ends-the-job-door-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (assoc (hut-world {:x 5 :y 64 :z 0} {}) :states open-door-states)
              {:keys [eng p seen]} (st/setup (assoc world :time st/night))]
          (.override (.-world p) "useOn" (fn [token a impl] (impl token (clj->js {:pos {:x 3 :y 64 :z 0}}))))
          (core/submit! eng '(jobs.survival.enter-room {}) {})
          (await (tu/run-until-empty eng 30))
          (is (true? (door-open? p)) "the click did nothing")
          (is (= [{:door [5 64 2]}] (st/entries eng :night-door-failed)))
          (is (empty? (st/emitted seen :shelter.door-failed)) "the ending says it, no warn beside it")
          (is (seq (st/ended-because seen :door-open))))))))

(deftest a-forgotten-home-and-no-roof-ends-no-room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]} (await (enter! {:floor tu/walk-floor :self {:pos {:x 0 :y 64 :z 0}}} {}))]
          (is (seq (st/ended-because seen :no-room)))
          (is (empty? (st/calls p "place"))))))))

(deftest a-body-outside-walks-into-the-home-and-shuts-the-door
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (enter! (assoc (hut-world {:x 5 :y 64 :z 6} {}) :states open-door-states) {} {:x 5 :y 64 :z 0}))]
          (is (< (:z (st/pos-of p)) 2) "inside the hut")
          (is (false? (door-open? p)) "the door is shut"))))))

;; go-to's walk price (risk/status) says :later when the walk costs more than waiting; a zombie by the way is priced, not gated.
(deftest a-listed-enter-room-waits-while-go-to-says-later
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (st/setup (assoc (hut-world {:x 5 :y 64 :z 6} {}) :time st/night
                                                  :entities [{:id 8 :name "zombie" :kind "hostile" :pos {:x 5 :y 64 :z 3} :health 20}]))]
          (mem/write! (:store eng) :home {:pos {:x 5 :y 64 :z 0}} mem/place-policy)
          (with-redefs [risk/status (let [r #(contract/later :risky "test: the walk costs more than waiting" :recheck-after-ms 1000)]
                                      (fn ([_ _] (r)) ([_ _ _] (r))))]
            (core/submit! eng '(jobs.survival.enter-room {}) {})
            (await (tu/run-until-empty eng 5)))
          (is (= [:risky] (distinct (map :reason (filter #(= :waiting (:kind %)) @seen)))))
          (is (empty? (filter #(= :stopped (:kind %)) @seen)) "it waits, it does not end no-walk"))))))

(def stones st/dirt-stack)

(def day {:time st/noon})

(def doorway-hut
  "The hut with its door cells open air: a roofed room with a doorway gap."
  (merge day (hut-world {:x 5 :y 64 :z 0} {"5,64,2" "air" "5,65,2" "air"}) {:states {}}))

(def holed-hut (merge day (hut-world {:x 5 :y 64 :z 0} {"5,67,0" "air"})))

(defn ^:async enter-with
  "enter! with the body carrying inventory: {:eng :p :seen}."
  [world inventory]
  (await (enter! (assoc world :inventory inventory) {})))

(deftest a-doorway-gap-is-filled-and-entered-in-the-ledger-for-the-morning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (enter-with doorway-hut stones))]
          (is (seq (filter #(= 2 (:z (st/arg-pos %))) (st/calls p "place"))) "the doorway was filled")
          (is (seq (filter #(= :doorway (:purpose %)) (ledger/open-entries (mem/view (:store eng))))))
          (is (seq (st/entries eng :doorway-tried)))
          (is (empty? (st/ended-because seen :no-blocks))))))))

(deftest a-roof-hole-is-plugged-and-the-cell-recorded
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (enter-with holed-hut stones))
              placed (st/calls p "place")]
          (is (= [67] (distinct (map #(:y (st/arg-pos %)) placed))) "one block over the body"))))))

(deftest a-room-with-a-gap-or-hole-and-no-block-ends-no-blocks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [world [doorway-hut holed-hut]]
          (let [{:keys [p seen]} (await (enter-with world []))]
            (is (seq (st/ended-because seen :no-blocks)))
            (is (empty? (st/calls p "place")))))
        (let [{:keys [eng]} (await (enter-with doorway-hut []))]
          (is (empty? (st/entries eng :doorway-tried)) "the night, not enter-room, marks a doorway it could not fill"))))))

(deftest the-quote-is-infinite-when-the-room-cannot-be-closed-and-free-when-it-is-shut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [quote-of (:quote-of (get registry/jobs 'jobs.survival.enter-room))
              mean (fn ^:async mean-of [world inventory]
                     (let [{:keys [eng p]} (st/setup (assoc world :inventory inventory))
                           q (await (quote-of {:primitives p :args {} :view (fn [] (mem/view (:store eng)))
                                               :quote-call {:use {:blocks ["dirt"]}}}))]
                       (get-in q [:uses :job-time 1])))]
          (is (= 0 (await (mean (hut-world {:x 5 :y 64 :z 0} {}) []))))
          (is (= ##Inf (await (mean doorway-hut []))))
          (is (= 6 (await (mean doorway-hut stones)))))))))

(deftest a-zombie-farther-than-the-old-race-radius-that-gets-to-the-door-first-loses-the-race
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [lost? (fn [zombie-z]
                      (let [{:keys [p]} (st/setup (assoc (hut-world {:x 5.5 :y 64 :z 30.5} {})
                                                         :floor [-40 -40 40 40]
                                                         :entities [{:id 7 :uuid "u7" :name "zombie" :kind "hostile" :pos {:x 5.5 :y 64 :z zombie-z} :visible true :chase {:speed 0.4}}]))]
                        (enter-room/race-lost? {:primitives p} {:pos {:x 5 :y 64 :z 0}})))]
          (is (true? (lost? 5.5)) "a zombie 25 blocks off, 3 from the door, is there first")
          (is (false? (lost? 80.5)) "one far behind the body is not"))))))

(deftest a-walk-to-a-seen-roof-with-open-sides-is-not-quoted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [quote-of (:quote-of (get registry/jobs 'jobs.survival.enter-room))
              {:keys [eng p]} (st/setup {:floor tu/walk-floor :self {:pos {:x 5 :y 64 :z 12}} :inventory stones
                                         :blocks (tu/box 3 67 -2 7 67 2 "cobblestone")})
              _ (mem/write! (:store eng) :home {:pos {:x 5 :y 64 :z 0}} mem/place-policy)
              at (.-sensedAt p)
              mean (fn ^:async mean-of [] (get-in (await (quote-of {:primitives p :args {} :view (fn [] (mem/view (:store eng)))
                                                              :quote-call {:use {:blocks ["dirt"]}}}))
                                           [:uses :job-time 1]))]
          (is (= ##Inf (await (mean))))
          ;; the dark hides the sky beside the slab; the slab over the home is still seen
          (aset p "sensedAt" (fn [pos] (if (and (> (.-z pos) 2) (>= (.-y pos) 66))
                                         #js {:unknown true :pos pos}
                                         (.call at p pos))))
          (is (= ##Inf (await (mean))) "an unseen sky beside a seen roof on open sides is no reason to walk"))))))

(deftest walls-under-the-sky-are-unclosable-a-room-with-one-roof-hole-is-not
  ;; A walled yard is a closed room (no walker gets in) that enter-room can never shut the night out of; the night walked
  ;; into one as its :home and ended :open.
  (let [yard (st/setup (hut-world {:x 5 :y 64 :z 12} (into {} (for [x [4 5 6] z [-1 0 1]] [(str x ",67," z) "air"]))))
        holed (st/setup (assoc holed-hut :self {:pos {:x 5 :y 64 :z 12}}))]
    (is (true? (room-lib/refuge-room? (:p yard) {:x 5 :y 64 :z 0})) "the yard is closed")
    (is (true? (enter-room/unclosable? (:p yard) {:x 5 :y 64 :z 0})))
    (is (false? (enter-room/unclosable? (:p holed) {:x 5 :y 64 :z 0})) "one hole is mended by a plug")))

(deftest a-home-in-anothers-zone-is-not-walked-into
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              [seen sink] (tu/legacy-capture-sink)
              world (assoc (hut-world {:x 5 :y 64 :z 6} {}) :states open-door-states :time st/night)
              p (tu/fake (merge {:offlineScale 0.0001 :floor tu/walk-floor} world))
              _ (st/dawn-after! p st/default-dawn)
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})
                                :world (ew/of-data {} {} [{:name "yard" :min [3 60 -2] :max [7 70 2] :owner "Miles"}])})]
          (mem/write! (:store eng) :home {:pos {:x 5 :y 64 :z 0}} mem/place-policy)
          (core/submit! eng '(jobs.survival.enter-room {}) {})
          (await (tu/run-until-empty eng 30))
          (is (seq (st/ended-because seen :no-room)))
          (is (> (:z (st/pos-of p)) 3) "stayed outside"))))))

(deftest with-no-home-the-nearest-closed-room-seen-is-entered
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (enter! (assoc (hut-world {:x 5 :y 64 :z 6} {}) :states open-door-states) {}))]
          (is (< (:z (st/pos-of p)) 2) "inside the hut")
          (is (false? (door-open? p)) "the door is shut"))))))

(deftest a-flee-entry-walks-legs-into-the-room-named-by-a-cell-and-shuts-the-door
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (enter! (assoc (hut-world {:x 5 :y 64 :z 6} {}) :states open-door-states) {:at {:x 5 :y 64 :z 1} :flee? true}))]
          (is (< (:z (st/pos-of p)) 2) "inside the hut, no home remembered")
          (is (false? (door-open? p)) "the door is shut"))))))

(defn shifted [m dx dz]
  (into {} (for [[k v] m :let [[x y z] (map js/Number (.split k ","))]] [(str (+ x dx) "," y "," (+ z dz)) v])))

(deftest with-no-home-the-closed-room-seen-that-is-walked-to-in-fewest-steps-is-entered-not-the-nearer-door-behind-a-wall
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [far (hut-world {:x 5 :y 64 :z 8} {})
              world (-> (hut-world {:x 5 :y 64 :z 8} {})
                        (update :blocks merge (shifted (:blocks far) 12 12) (tu/box -30 64 5 30 66 5 "stone"))
                        (assoc :floor [-30 -10 40 30])
                        (update :states merge open-door-states (shifted open-door-states 12 12)))
              {:keys [p]} (await (enter! world {}))]
          (is (> (:z (st/pos-of p)) 9) "in the far hut, not walked round the wall to the near one"))))))

(deftest the-pick-is-made-again-whenever-the-doors-seen-change
  (let [room (fn [n] {:pos {:x n :y 0 :z 0} :door {:x n :y 0 :z 1}})
        [a b c] (map room [1 2 3])
        picks? (fn [starts picked-doors]
                 (with-redefs [enter-room/target-of (fn [_] {:pos (:pos a)})
                               enter-room/seen-starts (fn [_] starts)
                               ctx/mem (fn [_] {:picked-doors picked-doors})]
                   (enter-room/picks-seen? {:args {}})))]
    (is (true? (picks? [a b] nil)) "two doors seen, nothing picked yet")
    (is (false? (picks? [a b] #{(:door a) (:door b)})) "the doors stand")
    (is (true? (picks? [a b c] #{(:door a) (:door b)})) "a newly seen door is priced too")
    (is (false? (picks? [a] nil)) "one door: nothing to choose")))

(defn ^:async pick-over
  "pick-seen over closed rooms behind starts, the body at the origin, route-cost from price (door -> seconds), called again
  with the state it leaves until it is done: {:pick :quoted [door...] :calls [[door...] per call]}. candidate (default
  every start a closed room, no flood) stands for candidate?."
  [starts price & [candidate]]
  (let [quoted (atom [])
        state (atom {})
        [ss cand self route mid mem] [enter-room/seen-starts enter-room/candidate? jobs.lib.util/self-pos room-lib/route-cost room-lib/mid ctx/mem]]
    (set! enter-room/seen-starts (fn [_] starts))
    (set! enter-room/candidate? (or candidate (fn [_ _ _] true)))
    (set! jobs.lib.util/self-pos (fn [_] {:x 0 :y 0 :z 0}))
    (set! room-lib/mid identity)
    (set! ctx/mem (fn [_] @state))
    (set! room-lib/route-cost (fn ^:async f [_ args] (swap! quoted conj (:pos args)) (price (:pos args))))
    (try (loop [calls []]
           (let [before (count @quoted)
                 {:keys [pick done?] :as r} (await (enter-room/pick-seen {:primitives #js {}} (:pick-state @state)))
                 calls (conj calls (subvec @quoted before))]
             (reset! state {:pick-state (:state r)})
             (if (or done? (> (count calls) (* 2 (count starts))))
               {:pick pick :quoted @quoted :calls calls}
               (recur calls))))
         (finally (set! enter-room/seen-starts ss) (set! enter-room/candidate? cand) (set! jobs.lib.util/self-pos self)
                  (set! room-lib/route-cost route) (set! room-lib/mid mid) (set! ctx/mem mem)))))

(defn room-at [n] {:pos {:x n :y 0 :z 0} :door {:x n :y 0 :z 1}})

(deftest the-room-whose-route-is-cheapest-is-picked-whatever-the-door-order
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[near far] (map room-at [1 40])
              price {(:door near) 90 (:door far) 4}
              {:keys [pick]} (await (pick-over [near far] price))]
          (is (= {:pos (:pos far)} pick) "the far door by line, the cheap dig, wins"))))))

(deftest among-twenty-seen-huts-only-the-near-doors-a-route-could-still-beat-are-quoted
  ;; a quote is a go-to search: none for a door whose straight walk already costs more than the best route less the CPU
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [huts (map #(room-at (* 2 %)) (range 1 21))
              doors (map :door huts)
              ;; the nearest is behind a wall: the walk round it is long, the rest are walked straight
              price (fn [{:keys [x] :as d}] (if (= d (first doors)) 30 (* 0.3 x)))
              {:keys [pick quoted calls]} (await (pick-over huts price))]
          (is (= (:pos (second huts)) (:pos pick)) "the cheapest route of those quoted, the second hut")
          (is (< 1 (count quoted) 20) (str "quoted only while a nearer route could still win: " (count quoted)))
          (is (= (take (count quoted) doors) quoted) "nearest first")
          (is (> (+ (* 2 (room-lib/judge-s)) (room-lib/way-s)) (cost/search-budget-s)) "a door's floods and quote are over the search budget")
          (is (every? #(<= (count %) 1) calls) (str "so a call quotes one door and the next goes on from the state it left, none twice: " calls)))))))

(deftest a-call-that-floods-leaves-its-quote-to-the-next
  ;; a call's floods are one search and a quote one of its own: never both in a call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[near far] (map room-at [1 40])
              floods (atom [])
              flooding (fn [_ {:keys [door]} _] (swap! floods conj door) (reach/charge! 10 :read-s false) true)
              {:keys [pick quoted calls]} (await (pick-over [near far] {(:door near) 90 (:door far) 4} flooding))]
          (is (= {:pos (:pos far)} pick))
          (is (= [(:door near) (:door far)] quoted))
          (is (= [[] [(:door near)] [] [(:door far)]] calls) (str "each door's floods in one call, its quote in the next: " calls)))))))

(deftest a-doors-unseen-check-floods-nothing-its-judgement-did-not
  ;; candidate?'s unseen check rides on the floods side-s prices (reach-room/look, kept per body), none of its own
  (let [{:keys [p]} (fh/setup (assoc (fh/hut {:x -13.5 :y 64 :z 10.5} (merge door-states fh/lone-door-states) [] fh/lone-doors)
                                     :home false :perceive true))
        side {:pos {:x -6 :y 64 :z 7}}
        floods (atom 0)
        flood reach-room/flood-cells
        _ (enter-room/candidate? p side true)
        kept? (zero? (room-lib/side-s p (:pos side)))
        _ (set! reach-room/flood-cells (fn [n s b] (swap! floods inc) (flood n s b)))
        again (try (enter-room/candidate? p side true) (finally (set! reach-room/flood-cells flood)))]
    (is kept? "the side's judgement is kept: side-s prices it at none")
    (is (= 0 @floods) (str "so asking again floods nothing: " @floods " floods, candidate " again))))

(deftest a-quotes-pick-not-done-goes-on-at-the-next-quote
  ;; a quote's pick is a search a call goes on with: the next quote takes up the state the last one left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (fh/setup (assoc (fh/hut {:x -13.5 :y 64 :z 10.5} (merge door-states fh/lone-door-states) [] fh/lone-doors) :home false))
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})
              _ (.setOwner (:primitives eng) "tx")
              kept (atom [])
              [picks? pick] [enter-room/picks-seen? enter-room/pick-seen]]
          (set! enter-room/picks-seen? (constantly true))
          (set! enter-room/pick-seen (fn ^:async ps [_ state] (swap! kept conj state) {:pick nil :done? false :state {:n (inc (:n state 0))}}))
          (try (await (enter-room/quote-of c))
               (await (enter-room/quote-of c))
               (finally (set! enter-room/picks-seen? picks?) (set! enter-room/pick-seen pick)))
          (is (= [nil {:n 1}] @kept) (str "the second quote goes on from the first's state: " @kept)))))))
