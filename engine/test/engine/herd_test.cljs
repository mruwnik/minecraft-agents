(ns engine.herd-test
  "jobs.animals.herd against the fake world: a 5x5 pen (fence ring x 10..16, z 0..6) with a gate in its west wall."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.hostile-test :as h]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.triggers :as triggers]
            [jobs.animals.herd :as herd]
            [engine.test-util :as tu]))

(def job 'jobs.animals.herd)

(def gate {:x 10 :y 64 :z 3})
(def gate-key "10,64,3")
(def box {:min {:x 11 :y 64 :z 1} :max {:x 15 :y 64 :z 5}})

(def ground
  (into {} (for [x (range -10 30) z (range -10 16)] [(str x ",63," z) "stone"])))

(def fence-ring
  (into {} (for [x (range 10 17) z (range 0 7) :when (or (#{10 16} x) (#{0 6} z))] [(str x ",64," z) "oak_fence"])))

(defn cow [id x z & [more]]
  (merge {:id id :uuid (str "u" id) :name "cow" :kind "passive" :pos {:x x :y 64 :z z}} more))

(def wheat {:name "wheat" :count 4})

(defn leads
  "n leads and the cows' food."
  [n]
  [{:name "lead" :count n} wheat])

(defn world
  "The pen with its gate shut, the body west of it; more blocks and states merged over, other world keys passed on."
  [{:keys [entities inventory blocks states] :as w}]
  (merge w
         {:self {:pos {:x 2 :y 64 :z 3}}
          :inventory (or inventory (leads 2))
          :entities (or entities [])
          :blocks (merge ground fence-ring {gate-key "oak_fence_gate"} blocks)
          :states (merge {gate-key {:open false :facing "east"}} states)}))

(defn clock-on-wait!
  "Make the fake's wait advance the test clock by its :ms, as a real wait takes that long: a herd round runs many
  steps, and its waits and timeouts read the clock. Each wait also calls the observers in :steps (on-step!), the
  test's look at the world between two steps of one round."
  [{:keys [p clock] :as s}]
  (let [steps (atom [])]
    (.override (.-world p) "wait" (fn [token a impl]
                                    (swap! clock + (or (.-ms a) 0))
                                    (doseq [f @steps] (f))
                                    (impl token a)))
    (assoc s :steps steps)))

(defn on-step! [s f] (swap! (:steps s) conj f) s)

(defn setup [w] (clock-on-wait! (h/setup w)))

(defn ^:async run-ticks
  [{:keys [eng clock]} n]
  (dotimes [_ n]
    (swap! clock + 700)
    (await (core/tick! eng))))

(defn submit! [s args]
  (core/submit! (:eng s) (list job (merge {:mob "cow" :box box} args)) {})
  s)

(defn ^:async scenario [args w n]
  (let [s (submit! (setup (world w)) args)]
    (await (run-ticks s n))
    s))

(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))
(defn done-event [s] (first (events-of s :herd.done)))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn calls-of [{:keys [p]} name] (h/calls p name))
(defn entities-of [{:keys [p]}] (fake/entities p))
(defn cow-of [s id] (first (filter #(= id (:id %)) (entities-of s))))
(defn in-pen? [c] (let [[x _ z] (:pos c)] (and (<= 11 x 15) (<= 1 z 5))))
(defn on-lead [s] (mapv :id (filter #(true? (:leashed-to-me %)) (entities-of s))))
(defn update-entity!
  "Apply f to the fake's entity with the id."
  [{:keys [p]} id f & args]
  (swap! (fake/state p) update :entities (fn [es] (mapv #(if (= id (:id %)) (apply f % args) %) es))))
(defn set-x! [s id x] (update-entity! s id update :pos assoc 0 x))
(defn gate-open? [{:keys [p]}] (true? (some-> (.blockAt p (clj->js gate)) .-properties .-open)))
(defn self-x [{:keys [p]}] (.. p self -pos -x))
(defn count-of [{:keys [p]} item]
  (reduce + (map :count (filter #(= item (:name %)) (js->clj (.-inventory (.self p)) :keywordize-keys true)))))
(defn clicked-ids [s] (set (map #(.. % -args -id) (calls-of s "interact"))))
(defn held [{:keys [p]}] (.-held (.self p)))

(defn pos-map [c] (let [[x y z] (:pos c)] {:x x :y y :z z}))

(defn watch-gate!
  "Record every gate click of the fake as {:was-open :overlapping :held}: the gate state before it, the cows whose box
  overlaps the gate cell then, the :gate-held entries then. Returns the atom of the records."
  [{:keys [p eng] :as s}]
  (let [log (atom [])]
    (.override (.-world p) "useOn"
               (fn [token args impl]
                 (swap! log conj {:was-open (gate-open? s)
                                  :overlapping (count (filter #(herd/overlaps-cell? (pos-map %) 0.45 [10 64 3]) (entities-of s)))
                                  :held (count (mem/entries (mem/view (:store eng)) :gate-held))})
                 (impl token args)))
    log))

(defn ^:async scenario-log
  "As scenario, with the gate clicks recorded: [s log]."
  [args w n]
  (let [s (submit! (setup (world w)) args)
        log (watch-gate! s)]
    (await (run-ticks s n))
    [s log]))

(defn held-entries [{:keys [eng]}] (mem/entries (mem/view (:store eng)) :gate-held))

(deftest brings-only-the-missing-ones-and-shuts-the-gate-behind-them
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 3}
                                 {:entities [(cow 9 14 1) (cow 1 4 3) (cow 2 6 4) (cow 3 -8 12)]}
                                 400))
              e (done-event s)]
          (is (finished? s))
          (is (= :brought (:reason e)))
          (is (= 3 (:inside e)))
          (is (= #{"u1" "u2"} (set (:brought e))))
          (is (every? in-pen? [(cow-of s 1) (cow-of s 2) (cow-of s 9)]))
          (is (not (in-pen? (cow-of s 3))) "the third cow outside was not wanted")
          (is (not (contains? (clicked-ids s) 9)) "the cow already inside is never touched")
          (is (empty? (on-lead s)))
          (is (not (gate-open? s)) "the gate is shut")
          (is (< (self-x s) 10) "the body ends outside the pen")
          (is (= 2 (count-of s "lead")) "both leads are back")
          (is (= 4 (count-of s "wheat")) "no food is used")
          (is (empty? (events-of s :herd.gave-up))))))))

(deftest one-cow-off-the-axis-six-out-is-led-through-the-gate-in-steps
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[s log] (await (scenario-log {:target 1} {:entities [(cow 1 3 5)] :inventory [{:name "lead" :count 1}]} 200))
              e (done-event s)]
          (is (finished? s))
          (is (= :brought (:reason e)))
          (is (= ["u1"] (:brought e)))
          (is (in-pen? (cow-of s 1)))
          (is (not (gate-open? s)))
          (is (< (self-x s) 10) "the body ends outside")
          (is (= 1 (count-of s "lead")) "the lead is back")
          (is (= [false true false true] (mapv :was-open @log)) "open, shut behind the cow, open, shut from outside")
          (is (= [0 0] (mapv :overlapping (filter :was-open @log))) "never a shut while the cow stood in the gate cell")
          (is (empty? (events-of s :herd.gave-up))))))))

(deftest the-gate-is-held-before-every-open-and-gone-after-every-shut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[s log] (await (scenario-log {:target 1} {:entities [(cow 1 4 3)]} 200))]
          (is (= :brought (:reason (done-event s))))
          (is (= [1 1] (mapv :held (remove :was-open @log))) "an entry before each open")
          (is (empty? (held-entries s)) "dropped after the last shut"))))))

(deftest the-held-entry-names-the-gate-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (submit! (setup (world {:entities [(cow 1 4 3)]})) {:target 1})
              while-open (atom nil)]
          (on-step! s #(when (and (gate-open? s) (nil? @while-open)) (reset! while-open (held-entries s))))
          (await (run-ticks s 100))
          (is (some? @while-open) "the gate stood open at a step")
          (is (= [{:cell [10 64 3]}] (mapv #(select-keys (:data %) [:cell]) (map #(update % :data (fn [d] (update d :cell vec))) @while-open)))))))))

(deftest two-cows-each-get-their-own-open-and-shut-pairs-and-the-first-stays-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (submit! (setup (world {:entities [(cow 1 4 3) (cow 2 5 5)]})) {:target 2})
              log (watch-gate! s)
              stayed (atom true)]
          (loop [n 0 seen #{}]
            (when (< n 500)
              (await (run-ticks s 1))
              (let [now (set (map :id (filter #(and (= "cow" (:name %)) (in-pen? %)) (entities-of s))))]
                (when-not (every? now seen) (reset! stayed false))
                (recur (inc n) now))))
          (is (finished? s))
          (is (= :brought (:reason (done-event s))))
          (is (every? in-pen? [(cow-of s 1) (cow-of s 2)]))
          (is @stayed "a cow that is in is never out again")
          (is (= [false true false true false true false true] (mapv :was-open @log)))
          (is (every? zero? (map :overlapping (filter :was-open @log))))
          (is (not (gate-open? s))))))))

(deftest a-cow-sixteen-blocks-out-is-brought
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 1} {:entities [(cow 1 -6 5)]} 300))]
          (is (= :brought (:reason (done-event s))))
          (is (in-pen? (cow-of s 1)))
          (is (not (gate-open? s))))))))

(deftest one-long-walk-through-the-gate-drags-the-cow-into-a-jam
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup (world {:self {:pos {:x 7 :y 64 :z 3}}
                                 :entities [(cow 1 5 3 {:leashed true :leashedToMe true})]
                                 :states {gate-key {:open true}}}))]
          (core/submit! (:eng s) (list 'jobs.movement.go-to {:pos {:x 14 :y 64 :z 3} :range 0 :doors :leave-open}) {})
          (await (run-ticks s 30))
          (is (>= (self-x s) 14) "the body walked in")
          (is (not (in-pen? (cow-of s 1))) "the cow was dragged straight and stopped at the gate"))))))

(defn ^:async body-trail
  "The distinct consecutive x the body stood on at its steps (waits) over n ticks."
  [s n]
  (let [trail (atom [])]
    (on-step! s #(when-not (= (last @trail) (self-x s)) (swap! trail conj (self-x s))))
    (await (run-ticks s n))
    @trail))

(deftest a-cow-that-never-follows-in-short-steps-is-backed-off-twice-retried-once-and-given-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (submit! (setup (world {:entities [(cow 1 4 3 {:pin true})]})) {:target 1 :timeout-s 600})
              log (watch-gate! s)
              trail (await (body-trail s 900))
              e (done-event s)]
          (is (finished? s))
          (is (= :short (:reason e)))
          (is (= {"u1" :jammed} (:given-up e)))
          (is (= [] (:brought e)))
          (is (= 2 (count (filter #{6} trail))) "out-4 at the approach and again at the retry")
          (is (= 6 (count (filter #{10} trail))) "the gate cell: three tries, two back-steps between them, twice over")
          (is (empty? (on-lead s)) "the cow is let go")
          (is (< (first (:pos (cow-of s 1))) 10) "outside")
          (is (not (gate-open? s)))
          (is (every? zero? (map :overlapping (filter :was-open @log))))
          (is (= 1 (count (events-of s :herd.gave-up)))))))))

(defn ^:async run-until
  "Tick until pred holds on s, at most n ticks."
  [s pred n]
  (loop [i 0]
    (when (and (< i n) (not (pred s)))
      (await (run-ticks s 1))
      (recur (inc i)))))

(deftest a-pen-cow-in-the-gate-cell-is-waited-out-before-the-shut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (submit! (setup (world {:entities [(cow 1 4 3) (cow 8 10.2 3.5)]})) {:target 2})
              log (watch-gate! s)]
          (on-step! s #(when (and (>= (self-x s) 14) (< (first (:pos (cow-of s 8))) 11.5))
                         (set-x! s 8 13.5)))
          (await (run-ticks s 500))
          (is (finished? s))
          (is (= :brought (:reason (done-event s))))
          (is (every? zero? (map :overlapping (filter :was-open @log))) "no shut while a cow overlapped the gate cell")
          (is (not (gate-open? s))))))))

(deftest a-pen-cow-that-never-leaves-the-gate-cell-is-never-shut-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[s log] (await (scenario-log {:target 2 :timeout-s 900} {:entities [(cow 1 4 3) (cow 8 10.2 3.5)]} 1200))]
          (is (finished? s))
          (is (every? zero? (map :overlapping (filter :was-open @log))) "never a shut while it overlapped")
          (is (seq (events-of s :herd.gate-open)) "the gate is left open with a warn, for the pen-gate trigger")
          (is (empty? (held-entries s))))))))

(deftest a-cow-near-the-cell-inside-the-gate-holds-the-opens-up-then-they-go-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 2} {:entities [(cow 1 4 3) (cow 8 12.2 3.5)]} 500))]
          (is (= :brought (:reason (done-event s))))
          (is (= 2 (count (events-of s :herd.crowded-gate))) "once before the way in, once before the way out")
          (is (not (gate-open? s))))))))

(defn restartable
  "A herd run whose engine can be shut down and made again over the same files: the setup map with :make."
  [w args]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake (world w))
        dir (tu/tmp-dir)
        make #(core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir dir :now (fn [] @clock)
                            :events (events/make {:body "Fake" :sinks [sink] :now (fn [] @clock)})})
        s {:eng (make) :p p :seen seen :clock clock :make make}]
    (submit! (clock-on-wait! s) args)))

(deftest a-cut-during-the-steps-goes-on-with-the-animal-and-shuts-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng make] :as s} (restartable {:entities [(cow 1 4 3)]} {:target 1})
              stopped (atom false)]
          (on-step! s #(when (and (not @stopped) (>= (self-x s) 12))
                         (reset! stopped true)
                         (core/shutdown! eng)))
          (await (run-until s (fn [_] @stopped) 200))
          (is @stopped)
          (is (gate-open? s) "cut with the gate open and the body in the pen")
          (let [again (make)]
            (await (run-ticks (assoc s :eng again) 200))
            (is (empty? (:list (core/state again))))
            (is (in-pen? (cow-of s 1)))
            (is (not (gate-open? s)))
            (is (< (self-x s) 10))
            (is (empty? (on-lead s)))))))))

(deftest a-run-ending-with-the-gate-open-and-an-animal-led-lets-it-go-and-shuts-the-gate
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (submit! (setup (world {:entities [(cow 1 4 3 {:pin true})]})) {:target 1 :timeout-s 40})
              opened (atom false)]
          (on-step! s #(when (gate-open? s) (reset! opened true)))
          (await (run-ticks s 400))
          (is @opened "the gate was opened")
          (is (finished? s))
          (is (= :timeout (:reason (done-event s))))
          (is (empty? (on-lead s)) "the animal is let go")
          (is (not (gate-open? s)))
          (is (empty? (held-entries s))))))))

(deftest a-led-animal-is-let-go-when-the-way-to-the-gate-is-blocked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 1} {:entities [(cow 1 4 3)] :unreachable ["9,64,3"]} 100))]
          (is (finished? s))
          (is (= :unreachable (:reason (done-event s))))
          (is (empty? (on-lead s)) "never left tethered to the body")
          (is (= 2 (count-of s "lead")))
          (is (empty? (calls-of s "useOn")))
          (is (= 1 (count (events-of s :herd.gave-up)))))))))

(deftest a-pen-with-animals-keeps-them-in-while-the-gate-stands-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 3}
                                 {:entities [(cow 8 14.2 1.5) (cow 9 13.2 4.5) (cow 1 4 3)]}
                                 300))
              e (done-event s)]
          (is (= :brought (:reason e)))
          (is (every? in-pen? [(cow-of s 8) (cow-of s 9) (cow-of s 1)]))
          (is (not (gate-open? s))))))))

(deftest a-gate-standing-open-at-the-start-is-used-and-shut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 1} {:entities [(cow 1 4 3)] :states {gate-key {:open true :facing "east"}}} 200))]
          (is (= :brought (:reason (done-event s))))
          (is (not (gate-open? s))))))))

(deftest a-gate-set-crosswise-in-the-fence-line-and-standing-open-is-used-and-shut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 1} {:entities [(cow 1 4 3)] :states {gate-key {:open true :facing "north"}}} 300))]
          (is (= :brought (:reason (done-event s))))
          (is (in-pen? (cow-of s 1)))
          (is (< (self-x s) 10) "the body ends outside")
          (is (not (gate-open? s))))))))

(deftest a-gate-set-crosswise-in-the-fence-line-and-shut-is-opened-used-and-shut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 1} {:entities [(cow 1 4 3)] :states {gate-key {:open false :facing "south"}}} 300))]
          (is (= :brought (:reason (done-event s))))
          (is (in-pen? (cow-of s 1)))
          (is (< (self-x s) 10) "the body ends outside")
          (is (not (gate-open? s))))))))

;; ------------------------------------------------------------------ the live cases (ProbeHerdB, card 4f4ab883)

(defn lazy-cow
  "A cow that stays put on the lead while the body is within 4 of it and, once it walks, stops 3.7 behind, as the live
  cows that jammed did (ProbeHerdB: at rest 3.5 to 3.7 from the body, 1.3 to 2 off the axis)."
  [id x z]
  (cow id x z {:follow-at 4.0 :rest-at 3.7}))

(defn ^:async brought-and-shut-up
  "Run args in world w for n ticks, the body at its true position (the fake's hitbox mode: distances to the cows are
  measured as live, not from a cell corner), and check the run ended :brought, the cows in the pen, the body outside,
  the gate shut; label names the case."
  [label args w n]
  (let [s (await (scenario args (assoc w :bodyHitbox true) n))
        e (done-event s)]
    (is (finished? s) label)
    (is (= :brought (:reason e)) label)
    (is (= {} (:given-up e)) label)
    (is (every? in-pen? (filter #(= "cow" (:name %)) (entities-of s))) label)
    (is (< (self-x s) 10) (str label ": the body ends outside"))
    (is (not (gate-open? s)) label)
    (is (= (count (set (:brought e))) (count (:brought e))) (str label ": an animal is booked brought once"))
    s))

(deftest a-lazy-cow-off-the-axis-is-brought-through-a-gate-standing-open-either-way
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [facing ["east" "west"]]
          (await (brought-and-shut-up (str "open, facing " facing) {:target 1}
                                      {:entities [(lazy-cow 1 3 5)] :states {gate-key {:open true :facing facing}}} 400)))))))

(deftest a-lazy-cow-is-brought-through-a-shut-gate
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (brought-and-shut-up "shut" {:target 1} {:entities [(lazy-cow 1 3 5)]} 400))))))

(deftest a-lazy-cow-is-brought-through-a-crosswise-gate
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label state] [["crosswise, open" {:open true :facing "north"}]
                               ["crosswise, shut" {:open false :facing "south"}]]]
          (await (brought-and-shut-up label {:target 1} {:entities [(lazy-cow 1 3 5)] :states {gate-key state}} 400)))))))

(deftest two-lazy-cows-are-both-brought
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (brought-and-shut-up "two cows" {:target 2} {:entities [(lazy-cow 1 3 5) (lazy-cow 2 4 1)]} 900))))))

(defn far-shut-clicks!
  "Make the fake answer a shut click from 4 or more cells along the axis out of reach, as the live game does for a
  body that stops short of the middle of in-4 (eye to gate centre 4.5 and more). Returns the atom of those refusals."
  [{:keys [p] :as s}]
  (let [refused (atom 0)]
    (.override (.-world p) "useOn"
               (fn [token args impl]
                 (if (and (gate-open? s) (>= (- (self-x s) 10) 4))
                   (do (swap! refused inc)
                       (js/Promise.resolve #js {:status "unreachable" :reason "too-far" :distance 4.53}))
                   (impl token args))))
    refused))

(deftest a-shut-click-out-of-reach-from-in-4-walks-closer-and-shuts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (submit! (setup (world {:entities [(cow 1 4 3)]})) {:target 1})
              refused (far-shut-clicks! s)]
          (await (run-ticks s 400))
          (let [e (done-event s)]
            (is (pos? @refused) "the click from in-4 was refused")
            (is (= :brought (:reason e)))
            (is (in-pen? (cow-of s 1)))
            (is (< (self-x s) 10) "the body ends outside")
            (is (not (gate-open? s)))))))))

(deftest a-gate-that-will-not-shut-from-inside-is-shut-from-outside-the-body-never-ends-in-the-pen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (submit! (setup (world {:entities [(cow 1 4 3)]})) {:target 1})]
          ;; every shut click from a pen cell is refused out of reach, however close the body walks
          (.override (.-world (:p s)) "useOn"
                     (fn [token args impl]
                       (if (and (gate-open? s) (>= (self-x s) 11))
                         (js/Promise.resolve #js {:status "unreachable" :reason "too-far" :distance 4.6})
                         (impl token args))))
          (await (run-ticks s 600))
          (is (finished? s))
          (is (< (self-x s) 10) "the body ends outside")
          (is (not (gate-open? s)) "the gate is shut from outside")
          (is (empty? (on-lead s)))
          (is (in-pen? (cow-of s 1)) "the cow let go inside stays in"))))))

(deftest a-retry-from-out-shuts-the-gate-before-the-walk-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (submit! (setup (world {:entities [(cow 1 4 3 {:pin true})]})) {:target 1 :timeout-s 600})
              opened-at (atom nil)
              longest (atom 0)]
          (loop [n 0]
            (when (< n 900)
              (await (run-ticks s 1))
              (let [now @(:clock s)]
                (if (gate-open? s)
                  (do (when-not @opened-at (reset! opened-at now))
                      (swap! longest max (- now @opened-at)))
                  (reset! opened-at nil)))
              (recur (inc n))))
          (is (finished? s))
          (is (= {"u1" :jammed} (:given-up (done-event s))))
          (is (< @longest 40000) "the gate is never left open through the walk out and the second line-up")
          (is (not (gate-open? s))))))))

(defn body-in-pen-or-gate? [s] (>= (self-x s) 10))

(defn watch-notify!
  "Record the world at every job.notify event (another listed job's round): {:open :led :inside}. Returns the atom."
  [s]
  (let [log (atom [])]
    (add-watch (:seen s) ::notify
               (fn [_ _ before after]
                 (when (and (> (count after) (count before)) (= :job.notify (:kind (peek after))))
                   (swap! log conj {:open (gate-open? s) :led (on-lead s) :inside (body-in-pen-or-gate? s)}))))
    log))

(def unsafe-cases
  "[label world args]: herd runs with another listed job beside it."
  [["one cow" {:entities [(cow 1 4 3)]} {:target 1}]
   ["two cows" {:entities [(cow 1 4 3) (cow 2 6 4)]} {:target 2}]
   ["a lazy cow" {:entities [(lazy-cow 1 3 5)] :bodyHitbox true} {:target 1}]])

(deftest no-other-listed-job-gets-a-round-while-the-gate-is-open-an-animal-led-or-the-body-inside
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label w args] unsafe-cases]
          (let [s (submit! (setup (world w)) args)
                log (watch-notify! s)]
            (core/submit! (:eng s) '(repeat (jobs.debug.notify {:text "between"})) {})
            (await (run-ticks s 400))
            (is (= :brought (:reason (done-event s))) label)
            (is (< 1 (count @log)) (str label ": the other job still gets rounds (round-robin, no hold)"))
            (is (every? #(= {:open false :led [] :inside false} %) @log)
                (str label ": every other round starts with the gate shut, nobody led, the body outside"))))))))

(defn cut-at!
  "On the nth call of the fake act named act, an agent's do-now cuts the herd there: the act rejects with cut, undone."
  [{:keys [p eng]} act n]
  (let [k (atom 0)]
    (.override (.-world p) act
               (fn [token a impl]
                 (if (= n (swap! k inc))
                   (do (core/do-now! eng '(jobs.debug.notify {:text "cut"}))
                       (js/Promise.reject (core/cut-error)))
                   (impl token a))))))

(def cut-cases
  "[label act n what the restart saw]"
  [["the open at out-1: led, outside, gate shut" "useOn" 1 {:led ["u1"] :inside false :gate-open false}]
   ["the shut from in-4: led, inside, gate open" "useOn" 2 {:led ["u1"] :inside true :gate-open true}]
   ["the unequip after the release: inside, nobody led" "unequip" 1 {:led [] :inside true :gate-open false}]])

(deftest a-cut-in-the-unsafe-stretch-restarts-from-what-the-world-shows
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label act n saw] cut-cases]
          (let [s (submit! (setup (world {:entities [(cow 1 4 3)]})) {:target 1})
                _ (cut-at! s act n)
                _ (await (run-ticks s 300))
                restarts (events-of s :herd.restarted)
                kinds (mapv :kind @(:seen s))]
            (is (= :brought (:reason (done-event s))) label)
            (is (= [saw] (mapv #(select-keys % [:led :inside :gate-open]) restarts)) (str label ": one restart"))
            (is (< (.indexOf kinds :job.notify) (.indexOf kinds :herd.restarted)) (str label ": the cutting job ran first"))
            (is (in-pen? (cow-of s 1)) label)
            (is (not (gate-open? s)) label)
            (is (< (self-x s) 10) (str label ": the body ends outside"))
            (is (empty? (on-lead s)) label)
            (is (empty? (held-entries s)) label)))))))

(deftest a-herd-not-started-yet-does-not-jump-the-queue
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup (world {:entities [(cow 1 4 3)]}))]
          (core/submit! (:eng s) '(jobs.debug.notify {:text "first"}) {})
          (submit! s {:target 1})
          (await (run-ticks s 400))
          (let [kinds (mapv :kind @(:seen s))]
            (is (< (.indexOf kinds :job.notify) (.indexOf kinds :herd.done)))))))))

(deftest a-full-pen-declines-without-acting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 2} {:entities [(cow 8 12 2) (cow 9 13 3) (cow 1 4 3)]} 5))]
          (is (nil? (done-event s)))
          (is (empty? (calls-of s "moveTo")))
          (is (empty? (calls-of s "interact"))))))))

(deftest babies-are-not-fetched
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 1} {:entities [(cow 1 3 3 {:baby true}) (cow 2 6 3)]} 200))]
          (is (= :brought (:reason (done-event s))))
          (is (= ["u2"] (:brought (done-event s))))
          (is (not (in-pen? (cow-of s 1)))))))))

(deftest fewer-than-wanted-ends-short-with-one-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 3} {:inventory (leads 3) :entities [(cow 1 4 3)]} 200))
              e (done-event s)]
          (is (finished? s))
          (is (= :short (:reason e)))
          (is (= 1 (:inside e)))
          (is (= ["u1"] (:brought e)))
          (is (not (gate-open? s)))
          (is (= 1 (count (events-of s :herd.gave-up)))))))))

(deftest no-food-carried-still-brings-the-animal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 1} {:inventory [{:name "lead" :count 2}] :entities [(cow 1 4 3)]} 200))]
          (is (= :brought (:reason (done-event s))))
          (is (in-pen? (cow-of s 1))))))))

(deftest gives-up-before-touching-the-gate
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label w reason]
                [["no lead" {:inventory [wheat] :entities [(cow 1 4 3)]} :no-lead]
                 ["no cow outside" {:entities [(cow 9 13 3)]} :none]
                 ["a gap in the fence" {:entities [(cow 1 4 3)] :blocks {"16,64,3" "air"}} :leaky]
                 ["no gate" {:entities [(cow 1 4 3)] :blocks {gate-key "oak_fence"}} :no-gate]
                 ["only a corner gate" {:entities [(cow 1 4 3)] :blocks {gate-key "oak_fence" "10,64,0" "oak_fence_gate"}} :no-gate]]]
          (let [s (await (scenario {:target 2} w 20))]
            (is (finished? s) label)
            (is (= reason (:reason (done-event s))) label)
            (is (empty? (calls-of s "useOn")) label)
            (is (empty? (on-lead s)) label)
            (is (= 1 (count (events-of s :herd.gave-up))) label)))))))

(deftest a-gate-that-will-not-open-is-given-up-and-the-animals-let-go
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 1} {:entities [(cow 1 4 3)] :states {gate-key {:open false :locked true}}} 100))]
          (is (finished? s))
          (is (= :gate-stuck (:reason (done-event s))))
          (is (empty? (on-lead s)))
          (is (not (gate-open? s)))
          (is (empty? (held-entries s)))
          (is (= 1 (count (events-of s :herd.gave-up)))))))))

(deftest a-gate-that-will-not-shut-lets-the-animal-go-retries-twice-and-warns-once-with-the-position
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (submit! (setup (world {:entities [(cow 1 4 3)]})) {:target 1})
              shut-clicks (atom 0)]
          ;; the gate opens as usual and then ignores every click: a shut never takes
          (.override (.-world (:p s)) "useOn"
                     (fn [token args impl]
                       (if (gate-open? s)
                         (do (swap! shut-clicks inc)
                             (js/Promise.resolve #js {:status "unchanged" :before #js {:name "oak_fence_gate" :properties #js {:open true}}
                                                      :after #js {:name "oak_fence_gate" :properties #js {:open true}}}))
                         (impl token args))))
          (await (run-ticks s 600))
          (let [warns (events-of s :herd.gate-open)]
            (is (finished? s))
            (is (empty? (on-lead s)) "the animal is unleashed before the shut is tried again")
            (is (= 1 (count warns)))
            (is (= gate (:gate (first warns))))
            (is (= "the gate at 10 64 3 could not be shut and stays open" (:text (first warns))))
            (is (>= @shut-clicks 3) "the shut was tried again after each failure")))))))

(deftest the-escape-census-counts-the-animals-gone-from-the-pen
  (doseq [[before after escaped] [[3 3 0] [3 2 1] [2 0 2] [1 2 0]]]
    (is (= escaped (herd/escaped-count before after)) (str before " -> " after))))

(deftest a-cow-that-walks-out-through-the-open-gate-during-the-exit-is-counted-escaped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (submit! (setup (world {:entities [(cow 1 4 3)]})) {:target 1})
              pushed (atom false)]
          ;; the first gate click after the hand is emptied (the exit's open) lets the cow out
          (.override (.-world (:p s)) "useOn"
                     (fn [token args impl]
                       (when (and (seq (calls-of s "unequip")) (not @pushed) (not (gate-open? s)))
                         (reset! pushed true)
                         (set-x! s 1 8))
                       (impl token args)))
          (await (run-ticks s 400))
          (let [escaped (first (events-of s :herd.escaped))]
            (is @pushed "the cow was moved out during the exit")
            (is (= 1 (:escaped escaped)))
            (is (= 1 (:before escaped)))
            (is (= 0 (:after escaped)))
            (is (= 1 (:escaped (done-event s))))))))))

(deftest a-lead-that-keeps-breaking-is-given-up-after-one-retry
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 1} {:entities [(cow 1 4 3 {:snaps true})]} 200))
              e (done-event s)]
          (is (finished? s))
          (is (= :lost (:reason e)))
          (is (= {"u1" :lead-broke} (:given-up e)))
          (is (= 2 (count (filter #(= "lead" (.. % -args -item)) (calls-of s "interact")))) "leashed twice, not more")
          (is (empty? (calls-of s "useOn")) "the gate is never opened")
          (is (= 1 (count (events-of s :herd.gave-up)))))))))

(deftest leads-dropped-mid-way-are-picked-up-and-the-animals-leashed-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (submit! (setup (world {:entities [(cow 1 4 3)]})) {:target 1})
              dropped (atom false)]
          ;; what a log-out does, at the first step with the cow on the lead: the lead comes off and lies on the ground
          (on-step! s #(when (and (not @dropped) (= 1 (count (on-lead s))))
                         (reset! dropped true)
                         (doseq [c (filter (fn [e] (true? (:leashed-to-me e))) (entities-of s))]
                           (update-entity! s (:id c) assoc :leashed false :leashed-to-me false)
                           (fake/add-entity! (:p s) {:id (+ 100 (:id c)) :name "item" :kind "item" :pos (:pos c)
                                                     :item {:name "lead" :count 1}}))))
          (await (run-ticks s 200))
          (is @dropped "on the lead before the leads came off")
          (is (finished? s))
          (is (= :brought (:reason (done-event s))))
          (is (in-pen? (cow-of s 1)))
          (is (not (gate-open? s)))
          (is (= 2 (count-of s "lead"))))))))

(def pen-cells
  (set (for [x (range 11 16) z (range 1 6)] [x 64 z])))

;; ------------------------------------------------------------------ the pure geometry

(def g [10 64 3])

(def facings
  "[label gate-cell in-cell out-cell]: the four directions of the axis out -> gate -> in."
  [["east" [10 64 3] [11 64 3] [9 64 3]]
   ["west" [10 64 3] [9 64 3] [11 64 3]]
   ["south" [10 64 3] [10 64 4] [10 64 2]]
   ["north" [10 64 3] [10 64 2] [10 64 4]]])

(deftest axis-cells-run-from-out-4-through-the-gate-to-in-depth
  (doseq [[label gc in out expected-x expected-z]
          [["east" g [11 64 3] [9 64 3] [6 7 8 9 10 11 12 13 14 15 16] (repeat 3)]
           ["west" g [9 64 3] [11 64 3] [14 13 12 11 10 9 8 7 6 5 4] (repeat 3)]
           ["south" g [10 64 4] [10 64 2] (repeat 10) [-1 0 1 2 3 4 5 6 7 8 9]]
           ["north" g [10 64 2] [10 64 4] (repeat 10) [7 6 5 4 3 2 1 0 -1 -2 -3]]]]
    (is (= (mapv (fn [x z] [x 64 z]) expected-x expected-z) (herd/axis-cells gc in out 6)) label)))

(deftest axis-cells-use-at-most-six-pen-cells-and-no-more-than-the-depth
  (is (= 11 (count (herd/axis-cells g [11 64 3] [9 64 3] 9))) "9 deep: capped at 6 -> 4 + 1 + 6")
  (is (= 8 (count (herd/axis-cells g [11 64 3] [9 64 3] 3))) "3 deep: 4 + 1 + 3"))

(defn rect [x0 x1 z0 z1] (set (for [x (range x0 (inc x1)) z (range z0 (inc z1))] [x 64 z])))

(deftest depth-counts-the-pen-cells-in-a-line-from-in-1
  (doseq [[label inside expected]
          [["5x5" (rect 11 15 1 5) 5]
           ["9x9" (rect 11 19 -1 7) 9]
           ["3x3" (rect 11 13 2 4) 3]
           ["a hole in the line" (disj (rect 11 15 1 5) [13 64 3]) 2]
           ["no cell beside the gate" (rect 12 15 1 5) 0]]]
    (is (= expected (herd/depth inside g [11 64 3])) label)))

(deftest depth-follows-the-facing
  (is (= 5 (herd/depth (rect 5 9 1 5) g [9 64 3])) "pen west of the gate")
  (is (= 4 (herd/depth (rect 8 12 4 7) g [10 64 4])) "pen south of the gate"))

(deftest an-entity-overlaps-a-cell-within-half-width-and-a-margin-on-both-axes
  (doseq [[label mob dx dz expected]
          [["0.5 off" "cow" 0.5 0.0 true]
           ["0.9 off" "cow" 0.9 0.0 true]
           ["0.95 off" "cow" 0.95 0.0 true]
           ["1.1 off" "cow" 1.1 0.0 false]
           ["off on the other axis" "cow" 0.0 1.1 false]
           ["diagonal inside" "cow" 0.9 0.9 true]
           ["sheep like a cow" "sheep" 1.1 0.0 false]
           ["chicken 0.75 off: inside 0.2 + 0.6" "chicken" 0.75 0.0 true]
           ["chicken 0.85 off" "chicken" 0.85 0.0 false]]]
    (is (= expected (herd/overlaps-cell? {:x (+ 10.5 dx) :y 64 :z (+ 3.5 dz)} (herd/half-width mob) g)) label)))

(deftest half-widths-by-mob
  (doseq [[mob expected] [["cow" 0.45] ["mooshroom" 0.45] ["sheep" 0.45] ["goat" 0.45] ["pig" 0.45] ["chicken" 0.2] ["llama" 0.45]]]
    (is (= expected (herd/half-width mob)) mob)))

(deftest settle-step-reads-the-distance-and-the-movement
  (doseq [[label dist moved waited expected]
          [["close and still" 3.3 0.0 0 :settled]
           ["a live cow's rest, off the axis" 3.7 0.0 0 :settled]
           ["close, at the limit" 4.3 0.1 0 :settled]
           ["close but still moving" 3.0 0.25 0 :wait]
           ["far, not long" 4.4 0.0 5999 :wait]
           ["far, long enough" 4.4 0.0 6000 :pinned]
           ["far and moving, long enough" 5.0 1.0 9000 :pinned]]]
    (is (= expected (herd/settle-step dist moved waited)) label)))

(deftest step-in-step-backs-off-twice-retries-from-out-once-then-gives-up
  (doseq [[label index backs retried expected]
          [["first pin" 3 0 false :back]
           ["second pin" 2 1 false :back]
           ["two backs made" 1 2 false :retry-from-out]
           ["two backs made, retried" 1 2 true :give-up]]]
    (is (= expected (herd/step-in-step index backs retried)) label)))

(deftest the-let-go-cell-is-the-pen-cell-farthest-from-the-gate-off-the-axis-when-tied
  (is (= [15 64 1] (herd/let-go-cell (rect 11 15 1 5) g [11 64 3])) "corners are far, the axis end is not farther: a corner")
  (is (= [15 64 3] (herd/let-go-cell (rect 11 15 3 3) g [11 64 3])) "a one-wide pen: the far end of the axis")
  (is (= [16 64 3] (herd/let-go-cell (conj (rect 11 15 1 5) [16 64 3]) g [11 64 3])) "farther on the axis than any off it"))

;; ------------------------------------------------------------------ the deep release and the retries

(deftest the-exit-opens-passes-and-shuts-the-gate-in-one-walk-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (submit! (setup (world {:entities [(cow 1 4 3)]})) {:target 1})]
          (loop [n 0]
            (when (< n 400)
              (await (run-ticks s 1))
              (recur (inc n))))
          (let [acts (filterv #(and (= :action (:source %)) (= :started (:kind %))) @(:seen s))
                after (rest (drop-while #(not= "unequip" (:name %)) acts))
                clicks (filterv #(= "useOn" (:name %)) after)
                unequip (first (filter #(= "unequip" (:name %)) acts))]
            (is (= :brought (:reason (done-event s))))
            (is (= 2 (count clicks)) "one open and one shut after the hand is emptied")
            (is (= #{[(:job unequip) (:round (first clicks))]} (set (map (juxt :job :round) clicks)))
                "the open, the pass and the shut are one round of the herd job itself, not toggle children")
            (is (not (gate-open? s)))
            (is (< (self-x s) 10) "the body ends outside")
            (is (zero? (:escaped (done-event s))))
            (is (in-pen? (cow-of s 1)))))))))

(deftest outside-gate-is-the-body-past-the-gate-cell-on-the-out-side
  (doseq [[pos out?] [[{:x 9.5 :z 3.5} true] [{:x 9.9 :z 3.5} true] [{:x 10.5 :z 3.5} false] [{:x 11.5 :z 3.5} false] [{:x 9.5 :z 5.5} true]]]
    (is (= out? (herd/outside-gate? [10 64 3] [11 64 3] pos)) (str pos))))

(deftest a-shut-with-the-body-on-the-pen-side-opens-the-gate-again-twice-at-most
  (doseq [[outside? reopens step] [[true 0 :census] [true 2 :census] [false 0 :reopen] [false 1 :reopen] [false 2 :stuck]]]
    (is (= step (herd/shut-side-step outside? reopens)) (str outside? " " reopens))))

(deftest a-pen-cow-on-the-gate-cell-at-the-exit-never-leaves-the-body-inside-a-shut-pen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (submit! (setup (world {:entities [(cow 1 4 3) (cow 8 14.5 5.5)]})) {:target 2})
              placed (atom false)]
          (loop [n 0]
            (when (< n 900)
              (await (run-ticks s 1))
              (let [pen-cow (cow-of s 8)]
                (when (and (seq (calls-of s "unequip")) (not @placed))
                  (reset! placed true)
                  (update-entity! s 8 assoc :pos [10.6 64 3.5]))
                (when (and @placed (gate-open? s) (>= (self-x s) 11) (< (first (:pos pen-cow)) 11.5))
                  (set-x! s 8 13.5)))
              (recur (inc n))))
          (is (finished? s))
          (is (< (self-x s) 10) "the body is outside")
          (is (not (gate-open? s)) "the gate is shut"))))))

(deftest the-hand-is-emptied-once-per-animal-between-the-deep-release-and-the-exit-toggles
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 1} {:entities [(cow 1 4 3)]} 400))]
          (is (= :brought (:reason (done-event s))))
          (is (= 1 (count (calls-of s "unequip"))))
          (is (= ["useOn" "useOn" "unequip" "useOn" "useOn"]
                 (->> (.-calls (.-world (:p s)))
                      (map #(.-name %))
                      (filter #{"useOn" "unequip"})
                      vec))
              "open and shut for the trip, then the hand is emptied, then open and shut to leave"))))))

(deftest an-animal-fetched-twice-and-still-outside-is-never-fetched-again
  (is (= ["a" "b"] (herd/tired-keys {"a" 2 "b" 3 "c" 1} #{"c"})))
  (is (= [] (herd/tired-keys {"a" 2 "b" 3} #{"a" "b"})) "in the pen is not skipped by tries")
  (is (= [] (herd/tired-keys {} #{}))))

;; ------------------------------------------------------------------ the survey's new declines

(def box3 {:min {:x 11 :y 64 :z 1} :max {:x 13 :y 64 :z 3}})
(def ring3 (into {} (for [x (range 10 15) z (range 0 5) :when (or (#{10 14} x) (#{0 4} z))] [(str x ",64," z) "oak_fence"])))

(defn ^:async scenario3
  "A 3x3 pen (box3, gate at 10,64,2) with one cow outside; blocks merged over."
  [blocks n]
  (let [w (world {:entities [(cow 1 4 2)] :self {:pos {:x 2 :y 64 :z 2}}})
        s (submit! (setup (assoc w :self {:pos {:x 2 :y 64 :z 2}}
                                   :blocks (merge ground ring3 {"10,64,2" "oak_fence_gate"} blocks)
                                   :states {"10,64,2" {:open false}}))
                   {:box box3})]
    (await (run-ticks s n))
    s))

(deftest a-pen-less-than-five-deep-along-the-gate-line-is-too-shallow
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario3 {} 20))
              e (done-event s)]
          (is (finished? s))
          (is (= :too-shallow (:reason e)))
          (is (empty? (calls-of s "useOn")))
          (is (empty? (on-lead s)))
          (is (= 1 (count (events-of s :herd.gave-up)))))))))

(deftest an-approach-blocked-three-cells-out-is-no-gate-with-no-approach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 2} {:entities [(cow 1 4 3)] :blocks {"7,64,3" "stone"}} 20))
              e (done-event s)]
          (is (finished? s))
          (is (= :no-gate (:reason e)))
          (is (= :no-approach (:why e)))
          (is (empty? (calls-of s "useOn")))
          (is (= 1 (count (events-of s :herd.gave-up)))))))))

(deftest no-food-is-not-a-reason-to-decline
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 1} {:inventory [{:name "lead" :count 2}] :entities [(cow 1 4 3)]} 5))]
          (is (not= :no-food (:reason (done-event s)))))))))

;; ------------------------------------------------------- looking round while herding (card 9970c377)

(defn watched [{:keys [eng]}] (mem/entries (mem/view (:store eng)) :watched))

(deftest herding-in-the-dark-looks-round-and-in-the-light-does-not
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w {:entities [(cow 1 4 3)]}
              seeing (fn [light] (submit! (clock-on-wait! (h/setup-seeing (world w) light)) {:target 1}))
              lit (seeing nil)
              dk (seeing [0 0])
              _ (await (run-ticks lit 400))
              _ (await (run-ticks dk 400))]
          (is (empty? (watched lit)))
          (is (seq (watched dk))))))))

(deftest a-body-standing-in-the-open-gate-steps-out-to-shut-it-and-is-never-parked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 1}
                                 {:entities [(cow 1 4 3)] :self {:pos {:x 10.5 :y 64 :z 3.5}}
                                  :states {gate-key {:open true :facing "east"}}} 300))]
          (is (some? (done-event s)) (str "the run ends, it is not parked on the toggle's :standing-in wait: " (pr-str (first (events-of s :waiting)))))
          (is (not (gate-open? s)))
          (is (< (self-x s) 10) "the body ends outside"))))))
