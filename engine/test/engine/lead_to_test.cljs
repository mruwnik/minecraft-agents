(ns engine.lead-to-test
  "jobs.animals.lead-to against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [jobs.lib.walk :as walk]
            [jobs.animals.lead-to :as lead-to]
            [engine.fetch-test :as fetch-test]
            [engine.hostile-test :as h]
            [engine.memory :as mem]
            [engine.test-util :as tu]))

(defn cow [id x & [more]]
  (merge {:id id :uuid (str "u" id) :name "cow" :kind "passive" :pos {:x x :y 64 :z 0}} more))

(def lead [{:name "lead" :count 1}])
(def fence {"31,64,0" "oak_fence"})
(def goal {:x 30 :y 64 :z 0})

(def call-ms
  "Fake time each primitive call takes: waits inside one call end by the clock."
  50)

(defn setup
  ([world] (h/setup world call-ms))
  ([world _ store] (h/setup world call-ms store)))

(defn ^:async run-ticks
  [{:keys [eng clock]} n step]
  (dotimes [_ n]
    (swap! clock + step)
    (await (core/tick! eng))))

(defn ^:async submit
  "Submit (jobs.animals.lead-to args) in the setup s; run n ticks 700 ms apart."
  [s args n]
  (core/submit! (:eng s) (list 'jobs.animals.lead-to (merge {:mob "cow" :pos goal} args)) {})
  (await (run-ticks s n 700))
  s)

(defn ^:async scenario [args world n]
  (await (submit (setup (merge {:floor tu/walk-floor} world)) args n)))

(defn done-event [{:keys [seen]}] (first (filter #(= :lead-to.done (:kind %)) @seen)))
(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn calls-of [{:keys [p]} name] (h/calls p name))
(defn count-of [{:keys [p]} item]
  (reduce + (map :count (filter #(= item (:name %)) (js->clj (.-inventory (.self p)) :keywordize-keys true)))))
(defn cow-of [{:keys [p]} id] (first (filter #(= id (:id %)) (fake/entities p))))

(deftest leads-the-cow-to-the-spot-and-lets-it-go
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory lead :entities [(cow 1 3)]} 12))
              c (cow-of s 1)]
          (is (finished? s))
          (is (= :unleashed (:reason (done-event s))))
          (is (= "u1" (:animal (done-event s))))
          (is (<= (js/Math.abs (- 30 (first (:pos c)))) 4) "the cow stands at the spot: the body stops within 2 and the cow is led 2 behind it")
          (is (not (true? (:leashed c))))
          (is (= 1 (count-of s "lead")) "the lead is back in the inventory")
          (is (empty? (events-of s :lead-to.gave-up))))))))

;; one call is the whole attempt
(deftest one-call-leads-the-cow-and-lets-it-go
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory lead :entities [(cow 1 3)]} 1))]
          (is (finished? s))
          (is (= :unleashed (:reason (done-event s)))))))))

(deftest one-call-gathers-a-trailing-cow
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory lead :entities [(cow 1 3 {:trail 6})]} 1))]
          (is (finished? s))
          (is (= :unleashed (:reason (done-event s)))))))))

(deftest one-call-ties-the-cow
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:fence {:x 31 :y 64 :z 0}} {:inventory lead :blocks fence :entities [(cow 1 3)]} 1))]
          (is (finished? s))
          (is (= :tied (:reason (done-event s)))))))))

(deftest one-call-gives-up-a-lagging-cow
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory lead :entities [(cow 1 3 {:trail 9})]} 1))]
          (is (finished? s))
          (is (= :lagging (:reason (done-event s)))))))))

(deftest gathers-a-trailing-cow-to-the-spot-before-letting-it-go
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory lead :entities [(cow 1 3 {:trail 6})]} 20))
              c (cow-of s 1)]
          (is (finished? s))
          (is (= :unleashed (:reason (done-event s))))
          (is (<= (js/Math.abs (- 30 (first (:pos c)))) 4) "the body walked on until the cow was within the gather radius")
          (is (< 1 (count (tu/walked-to (:eng s)))) "at least one pull after the walk to the spot")
          (is (empty? (events-of s :lead-to.gave-up))))))))

(deftest pulls-a-trailing-cow-by-walking-past-the-spot
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory lead :entities [(cow 1 3 {:trail 6})]} 20))]
          (is (some #(< 30 (:x %)) (tu/walked-to (:eng s))) "a walk after the one to the spot goes on past it, so the lead drags the cow to it"))))))

(deftest does-not-pull-a-cow-that-is-already-at-the-spot
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory lead :entities [(cow 1 3 {:trail 1})]} 12))]
          (is (= :unleashed (:reason (done-event s))))
          (is (= goal (last (tu/walked-to (:eng s)))) "the legs end at the spot")
          (is (every? #(<= (:x %) 30) (tu/walked-to (:eng s))) "no pull"))))))

(defn stall-steers-from!
  "Steers from x on only creep 0.5 and time out (counted in steers); earlier ones go through."
  [p x steers]
  (.override (.-world p) "steer"
             (fn [token args impl]
               (if (< (first (:pos (fake/self p))) x)
                 (impl token args)
                 (do (swap! steers inc)
                     (fake/swap-self! p update-in [:pos 0] + 0.5)
                     (js/Promise.resolve #js {:status "timeout" :pose #js {}}))))))

(defn lose-cow-past!
  "After a steer that leaves the body beyond x, the first entity (the cow) is gone."
  [p x]
  (.override (.-world p) "steer"
             (fn [token args impl]
               (let [r (impl token args)]
                 (when (< x (first (:pos (fake/self p))))
                   (swap! (fake/state p) update :entities subvec 1))
                 r))))

(defn slow-clock-from!
  "Each steer from x on takes 25 s of the clock."
  [p x clock]
  (.override (.-world p) "steer"
             (fn [token args impl]
               (when (<= x (first (:pos (fake/self p)))) (swap! clock + 25000))
               (impl token args))))

(defn ^:async scenario-slow-at-the-end
  "A cow that keeps up until the body is within a leg of the spot, then is trail blocks behind the spot (more is
  :break-at). The body would wait for such a cow on the way; at the end it is the gather phase's business."
  [trail more]
  (let [{:keys [p] :as s} (setup {:floor tu/walk-floor :inventory lead :entities [(cow 1 3 {:trail 5})]})]
    (.override (.-world p) "steer"
               (fn [token args impl]
                 (when (<= 26 (first (:pos (fake/self p))))
                   (swap! (fake/state p) update :entities (fn [es] (mapv #(merge % {:trail trail :pos [(- 30 trail) 64 0]} more) es))))
                 (impl token args)))
    (await (submit s {} 24))))

(deftest reports-whether-the-cow-was-gathered
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label cow-spec] [["at the spot" {}] ["trailing, pulled in" {:trail 5}]]]
          (let [s (await (scenario {} {:inventory lead :entities [(cow 1 3 cow-spec)]} 24))]
            (is (= :unleashed (:reason (done-event s))) label)
            (is (true? (:gathered (done-event s))) label)
            (is (empty? (events-of s :lead-to.gather-short)) label)))
        (let [s (await (scenario-slow-at-the-end 10 nil))]
          (is (= :unleashed (:reason (done-event s))) "too far out to pull")
          (is (false? (:gathered (done-event s))) "too far out to pull")
          (is (= 1 (count (events-of s :lead-to.gather-short))) "too far out to pull"))))))

(deftest a-cow-10-out-is-not-pulled-so-far-that-the-lead-breaks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario-slow-at-the-end 10 {:break-at 12}))
              c (cow-of s 1)
              [short] (events-of s :lead-to.gather-short)]
          (is (= :unleashed (:reason (done-event s))) "no snap: the body did not walk on")
          (is (<= 8 (:distance short)) "the warn names the distance the animal was left at")
          (is (every? #(<= (:x %) 30) (tu/walked-to (:eng s))) "no walk past the spot: no pull was started")
          (is (not (true? (:leashed c)))))))))

(def wall-32
  "A wall at x 32 from z -8 to 8, two high: a body going from 30 to 33 detours round its end at z 9."
  (into {} (for [z (range -8 9) y [64 65]] [(str "32," y "," z) "stone"])))

(deftest a-pull-whose-path-leaves-the-lead-range-is-not-walked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory lead :blocks wall-32 :entities [(cow 1 3 {:trail 6 :breakAt 12})]} 30))
              [short] (events-of s :lead-to.gather-short)]
          (is (= :unleashed (:reason (done-event s))) "the lead did not break")
          (is (false? (:gathered (done-event s))))
          (is (every? #(<= (:x %) 30) (take 1 (tu/walked-to (:eng s)))))
          (is (not-any? #(= 33 (:x %)) (tu/walked-to (:eng s))) "no walk to the pull target behind the wall")
          (is (<= 5 (:distance short)) "the distance is the cow's at the time of giving up"))))))

(deftest stops-after-the-pull-limit-and-says-the-cow-was-not-gathered
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:gather-tries 1 :gather-radius 0.5} {:inventory lead :entities [(cow 1 3 {:trail 5})]} 40))]
          (is (= :unleashed (:reason (done-event s))))
          (is (false? (:gathered (done-event s))))
          (let [xs (mapv :x (tu/walked-to (:eng s)))]
            (is (= [32 33 27] (take-last 3 xs)) "the legs of one pull, then the body goes to the cow to take the lead off")
            (is (= 33 (apply max xs)) "no second pull"))
          (is (= 1 (count (events-of s :lead-to.gather-short)))))))))

(deftest a-pull-that-cannot-arrive-lets-the-cow-go-and-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory lead :entities [(cow 1 3 {:trail 6})] :unreachable ["33,64,0"]} 40))]
          (is (finished? s))
          (is (= :unleashed (:reason (done-event s))))
          (is (false? (:gathered (done-event s))))
          (is (= 1 (count (events-of s :lead-to.gather-short))))
          (is (= [30 32 32 32 22] (take-last 5 (mapv :x (tu/walked-to (:eng s))))) "the walk to the spot, one pull that go-to gives up after its three fruitless rounds, no second pull, then the body goes to the cow to take the lead off"))))))

(deftest a-lead-that-breaks-during-the-pull-is-reported
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory lead :entities [(cow 1 3 {:trail 6 :breakAt 7 :pace 0.5})]} 40))]
          (is (= :lead-broke (:reason (done-event s))))
          (is (false? (:gathered (done-event s)))))))))

(deftest a-pull-that-keeps-falling-short-is-given-up-by-its-go-to-in-one-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; go-to's call is a whole attempt: three walks gaining under a block each end it inside one tick, so the
        ;; pull is no longer carried across ticks for the 20 s limit to cut
        (let [{:keys [p eng] :as s} (setup {:floor tu/walk-floor :inventory lead :entities [(cow 1 3 {:trail 6})]})
              steers (atom 0)
              pull-slot #(get-in (mem/job-mem (mem/view (:store eng)) "j1" []) [:children :pull])]
          (stall-steers-from! p 27 steers)
          (core/submit! eng (list 'jobs.animals.lead-to {:mob "cow" :pos goal}) {})
          (await (run-ticks s 20 700))
          (is (pos? @steers) "a pull was walked")
          (is (nil? (pull-slot)) "its child is dropped")
          (is (finished? s))
          (is (= 1 (count (events-of s :lead-to.gather-short))))
          (is (false? (:gathered (done-event s)))))))))

(deftest a-cow-lost-during-the-pull-is-lost
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (setup {:floor tu/walk-floor :inventory lead :entities [(cow 1 3 {:trail 6})]})]
          (lose-cow-past! p 29.5)
          (await (submit s {} 40))
          (is (finished? s))
          (is (= :lost (:reason (done-event s)))))))))

(deftest ties-the-cow-to-the-named-fence
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:fence {:x 31 :y 64 :z 0}} {:inventory lead :blocks fence :entities [(cow 1 3)]} 12))
              c (cow-of s 1)]
          (is (finished? s))
          (is (= :tied (:reason (done-event s))))
          (is (true? (:leashed c)))
          (is (not (true? (:leashed-to-me c))) "held by the post, not by the body")
          (is (= 0 (count-of s "lead")) "the lead stays on the cow")
          (is (empty? (events-of s :lead-to.gave-up))))))))

(deftest declines-before-touching-anything
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label args world reason]
                [["no cow" {} {:inventory lead :entities []} :none]
                 ["no fence at the named cell" {:fence {:x 31 :y 64 :z 0}} {:inventory lead :entities [(cow 1 3)]} :no-fence]
                 ["a block that is not a fence" {:fence {:x 31 :y 64 :z 0}} {:inventory lead :blocks {"31,64,0" "stone"} :entities [(cow 1 3)]} :no-fence]]]
          (let [s (await (scenario args world 5))]
            (is (finished? s) label)
            (is (= reason (:reason (done-event s))) label)
            (is (empty? (tu/walk-calls (:p s))) label)
            (is (= [:lead-to.gave-up] (mapv :kind (events-of s :lead-to.gave-up))) label)))))))

(deftest a-lead-that-breaks-on-the-way-is-reported-not-succeeded
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory lead :entities [(cow 1 3 {:snaps true})]} 12))
              c (cow-of s 1)]
          (is (finished? s))
          (is (= :lead-broke (:reason (done-event s))))
          (is (= "u1" (:animal (done-event s))))
          (is (not (true? (:leashed c))))
          (is (empty? (calls-of s "useOn")) "no tie was tried")
          (is (= [:lead-to.gave-up] (mapv :kind (events-of s :lead-to.gave-up)))))))))

(deftest a-cow-that-is-gone-after-the-walk-is-lost
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (setup {:floor tu/walk-floor :inventory lead :entities [(cow 1 3)]})]
          (.override (.-world p) "steer"
                     (fn [token args impl]
                       (let [r (impl token args)]
                         (swap! (fake/state p) update :entities empty)
                         r)))
          (await (submit s {} 40))
          (is (finished? s))
          (is (= :lost (:reason (done-event s)))))))))

(deftest an-unreachable-spot-ends-with-the-cow-still-on-the-lead
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory lead :entities [(cow 1 3)] :unreachable ["30,64,0"]} 14))
              c (cow-of s 1)]
          (is (finished? s))
          (is (= :unreachable (:reason (done-event s))))
          (is (true? (:leashed-to-me c)))
          (is (true? (:still-led (done-event s)))))))))

(def animal-at-3 {:x 3 :y 64 :z 0})

(deftest the-reach-check-takes-a-fractional-pull-target-without-throwing
  (let [s (setup {:floor tu/walk-floor :blocks wall-32 :inventory lead})
        c {:primitives (:p s)}]
    (is (boolean? (lead-to/path-leaves-reach? c {:x 33.4 :y 64 :z 0.6} animal-at-3)))))

(deftest the-reach-check-judges-a-fractional-target-by-its-walk
  (let [far (setup {:floor tu/walk-floor :blocks wall-32 :inventory lead})
        near (setup {:floor tu/walk-floor :inventory lead})]
    (is (true? (lead-to/path-leaves-reach? {:primitives (:p far)} {:x 33.4 :y 64 :z 0.6} animal-at-3)) "detour past the wall end")
    (is (false? (lead-to/path-leaves-reach? {:primitives (:p near)} {:x 9.4 :y 64 :z 0.6} animal-at-3)) "open floor")))

(deftest the-reach-check-plans-to-the-floored-cell
  (let [s (setup {:floor tu/walk-floor :inventory lead})
        asked (atom nil)]
    (with-redefs [walk/plan-walk (fn ([_ _ to _ _] (reset! asked to) nil) ([_ _ to _ _ _] (reset! asked to) nil))]
      (lead-to/path-leaves-reach? {:primitives (:p s)} {:x 9794.82 :y 64 :z -3.5} animal-at-3))
    (is (= [9794 64 -4] @asked) "the planner is given whole cells, as go-to gives it")))

(deftest the-reach-check-is-false-when-planning-throws
  (let [s (setup {:floor tu/walk-floor :inventory lead})]
    (with-redefs [walk/plan-walk (fn ([_ _ _ _ _] (throw (js/RangeError. "cannot be converted to a BigInt"))) ([_ _ _ _ _ _] (throw (js/RangeError. "cannot be converted to a BigInt"))))]
      (is (false? (lead-to/path-leaves-reach? {:primitives (:p s)} {:x 33.4 :y 64 :z 0.6} animal-at-3))))))

;; ------------------------------------------------------- looking round while leading (card 9970c377)

(defn watched [{:keys [eng]}] (mem/entries (mem/view (:store eng)) :watched))

(deftest leading-in-the-dark-looks-round-and-in-the-light-does-not
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w {:floor tu/walk-floor :inventory lead :entities [(cow 1 3)]}
              lit (await (submit (h/setup-seeing w nil call-ms) {} 12))
              dark (await (submit (h/setup-seeing w [0 0] call-ms) {} 12))]
          (is (empty? (watched lit)))
          (is (seq (watched dark)))
          (is (= :unleashed (:reason (done-event dark)))))))))

;; ------------------------------------------------------- short go-to legs (card af0984f9)

(deftest the-walk-to-the-spot-is-short-legs-toward-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory lead :entities [(cow 1 3)]} 40))
              legs (tu/walked-to (:eng s))]
          (is (= :unleashed (:reason (done-event s))))
          (is (< 4 (count legs)) "more than a few legs")
          (is (every? #(<= % 5) (map #(- (:x %2) (:x %1)) legs (rest legs))) "each leg is a few blocks long")
          (is (= 30 (:x (last (take-while #(<= (:x %) 30) legs)))) "the last leg ends at the spot"))))))

(deftest a-lead-that-breaks-on-the-first-leg-is-noticed-after-that-leg-not-at-arrival
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory lead :entities [(cow 1 3 {:snaps true})]} 40))
              legs (tu/walked-to (:eng s))]
          (is (= :lead-broke (:reason (done-event s))))
          (is (< (first (:pos (fake/self (:p s)))) 15) "the body did not walk on to the spot")
          (is (every? #(< (:x %) 15) legs)))))))

(deftest a-pull-over-20-s-is-given-up-even-while-each-leg-makes-progress
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng clock] :as s} (setup {:floor tu/walk-floor :inventory lead :entities [(cow 1 3 {:trail 5})]})]
          (slow-clock-from! p 28 clock)
          (core/submit! eng (list 'jobs.animals.lead-to {:mob "cow" :pos goal :gather-radius 0}) {})
          (await (run-ticks s 60 700))
          (is (finished? s))
          (is (= :unleashed (:reason (done-event s))))
          (is (false? (:gathered (done-event s))))
          (is (= 1 (count (events-of s :lead-to.gather-short))))
          (let [pull-legs (filter #(< 30 (:x %)) (tu/walked-to eng))]
            (is (= 1 (count pull-legs)) "the pull was cut after one leg")
            (is (every? #(< (:x %) 34) pull-legs) "that leg is short of the pull's end")))))))

;; ------------------------------------------------------- a led cow out of sight is not lost (card 0dd5dca8)

(defn hide-cow!
  "The cow goes out of sight after the first steer for which hide? holds, and is back in sight at the k-th wait after that."
  [p hide? k]
  (let [saved (atom nil)
        waits (atom 0)]
    (.override (.-world p) "steer"
               (fn [token args impl]
                 (let [r (impl token args)]
                   (when (and (nil? @saved) (hide?))
                     (reset! saved (first (:entities @(fake/state p))))
                     (swap! (fake/state p) update :entities subvec 1))
                   r)))
    (.override (.-world p) "wait"
               (fn [token args impl]
                 (when (and @saved (= k (swap! waits inc)))
                   (swap! (fake/state p) update :entities conj @saved))
                 (impl token args)))))

(defn ^:async hide-cow-for-legs
  "One call; the cow goes out of sight at the first leg's steer and is back in sight k waits later."
  [k]
  (let [{:keys [p] :as s} (setup {:floor tu/walk-floor :inventory lead :entities [(cow 1 3)]})]
    (hide-cow! p (constantly true) k)
    (await (submit s {} 1))))

(deftest a-cow-out-of-sight-for-a-while-is-waited-for-not-lost
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (hide-cow-for-legs 1))]
          (is (= :unleashed (:reason (done-event s)))))))))

(deftest a-cow-out-of-sight-for-two-looks-is-not-lost
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (hide-cow-for-legs 2))]
          (is (= :unleashed (:reason (done-event s)))))))))

(deftest a-cow-out-of-sight-for-good-is-lost-and-the-body-did-not-walk-on-without-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (hide-cow-for-legs 100))]
          (is (= :lost (:reason (done-event s))))
          (is (< (first (:pos (fake/self (:p s)))) 8) "the body waited for the cow instead of walking on"))))))

(deftest a-cow-lagging-far-behind-is-waited-for-then-given-up-as-lagging
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory lead :entities [(cow 1 3 {:trail 9})]} 60))]
          (is (= :lagging (:reason (done-event s))))
          (is (true? (:still-led (done-event s))))
          (is (< (first (:pos (fake/self (:p s)))) 14) "the body stopped when the cow fell 8 or more behind"))))))

(deftest a-spot-in-the-air-is-still-walked-in-legs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:pos {:x 30 :y 70 :z 0}} {:inventory lead :entities [(cow 1 3)]} 40))
              legs (tu/walked-to (:eng s))]
          (is (< 4 (count legs)) "more than a few legs"))))))

(deftest no-animal-in-radius-is-a-stop-that-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:radius 8} {:inventory lead :entities [(cow 1 35)]} 5))
              stopped (first (events-of s :stopped))]
          (is (finished? s))
          (is (= :none (:reason (done-event s))))
          (is (= 1 (count (events-of s :stopped))) "the job ends stopped, not completed")
          (is (re-find #"no cow seen within 8" (:text stopped)))
          (is (not (re-find #":radius" (:text stopped))) "widening the radius does not help a cow that is not seen"))))))

(deftest a-success-is-not-a-stop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory lead :entities [(cow 1 3)]} 12))]
          (is (= :unleashed (:reason (done-event s))))
          (is (empty? (events-of s :stopped))))))))

(deftest a-pull-aims-a-block-past-the-radius-for-a-body-that-stops-short
  (let [t (lead-to/pull-point {:x 24 :z 0} {:x 30 :y 64 :z 0} 3)]
    (is (= 34 (:x t)) "lead length 6 less radius 3, plus the 1 block the walk's range may leave")))

(defn ^:async hide-cow-after-the-walk
  "One call; the cow goes out of sight once the body is at the spot and is back in sight k waits later."
  [k]
  (let [{:keys [p] :as s} (setup {:floor tu/walk-floor :inventory lead :entities [(cow 1 3 {:trail 1})]})]
    (hide-cow! p #(<= 27 (first (:pos (fake/self p)))) k)
    (await (submit s {} 1))))

(deftest a-cow-out-of-sight-for-one-look-after-the-walk-is-not-lost
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (hide-cow-after-the-walk 1))]
          (is (= :unleashed (:reason (done-event s)))))))))

(deftest a-cow-out-of-sight-for-good-after-the-walk-is-lost
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (hide-cow-after-the-walk 1000))]
          (is (= :lost (:reason (done-event s)))))))))

(deftest crafts-a-lead-when-none-is-carried-then-leads-the-cow
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (fetch-test/start {:self {:pos {:x 0 :y 64 :z 0}}
                                   :recipes {"lead" {:count 2 :needs {"string" 5} :table true}}
                                   :inventory [{:name "string" :count 5} {:name "oak_planks" :count 4}]
                                   :entities [(cow 1 3)]}
                                  [])]
          (core/submit! (:eng s) (list 'jobs.animals.lead-to {:mob "cow" :pos goal}) {})
          (await (fetch-test/run-ticks s 60))
          (is (empty? (fetch-test/listed s)))
          (is (= 2 (get (fetch-test/inv s) "lead")) "the crafted pair is carried, the cow let go")
          (is (empty? (fetch-test/events-of s :lead-to.gave-up))))))))

(deftest no-lead-and-no-way-to-get-one-stops-no-lead-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory [] :entities [(cow 1 3)]} 6))
              r (done-event s)
              [gave-up] (events-of s :lead-to.gave-up)]
          (is (finished? s))
          (is (= :no-lead (:reason r)))
          (is (re-find #"no lead carried and none could be got" (:text gave-up)))
          (is (= [:no-lead] (mapv :reason (events-of s :lead-to.gave-up))))
          (is (empty? (tu/walked-to (:eng s))) "the body did not move"))))))

(deftest an-animal-in-anothers-zone-is-not-led-unless-ignored
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[extra refused? declined] [[{} true [:refused]] [{:ignore-zones? true} false []]]]
          (let [s (await (submit (setup {:floor tu/walk-floor :inventory lead :entities [(cow 1 3)]} 0 (h/zone-store 3 "Miles"))
                                 extra 6))]
            (is (= refused? (= :refused (:reason (done-event s)))) (pr-str extra))
            (is (= declined (mapv :reason (events-of s :leash.declined))) (pr-str extra))))))))
