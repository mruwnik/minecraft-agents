(ns engine.recover-test
  "jobs.survival.recover and the health-low trigger against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(def t0 1000000)

(def minute (* 60 1000))

(defn setup [world]
  (let [clock (atom t0)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock}))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))

(defn set-self! [p k v] (aset (.. p -world -state -self) k v))

(defn hurt-entries [eng] (mem/entries (mem/view (:store eng)) :hurt))

(defn know-place! [eng kind pos] (mem/write! (:store eng) kind {:pos pos} mem/place-policy))

(def zombie {:id 7 :name "zombie" :kind "hostile" :pos {:x 12 :y 64 :z 0}})

(defn ^:async runs?
  "Does a freshly submitted recover job pass its check, given a body at
  health with a :hurt entry hurt-ms-ago (nil for none)?"
  [health hurt-ms-ago]
  (let [{:keys [eng clock]} (setup {:self {:health health}})]
    (when hurt-ms-ago
      (mem/write! (:store eng) :hurt {:health 5})
      (swap! clock + hurt-ms-ago))
    (core/submit! eng '(jobs.survival.recover) {})
    (let [r (core/tick! eng)]
      (when r (await r))
      (some? r))))

(deftest check-reads-both-thresholds-with-hysteresis
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (true? (await (runs? 6 nil))) "below :health runs")
        (is (false? (await (runs? 7 nil))) "at :health with no hurt entry declines")
        (is (false? (await (runs? 12 nil))) "above :health with no hurt entry declines")
        (is (true? (await (runs? 12 minute))) "mid-health keeps going while a recent :hurt entry exists")
        (is (true? (await (runs? 15 minute))) "just under :healed keeps going")
        (is (false? (await (runs? 16 minute))) "healed stops")
        (is (false? (await (runs? 12 (* 10 minute)))) "an old :hurt entry no longer holds the job")))))

(deftest check-takes-the-thresholds-as-args
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:self {:health 10}})]
          (core/submit! eng '(jobs.survival.recover {:health 11}) {})
          (is (some? (core/tick! eng)) "a raised :health makes 10 low"))))))

(deftest a-hostile-in-sight-is-fled-from
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:health 5 :food 10} :entities [zombie]
                                      :inventory [{:name "bread" :count 1}]})]
          (know-place! eng :bed {:x 30 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.recover) {})
          (await (core/tick! eng))
          (is (< (.-x (.-pos (.self p))) 0) "moved away from the zombie, not toward the bed")
          (is (= ["bread"] (mapv #(.. % -args -item) (calls p "eat")))
              "the zombie is 12 away, beyond retreat's eat gap: it eats once on the run")
          (is (= 1 (count (:list (core/state eng)))) "still recovering"))))))

(deftest with-no-hostile-it-walks-to-the-bed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:health 5}})]
          (know-place! eng :home {:x -20 :y 64 :z 0})
          (know-place! eng :bed {:x 30 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.recover) {})
          (await (core/tick! eng))
          (is (= {:x 30 :y 64 :z 0} (core/self-pos p))))))))

(deftest without-a-bed-it-walks-home
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:health 5}})]
          (know-place! eng :home {:x -20 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.recover) {})
          (await (core/tick! eng))
          (is (= {:x -20 :y 64 :z 0} (core/self-pos p))))))))

(deftest without-a-place-it-stays-put
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:health 5}})]
          (core/submit! eng '(jobs.survival.recover) {})
          (await (core/tick! eng))
          (is (= [] (calls p "moveTo"))))))))

(deftest at-safety-it-eats-when-hungry-and-carrying-food
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:health 5 :food 10} :inventory [{:name "bread" :count 1}]})]
          (core/submit! eng '(jobs.survival.recover) {})
          (await (core/tick! eng))
          (is (= 15 (.-food (.self p))))
          (is (= 1 (count (calls p "eat")))))))))

(deftest it-does-not-call-eat-when-full-or-without-food
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [full (setup {:self {:health 5 :food 20} :inventory [{:name "bread" :count 1}]})
              hungry (setup {:self {:health 5 :food 10} :inventory [{:name "oak_log" :count 3}]})]
          (core/submit! (:eng full) '(jobs.survival.recover) {})
          (await (core/tick! (:eng full)))
          (core/submit! (:eng hungry) '(jobs.survival.recover) {})
          (await (core/tick! (:eng hungry)))
          (is (= [] (calls (:p full) "eat")))
          (is (= [] (calls (:p hungry) "eat")))
          (is (= [] (:list (core/state (:eng hungry))))
              "food 10 is below the 18 regeneration needs and nothing is carried to eat: it gives up"))))))

(deftest it-gives-up-when-health-cannot-regenerate
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:health 5 :food 17}})]
          (core/submit! eng '(jobs.survival.recover) {})
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))) "17 food does not regenerate health")
          (is (= 1 (count (filter #(= :cannot_heal (:kind %)) @seen)))))
        (let [{:keys [eng]} (setup {:self {:health 5 :food 18}})]
          (core/submit! eng '(jobs.survival.recover) {})
          (await (core/tick! eng))
          (is (= 1 (count (:list (core/state eng)))) "18 food regenerates: it waits"))))))

(deftest it-waits-until-healed-then-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:health 5}})]
          (core/submit! eng '(jobs.survival.recover) {})
          (await (core/tick! eng))
          (set-self! p "health" 12)
          (await (core/tick! eng))
          (is (= 1 (count (:list (core/state eng)))) "12 of 20 is not healed")
          (set-self! p "health" 16)
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng)))))))))

(deftest the-hurt-entry-is-written-once-per-spell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:self {:health 5} :entities [zombie]})]
          (core/submit! eng '(jobs.survival.recover) {})
          (await (core/tick! eng))
          (await (core/tick! eng))
          (await (core/tick! eng))
          (let [es (hurt-entries eng)]
            (is (= 1 (count es)))
            (is (= 5 (:health (:data (first es)))))
            (is (= {:x 0 :y 64 :z 0} (:pos (:data (first es)))))
            (is (= "zombie" (:name (:hostile (:data (first es)))))))
          (is (= {:cap 20 :ttl (* 60 minute)} (mem/policy (mem/view (:store eng)) :hurt))))))))

(deftest a-new-spell-writes-a-new-entry
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:health 5}})]
          (core/submit! eng '(jobs.survival.recover) {})
          (await (core/tick! eng))
          (set-self! p "health" 18)
          (await (core/tick! eng))
          (set-self! p "health" 4)
          (core/submit! eng '(jobs.survival.recover) {})
          (await (core/tick! eng))
          (is (= [5 4] (mapv (comp :health :data) (hurt-entries eng)))))))))

;; ----------------------------------------------------------------- the trigger

(defn holds? [health args]
  ((:when triggers/health-low) (tu/fake {:self {:health health}}) {:data mem/empty-data :now t0} args))

(deftest health-low-is-below-health-with-default-seven
  (is (true? (holds? 6 {})))
  (is (false? (holds? 7 {})))
  (is (false? (holds? 7 {:health 7})))
  (is (true? (holds? 9 {:health 10}))))

(deftest health-low-runs-recover-by-default
  (is (= '(jobs.survival.recover) (:job triggers/health-low))))

(deftest it-idles-with-a-wait-not-empty-rounds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:health 5}})]
          (core/submit! eng '(jobs.survival.recover) {})
          (await (core/tick! eng))
          (is (= [2000] (mapv #(.. % -args -ms) (calls p "wait")))))))))
