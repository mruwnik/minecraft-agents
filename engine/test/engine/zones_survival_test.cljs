(ns engine.zones-survival-test
  "Survival jobs and zones: they prefer cells the zone rules permit and break another's stuff only as a last resort
  (one <job>.trespass-last-resort warn naming the zone); a missing zone list never blocks them."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.shelter-test :as st]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [engine.unstick-test :as ut]
            [jobs.lib.world-files :as ew]
            [plan.shape :as shape]))

(defn setup [world zones]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake (merge {:offlineScale 0.0001} world))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :backoff false :world (ew/of-data {} {} zones)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn ^:async run-until-empty [eng n]
  (loop [i 0]
    (when (and (< i n) (seq (:list (core/state eng))))
      (await (core/tick! eng))
      (recur (inc i)))))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn arg-pos [call] (js->clj (.-pos (.-args call)) :keywordize-keys true))
(defn trespass [seen kind] (filterv #(= kind (:kind %)) @seen))

(defn zone
  "A box zone [x0 y0 z0]-[x1 y1 z1] owned by owner."
  [owner a b] {:name "vault" :owner owner :min a :max b})

(def whole {:min [-3 58 -3] :max [3 70 3]})
(defn whole-zone [owner] (zone owner (:min whole) (:max whole)))

;; ------------------------------------------------------------------ breathe

(deftest breathe-digs-out-of-a-foreign-zone-as-a-last-resort-and-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[zones warns] [[[(whole-zone "Miles")] [["vault"]]]
                               [[(whole-zone "fake")] []]
                               [nil []]
                               [[] []]]]
          (let [{:keys [eng p seen]} (setup {:blocks {"0,65,0" "stone" "0,66,0" "stone"}} zones)]
            (core/submit! eng '(jobs.survival.breathe {:min-oxygen 12}) {})
            (await (core/tick! eng))
            (is (= [{:x 0 :y 65 :z 0} {:x 0 :y 66 :z 0}] (mapv arg-pos (calls p "dig"))) (pr-str zones))
            (is (= warns (mapv :zones (trespass seen :breathe.trespass-last-resort))) (pr-str zones))))))))

(defn setup-with-plan [world plan]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake (merge {:offlineScale 0.0001} world))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :backoff false :world (ew/of-data {"hut" plan} {} [])
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(def roof-plan {:id "hut" :parts [{:id "roof" :box [[0 65 0] [0 66 0]] :want "stone"}]})

(deftest trespass-warns-of-a-plans-footprint-only-when_another_made_it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[by warns] [[nil [["hut"]]]
                            ["Miles" [["hut"]]]
                            ["Fake" []]
                            ["fake" []]]]
          (let [{:keys [eng p seen]} (setup-with-plan {:blocks {"0,65,0" "stone" "0,66,0" "stone"}}
                                                       (cond-> roof-plan by (shape/with-author by)))]
            (core/submit! eng '(jobs.survival.breathe {:min-oxygen 12}) {})
            (await (core/tick! eng))
            (is (= 2 (count (calls p "dig"))) (pr-str by))
            (is (= warns (mapv :plans (trespass seen :breathe.trespass-last-resort))) (pr-str by))))))))

;; ------------------------------------------------------------------ extinguish

(def pool {"4,64,0" "water"})
(def burning-with-bucket {:self {:onFire true} :inventory [{:name "water_bucket" :count 1}]})
(def feet-zone (zone "Miles" [0 64 0] [0 64 0]))

(deftest extinguish-prefers-water-nearby-to-pouring-in-a-foreign-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup (assoc burning-with-bucket :blocks (merge st/floor pool)) [feet-zone])]
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (run-until-empty eng 4))
          (is (= [] (calls p "place")) "no bucket poured over another's cell")
          (is (= [{:x 4 :y 64 :z 0}] (mapv arg-pos (calls p "moveTo"))))
          (is (= [] (trespass seen :extinguish.trespass-last-resort))))))))

(deftest extinguish-pours-in-a-foreign-zone-when_nothing_else_is_left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[zones warns] [[[feet-zone] [["vault"]]]
                               [[(zone "FAKE" [0 64 0] [0 64 0])] []]
                               [nil []]]]
          (let [{:keys [eng p seen]} (setup (assoc burning-with-bucket :blocks {}) zones)]
            (core/submit! eng '(jobs.survival.extinguish) {})
            (await (core/tick! eng))
            (is (= [{:x 0 :y 64 :z 0} {:x 0 :y 64 :z 0}] (mapv arg-pos (calls p "place"))) (str (pr-str zones) " poured, then scooped back"))
            (is (= warns (mapv :zones (trespass seen :extinguish.trespass-last-resort))) (pr-str zones))))))))

;; ------------------------------------------------------------------ dig-in

(def night-world {:floor tu/walk-floor :time st/night :inventory st/dirt-stack :blocks st/floor})

(deftest dig-in-digs-down-when-the-walls-would-be-in-a-foreign-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup night-world [(zone "Miles" [1 64 -1] [1 66 1])])]
          (core/submit! eng '(jobs.survival.dig-in) {})
          (await (run-until-empty eng 10))
          (is (= [{:x 0 :y 63 :z 0} {:x 0 :y 62 :z 0} {:x 0 :y 61 :z 0}] (mapv arg-pos (calls p "dig"))) "the pit, not the walls")
          (is (= [{:x 0 :y 63 :z 0}] (mapv arg-pos (calls p "place"))))
          (is (= [] (trespass seen :dig-in.trespass-last-resort))))))))

(deftest dig-in-trespasses-only-when-every-way-is-foreign
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[zones places warns] [[[(whole-zone "Miles")] 10 [["vault"]]]
                                      [[(whole-zone "Fake")] 10 []]
                                      [nil 10 []]]]
          (let [{:keys [eng p seen]} (setup night-world zones)]
            (core/submit! eng '(jobs.survival.dig-in) {})
            (await (run-until-empty eng 10))
            (is (= places (count (calls p "place"))) (pr-str zones))
            (is (= warns (mapv :zones (trespass seen :dig-in.trespass-last-resort))) (pr-str zones))))))))

(deftest shelter-through-dig-in-follows-the-same-rule
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup night-world [(whole-zone "Miles")])]
          (st/dawn-after! p 2)
          (core/submit! eng '(jobs.survival.night) {})
          (await (run-until-empty eng 12))
          (is (= 10 (count (calls p "place"))))
          (is (= 1 (count (trespass seen :dig-in.trespass-last-resort)))))))))

;; ------------------------------------------------------------------ unstick

(defn dug [p] (mapv arg-pos (calls p "dig")))

(deftest unstick-in-a-pit-in-another-s-zone-neither-pillars-nor-digs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [here {:x 5 :y 61 :z 0}
              {:keys [eng p seen]} (setup {:self {:pos here} :blocks ut/pit :inventory [{:name "dirt" :count 4}]}
                                          [(zone "Miles" [0 55 -5] [12 70 5])])]
          (ut/seed-moved! eng (repeat 4 (ut/bad-move-at here ut/goal)))
          (core/submit! eng '(jobs.maintenance.unstick) {})
          (await (run-until-empty eng 100))
          (is (= [] (calls p "jumpPlace")))
          (is (= [] (calls p "dig")))
          (is (= {:step :pillar :reason :zone} (select-keys (:escalation (ut/failed-event seen)) [:step :reason]))
              "go-to's pillar refuses another's zone, and unstick says so"))))))

;; ------------------------------------------------------------------ get-food

(def two-carrots {:floor tu/walk-floor :blocks {"2,64,0" "carrots" "3,64,0" "carrots"} :ages {"2,64,0" 7 "3,64,0" 7} :drops {:carrots "carrot"}})

(deftest get-food-digs-the-permitted-crop-first-and-a-foreign-one-only-when-alone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[world zones expected warns] [[two-carrots [(zone "Miles" [2 64 0] [2 64 0])] [{:x 3 :y 64 :z 0}] []]
                                              [(update two-carrots :blocks dissoc "3,64,0") [(zone "Miles" [2 64 0] [2 64 0])] [{:x 2 :y 64 :z 0}] [["vault"]]]
                                              [(update two-carrots :blocks dissoc "3,64,0") nil [{:x 2 :y 64 :z 0}] []]]]
          (let [{:keys [eng p seen]} (setup (assoc world :self {:food 1}) zones)]
            (core/submit! eng '(jobs.survival.get-food) {})
            (await (run-until-empty eng 6))
            (is (= expected (take (count expected) (dug p))) (pr-str [zones expected]))
            (is (= warns (mapv :zones (trespass seen :get-food.trespass-last-resort))) (pr-str [zones expected]))))))))
