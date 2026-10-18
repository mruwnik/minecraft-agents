(ns engine.reflex-contract-test
  "survival.fight-back, block-arrow-gap and extinguish as contract jobs: they export the contract, quote validly, and
  end with a kind and a cause."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.block-arrow-gap-test :as gt]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.extinguish-test :as et]
            [engine.hostile-test :as h]
            [engine.registry :as registry]
            [engine.seek-contract-test :as sc]
            [engine.test-util :as tu]
            [jobs.survival.block-arrow-gap :as gap]
            [jobs.survival.extinguish :as extinguish]
            [jobs.survival.fight-back :as fight-back]
            [engine.settings :as settings]
            [jobs.survival.respond-to-hostile :as respond]))

(defn job [sym] (get registry/jobs sym))

(deftest each-job-exports-the-contract-and-quotes-validly
  (doseq [sym ['jobs.survival.fight-back 'jobs.survival.block-arrow-gap 'jobs.survival.extinguish 'jobs.survival.respond-to-hostile]]
    (is (nil? (:check (job sym))) (str sym " has no old check"))
    (is (vector? (:target (job sym))) (str sym " has a target"))
    (is (nil? (contract/quote-problem ((:quote-of (job sym)) {:primitives (tu/fake {}) :args {} :quote-call {}}))) (str sym))))

(deftest every-survival-combat-and-food-contract-job-exports-a-target-and-a-time-spend
  (doseq [[sym def] registry/jobs
          :when (and (some some? ((juxt :can-start? :can-run?) def)) (re-find #"^jobs\.(survival|combat|food)\." (str sym)))]
    (is (vector? (:target def)) (str sym " has a target"))
    (is (some? (get-in def [:defaults :spend :job-time])) (str sym " has a default :spend :job-time"))))

(defn hold-ctx
  "A ctx over an empty fake whose job memory says it has held since 0, the clock past every hold cap."
  [args mem]
  {:primitives (tu/fake {}) :args args :root "j1" :slots [] :update-mem (fn [_ _])
   :view (fn [] {:now 100000000 :data {:entries {:job/j1 [{:t 0 :data mem}]} :policies {:job/j1 {:cap 1 :ttl :forever}}}})})

(deftest a-held-fight-waits-hold-s-from-its-last-swing-not-from-its-start
  (are [args mem idle?] (= idle? (boolean (fight-back/held-idle? (hold-ctx args mem))))
    {:hold true} {:started-at 0} true
    {:hold true} {:started-at 0 :last-attack 99999000} false
    {:hold true} {:started-at 99999000} false
    {:hold true} {:started-at 0 :last-attack 99980000} true
    {} {:started-at 0} false))

(defn ^:async fight [args world]
  (await (sc/ending-of #(tu/fake-on-floor (merge {:floor h/flight-floor} world)) (list 'jobs.survival.fight-back args))))

(deftest a-cleared-fight-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= :completed (:event (await (fight {} {:inventory h/sword :entities [(h/zombie 7 3 0)]})))))))))

(deftest a-fight-that-gave-up-on-every-target-is-nothing-possible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :stopped :kind :nothing-possible :reason :given-up}
               (await (fight {} {:inventory h/sword :entities [(assoc (h/zombie 7 3 0) :invulnerable true)]}))))))))

;; ------------------------------------------------------------------ respond-to-hostile

(deftest the-hits-a-fight-landed-are-kept-as-long-as-the-settings-say
  (is (= {:cap 8 :ttl 30000} (respond/struck-policy)))
  (is (= {:cap 2 :ttl 5000} (settings/with-settings {::respond/struck-s 5 ::respond/struck-cap 2} respond/struck-policy))))

(deftest respond-fights-with-the-weapon-the-call-names
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (h/flight-round '(jobs.survival.respond-to-hostile {:use {:tools ["stone_sword"]}})
                                                 {:inventory [{:name "iron_sword" :count 1} {:name "stone_sword" :count 1}]
                                                  :entities [(h/zombie 3 0)]}))]
          (is (= #{"stone_sword"} (set (map #(.-item (.-args %)) (h/calls p "equip"))))))))))

(deftest respond-with-no-danger-near-is-done-already
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= :completed (:event (await (sc/ending-of #(tu/fake-on-floor {:floor h/flight-floor}) '(jobs.survival.respond-to-hostile))))))
        (is (= :completed (:event (await (sc/ending-of #(tu/fake-on-floor {:floor h/flight-floor :inventory h/sword
                                                                            :entities [(h/zombie 7 3 0)]})
                                                       '(jobs.survival.respond-to-hostile))))))))))

;; ------------------------------------------------------------------ block-arrow-gap

(defn ^:async gap-end [spec args]
  (await (sc/ending-of #(:p (gt/setup [] spec)) (list 'jobs.survival.block-arrow-gap args))))

(deftest a-stoned-gap-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= :completed (:event (await (gap-end {:entities [gt/pit-skeleton] :blocks (merge gt/ground gt/pit gt/shell)} {})))))))))

(deftest no-blocks-is-a-failed-condition
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :stopped :kind :condition-failed :reason :no-blocks}
               (await (gap-end {:inventory []} {:fetch false}))))))))

(deftest no-open-cell-to-stop-the-arrows-is-nothing-possible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :stopped :kind :nothing-possible :reason :no-gap}
               (await (gap-end {:blocks (merge gt/ground gt/pit gt/shell)  :entities [gt/pit-skeleton]} {:settings {:jobs.survival.block-arrow-gap/reach 0}}))))))))

;; ------------------------------------------------------------------ extinguish

(defn ^:async burn-end [world]
  (await (sc/ending-of #(tu/seeing-all (tu/fake world)) '(jobs.survival.extinguish))))

(deftest a-body-that-burns-on-standing-is-limit-reached
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :stopped :kind :limit-reached :reason :still-burning}
               (await (burn-end {:self {:onFire true} :blocks (et/floor 8)}))))))))

(deftest a-body-with-no-safe-cell-is-nothing-possible-stuck
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :stopped :kind :nothing-possible :reason :stuck}
               (await (burn-end {:self {:onFire true} :blocks {"0,63,0" "stone" "1,64,0" "fire"}}))))))))

(deftest a-body-that-is-not-burning-is-done-already
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (et/setup {:blocks (et/floor 8)})]
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))) "the goal holds: done, not queued")
          (is (= [] (vec (.-calls (.-world p))))))))))

(deftest standing-beside-a-danger-is-a-failed-condition
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :stopped :kind :condition-failed :reason :hostile-near}
               (await (burn-end {:self {:onFire true} :blocks (et/floor 8) :entities [{:id 7 :name "zombie" :kind "hostile" :pos {:x 2 :y 64 :z 0}}]}))))))))

(deftest a-cut-extinguish-round-yields
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= :continue (await (extinguish/round {:alive? (constantly false)}))))))))

(deftest extinguish-places-the-cover-block-the-call-names
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (et/setup {:self {:onFire true}
                                        :inventory [{:name "cobblestone" :count 4} {:name "netherrack" :count 4}]
                                        :blocks (merge (et/floor 8) {"1,64,0" "lava"})})]
          (core/submit! eng '(jobs.survival.extinguish {:use {:blocks ["netherrack"]}}) {})
          (await (core/tick! eng))
          (is (= ["netherrack"] (mapv :item (map et/call-args (et/calls p "place"))))))))))

(deftest block-arrow-gap-places-the-block-the-call-names
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (gt/setup [] {:entities [gt/pit-skeleton]
                              :inventory [{:name "cobblestone" :count 4} {:name "netherrack" :count 4}]
                              :blocks (merge gt/ground gt/pit gt/shell)})]
          (await (gt/run-job! s {:use {:blocks ["netherrack"]}}))
          (let [items (map #(:item (et/call-args %)) (et/calls (:p s) "place"))]
            (is (seq items))
            (is (= #{"netherrack"} (set items)))))))))
