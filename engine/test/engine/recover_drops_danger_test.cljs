(ns engine.recover-drops-danger-test
  "recover-drops against the mobs that killed the body: the killer is priced at the pile, and a trip cut by hostiles
  over and over is given up."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.death-test :as dt :refer [die! job recovered recovered-trip pile-drops fetching-events]]
            [engine.library-test :refer [setup]]
            [engine.memory :as mem]
            [engine.test-util :as tu :refer [run-until-empty]]
            [triggers.survival.died :as died]))

(defn fetch-run
  "Die with the field-report pile and cause, run the job to its decision: the :recover-drops.decided or .fetching event."
  ([cause] (fetch-run cause {}))
  ([cause body]
  (let [pile dt/death-pile-of-the-field-report]
    (fn ^:async t []
      (let [{:keys [eng seen]} (setup (merge {:floor tu/walk-floor :entities (vec (pile-drops pile))} body))]
        (die! eng (cond-> {:pos dt/death-pos :inventory pile} cause (assoc :cause cause)))
        (core/submit! eng job {})
        (await (run-until-empty eng 20))
        {:eng eng :seen seen})))))

(defn decided-text [seen]
  (:text (first (filter #(contains? #{:recover-drops.fetching :recover-drops.decided} (:kind %)) @seen))))

(deftest a-skeleton-killer-is-priced-at-the-pile
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [with-cause (await ((fetch-run "skeleton")))
              without (await ((fetch-run nil)))]
          (is (re-find #"danger [1-9][0-9.]*,.*: skeleton" (decided-text (:seen with-cause))))
          (is (re-find #"danger 0," (decided-text (:seen without)))))))))

(deftest a-damage-type-cause-adds-no-danger
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await ((fetch-run "fall")))]
          (is (re-find #"danger 0," (decided-text (:seen r)))))))))

(def iron-kit
  {:inventory [{:name "iron_sword" :count 1}]
   :equipment {:head {:name "iron_helmet"} :torso {:name "iron_chestplate"} :legs {:name "iron_leggings"} :feet {:name "iron_boots"}}})

(deftest an-unarmed-body-skips-a-pile-its-killer-guards
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (await ((fetch-run "skeleton")))]
          (is (= :skip (:decision (recovered eng))))
          (is (re-find #"skip: .*danger [1-9][0-9.]*,.*: skeleton" (decided-text seen))))))))

(deftest a-body-with-an-iron-sword-and-armour-goes-for-the-guarded-pile
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen]} (await ((fetch-run "skeleton" iron-kit)))]
          (is (re-find #"^fetch: " (decided-text seen))))))))

;; ------------------------------------------------------------ cut trips

(defn death-t [eng] (:t (mem/latest (mem/view (:store eng)) :died)))

(defn resume-run
  "A trip decided and cut `cuts` times before: a saved :recover-trip, then the job."
  [cuts]
  (fn ^:async t []
    (let [{:keys [eng seen]} (setup {:floor tu/walk-floor :entities (vec (pile-drops dt/diamonds))})]
      (die! eng {:pos dt/death-pos :inventory dt/diamonds})
      (mem/write! (:store eng) :recover-trip
                  {:death-t (death-t eng) :decided {:value 1400 :cost 50} :baseline {} :phase :go :cuts cuts}
                  {:cap 1 :ttl 1000000})
      (core/submit! eng job {})
      (await (run-until-empty eng 20))
      {:eng eng :seen seen})))

(deftest a-trip-cut-by-hostiles-twice-is-abandoned-as-danger
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await ((resume-run 1)))]
          (is (= {:decision :abandoned :reason :danger} (select-keys (recovered eng) [:decision :reason])))
          (is (false? (died/died nil (mem/view (:store eng)) {})) "the died trigger stops firing"))))))

(deftest a-trip-cut-once-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await ((resume-run 0)))]
          (is (= :collected (:decision (recovered eng)))))))))
