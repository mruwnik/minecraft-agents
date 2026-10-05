(ns engine.tidy-test
  "Tidying up after trespassing: a dig or place in another's zone is recorded in body memory, and
  jobs.survival.restore-broken puts the cells back when the body is safe."
  (:require [cljs.test :refer [deftest is async]]
            [engine.zones-survival-test :as zs]
            [engine.fake :as fake]
            [engine.core :as core]
            [engine.jobs.tidy :as tidy]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [engine.triggers.tidy-pending :as tidy-pending]))

(defn tidy-entries [eng] (mapv :data (mem/entries (mem/view (:store eng)) :tidy)))

(defn seed! [eng entries]
  (doseq [e entries] (mem/write! (:store eng) :tidy e tidy/tidy-policy)))

(def dug {:cell [0 65 0] :action :dig :was "stone" :now "air" :zone "vault" :tries 0})
(def placed {:cell [0 65 0] :action :place :was "air" :now "cobblestone" :zone "vault" :tries 0})
(def stone (:inventory {:inventory [{:name "stone" :count 2}]}))
(def zombie {:id 7 :name "zombie" :kind "hostile" :pos {:x 3 :y 64 :z 0}})

(def enclosed {"0,65,0" "stone" "0,66,0" "stone"})

(deftest breathe-records-what-it-dug-in-a-foreign-zone-only
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[zones recorded] [[[(zs/whole-zone "Miles")] [{:cell [0 65 0] :action :dig :was "stone" :now "air" :zone "vault" :tries 0}
                                                              {:cell [0 66 0] :action :dig :was "stone" :now "air" :zone "vault" :tries 0}]]
                                  [[(zs/whole-zone "fake")] []]
                                  [[] []]
                                  [nil []]]]
          (let [{:keys [eng]} (zs/setup {:blocks enclosed} zones)]
            (core/submit! eng '(jobs.survival.breathe {:min-oxygen 12}) {})
            (await (core/tick! eng))
            (is (= recorded (tidy-entries eng)) (pr-str zones))))))))

(defn restore!
  "Run restore-broken on a world with seeded entries; [eng p seen] after the job ran."
  [world zones entries]
  (let [{:keys [eng] :as s} (zs/setup world zones)]
    (seed! eng entries)
    (core/submit! eng '(jobs.survival.restore-broken) {})
    s))

(deftest restore-places-the-dug-block-when-carried-and-safe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (restore! {:inventory [{:name "stone" :count 2}]} [(zs/whole-zone "Miles")] [dug])]
          (await (zs/run-until-empty eng 6))
          (is (= [{:x 0 :y 65 :z 0}] (mapv zs/arg-pos (zs/calls p "place"))))
          (is (= [] (zs/calls p "dig")) "digs nothing")
          (is (= [] (tidy-entries eng)))
          (is (= [[[0 65 0]]] (mapv :cells (zs/trespass seen :tidy.restored))))
          (is (= [] (zs/trespass seen :tidy.not-restored)) "nothing left, no warn"))))))

(deftest restore-digs-a-placed-block-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (restore! {:blocks {"0,65,0" "cobblestone"}} [(zs/whole-zone "Miles")] [placed])]
          (await (zs/run-until-empty eng 6))
          (is (= [{:x 0 :y 65 :z 0}] (mapv zs/arg-pos (zs/calls p "dig"))))
          (is (= [] (zs/calls p "place")))
          (is (= [[[0 65 0]]] (mapv :cells (zs/trespass seen :tidy.restored)))))))))

(deftest restore-skips-and-warns-what-it-cannot-put-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[world entries why kept]
                [[{:inventory []} [dug] :not-carried 1]
                 [{:inventory [{:name "stone" :count 2}] :self {:health 6}} [dug] :unsafe 1]
                 [{:inventory [{:name "stone" :count 2}] :entities [zombie]} [dug] :unsafe 1]
                 [{:inventory [{:name "stone" :count 2}] :blocks {"0,65,0" "dirt"}} [dug] :changed 0]]]
          (let [{:keys [eng p seen]} (restore! world [(zs/whole-zone "Miles")] entries)]
            (await (zs/run-until-empty eng 6))
            (is (= [] (zs/calls p "place")) (pr-str why))
            (is (= [] (zs/calls p "dig")) (pr-str why))
            (is (= kept (count (tidy-entries eng))) (pr-str why))
            (is (= [[{:cell [0 65 0] :was "stone" :why why}]] (mapv :cells (zs/trespass seen :tidy.not-restored))) (pr-str why))
            (is (= [[]] (mapv :cells (zs/trespass seen :tidy.restored))) (pr-str why))))))))

;; ------------------------------------------------------------------ the tidy-pending trigger

(defn holds?
  "Whether the trigger holds on a fake world with the memory entries [[kind data] ...] written."
  [world memory]
  (let [s (mem/open (tu/tmp-dir) {:now (constantly 1000)})]
    (doseq [[kind data] memory] (mem/write! s kind data tidy/tidy-policy))
    ((:when tidy-pending/trigger) (tu/fake world) (mem/view s) tidy-pending/defaults nil)))

(def with-stone {:inventory [{:name "stone" :count 2}]})
(def reported [:tidy-reported {:cells [[0 65 0]]}])

(deftest the-trigger-holds-for-a-restorable-entry-when-safe-and-for-an-unreported-one
  (doseq [[world memory expected why]
          [[with-stone [[:tidy dug]] true "carried, cell as left, safe"]
           [with-stone [[:tidy dug] reported] true "still restorable after a report"]
           [{:blocks {"0,65,0" "cobblestone"}} [[:tidy placed]] true "a placed block is dug again, no item needed"]
           [{} [[:tidy dug]] true "not carried and not reported yet: the job warns once"]
           [{} [[:tidy dug] reported] false "not carried and reported: nothing changed"]
           [{:blocks {"0,65,0" "dirt"}} [[:tidy dug] reported] false "changed cell reported"]
           [{:blocks {"0,65,0" "dirt"}} [[:tidy dug]] true "changed cell is forgotten by one run"]
           [(assoc with-stone :self {:health 6}) [[:tidy dug]] false "unsafe: health"]
           [(assoc with-stone :entities [zombie]) [[:tidy dug]] false "unsafe: hostile"]
           [with-stone [] false "no entries"]
           [with-stone [[:tidy (assoc dug :tries tidy/max-tries)] reported] false "given up and reported"]
           [(assoc with-stone :unloaded ["0,65,0"]) [[:tidy dug]] false "cell not loaded"]]]
    (is (= expected (holds? world memory)) why)))

(deftest the-trigger-is-a-builtin-with-the-stop-persistence
  (is (= tidy-pending/trigger (:tidy-pending triggers/all)))
  (is (= '(jobs.survival.restore-broken) (:job tidy-pending/trigger)))
  (is (= :stop (:persistence tidy-pending/trigger))))

(defn ^:async ticks! [eng n]
  (dotimes [_ n] (await (core/tick! eng))))

(deftest a-trespass-is-restored-by-itself-through-the-trigger
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (zs/setup with-stone [(zs/whole-zone "Miles")])]
          (seed! eng [dug])
          (core/register-reflex! eng {:trigger :tidy-pending})
          (await (ticks! eng 12))
          (is (= [{:x 0 :y 65 :z 0}] (mapv zs/arg-pos (zs/calls p "place"))))
          (is (= [] (tidy-entries eng)))
          (is (= [[[0 65 0]]] (mapv :cells (zs/trespass seen :tidy.restored))))
          (is (= [] (zs/trespass seen :tidy.not-restored))))))))

(deftest unrestorable-entries-warn-once-and-do-not-loop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (zs/setup {} [(zs/whole-zone "Miles")])]
          (seed! eng [dug])
          (core/register-reflex! eng {:trigger :tidy-pending})
          (await (ticks! eng 40))
          (is (= [] (zs/calls p "place")))
          (is (= 1 (count (zs/trespass seen :tidy.not-restored))))
          (is (= 1 (count (zs/trespass seen :tidy.restored))) "the job ran once")
          (is (= 1 (count (tidy-entries eng))) "the entry is kept for later"))))))

(deftest a-reported-entry-is-restored-once-the-item-is-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (zs/setup {} [(zs/whole-zone "Miles")])]
          (seed! eng [dug])
          (core/register-reflex! eng {:trigger :tidy-pending})
          (await (ticks! eng 20))
          (is (= 1 (count (zs/trespass seen :tidy.not-restored))))
          (fake/add-item! p "stone" 2)
          (await (ticks! eng 20))
          (is (= [{:x 0 :y 65 :z 0}] (mapv zs/arg-pos (zs/calls p "place"))))
          (is (= [] (tidy-entries eng))))))))
