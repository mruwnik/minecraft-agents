(ns engine.done-status-survival-test
  "done? of the survival, combat and village jobs is :ok when the whole goal holds, read from the world and memory
  (docs/contract-conditions-design.md 2.2); a record the call still has to write keeps it :later."
  (:require [cljs.test :refer [deftest is are]]
            [engine.expr :as expr]
            [engine.flee-test :as ft]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.lib.world-files :as world]))

(defn done-in
  "done? of job sym in a world spec, args over its defaults, job memory m, memory entries by kind in entries."
  [sym world args m entries]
  (let [c {:primitives (tu/seeing-all (tu/fake world)) :args (second (expr/leaf registry/jobs sym args)) :root "j" :slots []
           :id "j" :engine {:warned (atom #{}) :world (world/of-data {} {} [])} :emit (fn [& _]) :sym sym
           :view (fn [] {:now 1000 :data {:entries (merge {(mem/job-kind "j") [{:t 1000 :data m}]} entries)}})}]
    ((:done? (get registry/jobs sym)) c)))

(defn done-of [sym world args m] (done-in sym world args m {}))

(deftest retreat-is-done-when-no-danger-is-on-and-the-flight-has-nothing-to-write
  (are [world m want] (= want (tu/kind-reason (done-of 'jobs.survival.retreat world {} m)))
    {} {} [:ok :clear]
    {:floor ft/big-floor :entities [(ft/zombie 7 5 {})]} {} [:later :fleeing]
    {} {:chasers {7 {:id 7}}} [:later :fleeing]
    {} {:fled [{:id 7 :ended :far}]} [:later :fleeing]
    {} {:refuge {:kind :seal}} [:later :fleeing]))

(defn zombie [id x] (ft/zombie id x {}))

(deftest attack-is-done-when-every-target-seen-is-killed-and-none-is-left-for-lost-s
  (are [world m want] (= want (tu/kind-reason (done-of 'jobs.combat.attack world {:targets "zombie" :lost-s 5} m)))
    {} {} [:later :fighting]
    {} {:seen #{7} :killed [7] :last-seen 0} [:later :fighting]
    {} {:seen #{7} :killed [7] :last-seen -9000} [:ok :cleared]
    {} {:seen #{7 8} :killed [7] :last-seen -9000} [:later :fighting]
    {} {:seen #{7 8} :killed [7] :given-up {8 :unreachable} :last-seen -9000} [:later :fighting]
    {:entities [(zombie 9 4)]} {:seen #{7} :killed [7] :last-seen -9000} [:later :fighting]))

(deftest hunt-is-done-when-the-count-is-killed-and-the-drops-are-collected
  (are [args m want] (= want (tu/kind-reason (done-of 'jobs.combat.hunt {} args m)))
    {:mob "cow" :count 2} {} [:later :hunting]
    {:mob "cow" :count 2} {:killed 1} [:later :hunting]
    {:mob "cow" :count 2} {:killed 2} [:ok :hunted]
    {:mob "cow" :count 2} {:killed 2 :target 5} [:later :hunting]
    {:mob "cow" :count 2} {:killed 2 :collecting true} [:later :hunting]
    {:mob ["cow" "pig"] :count 1} {:kinds {"cow" {:killed 1 :reason :count}}} [:later :hunting]
    {:mob ["cow" "pig"] :count 1} {:kinds {"cow" {:killed 1 :reason :count} "pig" {:killed 1 :reason :count}}} [:ok :hunted]))

(def bed {:x 3 :y 64 :z 0})
(def bed-world {:blocks {"3,64,0" "red_bed"} :floor [-3 -3 3 3]})

(deftest set-spawn-is-done-when-the-spawn-bed-is-recorded-at-the-bed
  (are [entries want] (= want (tu/kind-reason (done-in 'jobs.survival.set-spawn bed-world {:bed [3 64 0]} {} entries)))
    {} [:later :spawn-unset]
    {:spawn-set [{:t 900 :data {:pos bed}}]} [:later :spawn-unset]
    {:spawn-bed [{:t 900 :data {:pos bed}}]} [:ok :spawn-set]
    {:spawn-bed [{:t 900 :data {:pos {:x 30 :y 64 :z 0}}}]} [:later :spawn-unset]
    {:spawn-bed [{:t 800 :data {:pos bed}}] :spawn-reset [{:t 900 :data {}}]} [:later :spawn-unset]))

(deftest log-out-is-done-when-a-log-out-was-written-since-the-job-was-queued
  (is (= {:ms 3000 :how "ok"}
         (:result (nth (done-in 'jobs.survival.log-out {} {} {:queued-t 100} {:log-out [{:t 500 :data {:ms 3000 :status "ok"}}]}) 2))))
  (are [m entries want] (= want (tu/kind-reason (done-in 'jobs.survival.log-out {} {} m entries)))
    {:queued-t 100} {} [:later :online]
    {:queued-t 600} {:log-out [{:t 500 :data {:ms 3000 :status "ok"}}]} [:later :online]
    {:queued-t 100} {:log-out [{:t 500 :data {:ms 3000 :status "unsupported"}}]} [:later :online]
    {:queued-t 100} {:log-out [{:t 500 :data {:ms 3000 :status "cut"}}]} [:ok :logged-out]))

(def librarian {:profession "librarian" :trade "mending" :pos [4 64 4]})
(defn villager [id extra] (merge {:id id :uuid (str "v-" id) :name "villager" :kind "passive" :pos {:x 3 :y 64 :z 2}} extra))

(deftest village-maintain-is-done-when-no-step-is-left-to-run
  (are [args world m want] (= want (tu/kind-reason (done-of 'jobs.village.maintain world args m)))
    {} {} {} [:ok :in-order]
    {:plan "p"} {} {} [:later :steps]
    {:plan "p"} {} {:todo [:breed] :report {:repair {:placed 3}}} [:ok :in-order]
    {:plan "p"} {} {:todo [:breed] :report {:repair {:stopped :nothing-possible}}} [:later :steps]
    {:plan "p"} {} {:todo [:breed] :report {:repair {:placed 3}} :call-args {:plan "p"}} [:later :steps]
    {:roles [librarian]} {} {} [:later :steps]
    {:roles [librarian]} {} {:looked {:x 0 :y 64 :z 0}} [:ok :in-order]
    {:roles [librarian]} {:entities [(villager 1 {:profession "none"})]} {} [:later :steps]
    {:roles [librarian]} {:entities [(villager 1 {:profession "librarian"})]} {} [:ok :in-order]
    {:roles [(dissoc librarian :pos)]} {:entities [(villager 1 {:profession "none"})]} {} [:later :steps]))

(deftest fight-back-is-done-when-no-hostile-is-in-range
  (are [world m want] (= want (tu/kind-reason (done-of 'jobs.survival.fight-back world {} m)))
    {} {} [:ok :clear]
    {} {:killed [7]} [:ok :clear]
    {:entities [(zombie 9 3)]} {} [:later :fighting]
    {:entities [(zombie 9 3)]} {:killed [9]} [:ok :clear]))
