(ns engine.survival-can-start-test
  "can-start? of the survival jobs asked directly over a fake body: the status kind and reason each fact gives."
  (:require [cljs.test :refer [deftest is are]]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.shelter-support :as st]
            [engine.test-util :as tu]))

(defn start-of
  "[kind reason] of job's can-start? over a fake body (world), places/entries [[kind data policy] ..] written first."
  [job world args & [entries]]
  (let [store (mem/open (tu/tmp-dir) {:now (constantly 1000)})]
    (doseq [[kind data policy] entries] (mem/write! store kind data policy))
    (tu/kind-reason ((:can-start? (get registry/jobs job))
                     {:primitives (tu/seeing-all (tu/fake world)) :view (fn [] (mem/view store)) :root 1 :slots [] :args args}))))

(def home {:pos {:x 12 :y 64 :z 0}})
(def far-home {:pos {:x 300 :y 64 :z 0}})

(deftest go-home-start-facts
  (are [world entries expected] (= expected (start-of 'jobs.survival.go-home world {:limit 100} entries))
    {:time st/noon} [] [:later :no-home]
    {:time st/night} [[:home home mem/place-policy]] [:later :night]
    {:time st/noon} [[:home far-home mem/place-policy]] [:later :too-far]
    {:time st/noon} [[:home home mem/place-policy]] [:ok :daytime]))

(deftest set-spawn-start-facts
  (are [world args expected] (= expected (start-of 'jobs.survival.set-spawn world args))
    {:time st/night} {:bed {:x 3 :y 64 :z 0}} [:later :night]
    {:time st/noon} {} [:look :no-bed]
    {:time st/noon} {:bed {:x 3 :y 64 :z 0}} [:ok :daytime]))

(deftest hostile-attack-needs-a-hostile-in-range
  (are [world expected] (= expected (start-of 'jobs.combat.attack world {:targets :hostiles :radius 4 :ranged-radius 16 :leash 20 :skip []}))
    {} [:later :no-target]
    {:entities [{:id 7 :name "zombie" :kind "hostile" :pos {:x 3 :y 64 :z 0}}]} [:ok :target]
    {:self {:health 2} :entities [{:id 7 :name "zombie" :kind "hostile" :pos {:x 3 :y 64 :z 0}}]} [:ok :target]))

(defn done-of
  "[kind reason] of job's done? over a fake body, asked before the call starts or once it runs (its :contract :call written, an entry of `entries`)."
  [job world args entries]
  (let [store (mem/open (tu/tmp-dir) {:now (constantly 1000)})]
    (doseq [[kind data policy] (cons [:home home mem/place-policy] entries)] (mem/write! store kind data policy))
    (tu/kind-reason ((:done? (get registry/jobs job))
                     {:primitives (tu/seeing-all (tu/fake world)) :view (fn [] (mem/view store)) :root "1" :slots [] :args args}))))

(def running [[(mem/job-kind "1") {:contract {:call {:spend {}}}} {:cap 1 :ttl :forever}]])

(deftest a-goal-the-round-wraps-up-holds-only-before-the-start
  (are [job world args] (= [[:ok] [:later]] [(take 1 (done-of job world args [])) (take 1 (done-of job world args running))])
    'jobs.survival.go-home {:time st/noon :self {:pos {:x 12.5 :y 64 :z 0.5}}} {}
    'jobs.survival.respond-to-hostile {:time st/noon} {:range 4 :ranged-range 16}))
