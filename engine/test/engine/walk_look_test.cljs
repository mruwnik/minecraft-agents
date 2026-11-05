(ns engine.walk-look-test
  "jobs.movement.walk-look against the fake world: one leg down a dark tunnel to look at a side a look from here cannot
  make out, and back."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.lib.look :as look]))

(def tunnel
  "A 1x2 tunnel x 0..12 in stone, sealed at both ends, the body at its west end."
  {:self {:pos {:x 0.5 :y 64 :z 0.5}}
   :blocks (merge (into {} (remove (fn [[k _]] (let [[x y z] (map js/Number (.split k ","))] (and (<= 0 x 12) (<= 64 y 65) (= z 0)))))
                        (tu/box -1 63 -1 13 66 1 "stone")))})

(defn ^:async walk-look
  "Runs walk-look for the side [8 64 0] from {:x 6}; the side shows once the body has stood within `sight` blocks of it
  (nil: never). [farthest x the body reached, the cell it ended in, the :walk-looked cells, its ending kinds]."
  ([sight] (walk-look sight ##Inf))
  ([sight worth]
  (let [reached (atom 0)
        [seen sink] (tu/capture-sink)
        clock (atom 1000000)
        p (tu/seeing-all (tu/fake-on-floor tunnel))
        eng (core/create {:primitives p :dir (tu/tmp-dir) :now #(deref clock) :jobs registry/jobs
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (with-redefs [ctx/worth (constantly worth)
                  look/unseen? (fn [p _]
                                 (swap! reached max (.-x (.-pos (.self p))))
                                 (not (and sight (<= (- 8 @reached) sight))))]
      (core/submit! eng '(jobs.movement.walk-look {:side [8 64 0] :to [6 64 0]}) {})
      (await (tu/tick-until-idle! eng 40)))
    [(js/Math.floor @reached)
     (mapv js/Math.floor ((juxt #(.-x %) #(.-y %) #(.-z %)) (.-pos (.self p))))
     (:cells (:data (mem/latest (mem/view (:store eng)) :walk-looked)))
     (into [] (comp (filter #(and (= :job (:source %)) (#{:completed :stopped} (:kind %)) (= "jobs.movement.walk-look" (:name (:data %))))) (map :kind)) @seen)])))

(deftest a-walk-to-look-walks-the-leg-it-was-priced-for-then-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= [6 [0 64 0] nil [:completed]] (await (walk-look 2)))
            "it walks to its :to, no farther, sees the side there and comes back; a side that shows is not remembered")
        (is (= [6 [0 64 0] [[8 64 0]] [:completed]] (await (walk-look nil)))
            "a side still unseen from close by is remembered, so no walk is offered for it again")
        (is (= [0 [0 64 0] nil [:completed]] (await (walk-look nil 0)))
            "a walk the worth does not buy is not walked, and the side is not remembered as looked at from close by")))))

(deftest the-walk-looked-entry-is-one-and-belongs-to-the-latest-root-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              p (tu/seeing-all (tu/fake-on-floor tunnel))
              eng (core/create {:primitives p :dir (tu/tmp-dir) :now #(deref clock) :jobs registry/jobs
                                :events (events/make {:body "Fake" :sinks [(second (tu/capture-sink))] :now #(deref clock)})})
              sides (mapv (fn [z] [8 64 z]) (range 21))]
          (with-redefs [ctx/worth (constantly ##Inf)
                        look/unseen? (constantly true)]
            (doseq [side sides]
              (core/submit! eng (list 'jobs.movement.walk-look {:side side :to [6 64 0]}) {})
              (await (tu/tick-until-idle! eng 40))))
          (is (= [(last sides)] (:cells (:data (mem/latest (mem/view (:store eng)) :walk-looked))))
              "each call is a decision of its own: the sides of the calls before it are not kept")
          (is (= 1 (count (mem/entries (mem/view (:store eng)) :walk-looked))) "the entry is rewritten, not copied per leg"))))))

(deftest a-leg-whose-walk-is-not-bought-records-its-side-as-declined-so-it-is-not-offered-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              p (tu/seeing-all (tu/fake-on-floor tunnel))
              eng (core/create {:primitives p :dir (tu/tmp-dir) :now #(deref clock) :jobs registry/jobs
                                :events (events/make {:body "Fake" :sinks [(second (tu/capture-sink))] :now #(deref clock)})})]
          (with-redefs [ctx/worth (constantly 0)
                        look/unseen? (constantly true)]
            (core/submit! eng '(jobs.movement.walk-look {:side [8 64 0] :to [6 64 0]}) {})
            (await (tu/tick-until-idle! eng 40)))
          (let [d (:data (mem/latest (mem/view (:store eng)) :walk-looked))]
            (is (= [[8 64 0]] (:declined d)))
            (is (empty? (:cells d)))))))))
