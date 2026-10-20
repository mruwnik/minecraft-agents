(ns engine.flight-job
  "A test job that runs one flight (jobs.survival.respond-flight/flee!) as its own round: the flight loop without
  respond-to-hostile's fight-or-flee choice. Its :goal arg seeds the interrupted goal respond-to-hostile would pass on."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.survival.respond-flight :as flight]))

(def job
  (assoc (tu/job (fn ^:async flight-round [c]
                   (when-let [g (:goal (:args c))] (ctx/update-mem! c assoc :goal g))
                   (await (flight/flee! c))))
         :defaults {:checks {:safe? '(always) :fit? '(always)} :use {:tools :attack}}
         :done? (fn [c] (if (flight/clear? c)
                          (contract/ok :clear "no hostile is near" :units :hostiles :done 0 :left 0)
                          (contract/later :fleeing "a flight is on" :units :hostiles :done 0 :left 1)))))

(def jobs (assoc registry/jobs 'test.flight job))
