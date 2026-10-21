(ns engine.sand-shelter-test
  (:require [cljs.test :refer [deftest is async]]
            [engine.shut-in-pit-seen-test :as seen]
            [engine.test-util :as tu]))

(defn layer [name y0 y1] (into {} (for [x (range -14 15) y (range y0 (inc y1)) z (range -14 15)] [(str x "," y "," z) name])))

(def desert
  "Open sky over 4 blocks of sand on 6 of sandstone on stone, wide enough that the surface is no closed room."
  (merge (layer "air" 65 68) (seen/stone -14 14 50 54 -14 14) (layer "sandstone" 55 60) (layer "sand" 61 64)))

(defn ^:async night-shut-in! [ways]
  (await (seen/tick-out! (seen/setup (await (seen/see-surface! (seen/sensing {:blocks desert :inventory []}))) 'jobs.survival.shut-in {:ways ways :for :night}))))

(deftest an-empty-handed-body-on-sand-digs-down-to-the-sandstone-and-pockets-into-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (night-shut-in! #{:pocket}))]
          (is (= :done (:status @out)))
          (is (<= (seen/feet-y p) 58) "the pocket lies in the sandstone under the sand"))))))

(deftest a-pit-on-sand-stops-for-want-of-a-block-that-holds-a-roof
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (night-shut-in! #{:pit}))]
          (is (= {:pit {:reason :no-blocks}} (:stops @out)) "sand is dug by hand and falls: not a roof, and no longer read as stone to cut"))))))
