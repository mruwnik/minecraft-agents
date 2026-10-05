(ns engine.hurt-test
  "The body :hurt event: the health lost and the cause, merged over a short window."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.hurt :as hurt]
            [engine.library-test :refer [setup]]
            [engine.test-util :as tu]))

(def window-ms 30)

(defn ^:async settle [] (await (js/Promise. (fn [res] (js/setTimeout res (* 3 window-ms))))))

(defn hurt-events [{:keys [seen]}] (filterv #(= :hurt (:kind %)) @seen))

(defn hit!
  "The body loses amount health to a hit given as the raw adapter fields."
  [{:keys [p]} fields]
  (.emit (.-world p) (clj->js (merge {:kind "hurt" :food 20} fields))))

(deftest cause-names-the-attacker-first
  (are [raw expected] (= expected (hurt/cause raw))
    {:attacker {:id 4 :name "zombie"}} "zombie"
    {:attacker {:id 4 :name "zombie"} :damageType "mob_attack"} "zombie"
    {:damageType "fall"} "fall"
    {:damageType "on_fire"} "fire"
    {:damageType "in_fire"} "fire"
    {:damageType "lava"} "lava"
    {:damageType "drown"} "drowning"
    {:damageType "starve"} "starvation"
    {:damageType "explosion"} "explosion"
    {:damageType "player_explosion"} "explosion"
    {:cause "lava" :damageType "on_fire"} "lava"
    {} nil))

(deftest a-zombie-hit-emits-one-hurt-event-with-the-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {} )]
          (hurt/set-window! (:eng s) window-ms)
          (hit! s {:health 17 :amount 3 :attacker {:id 9 :name "zombie"}})
          (await (settle))
          (let [[e & more] (hurt-events s)]
            (is (empty? more))
            (is (= 3 (:amount e)))
            (is (= 17 (:health e)))
            (is (= ["zombie"] (:causes e)))))))))

(deftest a-fall-emits-cause-fall
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {})]
          (hurt/set-window! (:eng s) window-ms)
          (hit! s {:health 14 :amount 6 :damageType "fall"})
          (await (settle))
          (is (= ["fall"] (:causes (first (hurt-events s))))))))))

(deftest rapid-hits-merge-into-one-event
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {})]
          (hurt/set-window! (:eng s) window-ms)
          (doseq [h [18 16 14 12 10]]
            (hit! s {:health h :amount 2 :attacker {:id 9 :name "zombie"}}))
          (hit! s {:health 8 :amount 2 :damageType "on_fire"})
          (await (settle))
          (let [[e & more] (hurt-events s)]
            (is (empty? more))
            (is (= 12 (:amount e)))
            (is (= 6 (:hits e)))
            (is (= 8 (:health e)))
            (is (= ["zombie" "fire"] (:causes e)))))))))

(deftest hits-in-separate-windows-are-separate-events
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {})]
          (hurt/set-window! (:eng s) window-ms)
          (hit! s {:health 18 :amount 2 :damageType "fall"})
          (await (settle))
          (hit! s {:health 16 :amount 2 :damageType "fall"})
          (await (settle))
          (is (= 2 (count (hurt-events s)))))))))
