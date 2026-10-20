(ns engine.stuck-underground-test
  "The :stuck-underground trigger: an idle body deep underground walks home."
  (:require [cljs.test :refer [deftest is are]]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [triggers.survival.stuck-underground :as su]))

(def roof {"0,52,0" "stone"})

(defn holds?
  "The trigger over a fake body at (0 50 0) under a roof, with places [[kind x y z] ..] remembered and job memory [id ..] listed, read at each of the wall times ts (ms) in turn: the result at the last."
  [places & [over jobs ts]]
  (let [now (atom 1000)
        store (mem/open (tu/tmp-dir) {:now #(deref now)})
        p (tu/seeing-all (tu/fake-on-floor (merge {:floor [-4 -4 4 4] :self {:pos {:x 0.5 :y 50 :z 0.5}} :blocks roof} over)))]
    (doseq [[kind x y z] places]
      (mem/write! store kind {:pos {:x x :y y :z z}} mem/place-policy))
    (doseq [id jobs]
      (mem/write! store (mem/job-kind id) {:started true} mem/place-policy))
    (last (for [t (or ts [1000])]
            (do (reset! now t)
                (su/stuck-underground p (mem/view store) (assoc su/defaults :idle-s 120) nil nil))))))

(deftest an-idle-buried-body-far-below-its-home-is-stuck
  (are [places expected] (= expected (holds? places nil nil [1000 130000]))
    [[:home 30 64 0]] true
    [[:bed 30 64 0]] true
    [[:home 30 54 0]] false
    [[:home 1 55 1]] false
    [] false))

(deftest a-body-under-the-open-sky-or-with-a-listed-job-is-not-stuck
  (is (false? (holds? [[:home 30 64 0]] {:blocks {}} nil [1000 130000])))
  (is (false? (holds? [[:home 30 64 0]] nil ["j1"] [1000 130000]))))

(deftest the-trigger-waits-its-idle-limit-and-a-break-restarts-the-clock
  (are [ts expected] (= expected (holds? [[:home 30 64 0]] nil nil ts))
    [1000] false
    [1000 100000] false
    [1000 121000] true
    [1000 121000 122000] true))

(deftest a-job-listed-between-reads-restarts-the-idle-clock
  (let [now (atom 1000)
        store (mem/open (tu/tmp-dir) {:now #(deref now)})
        p (tu/seeing-all (tu/fake-on-floor {:floor [-4 -4 4 4] :self {:pos {:x 0.5 :y 50 :z 0.5}} :blocks roof}))
        read (fn [t] (reset! now t) (su/stuck-underground p (mem/view store) su/defaults nil nil))]
    (mem/write! store :home {:pos {:x 30 :y 64 :z 0}} mem/place-policy)
    (read 1000)
    (mem/write! store (mem/job-kind "j1") {:started true} mem/place-policy)
    (read 100000)
    (mem/forget! store (mem/job-kind "j1"))
    (is (false? (read 125000)))
    (is (true? (read 250000)))))
