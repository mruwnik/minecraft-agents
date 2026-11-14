(ns engine.stuck-underground-test
  "The :stuck-underground trigger: an idle body deep underground walks home."
  (:require [cljs.test :refer [deftest is are]]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [triggers.survival.stuck-underground :as su]))

(def roof {"0,52,0" "stone"})

(defn holds?
  "The trigger over a fake body at (0 50 0) under a roof, with places [[kind x y z] ..] remembered and the jobs [id ..] listed (the trigger's live input), read at each of the wall times ts (ms) in turn: the result at the last."
  [places & [over jobs ts]]
  (let [now (atom 1000)
        store (mem/open (tu/tmp-dir) {:now #(deref now)})
        p (tu/seeing-all (tu/fake-on-floor (merge {:floor [-4 -4 4 4] :self {:pos {:x 0.5 :y 50 :z 0.5}} :blocks roof} over)))]
    (doseq [[kind x y z] places]
      (mem/write! store kind {:pos {:x x :y y :z z}} mem/place-policy))
    (last (for [t (or ts [1000])]
            (do (reset! now t)
                (su/stuck-underground p (mem/view store) (assoc su/defaults :idle-s 120) nil (set jobs)))))))

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
        read (fn [t live] (reset! now t) (su/stuck-underground p (mem/view store) su/defaults nil live))]
    (mem/write! store :home {:pos {:x 30 :y 64 :z 0}} mem/place-policy)
    (read 1000 #{})
    (read 100000 #{"j1"})
    (is (false? (read 125000 #{})))
    (is (true? (read 250000 #{})))))

(deftest a-listed-job-that-leaves-the-body-idle-keeps-it-stuck-no-longer
  (let [now (atom 1000)
        store (mem/open (tu/tmp-dir) {:now #(deref now)})
        p (tu/seeing-all (tu/fake-on-floor {:floor [-4 -4 4 4] :self {:pos {:x 0.5 :y 50 :z 0.5}}
                                                      :blocks (assoc roof "1,52,0" "stone")}))
        read (fn [t] (reset! now t) (su/stuck-underground p (mem/view store) su/defaults nil #{"j1"}))]
    (mem/write! store :home {:pos {:x 30 :y 64 :z 0}} mem/place-policy)
    (read 1000)
    (is (false? (read 125000)) "a job listed and no body.idle record: the job works")
    (reset! now 126000)
    (mem/write! store :body-idle {:jobs [] :idle-ms 10000})
    (is (false? (read 200000)))
    (is (true? (read 246000)) "idle-s after the clock restarted under the working job")
    (swap! (fake/state p) assoc-in [:self :pos] [1.5 50 0.5])
    (is (false? (read 247000)) "leaving its cell restarts the clock")
    (is (true? (read 400000)) "the earlier body.idle record still counts: a push does not write a new one")))

(deftest a-change-of-the-listed-jobs-restarts-the-clock
  (let [now (atom 1000)
        store (mem/open (tu/tmp-dir) {:now #(deref now)})
        p (tu/seeing-all (tu/fake-on-floor {:floor [-4 -4 4 4] :self {:pos {:x 0.5 :y 50 :z 0.5}} :blocks roof}))
        read (fn [t live] (reset! now t) (su/stuck-underground p (mem/view store) su/defaults nil live))]
    (mem/write! store :home {:pos {:x 30 :y 64 :z 0}} mem/place-policy)
    (read 1000 #{"j1"})
    (reset! now 2000)
    (mem/write! store :body-idle {:jobs [] :idle-ms 10000})
    (is (false? (read 130000 #{"j1" "j2"})) "j2 listed: a new clock, though a record is on file")
    (reset! now 131000)
    (mem/write! store :body-idle {:jobs [] :idle-ms 10000})
    (is (false? (read 200000 #{"j1" "j2"})))
    (is (true? (read 251000 #{"j1" "j2"})))))

(deftest a-listed-job-that-holds-on-purpose-does-not-count-as-idle
  (let [now (atom 1000)
        store (mem/open (tu/tmp-dir) {:now #(deref now)})
        p (tu/seeing-all (tu/fake-on-floor {:floor [-4 -4 4 4] :self {:pos {:x 0.5 :y 50 :z 0.5}} :blocks roof}))
        read (fn [t] (reset! now t) (su/stuck-underground p (mem/view store) su/defaults nil #{"j1"}))]
    (mem/write! store :home {:pos {:x 30 :y 64 :z 0}} mem/place-policy)
    (read 1000)
    (reset! now 2000)
    (mem/write! store :body-idle {:jobs [{:job "j1" :holding {:reason :cooking}}] :holds [:cooking] :idle-ms 10000})
    (is (false? (read 130000)) "a declared hold: the body waits for something, it is not stuck")
    (is (false? (read 400000)))
    (reset! now 401000)
    (mem/write! store :body-idle {:jobs [{:job "j1" :waiting {:reason :nothing-to-do}}] :idle-ms 10000})
    (is (false? (read 402000)) "a plain idle record restarts the clock")
    (is (true? (read 530000)))))
