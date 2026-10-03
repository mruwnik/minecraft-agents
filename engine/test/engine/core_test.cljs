(ns engine.core-test
  (:require [cljs.test :refer [deftest is async testing]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.events :as events]
            [engine.jobs.samples :as samples]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            ["fs" :as fs]
            ["path" :as path]))

;; ---------------------------------------------------------------- test jobs

(defn ^:async count-round [c]
  (ctx/commit! c #(update % :n (fnil inc 0)))
  :continue)

(defn ^:async walk-round [c]
  (let [r (await (ctx/act c :moveTo #js {:pos (clj->js (:pos (:args c)))}))]
    (ctx/commit! c {:walked (.-status r)})
    :done))

(defn ^:async eat-round [c]
  (await (ctx/act c :eat #js {}))
  :done)

(defn ^:async fail-round [_]
  (throw (js/Error. "nope")))

(defn ^:async night-round [c]
  (if (.-isDay (.self (:primitives c)))
    :done
    {:status :continue :wake [:day]}))

(defn ^:async child-round [c]
  (let [n (:n (ctx/mem c) 0)]
    (ctx/commit! c {:n (inc n)})
    (if (>= (inc n) (:rounds (:args c) 2)) :done :continue)))

(defn ^:async parent-round [c]
  (let [a (await (ctx/step-child c :a :child {:rounds 2}))]
    (ctx/commit! c #(update % :seen (fnil conj []) (name a)))
    (if (= a :done) :done :continue)))

(defn ^:async submit-round [c]
  (ctx/submit! c :count {} {})
  :done)

(defn ^:async idle-round [c]
  (ctx/commit! c #(update % :n (fnil inc 0)))
  :not-ready)

(defn ^:async waking-child-round [_]
  {:status :not-ready :wake [:after 1005000]})

(defn ^:async waking-parent-round [c]
  (await (ctx/step-child c :a :waking-child {})))

(def flag (atom :not-yet))

(def catalog
  {:jobs {:count {:name :count :round count-round}
          :walk {:name :walk :round walk-round}
          :eat {:name :eat :round eat-round}
          :fail {:name :fail :round fail-round}
          :night {:name :night :round night-round}
          :child {:name :child :round child-round}
          :parent {:name :parent :round parent-round}
          :submitter {:name :submitter :round submit-round}
          :look-around samples/look-around
          :idle {:name :idle :round idle-round}
          :waking-child {:name :waking-child :round waking-child-round}
          :waking-parent {:name :waking-parent :round waking-parent-round}
          :gated {:name :gated :round count-round
                  :precondition (fn [_ _ _] @flag)}}
   :triggers {:hurt {:name :hurt :job :eat :persistence :retry
                     :when (fn [w _ _] (<= (.-health (.self w)) 8))}
              :hungry {:name :hungry :job :eat :persistence :cooldown :cooldown-s 30
                       :when (fn [w _ _] (< (.-food (.self w)) 10))}
              :near {:name :near :job :walk :args {:pos {:x 50 :y 64 :z 0}} :persistence :stop
                     :when (fn [w _ _] (seq (.entities w #js {:kind "hostile" :radius 8})))}
              :every-interval triggers/every-interval
              :never {:name :never :job :eat :when (constantly false)}}})

(defn setup
  ([] (setup {}))
  ([world] (setup world (tu/tmp-dir)))
  ([world dir]
   (let [clock (atom 1000000)
         [seen sink] (tu/capture-sink)
         p (tu/fake world)
         eng (core/create {:primitives p :catalog catalog :dir dir :now #(deref clock)
                           :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
     {:eng eng :p p :clock clock :seen seen :dir dir})))

(defn listed [eng] (:list (core/state eng)))
(defn job-mem [eng id] (mem/job (:store eng) [id]))
(defn ran [seen] (->> @seen (filter #(= :round_started (:kind %))) (mapv :job)))

;; ---------------------------------------------------------------- pure register

(def reg [{:id :a} {:id :b} {:id :c}])

(defn are-order [expected changes]
  (is (= expected (mapv :id (core/effective-register {:register reg :changes changes} 1000)))
      (str changes)))

(deftest effective-register-applies-moves-and-mutes
  (are-order [:a :b :c] {})
  (are-order [:a :c] {:b {:mute {:until 2000}}})
  (are-order [:a :b :c] {:b {:mute {:until 500}}})
  (are-order [:c :a :b] {:c {:position {:value {:above :a} :until 2000}}})
  (are-order [:b :a :c] {:a {:position {:value {:below :b} :until 2000}}})
  (are-order [:b :c :a] {:a {:position {:value {:above :gone} :until nil}}})
  (are-order [:c :b] {:c {:position {:value {:above :a} :until 2000}} :a {:mute {:until nil}}}))


;; ---------------------------------------------------------------- the list

(deftest round-robin-over-ready-jobs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (core/submit! eng :count {} {})
          (core/submit! eng :count {} {})
          (core/submit! eng :count {} {})
          (dotimes [_ 5] (await (core/tick! eng)))
          (is (= ["j1" "j2" "j3" "j1" "j2"] (ran seen)))
          (is (= {:n 2} (job-mem eng "j1")))
          (is (= {:n 1} (job-mem eng "j3"))))))))

(deftest done-removes-the-job-and-its-memory
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen dir]} (setup)]
          (core/submit! eng :walk {:pos {:x 3 :y 64 :z 0}} {})
          (core/submit! eng :count {} {})
          (await (core/tick! eng))
          (is (= ["j2"] (listed eng)))
          (is (false? (fs/existsSync (path/join dir "jobs" "j1.json"))))
          (is (some #(= [:completed "j1"] [(:kind %) (:job %)]) @seen))
          (await (core/tick! eng))
          (is (= ["j1" "j2"] (ran seen))))))))

(deftest preconditions-skip-and-false-warns-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (reset! flag :not-yet)
          (core/submit! eng :gated {} {})
          (is (nil? (core/tick! eng)))
          (reset! flag false)
          (core/tick! eng)
          (core/tick! eng)
          (is (= 1 (count (filter #(= :blocked (:kind %)) @seen))))
          (reset! flag true)
          (await (core/tick! eng))
          (is (= ["j1"] (ran seen))))))))

(deftest a-yield-wake-overrides-until-it-holds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup {:time 14000})]
          (core/submit! eng :night {} {})
          (await (core/tick! eng))
          (is (= [:day] (:wake (get-in (core/state eng) [:instances "j1"]))))
          (is (nil? (core/tick! eng)))
          (.setTime (.-world p) 1000)
          (await (core/tick! eng))
          (is (= [] (listed eng)))
          (is (= ["j1" "j1"] (ran seen))))))))

(deftest a-not-ready-round-without-a-wake-backs-off-for-the-recheck-interval
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (setup)]
          (core/submit! eng :idle {} {})
          (core/submit! eng :count {} {})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= ["j1" "j2" "j2"] (ran seen)) "the idle job is skipped while backing off")
          (swap! clock + 5000)
          (await (core/tick! eng))
          (is (= ["j1" "j2" "j2" "j1"] (ran seen)))
          (is (= 2 (:n (job-mem eng "j1")))))))))

(deftest a-lone-not-ready-job-is-stepped-again-only-after-the-interval
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (setup)]
          (core/submit! eng :idle {} {})
          (await (core/tick! eng))
          (swap! clock + 4999)
          (is (nil? (core/tick! eng)))
          (swap! clock + 1)
          (await (core/tick! eng))
          (is (= ["j1" "j1"] (ran seen)))
          (is (nil? (core/tick! eng))))))))

(deftest a-child-wake-is-booked-on-the-parent
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock]} (setup)]
          (core/submit! eng :waking-parent {} {})
          (await (core/tick! eng))
          (let [inst (get-in (core/state eng) [:instances "j1"])]
            (is (= [:after 1005000] (:wake inst)))
            (is (nil? (:not-before inst)) "a wake replaces the back-off"))
          (is (nil? (core/tick! eng)))
          (swap! clock + 5000)
          (await (core/tick! eng))
          (is (= 2 (:round (get-in (core/state eng) [:instances "j1"])))))))))

(deftest a-holding-job-is-always-chosen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (core/submit! eng :count {} {})
          (core/submit! eng :count {} {:hold? true})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= ["j2" "j2" "j2"] (ran seen))))))))

(deftest a-throwing-round-drops-the-job-with-a-warning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (core/submit! eng :fail {} {})
          (await (core/tick! eng))
          (is (= [] (listed eng)))
          (is (= :warn (:level (first (filter #(= :failed (:kind %)) @seen))))))))))

(deftest children-resume-by-slot-and-finish-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng dir]} (setup)]
          (core/submit! eng :parent {} {})
          (await (core/tick! eng))
          (is (= {:seen ["continue"]} (job-mem eng "j1")))
          (is (= {:n 1} (mem/job (:store eng) ["j1" :a])))
          (is (= {:n 1} (get-in (tu/read-json (path/join dir "jobs" "j1.json")) [:children :a :mem])))
          (await (core/tick! eng))
          (is (= [] (listed eng)))
          (is (false? (fs/existsSync (path/join dir "jobs" "j1.json")))))))))

(deftest a-done-child-is-not-run-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup)
              c (core/make-ctx eng {:id "j9" :path ["j9"] :chain ["j9"] :token nil :args {} :round 1})]
          (.setOwner (:primitives eng) nil)
          (is (= :continue (await (ctx/step-child c :x :child {:rounds 2}))))
          (is (= :done (await (ctx/step-child c :x :child {:rounds 2}))))
          (is (= :done (await (ctx/step-child c :x :child {:rounds 2}))))
          (is (= {} (mem/job (:store eng) ["j9" :x])))
          (is (= :continue (await (ctx/step-child c :y :child {:rounds 2})))))))))

(deftest submit-from-a-round-appends-with-the-next-id
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup)]
          (core/submit! eng :submitter {} {})
          (core/submit! eng :count {} {})
          (await (core/tick! eng))
          (is (= ["j2" "j3"] (listed eng))))))))

;; ---------------------------------------------------------------- reflexes and cuts

(deftest a-reflex-cuts-a-listed-job-which-resumes-next
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup {:inventory [{:name "bread" :count 3}]})
              world (.-world p)]
          (core/register-reflex! eng {:trigger :hurt})
          (core/submit! eng :walk {:pos {:x 5 :y 64 :z 0}} {})
          (core/submit! eng :count {} {})
          (.hold world "moveTo")
          (let [walking (core/tick! eng)]
            (set! (.. world -state -self -health) 6)
            (let [reflex-round (core/tick! eng)]
              (await walking)
              (await reflex-round)))
          (is (= {} (job-mem eng "j1")) "the cut round could not commit")
          (is (= [:fired :cut] (->> @seen (map :kind) (filter #{:fired :cut}))))
          (let [fired (first (filter #(= :fired (:kind %)) @seen))
                cut (first (filter #(= :cut (:kind %)) @seen))]
            (is (= (:seq fired) (:cause cut)))
            (is (= "j1" (:interrupted fired))))
          (set! (.. world -state -self -health) 20)
          (await (core/tick! eng))
          (is (= ["j1" "j3" "j1"] (ran seen)) "the cut job runs before j2")
          (is (= ["j2"] (listed eng))))))))

(deftest an-interval-reflex-cuts-a-listed-job-which-resumes-after-the-look
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p clock]} (setup {})
              world (.-world p)]
          (core/submit! eng :walk {:pos {:x 5 :y 64 :z 0}} {})
          (core/submit! eng :count {} {})
          (.hold world "moveTo")
          (let [walking (core/tick! eng)]
            (core/register-reflex! eng {:trigger :every-interval :args {:seconds 45}})
            (let [reflex-round (core/tick! eng)]
              (await walking)
              (await reflex-round)))
          (is (= [:fired :cut] (->> @seen (map :kind) (filter #{:fired :cut}))))
          (is (= ["look"] (mapv #(.-name %) (filter #(= "look" (.-name %)) (.-calls world)))))
          (await (core/tick! eng))
          (is (= ["j1" "j3" "j1"] (ran seen)) "the cut job resumes once the look is done")
          (swap! clock + 45000)
          (is (some? (core/tick! eng)) "the interval elapsed: it fires again"))))))

(deftest a-higher-reflex-drops-a-lower-one-and-a-lower-one-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup {:self {:food 5}
                                          :entities [{:id 1 :name "zombie" :kind "hostile" :pos {:x 3 :y 64 :z 0}}]})
              world (.-world p)]
          (core/register-reflex! eng {:trigger :near})
          (core/register-reflex! eng {:trigger :hungry})
          (.hold world "moveTo")
          (let [release-eat (.hold world "eat")
                r1 (core/tick! eng)]
            (is (= :near (:reflex (core/running eng))) "the first true entry in order fires")
            (core/move! eng :hungry {:below :near} 60)
            (is (nil? (core/tick! eng)) "a lower reflex does not interrupt")
            (core/move! eng :hungry {:above :near} 60)
            (let [r2 (core/tick! eng)]
              (await r1)
              (is (= :hungry (:reflex (core/running eng))))
              (is (some #(and (= :ended (:kind %)) (= "dropped" (name (:how %)))) @seen))
              (release-eat)
              (await r2)
              (is (nil? (core/running eng))))))))))

(deftest persistence-cooldown-retry-and-stop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (testing "cooldown"
          (let [{:keys [eng seen clock]} (setup {:self {:food 5}})]
            (core/register-reflex! eng {:trigger :hungry})
            (await (core/tick! eng))
            (is (some #(= "completed_not_cleared" (some-> (:how %) name)) @seen))
            (is (nil? (core/tick! eng)) "cooling down")
            (swap! clock + 31000)
            (await (core/tick! eng))
            (is (= 2 (count (ran seen))))))
        (testing "retry"
          (let [{:keys [eng seen]} (setup {:self {:health 5}})]
            (core/register-reflex! eng {:trigger :hurt})
            (await (core/tick! eng))
            (await (core/tick! eng))
            (is (= 2 (count (ran seen))))))
        (testing "stop until the condition was false once"
          (let [{:keys [eng seen p]} (setup {:entities [{:id 1 :name "zombie" :kind "hostile" :pos {:x 3 :y 64 :z 0}}]
                                             :unreachable ["50,64,0"]})
                state (.. p -world -state)]
            (core/register-reflex! eng {:trigger :near})
            (await (core/tick! eng))
            (is (nil? (core/tick! eng)))
            (set! (.-entities state) #js [])
            (core/tick! eng)
            (set! (.-entities state) #js [#js {:id 2 :name "zombie" :kind "hostile" :pos (tu/pos 3 64 0) :health 20}])
            (await (core/tick! eng))
            (is (= 2 (count (ran seen))))))))))

(deftest mutes-carry-a-ttl-replace-and-revert
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (setup {:self {:health 5}})]
          (core/register-reflex! eng {:trigger :hurt})
          (core/mute! eng :hurt 10)
          (core/mute! eng :hurt 60)
          (is (nil? (core/tick! eng)))
          (swap! clock + 30000)
          (is (nil? (core/tick! eng)) "the second mute replaced the first")
          (swap! clock + 31000)
          (await (core/tick! eng))
          (is (= 1 (count (filter #(= :reverted (:kind %)) @seen))))
          (is (= 1 (count (ran seen)))))))))

(deftest built-in-reflexes-cannot-be-removed
  (let [{:keys [eng]} (setup)]
    (core/register-reflex! eng {:trigger :hurt :builtin? true})
    (core/register-reflex! eng {:trigger :never})
    (is (false? (core/remove-reflex! eng :hurt)))
    (is (true? (core/remove-reflex! eng :never)))
    (is (= [:hurt] (mapv :id (:register (core/state eng)))))))

(deftest do-now-cuts-and-holds-at-the-front-and-cancel-removes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup)]
          (core/submit! eng :walk {:pos {:x 5 :y 64 :z 0}} {})
          (.hold (.-world p) "moveTo")
          (let [walking (core/tick! eng)]
            (core/do-now! eng :count {})
            (await walking))
          (is (= ["j2" "j1"] (listed eng)))
          (await (core/tick! eng))
          (await (core/tick! eng))
          (core/cancel! eng "j2")
          (await (core/tick! eng))
          (is (= ["j1" "j2" "j2" "j1"] (ran seen)))
          (is (some #(= :cancelled (:kind %)) @seen)))))))

;; ---------------------------------------------------------------- body events and restart

(deftest body-events-become-records
  (let [{:keys [eng p seen]} (setup)]
    (.emit (.-world p) #js {:kind "hurt" :health 5})
    (is (= [5] (mapv :health (mem/records (mem/snapshot (:store eng)) "hurt"))))
    (is (some #(= [:body :hurt] [(:source %) (:kind %)]) @seen))))

(deftest restart-restores-the-list-register-and-changes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              {:keys [eng p]} (setup {} dir)]
          (core/register-reflex! eng {:trigger :hurt})
          (core/register-reflex! eng {:trigger :never})
          (core/mute! eng :never 600)
          (core/submit! eng :walk {:pos {:x 5 :y 64 :z 0}} {})
          (core/submit! eng :count {} {})
          (.hold (.-world p) "moveTo")
          (core/tick! eng)
          (let [{again :eng seen :seen} (setup {} dir)
                s (core/state again)]
            (is (= ["j1" "j2"] (:list s)))
            (is (= [:hurt :never] (mapv :id (:register s))))
            (is (= #{:never} (set (keys (:changes s)))))
            (is (= "j1" (:resume s)) "the in-flight round is lost and resumes first")
            (is (= 2 (count (mem/records (mem/snapshot (:store again)) "restart"))) "one per start")
            (is (some #(= :restored (:kind %)) @seen))
            (core/submit! again :count {} {})
            (is (= ["j1" "j2" "j3"] (listed again)) "ids continue")
            (await (core/tick! again))
            (is (= ["j1"] (ran seen)))))))))
