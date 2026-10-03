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

(defn ^:async parent-walk-round [c]
  (ctx/commit! c {:before true})
  (await (ctx/act c :moveTo #js {:pos #js {:x 5 :y 64 :z 0}}))
  :done)

(defn ^:async eat-round [c]
  (await (ctx/act c :eat #js {}))
  :done)

(defn ^:async fail-round [_]
  (throw (js/Error. "nope")))

(defn ^:async bad-result-round [_]
  :not-ready)

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

(defn ^:async declined-parent-round [c]
  (let [r (await (ctx/step-child c :a :gated {}))]
    (ctx/commit! c {:child r})
    :continue))

(def flag (atom false))

(def always (constantly true))

(def catalog
  {:jobs {:count {:name :count :check always :round count-round}
          :walk {:name :walk :check always :round walk-round}
          :parent-walk {:name :parent-walk :check always :round parent-walk-round}
          :eat {:name :eat :check always :round eat-round}
          :fail {:name :fail :check always :round fail-round}
          :bad-result {:name :bad-result :check always :round bad-result-round}
          :child {:name :child :check always :round child-round}
          :parent {:name :parent :check always :round parent-round}
          :declined-parent {:name :declined-parent :check always :round declined-parent-round}
          :submitter {:name :submitter :check always :round submit-round}
          :look-around samples/look-around
          :gated {:name :gated :round count-round
                  :check (fn [_] @flag)}
          :no-check {:name :no-check :round count-round}}
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

(deftest a-declining-job-is-skipped-and-costs-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (reset! flag false)
          (core/submit! eng :gated {} {})
          (is (nil? (core/tick! eng)) "every check declines: idle")
          (core/submit! eng :count {} {})
          (dotimes [_ 2] (await (core/tick! eng)))
          (is (= ["j2" "j2"] (ran seen)))
          (reset! flag true)
          (dotimes [_ 2] (await (core/tick! eng)))
          (is (= ["j2" "j2" "j1" "j2"] (ran seen)) "round-robin once it passes")
          (is (not-any? #(= :blocked (:kind %)) @seen)))))))

(deftest the-check-sees-the-job-memory-and-args
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [seen-ctx (atom nil)
              cat (assoc-in catalog [:jobs :peek] {:name :peek :round count-round
                                                   :check (fn [c] (reset! seen-ctx [(:args c) (ctx/mem c)]) true)})
              {:keys [eng]} (setup)
              eng (assoc eng :catalog cat)]
          (core/submit! eng :peek {:a 1} {})
          (await (core/tick! eng))
          (core/tick! eng)
          (is (= [{:a 1} {:n 1}] @seen-ctx)))))))

(deftest a-job-without-a-check-is-refused
  (let [{:keys [eng]} (setup)]
    (is (thrown? js/Error (core/submit! eng :no-check {} {})))))

(deftest a-round-returning-anything-else-fails-the-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (core/submit! eng :bad-result {} {})
          (await (core/tick! eng))
          (is (= [] (listed eng)))
          (is (some #(= :failed (:kind %)) @seen)))))))

(deftest a-holding-job-whose-check-declines-idles-the-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (reset! flag false)
          (core/submit! eng :count {} {})
          (core/submit! eng :gated {} {:hold? true})
          (is (nil? (core/tick! eng)) "the holder declines; the others wait")
          (reset! flag true)
          (dotimes [_ 2] (await (core/tick! eng)))
          (is (= ["j2" "j2"] (ran seen))))))))

(deftest a-declining-child-returns-declined
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup)]
          (reset! flag false)
          (core/submit! eng :declined-parent {} {})
          (await (core/tick! eng))
          (is (= "declined" (name (:child (job-mem eng "j1"))))))))))

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

;; ---------------------------------------------------------------- shutdown

(deftest a-cut-the-engine-did-not-make-keeps-the-job-listed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup)]
          (.override (.-world p) "moveTo"
                     (fn [_ _ _] (js/Promise.reject (core/cut-error))))
          (core/submit! eng :parent-walk {} {})
          (await (core/tick! eng))
          (is (= ["j1"] (listed eng)) "a cut is not a failure")
          (is (= {:before true} (job-mem eng "j1")) "its memory survives")
          (is (= "j1" (:resume (core/state eng))))
          (is (not-any? #(= :failed (:kind %)) @seen))
          (is (some #(= :cut (:kind %)) @seen)))))))

(deftest shutdown-keeps-the-in-flight-job-for-the-restart
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              {:keys [eng p seen]} (setup {} dir)]
          (core/submit! eng :parent-walk {} {})
          (.hold (.-world p) "moveTo")
          (let [walking (core/tick! eng)]
            (core/shutdown! eng)
            (await walking))
          (is (= ["j1"] (listed eng)))
          (is (not-any? #(= :failed (:kind %)) @seen))
          (let [{again :eng seen2 :seen} (setup {} dir)]
            (is (= ["j1"] (listed again)))
            (is (= {:before true} (job-mem again "j1")))
            (is (= "j1" (:resume (core/state again))))
            (await (core/tick! again))
            (is (= ["j1"] (ran seen2)) "the body resumes from the list")))))))
