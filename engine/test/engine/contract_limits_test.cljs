(ns engine.contract-limits-test
  "A call's limits (engine.contract, engine.core.limits): clamped to the caller's, checked at acts and call boundaries,
  the outermost crossed frame ends :limit-reached when enforced, job.limit_would_end only in shadow mode."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [jobs.apiary.maintain :as apiary-maintain]
            [jobs.village.maintain :as village-maintain]))

(def world (atom {}))
(def clock (atom 0))

(defn setup [jobs]
  (reset! world {})
  (reset! clock 1000000)
  (let [[seen sink] (tu/capture-sink)
        p (tu/fake {})
        eng (core/create {:primitives p :dir (tu/tmp-dir) :now #(deref clock) :jobs jobs :triggers {}
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :seen seen :p p}))

(defn job-ev [seen kind] (filterv #(= [:job kind] [(:source %) (:kind %)]) @seen))
(defn stopped [seen] (select-keys (:data (first (job-ev seen :stopped))) [:kind :cause]))
(defn raw-mem [eng root slots] (mem/job-mem (mem/view (:store eng)) root slots))
(defn looks [p] (count (filter #(= "look" (.-name %)) (.-calls (.-world p)))))

(defn enforced [f] (settings/with-settings {:engine.contract/enforce-limits true} f))

(defn look! [c] (ctx/act c :look #js {:yaw 0 :pitch 0}))

(defn hurt! [c health] (fake/swap-self! (:primitives c) assoc :health health))

(def slow
  {:conditions [] :defaults {:spend {:time 5}}
   :round (fn [_] (swap! clock + 6000) (swap! world update :slow-rounds (fnil inc 0)) :continue)})

(deftest its-own-clock-ends-a-job-past-its-time
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (enforced
                (fn ^:async enforced-time []
                  (let [{:keys [eng seen]} (setup {'slow slow})]
                    (core/submit! eng '(slow) {})
                    (await (core/tick! eng))
                    (await (core/tick! eng))
                    (is (= 1 (:slow-rounds @world)) "the round after the crossing does not run")
                    (is (= {:kind :limit-reached :cause {:line :time :spend 5}} (stopped seen)))))))))))

(deftest shadow-mode-tells-once-and-ends-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'slow slow})
              id (core/submit! eng '(slow) {})]
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= 3 (:slow-rounds @world)))
          (is (= [id] (:list (core/state eng))))
          (let [told (job-ev seen :limit_would_end)
                data (:data (first told))]
            (is (= 1 (count told)) "once per frame and line")
            (is (= {:line :time :spend 5} (:cause data)))
            (is (= {:spend {:time 5} :least 1} (:call data)))
            (is (= 6 (get-in data [:report :uses :time])))))))))

;; ---------------------------------------------------------------- handed down

(defn kid [f]
  {:conditions []
   :round (fn ^:async kid-round [c]
            (swap! world assoc :kid-call (ctx/frozen-call c))
            (await (f c))
            (await (look! c))
            (swap! world update :kid-acts (fnil inc 0))
            :continue)})

(defn caller [kid-def kid-args]
  {:conditions []
   :round (fn ^:async caller-round [c]
            (let [e (await (ctx/call c :kid kid-def kid-args))]
              (swap! world assoc :kid-ending e)
              :continue))})

(deftest a-deadline-is-handed-down-and-ends-the-outermost-frame
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (enforced
                (fn ^:async enforced-deadline []
                  (let [jobs {'top (caller (kid (fn [_] (swap! clock + 5000))) {:deadline 9000000})}
                        {:keys [eng seen]} (setup jobs)
                        id (core/submit! eng '(top {:deadline 1002000}) {})]
                    (await (core/tick! eng))
                    (is (= 1002000 (:deadline (:kid-call @world))) "clamped to the caller's")
                    (is (= [[:deadline]] (:lines (:data (first (job-ev seen :budget_clamped))))))
                    (is (nil? (:kid-acts @world)) "the act past the deadline did not run")
                    (is (nil? (:kid-ending @world)) "the child's boundary passed it on")
                    (is (= {:kind :limit-reached :cause {:line :deadline :deadline 1002000}} (stopped seen)))
                    (is (empty? (raw-mem eng id [])))))))))))

(deftest a-clamped-child-and-its-caller-cross-together-and-the-caller-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (enforced
                (fn ^:async enforced-clamp []
                  (let [jobs {'top (merge (caller (kid (fn [_] (swap! clock + 11000))) {:spend {:time 60 :health 4}})
                                          {:defaults {:spend {:time 10}}})}
                        {:keys [eng seen]} (setup jobs)]
                    (core/submit! eng '(top) {})
                    (await (core/tick! eng))
                    (is (= {:time 10 :health 4} (:spend (:kid-call @world))) "time held to the caller's; health the caller has no line for")
                    (is (= [[:spend :time]] (:lines (:data (first (job-ev seen :budget_clamped))))))
                    (is (= {:kind :limit-reached :cause {:line :time :spend 10}} (stopped seen)))))))))))

(deftest a-mob-hit-ends-the-child-whose-floor-it-crosses-not-the-caller
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (enforced
                (fn ^:async enforced-hit []
                  (let [jobs {'top (merge (caller (kid #(hurt! % 12)) {:floor {:health 15}}) {:defaults {:floor {:health 5}}})}
                        {:keys [eng seen]} (setup jobs)
                        id (core/submit! eng '(top) {})]
                    (await (core/tick! eng))
                    (is (= {:kind :limit-reached :cause {:line :health :floor 15}} (dissoc (:kid-ending @world) :report)))
                    (is (= [id] (:list (core/state eng))) "the caller goes on")
                    (is (empty? (job-ev seen :stopped)))))))))))

(deftest a-callers-floor-ends-the-caller-from-a-grandchild
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (enforced
                (fn ^:async enforced-grandchild []
                  (let [grandchild (kid #(hurt! % 11))
                        jobs {'top (merge (caller (caller grandchild {}) {}) {:defaults {:floor {:health 12}}})}
                        {:keys [eng seen]} (setup jobs)]
                    (core/submit! eng '(top) {})
                    (await (core/tick! eng))
                    (is (= {:health 12} (:floor (:kid-call @world))) "the floor is handed down")
                    (is (nil? (:kid-ending @world)) "neither boundary below the caller's ends it")
                    (is (= {:kind :limit-reached :cause {:line :health :floor 12}} (stopped seen)))))))))))

(deftest a-child-called-below-its-floor-fails-its-conditions
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (enforced
                (fn ^:async enforced-below []
                  (let [{:keys [eng p]} (setup {'top (caller (kid (fn [_] nil)) {:floor {:health 15}})})]
                    (fake/swap-self! p assoc :health 12)
                    (core/submit! eng '(top) {})
                    (await (core/tick! eng))
                    (is (= {:kind :condition-failed :cause {:floor :health :min 15}} (dissoc (:kid-ending @world) :report)))
                    (is (nil? (:kid-acts @world)))))))))))

(deftest a-listed-job-below-its-floor-from-settings-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (enforced
                (fn ^:async enforced-wait []
                  (let [careful {:conditions [] :defaults {:floor {:health ::min-health}} :settings {::min-health {:default 10}}
                                 :round (fn [_] (swap! world update :careful-rounds (fnil inc 0)) :continue)}
                        {:keys [eng seen p]} (setup {'careful careful})]
                    (fake/swap-self! p assoc :health 8)
                    (core/submit! eng '(careful) {})
                    (await (core/tick! eng))
                    (is (nil? (:careful-rounds @world)))
                    (is (= {:reason :health :cause {:floor :health :min 10}}
                           (select-keys (:data (first (job-ev seen :waiting))) [:reason :cause])))
                    (fake/swap-self! p assoc :health 20)
                    (await (core/tick! eng))
                    (is (= 1 (:careful-rounds @world)))
                    (fake/swap-self! p assoc :health 8)
                    (await (core/tick! eng))
                    (is (= 1 (:careful-rounds @world)) "a started job waits too, as after a reflex cut")))))))))

;; ---------------------------------------------------------------- unwinding

(def cleans-up
  {:conditions [] :defaults {:floor {:health 12}}
   :round (fn ^:async cleans-up-round [c]
            (hurt! c 11)
            (try
              (await (look! c))
              (swap! world assoc :went-on true)
              (catch :default e
                (swap! world assoc :caught (contract/passes-through? e))
                (await (look! c))
                (try (await (ctx/call c :kid (kid (fn [_] nil)) {}))
                     (catch :default e2 (swap! world assoc :call-refused (contract/passes-through? e2))))
                (throw e)))
            :continue)})

(deftest an-unwinding-frame-may-act-but-not-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (enforced
                (fn ^:async enforced-unwind []
                  (let [{:keys [eng seen p]} (setup {'cleans-up cleans-up})]
                    (core/submit! eng '(cleans-up) {})
                    (await (core/tick! eng))
                    (is (true? (:caught @world)))
                    (is (nil? (:went-on @world)))
                    (is (= 1 (looks p)) "the cleanup act ran; the first one was stopped before it")
                    (is (true? (:call-refused @world)))
                    (is (nil? (:kid-call @world)))
                    (is (= {:kind :limit-reached :cause {:line :health :floor 12}} (stopped seen)))))))))))

(def shuts-the-door
  {:conditions [] :defaults {:floor {:health 12}}
   :round (fn ^:async door-round [c]
            (hurt! c 11)
            (await (ctx/atomic c :comfort :door (fn ^:async door [] (await (look! c)) (await (look! c)))))
            (swap! world assoc :after-section true)
            :continue)})

(deftest a-line-crossed-inside-an-atomic-section-is-held-until-it-closes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (enforced
                (fn ^:async enforced-atomic []
                  (let [{:keys [eng seen p]} (setup {'door shuts-the-door})]
                    (core/submit! eng '(door) {})
                    (await (core/tick! eng))
                    (is (= 2 (looks p)) "both acts of the section ran")
                    (is (nil? (:after-section @world)))
                    (is (= {:kind :limit-reached :cause {:line :health :floor 12}} (stopped seen)))))))))))

(defn rejected [p] (-> p (.then (constantly nil)) (.catch identity)))

(deftest a-maintain-catch-all-passes-a-limit-through
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (contract/limit-error "j1" {:line :time :spend 5})
              c {:update-mem (fn [_ _] (swap! world assoc :booked true))}]
          (reset! world {})
          (is (contract/passes-through? e))
          (is (contract/passes-through? (doto (js/Error. "cut") (aset "code" "cut"))))
          (is (not (contract/passes-through? (js/Error. "boom"))))
          (let [runs (with-redefs [ctx/call-child (fn [_ _ _ _] (js/Promise.reject e))
                       ctx/call (fn [_ _ _ _] (js/Promise.reject e))]
                       [(rejected (village-maintain/run-child! c :roll-1 {}))
                        (rejected (apiary-maintain/run-child! c :hives {}))])
                thrown (await (js/Promise.all (clj->js runs)))]
            (is (every? #(identical? e %) thrown))
            (is (nil? (:booked @world)) "not booked as a failed step")))))))

(defn told [seen]
  (mapv #(select-keys (:data %) [:name :cause :used :health]) (job-ev seen :limit_would_end)))

(deftest shadow-mode-tells-every-crossed-frame-and-line-with-its-amount
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [jobs {'top (merge (caller (kid (fn [c] (swap! clock + 6000) (hurt! c 12))) {:spend {:time 5}})
                                {:defaults {:spend {:time 10} :floor {:health 15}}})}
              {:keys [eng seen]} (setup jobs)]
          (core/submit! eng '(top) {})
          (await (core/tick! eng))
          (is (= [{:name 'top :cause {:line :health :floor 15} :health 12}
                  {:cause {:line :health :floor 15} :health 12}
                  {:cause {:line :time :spend 5} :used 6}]
                 (told seen))
              "the caller's floor, the child's floor and its time, the round in flight counted")
          (is (= 6 (get-in (:data (last (job-ev seen :limit_would_end))) [:report :uses :time])))
          (await (core/tick! eng))
          (is (= {:name 'top :cause {:line :time :spend 10} :used 12} (last (told seen))))
          (is (= 4 (count (told seen))) "each frame and line once"))))))

(deftest shadow-mode-holds-no-floor
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [careful {:conditions [] :defaults {:floor {:health 10}}
                       :round (fn [_] (swap! world update :careful-rounds (fnil inc 0)) :continue)}
              {:keys [eng seen p]} (setup {'top (caller (kid (fn [_] nil)) {:floor {:health 15}}) 'careful careful})]
          (fake/swap-self! p assoc :health 8)
          (core/submit! eng '(top) {})
          (core/submit! eng '(careful) {})
          (await (core/tick! eng))
          (await (core/tick! eng))
          (is (pos? (:kid-acts @world 0)) "a child called below its floor runs")
          (is (= :continue (:kid-ending @world)))
          (is (pos? (:careful-rounds @world 0)) "a listed job below its floor runs")
          (is (empty? (job-ev seen :waiting))))))))

(def cleans-up-old-call
  {:conditions [] :defaults {:floor {:health 12}}
   :round (fn ^:async cleans-up-old-round [c]
            (hurt! c 11)
            (try
              (await (look! c))
              (catch :default e
                (try (await (ctx/call-child c :old {:check (constantly true)
                                                    :round (fn [_] (swap! world assoc :old-kid-ran true) :done)} {}))
                     (catch :default e2 (swap! world assoc :call-refused (contract/passes-through? e2))))
                (throw e)))
            :continue)})

(deftest an-unwinding-frame-may-not-call-an-old-child-either
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (enforced
                (fn ^:async enforced-old-call []
                  (let [{:keys [eng seen]} (setup {'cleans-up cleans-up-old-call})]
                    (core/submit! eng '(cleans-up) {})
                    (await (core/tick! eng))
                    (is (true? (:call-refused @world)))
                    (is (nil? (:old-kid-ran @world)))
                    (is (= {:kind :limit-reached :cause {:line :health :floor 12}} (stopped seen)))))))))))
