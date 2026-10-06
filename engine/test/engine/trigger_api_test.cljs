(ns engine.trigger-api-test
  (:require [cljs.test :refer [deftest is are async]]
            [cljs.reader :as reader]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.event-api :as event-api]
            [engine.scenario :as scenario]
            [engine.trigger-api :as api]
            [engine.test-util :as tu]
            ["http" :as http]
            ["path" :as path]))

(def t0 1000000)

(def flags
  "The fake conditions' world: (flag :k) holds while :k is in here."
  (atom #{}))

(def status (atom "blocked"))

(def level (atom 0))

(defn fake-compile
  "Stand-in for the condition seam: (flag :k) compiles; anything else is refused the way engine.condition refuses."
  [form]
  (if (and (seq? form) (= 'flag (first form)) (keyword? (second form)))
    {:ok true :when (fn [& _] (contains? @flags (second form)))}
    {:ok false :reason :unknown-symbol :at form :message (str "unknown " (pr-str form)) :allowed ['(flag :k)]}))

(defn refuse-all [form]
  {:ok false :reason :unknown-symbol :at form :message "no longer known" :allowed []})

(defn ^:async bump-round
  "One moveTo answered with @status, then done: a fruitless round while @status is a failure."
  [c]
  (await (ctx/act c :moveTo #js {:pos #js {:x 0 :y 64 :z 0} :status @status}))
  :done)

(def always (constantly true))

(def jobs
  {'quick {:check always :round (fn [_] :done) :args {}}
   'spin {:check always :round (fn [_] :continue) :args {}}
   'bump {:check always :round bump-round :args {}}})

(def base-triggers
  {:high {:name :high :when (fn [_ _ args & _] (>= @level (:at args 5))) :job '(quick) :args {:at 5}
          :persistence :cooldown :cooldown-s 5}})

(defn triggers
  ([] (triggers fake-compile))
  ([compile] (api/with-conditions base-triggers compile)))

(defn setup
  ([] (setup {}))
  ([{:keys [dir compile] :or {compile fake-compile}}]
   (let [clock (atom t0)
         [seen sink] (tu/legacy-capture-sink)
         p (tu/fake {})
         eng (core/create {:primitives p :jobs jobs :triggers (triggers compile) :dir (or dir (tu/tmp-dir))
                           :now #(deref clock)
                           :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
     (reset! flags #{})
     (reset! status "blocked")
     (reset! level 0)
     (.override (.-world p) "moveTo"
                (fn [_ args _] (js/Promise. (fn [resolve] (js/setTimeout #(resolve #js {:status (.-status args)}) 0)))))
     (api/restore-conditions! eng)
     {:eng eng :p p :seen seen :clock clock})))

(defn ^:async tick-at [{:keys [eng clock]} t]
  (reset! clock t)
  (api/tick! eng)
  (await (core/tick! eng)))

(defn of-kind [seen source kind] (filterv #(and (= source (:source %)) (= kind (:kind %))) @seen))
(defn fired [seen] (count (of-kind seen :reflex :fired)))
(defn register-ids [eng] (mapv :id (:register (core/state eng))))
(defn entry [eng id] (some #(when (= id (:id %)) %) (:register (core/state eng))))

(def adhoc {:op :put :id :bread-low :when '(flag :low) :job '(quick) :by "steward"})

;; ---------------------------------------------------------------- validation (route and scenario)

(deftest entry-problems-name-the-offending-part
  (are [e reason at] (= [reason at] ((juxt :reason :at) (api/entry-problem jobs (triggers) e)))
    {:trigger :nope} :unknown-trigger [:trigger]
    {:trigger :high :when '(flag :a) :id :x :job '(quick)} :trigger-or-when []
    {:id :x :job '(quick)} :trigger-or-when []
    {:trigger :condition :id :x :job '(quick)} :bad-trigger [:trigger]
    {:trigger :high :job '(fly)} :bad-job [:job]
    {:trigger :high :job '(hold (quick))} :bad-job [:job]
    {:trigger :high :persistence :sometimes} :bad-persistence [:persistence]
    {:trigger :high :cooldown-s -1} :bad-cooldown [:cooldown-s]
    {:trigger :high :backoff {:after 0}} :bad-backoff [:backoff]
    {:trigger :high :ttl-s 0} :bad-ttl [:ttl-s]
    {:trigger :high :args 3} :bad-args [:args]
    {:trigger :high :colour :red} :unknown-key [:colour]
    {:trigger :high :id "x"} :bad-id [:id]
    {:when '(flag :a) :job '(quick)} :missing-id [:id]
    {:when '(flag :a) :id :x} :missing-job [:job]
    {:when '(flag :a) :id :x :job '(quick) :args {:a 1}} :bad-args [:args]
    {:when '(nope 1) :id :x :job '(quick)} :bad-condition [:when])
  (are [e] (nil? (api/entry-problem jobs (triggers) e))
    {:trigger :high}
    {:trigger :high :id :high-3 :args {:at 3} :persistence :stop :cooldown-s 0 :backoff false}
    {:id :x :when '(flag :a) :job '(quick) :persistence :cooldown :cooldown-s 2 :ttl-s 60}))

(deftest a-refused-condition-passes-its-own-refusal-through
  (let [r (api/entry-problem jobs (triggers) {:when '(nope 1) :id :x :job '(quick)})]
    (is (= {:reason :unknown-symbol :at '(nope 1) :allowed ['(flag :k)]}
           (select-keys (:condition r) [:reason :at :allowed])))
    (is (re-find #"unknown \(nope 1\)" (:message r)))))

(deftest without-the-condition-trigger-an-adhoc-entry-is-refused
  (is (= :conditions-unavailable
         (:reason (api/entry-problem jobs base-triggers {:when '(flag :a) :id :x :job '(quick)})))))

(deftest requests-are-refused-as-data
  (let [{:keys [eng]} (setup)]
    (api/request! eng {:op :put :id :high :trigger :high})
    (are [r reason at] (= [false reason at] ((juxt :ok :reason :at) (api/request! eng r)))
      {:op :fly :id :high} :bad-op [:op]
      [:op :put] :bad-request []
      {:op :put :id :x :when '(flag :a) :job '(quick) :builtin? true} :unknown-key [:builtin?]
      {:op :put :trigger :high :generation-id "old"} :generation-mismatch [:generation-id]
      {:op :put :trigger :high :by 7} :bad-by [:by]
      {:op :put :id :x :when '(nope 1) :job '(quick)} :bad-condition [:when]
      {:op :remove :id :none} :no-such-trigger [:id]
      {:op :remove} :bad-id [:id]
      {:op :mute :id :high :ttl-s -3} :bad-ttl [:ttl-s]
      {:op :mute :id :high :colour 1} :unknown-key [:colour]
      {:op :move :id :high} :bad-anchor [:above]
      {:op :move :id :high :above :none} :no-such-trigger [:above]
      {:op :move :id :high :above :high} :bad-anchor [:above]
      {:op :clear :id :high :property :colour} :bad-property [:property])
    (is (= [:high] (register-ids eng)) "nothing changed")))

(deftest scenario-problems-use-the-same-validation
  (let [ts (triggers)]
    (is (= [] (scenario/problems jobs ts '{:register [{:trigger :high} {:id :bread-low :when (flag :low) :job (quick)}]
                                           :queue [(quick)]})))
    (let [ps (scenario/problems jobs ts '{:register [{:id :x :when (nope 1) :job (quick)}
                                                     {:trigger :high} {:trigger :high}]
                                          :queue []})]
      (is (= 2 (count ps)))
      (is (re-find #"unknown \(nope 1\)" (first ps)))
      (is (re-find #"duplicate register id :high" (second ps))))))

;; ---------------------------------------------------------------- put, remove, mute, move, clear

(deftest put-twice-is-one-entry-replaced-in-place
  (let [{:keys [eng seen]} (setup)
        first-put (api/request! eng {:op :put :id :high-3 :trigger :high :args {:at 3} :by "steward"})]
    (api/request! eng adhoc)
    (is (= {:ok true :op :put :id :high-3 :created? true} (select-keys first-put [:ok :op :id :created?])))
    (swap! (:state eng) assoc-in [:reflex-state :high-3 :stopped?] true)
    (let [again (api/request! eng {:op :put :id :high-3 :trigger :high :args {:at 4} :by "builder"})]
      (is (false? (:created? again)))
      (is (= [:high-3 :bread-low] (register-ids eng)) "one entry, still in its place")
      (is (= {:at 4} (:args (entry eng :high-3))))
      (is (= "builder" (:by (entry eng :high-3))))
      (is (nil? (get-in (core/state eng) [:reflex-state :high-3])) "a replaced entry starts unlatched"))
    (is (= [false true] (mapv :replaced (filterv #(= :high-3 (:reflex %)) (of-kind seen :reflex :changed))))
        "one event per applied put")))

(deftest a-builtin-instance-takes-trigger-defaults-and-records-who-added-it
  (let [{:keys [eng]} (setup)]
    (api/request! eng {:op :put :trigger :high :by "steward"})
    (is (= {:id :high :trigger :high :job '(quick) :args {:at 5} :persistence :cooldown :cooldown-s 5
            :builtin? false :by "steward"}
           (entry eng :high)))
    (is (= {:id :bread-low :trigger :condition :when '(flag :low) :job '(quick) :args {:id :bread-low}
            :persistence :stop :cooldown-s 0 :builtin? false :by "steward"}
           (do (api/request! eng adhoc) (entry eng :bread-low)))
        "an ad hoc entry defaults to :stop: fire when true, re-arm when false")))

(deftest remove-mute-move-and-clear
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup)]
          (api/load-scenario! eng '{:register [{:trigger :high :builtin? true} {:id :spare :trigger :high}]})
          (is (= {:ok false :reason :builtin :at [:id]}
                 (select-keys (api/request! eng {:op :remove :id :high}) [:ok :reason :at])))
          (is (= {:ok false :reason :builtin :at [:id]}
                 (select-keys (api/request! eng {:op :put :id :high :trigger :high}) [:ok :reason :at])))
          (is (:ok (api/request! eng {:op :remove :id :spare :by "steward"})))
          (is (= [:high] (register-ids eng)))
          (is (:ok (api/request! eng {:op :mute :id :high :ttl-s 10})))
          (reset! level 9)
          (await (tick-at r t0))
          (is (= 0 (fired seen)) "muted")
          (await (tick-at r (+ t0 10000)))
          (is (= 1 (fired seen)) "the mute ran out and reverted")
          (is (= [:mute] (mapv :property (of-kind seen :reflex :reverted))))
          (api/request! eng adhoc)
          (is (:ok (api/request! eng {:op :move :id :bread-low :above :high})))
          (is (= [:bread-low :high] (:order (api/triggers-view eng))))
          (is (:ok (api/request! eng {:op :clear :id :bread-low :property :position})))
          (is (= [:high :bread-low] (:order (api/triggers-view eng)))))))))

(deftest an-entry-with-a-ttl-expires-with-an-event
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup)]
          (api/request! eng (assoc adhoc :ttl-s 10))
          (await (tick-at r (+ t0 9999)))
          (is (= [:bread-low] (register-ids eng)))
          (await (tick-at r (+ t0 10000)))
          (is (= [] (register-ids eng)))
          (is (= [:bread-low] (mapv :reflex (of-kind seen :reflex :expired)))))))))

;; ---------------------------------------------------------------- ad hoc conditions

(deftest an-adhoc-trigger-fires-on-true-latches-and-rearms-on-false
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup)]
          (is (:ok (api/request! eng adhoc)))
          (await (tick-at r t0))
          (is (= 0 (fired seen)) "false: nothing")
          (swap! flags conj :low)
          (await (tick-at r (+ t0 250)))
          (is (= 1 (fired seen)))
          (dotimes [i 4] (await (tick-at r (+ t0 500 (* i 250)))))
          (is (= 1 (fired seen)) "still true after the job: latched, no spin")
          (swap! flags disj :low)
          (await (tick-at r (+ t0 2000)))
          (swap! flags conj :low)
          (await (tick-at r (+ t0 2250)))
          (is (= 2 (fired seen)) "false once re-armed it"))))))

(deftest each-adhoc-entry-answers-with-its-own-condition
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup)]
          (api/request! eng (assoc adhoc :id :a :when '(flag :a)))
          (api/request! eng (assoc adhoc :id :b :when '(flag :b)))
          (swap! flags conj :b)
          (await (tick-at r t0))
          (is (= [:b] (mapv :reflex (of-kind seen :reflex :fired)))))))))

(deftest a-failing-adhoc-job-backs-off-and-raises-one-attention-request
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup)
              requests #(filterv (fn [[_ q]] (= :reflex-backoff (:reason q))) (core/outstanding eng))]
          (api/request! eng (assoc adhoc :job '(bump) :persistence :cooldown :cooldown-s 0))
          (swap! flags conj :low)
          (dotimes [_ 3] (await (tick-at r t0)))
          (is (= 3 (fired seen)))
          (await (tick-at r (+ t0 500)))
          (is (= 3 (fired seen)) "backing off")
          (is (= 1 (count (requests))))
          (await (tick-at r (+ t0 600)))
          (is (= 1 (count (requests))) "one request while it lasts")
          (is (= 1 (count (filterv #(= :required (:attention %)) (of-kind seen :job :reflex-backoff)))))
          (api/request! eng {:op :remove :id :bread-low})
          (api/tick! eng)
          (is (= [] (requests)) "resolved when the entry goes"))))))

(deftest the-backoff-request-resolves-when-the-job-recovers
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng] :as r} (setup)
              requests #(filterv (fn [[_ q]] (= :reflex-backoff (:reason q))) (core/outstanding eng))]
          (api/request! eng (assoc adhoc :job '(bump) :persistence :cooldown :cooldown-s 0))
          (swap! flags conj :low)
          (dotimes [_ 3] (await (tick-at r t0)))
          (await (tick-at r (+ t0 500)))
          (is (= 1 (count (requests))))
          (reset! status "arrived")
          (await (tick-at r (+ t0 1000)))
          (await (tick-at r (+ t0 1250)))
          (is (= [] (requests))))))))

(deftest removing-a-trigger-lets-its-round-finish-then-drops-the-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup)]
          (api/request! eng (assoc adhoc :job '(spin)))
          (swap! flags conj :low)
          (reset! (:clock r) t0)
          (let [round (core/tick! eng)]
            (is (:ok (api/request! eng {:op :remove :id :bread-low})))
            (is (= :bread-low (:reflex (core/running eng))) "the round in flight is not cut")
            (await round))
          (await (tick-at r (+ t0 250)))
          (is (= {} (:instances (core/state eng))))
          (is (= [:declined] (mapv :outcome (of-kind seen :reflex :ended))) "its one round ended it")
          (is (= 1 (count (of-kind seen :job :round_started))) "no second round"))))))

;; ---------------------------------------------------------------- restart and scenario

(deftest restart-restores-the-register-and-recompiles-conditions
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup)]
          (api/request! eng adhoc)
          (api/request! eng {:op :put :id :high-3 :trigger :high :args {:at 3}})
          (core/shutdown! eng)
          (let [{again :eng seen :seen :as r} (setup {:dir (:dir eng)})]
            (is (= [:bread-low :high-3] (register-ids again)))
            (is (= '(flag :low) (:when (entry again :bread-low))) "persisted as data")
            (swap! flags conj :low)
            (await (tick-at r t0))
            (is (= 1 (fired seen)) "the recompiled condition fires")))))))

(deftest a-condition-that-no-longer-compiles-is-dropped-with-one-warn
  (let [{:keys [eng]} (setup)]
    (api/request! eng adhoc)
    (api/request! eng {:op :put :id :high-3 :trigger :high :args {:at 3}})
    (core/shutdown! eng)
    (let [{again :eng seen :seen} (setup {:dir (:dir eng) :compile refuse-all})]
      (is (= [:high-3] (register-ids again)))
      (is (= [:bread-low] (mapv :reflex (of-kind seen :system :dropped))))
      (is (re-find #"no longer known" (:text (first (of-kind seen :system :dropped))))))))

(deftest a-scenario-loads-its-register-through-put
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup)]
          (api/load-scenario! eng '{:register [{:trigger :high} {:id :bread-low :when (flag :low) :job (quick)}]
                                    :queue [(spin)]})
          (is (= [:scenario :scenario] (mapv :by (:register (core/state eng)))))
          (is (= ["j1"] (:list (core/state eng))))
          (swap! flags conj :low)
          (await (tick-at r t0))
          (is (= 1 (fired seen))))))))

;; ---------------------------------------------------------------- over the socket

(defn http-request [socket-path method request-path body]
  (js/Promise.
   (fn [resolve reject]
     (let [req (.request http #js {:socketPath socket-path :path request-path :method method
                                   :headers #js {"content-type" "application/edn"}}
                         (fn [res]
                           (let [chunks (atom [])]
                             (.on res "data" #(swap! chunks conj (str %)))
                             (.on res "end" #(resolve {:status (.-statusCode res)
                                                       :value (reader/read-string (apply str @chunks))})))))]
       (.on req "error" reject)
       (when body (.write req body))
       (.end req)))))

(deftest the-socket-serves-trigger-and-job-edits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup)
              socket-path (path/join (:dir eng) "events.sock")
              server (event-api/create socket-path eng)
              gen (:generation-id (core/state eng))]
          (await ((:listen server)))
          (try
            (let [put (await (http-request socket-path "POST" "/triggers"
                                           "{:op :put :id :bread-low :when (flag :low) :job (quick) :by \"steward\"}"))
                  bad (await (http-request socket-path "POST" "/triggers"
                                           "{:op :put :id :x :when (nope 1) :job (quick)}"))
                  unreadable (await (http-request socket-path "POST" "/triggers" "{:op"))
                  listed (await (http-request socket-path "GET" "/triggers" nil))
                  one (await (http-request socket-path "GET" "/triggers?id=bread-low" nil))
                  job (await (http-request socket-path "POST" "/jobs"
                                           (pr-str {:op :submit :request-id "r1" :generation-id gen
                                                    :spec '(quick) :front? true :by "steward"})))]
              (is (= [200 true true] [(:status put) (get-in put [:value :ok]) (get-in put [:value :created?])]))
              (is (= [409 :bad-condition [:when]] [(:status bad) (get-in bad [:value :reason]) (get-in bad [:value :at])]))
              (is (= 400 (:status unreadable)))
              (is (= [:bread-low] (mapv :id (get-in listed [:value :items]))))
              (is (= '(flag :low) (get-in listed [:value :items 0 :when])))
              (is (= :bread-low (get-in one [:value :explain :id])))
              (is (= [200 "j1"] [(:status job) (get-in job [:value :job :id])]))
              (is (= "steward" (get-in (core/state eng) [:instances "j1" :by]))))
            (finally ((:close server)))))))))

;; ---------------------------------------------------------------- the condition language through the seam

(deftest the-condition-language-compiles-and-refuses-through-the-seam
  (is (true? (:ok (api/compile-condition '(< (health) 10)))))
  (is (true? (:ok (api/compile-condition "(< (health) 10)"))) "text from a command line too")
  (are [form reason] (= reason (:reason (api/compile-condition form)))
    '(nope) :unknown-symbol
    ''(< (health) 10) :quoted
    '(health) :not-boolean))

(deftest known-compiles-through-the-seam-and-fires-on-an-absent-place
  (let [{:keys [explain] fire :when :as r} (api/compile-condition '(not (known? (place :home))))
        memory {:now 0 :data {:entries {:home [{:t 0 :data {:pos {:x 0 :y 64 :z 0}}}]}
                              :policies {:home {:cap 50 :ttl :forever}}}}
        p (tu/fake)]
    (is (true? (:ok r)))
    (is (true? (fire p {:now 0 :data {:entries {} :policies {}}})) "no home remembered: fires")
    (is (false? (fire p memory)) "once a home is remembered it stops")
    (is (= [['(not (known? (place :home))) true] ['(known? (place :home)) false] ['(place :home) :engine.condition.facts/unknown]]
           (mapv (juxt :form :value) (take 3 (explain p {:now 0 :data {:entries {} :policies {}}})))))
    (is (= :arity (:reason (api/compile-condition '(known?)))))))

(deftest real-conditions-keep-their-own-held-for-timers
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup {:compile api/compile-condition})
              fired-ids #(mapv :reflex (of-kind seen :reflex :fired))]
          (api/request! eng (assoc adhoc :id :a :when '(held-for 2 (< (health) 30))))
          (await (tick-at r t0))
          (await (tick-at r (+ t0 1000)))
          (is (= 1000 (get-in (api/triggers-view eng "a") [:explain :terms 0 :remaining-ms])))
          (api/request! eng (assoc adhoc :id :b :when '(held-for 2 (< (health) 30))))
          (await (tick-at r (+ t0 2000)))
          (is (= 0 (get-in (api/triggers-view eng "a") [:explain :terms 0 :remaining-ms])))
          (is (= [:a] (fired-ids)) "a has held 2 s; b only started")
          (await (tick-at r (+ t0 3000)))
          (is (= [:a] (fired-ids)))
          (await (tick-at r (+ t0 4000)))
          (is (= [:a :b] (fired-ids))))))))

(deftest a-false-real-condition-never-fires
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup {:compile api/compile-condition})]
          (api/request! eng (assoc adhoc :when '(> (health) 30)))
          (await (tick-at r t0))
          (is (= 0 (fired seen))))))))

(deftest one-entry-explains-its-condition
  (let [{:keys [eng]} (setup {:compile api/compile-condition})]
    (api/request! eng (assoc adhoc :when '(< (health) 10)))
    (api/request! eng {:op :put :trigger :high})
    (is (= [['(< (health) 10) false] ['(health) 20] [10 10]]
           (mapv (juxt :form :value) (get-in (api/triggers-view eng "bread-low") [:explain :terms]))))
    (is (= :bread-low (get-in (api/triggers-view eng :bread-low) [:explain :id])))
    (is (string? (get-in (api/triggers-view eng "high") [:explain :message])) "a built-in has no condition to explain")
    (is (nil? (:explain (api/triggers-view eng))))))

;; ---------------------------------------------------------------- restart with a newer scenario

(def day1 '{:register [{:trigger :high} {:id :bread-low :when (flag :low) :job (quick)}]})
(def day2 '{:register [{:trigger :high} {:id :wedge :when (flag :wedged) :job (quick)} {:id :bread-low :when (flag :low) :job (quick)}
                       {:id :night :when (flag :night) :job (quick)}]})

(defn restarted
  "A body started on day1, then the agent's edit f, then restarted: the fresh setup."
  [f]
  (let [{:keys [eng]} (setup)]
    (api/load-scenario! eng day1)
    (f eng)
    (core/shutdown! eng)
    (setup {:dir (:dir eng)})))

(defn offered [seen]
  (mapv :ids (of-kind seen :system :new-default-triggers)))

(deftest a-restart-offers-new-defaults-once-and-adds-nothing
  (let [{:keys [eng seen]} (restarted identity)]
    (is (= [:wedge :night] (api/resume-scenario! eng day2 false)))
    (is (= [:high :bread-low] (register-ids eng)) "offered, not added")
    (is (= [[:wedge :night]] (offered seen)))
    (is (= 1 (count (filter #(= :new-default-triggers (:reason %)) (vals (:attention (core/state eng)))))))
    (is (re-find #"new default triggers available: :wedge, :night; ./bin/triggers upgrade \[ids\] adds them, decline \[ids\] never offers them again"
                 (:message (:event (first (vals (:attention (core/state eng))))))))))

(deftest upgrade-adds-the-offered-defaults-at-their-scenario-priority
  (let [{:keys [eng]} (restarted identity)]
    (api/resume-scenario! eng day2 false)
    (is (= {:ok true :op :upgrade :added [:wedge :night] :offered []} (api/request! eng {:op :upgrade})))
    (is (= [:high :wedge :bread-low :night] (register-ids eng)))
    (is (empty? (:attention (core/state eng))) "the request is closed")
    (is (= [] (api/resume-scenario! eng day2 false)) "not offered again")))

(deftest the-upgrade-flag-adds-them-without-asking
  (let [{:keys [eng seen]} (restarted identity)]
    (is (= [:wedge :night] (api/resume-scenario! eng day2 true)))
    (is (= [:high :wedge :bread-low :night] (register-ids eng)))
    (is (empty? (offered seen)))
    (is (empty? (:attention (core/state eng))))))

(deftest a-trigger-the-agent-removed-is-not-offered-back
  (let [{:keys [eng]} (restarted #(api/request! % {:op :remove :id :bread-low}))
        {again :eng} (do (core/shutdown! eng) (setup {:dir (:dir eng)}))]
    (is (= [:high] (register-ids again)))
    (is (= [:wedge :night] (api/resume-scenario! again day2 false)) "only the never-had ids")))

(deftest a-body-saved-before-seen-triggers-existed-treats-its-register-as-seen
  (let [{:keys [eng]} (restarted #(swap! (:state %) dissoc :seen-triggers))]
    (is (= [:wedge :night] (api/resume-scenario! eng day2 false)))))

(deftest upgrade-skips-an-id-the-register-already-has
  (let [{:keys [eng]} (restarted identity)]
    (api/resume-scenario! eng day2 false)
    (api/request! eng {:op :put :id :night :when '(flag :other) :job '(quick) :by "agent"})
    (is (= [:wedge] (:added (api/request! eng {:op :upgrade}))))
    (is (= '(flag :other) (:when (entry eng :night))) "the hand-made entry is kept")
    (is (= "agent" (:by (entry eng :night))))
    (is (empty? (:attention (core/state eng))))))

(deftest upgrade-with-ids-adds-only-those-and-keeps-the-rest-offered
  (let [{:keys [eng]} (restarted identity)]
    (api/resume-scenario! eng day2 false)
    (is (= {:ok true :op :upgrade :added [:night] :offered [:wedge]} (api/request! eng {:op :upgrade :ids [:night]})))
    (is (= [:high :bread-low :night] (register-ids eng)))
    (is (= 1 (count (:attention (core/state eng)))) "the request stays for :wedge")))

(deftest decline-marks-seen-without-adding-and-is-never-offered-again
  (let [{:keys [eng]} (restarted identity)]
    (api/resume-scenario! eng day2 false)
    (is (= {:ok true :op :decline :declined [:wedge] :offered [:night]} (api/request! eng {:op :decline :ids [:wedge]})))
    (is (= [:high :bread-low] (register-ids eng)))
    (is (= [:night] (api/resume-scenario! eng day2 false)) "still offered only :night")
    (api/request! eng {:op :decline})
    (is (empty? (:attention (core/state eng))))
    (core/shutdown! eng)
    (let [{again :eng seen :seen} (setup {:dir (:dir eng)})]
      (is (= [] (api/resume-scenario! again day2 false)) "declined ids stay declined across a restart")
      (is (empty? (offered seen))))))

(deftest upgrade-and-decline-refuse-bad-ids
  (let [{:keys [eng]} (restarted identity)]
    (is (= :bad-ids (:reason (api/request! eng {:op :decline :ids "night"}))))
    (is (= :bad-ids (:reason (api/request! eng {:op :upgrade :ids [:Night]}))))))
