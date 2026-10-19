(ns engine.contract-quote-test
  "Quotes (engine.core.quote): ctx/quote under a quote ctx, the default quote, the quote deadline, duplicate quotes,
  the least check at call start and ctx/next-way."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [engine.test-util :as tu]))

(def world (atom {}))
(def clock (atom 0))

(defn setup [jobs]
  (reset! world {})
  (reset! clock 1000000)
  (let [[seen sink] (tu/capture-sink)
        eng (core/create {:primitives (tu/fake {}) :dir (tu/tmp-dir) :now #(deref clock) :jobs jobs :triggers {}
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :seen seen}))

(defn stopped [seen]
  (some #(when (= [:job :stopped] [(:source %) (:kind %)]) (select-keys (:data %) [:kind :cause])) @seen))

(def no-args {})
(def pos-args {:pos {:doc "target" :default nil}})

(defn asking
  "A contract job whose round runs (f c), keeps what it returns in world :got and ends :done."
  [f]
  {:defaults {:spend {:job-time 100} :floor {:health 5}} :args no-args
   :round (fn ^:async asking-round [c] (swap! world assoc :got (await (f c))) (ctx/end c :done nil))})

;; ---------------------------------------------------------------- the quote ctx

(def kid
  {:target [:pos] :ways [:a :b] :defaults {:ways #{:a} :spend {:job-time 30}} :args pos-args
   :quote-of (fn ^:async kid-quote [c]
               (swap! world assoc :seen {:pos (:pos (:args c)) :left (ctx/left c) :a (ctx/way? c :a) :b (ctx/way? c :b)
                                         :mem (ctx/mem c)})
               (try (await (ctx/act c :look #js {:yaw 0 :pitch 0}))
                    (catch :default e (swap! world assoc :act-refused (.-message e))))
               {:uses {:job-time [1 2 ##Inf]} :yields {:items {"dirt" [0 1 2]}}})
   :round (fn [_] (swap! world assoc :kid-ran true) :continue)})

(deftest a-quote-reads-like-a-round-plus-what-is-left-and-cannot-act
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {'kid kid 'asker (asking #(ctx/quote % 'kid {:pos [1 2 3] :spend {:job-time 500}}))})]
          (core/submit! eng '(asker) {})
          (await (core/tick! eng))
          (is (= {:uses {:job-time [1 2 ##Inf]} :yields {:items {"dirt" [0 1 2]}}} (:got @world)))
          (is (= {:pos [1 2 3] :left {:spend {:job-time 100} :floor {:health 5}} :a true :b false :mem {}} (:seen @world))
              "the would-be call: given spend clamped to the caller's remainder, the caller's floor, the default ways")
          (is (re-find #"quote cannot act" (str (:act-refused @world))) "a clear error, not a cut")
          (is (not (:kid-ran @world)) "a quote runs no round"))))))

;; ---------------------------------------------------------------- default quote

(def plain {:args no-args :round (fn [c] (ctx/end c :done nil))})
(def old {:args no-args :round #(ctx/end % :done nil)})

(deftest the-default-quote-is-the-mean-of-the-reports-else-the-default-time
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (settings/with-settings {:engine.contract/default-time-s 7}
                 (fn ^:async default-time []
                   (let [{:keys [eng]} (setup {'plain plain 'old old
                                               'asker (asking (fn ^:async both [c]
                                                                (let [p (await (ctx/quote c 'plain {}))
                                                                      o (await (ctx/quote c 'old {}))]
                                                                  [p o])))})]
                     (doseq [s [4 8]] (mem/write! (:store eng) :job-report {:job 'plain :report {:uses {:job-time s}}}))
                     (core/submit! eng '(asker) {})
                     (await (core/tick! eng))
                     (is (= [{:uses {:job-time [0 6 ##Inf]}} {:uses {:job-time [0 7 ##Inf]}}] (:got @world)))))))))))

;; ---------------------------------------------------------------- the quote deadline

(def slow-quote
  {:args no-args :round (constantly :continue)
   :quote-of (fn [_] (swap! world update :slow-quotes (fnil inc 0)) (swap! clock + 5000) {:uses {:job-time [1 1 1]}})})

(def outer
  {:args no-args :round (constantly :continue)
   :quote-of (fn ^:async outer-quote [c]
               (swap! world assoc :handed (:quote-deadline c))
               (swap! world assoc :inner (await (ctx/quote c 'slow {})))
               (swap! world assoc :after (await (ctx/quote c 'slow {})))
               {:uses {:job-time [1 2 3]}})})

(deftest a-quote-asked-past-the-deadline-is-infinite-but-an-answer-past-it-is-kept
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {'slow slow-quote 'outer outer
                                    'asker (asking #(ctx/quote % 'outer {} (+ @clock 1000)))})
              inf {:uses {:job-time [##Inf ##Inf ##Inf]}}]
          (core/submit! eng '(asker) {})
          (await (core/tick! eng))
          (is (= 1001000 (:handed @world)) "handed down")
          (is (= {:uses {:job-time [1 1 1]}} (:inner @world)) "ran past it, answered")
          (is (= inf (:after @world)) "asked past it")
          (is (= 1 (:slow-quotes @world)) "a quote asked past the deadline does not run")
          (is (= {:uses {:job-time [1 2 3]}} (:got @world)) "the outer quote ended past it, answered"))))))

;; ---------------------------------------------------------------- duplicates

(def ping
  {:target [:pos] :args pos-args
   :quote-of (fn ^:async ping-quote [c]
               (swap! world assoc :inner (await (ctx/quote c 'ping {:pos (:pos (:args c))})))
               {:uses {:job-time [1 1 1]}})
   :round (fn ^:async ping-round [c]
            (swap! world assoc :same (await (ctx/quote c 'ping {:pos (:pos (:args c))}))
                   :other (await (ctx/quote c 'ping {:pos 2})))
            (ctx/end c :done nil))})

(deftest a-quote-equal-to-a-callers-or-a-quoting-ancestors-is-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {'ping ping})]
          (core/submit! eng '(ping {:pos 1}) {})
          (await (core/tick! eng))
          (let [{:keys [same other inner]} @world]
            (is (= :duplicate-call (contract/cause-key same)) "the caller's frame")
            (is (= :duplicate-call (contract/cause-key inner)) "the quote that asks for it")
            (is (= {:uses {:job-time [1 1 1]}} other))))))))

;; ---------------------------------------------------------------- the least check

(def field
  {:args no-args
   :done? (fn [_] (contract/later :not-done "not done" :done 0 :left 4 :units :cells))
   :quote-of (fn [_] {:uses {:job-time [1 1 1]} :done [0 2 2]})
   :round (fn [c] (swap! world assoc :ran true) (ctx/end c :done nil))})

(def idle-field
  (assoc field :quote-of (fn [_] (swap! world update :quotes (fnil inc 0)) {:uses {:job-time [1 1 1]} :done [0 0 0]})))

(deftest the-default-least-asks-no-quote
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {'idle-field idle-field})]
          (core/submit! eng '(idle-field) {})
          (await (core/tick! eng))
          (is (:ran @world))
          (is (nil? (:quotes @world))))))))

(deftest a-call-whose-quote-cannot-reach-its-least-does-not-start
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[least ran? ending] [[3 nil {:kind :nothing-possible :cause {:reason :least :least 3 :most 2}}]
                                     [:all nil {:kind :nothing-possible :cause {:reason :least :least :all :most 2}}]
                                     [2 true nil]]]
          (let [{:keys [eng seen]} (setup {'field field})]
            (core/submit! eng (list 'field {:least least}) {})
            (await (core/tick! eng))
            (is (= ran? (:ran @world)) (str "least " least))
            (is (= ending (stopped seen)) (str "least " least))))))))

;; ---------------------------------------------------------------- next-way

(def failer {:args no-args :round (fn [c] (ctx/end c :nothing-possible :empty))})
(def winner {:args no-args :round (fn [c] (ctx/end c :done nil))})

(defn way-quote [way]
  (case way
    :b {:uses {:job-time [20 30 40]}}
    :d (contract/ending :nothing-possible nil nil)
    {:uses {:job-time [1 2 3]}}))

(def chooser
  {:ways [:a :b :c :d :e] :defaults {:ways #{:b :c :d :e} :spend {:job-time 10}} :args no-args
   :round (fn ^:async chooser-round [c]
            (let [w1 (await (ctx/next-way c way-quote :x))
                  _ (await (ctx/call c :try 'failer {}))
                  w2 (await (ctx/next-way c way-quote :x))
                  w3 (await (ctx/next-way c way-quote :y))
                  w4 (await (ctx/next-way c (constantly {:uses {:job-time [##Inf ##Inf ##Inf]}}) :x))]
              (swap! world assoc :ways [w1 w2 w3 w4])
              (ctx/end c :done nil)))})

(def slow {:line :job-time :least 20 :left 10})

(deftest next-way-drops-the-ways-not-allowed-failed-or-over-a-remainder
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {'chooser chooser 'failer failer})]
          (core/submit! eng '(chooser) {})
          (await (core/tick! eng))
          (let [[w1 w2 w3 w4] (:ways @world)]
            (is (= {:way :c :dropped {:a :not-allowed :b slow}} w1))
            (is (= {:way :e :dropped {:a :not-allowed :b slow :c {:failed :empty} :d :ending}} w2)
                "failed on :x for :empty; a nil cause is dropped as :ending")
            (is (= :c (:way w3)) "another thing")
            (is (= (let [inf {:line :job-time :least ##Inf :left 10}]
                     {:way nil :dropped {:a :not-allowed :b inf :c {:failed :empty} :d inf :e inf}})
                   w4))))))))

(def booker
  {:ways [:a :b :c] :defaults {:ways #{:c}} :args no-args
   :round (fn ^:async booker-round [c]
            (let [w1 (await (ctx/next-way c way-quote :x))
                  _ (await (ctx/call c :ok 'winner {}))
                  _ (await (ctx/call c :bad 'failer {}))
                  w2 (await (ctx/next-way c way-quote :x))]
              (swap! world assoc :ways [w1 w2])
              (ctx/end c :done nil)))})

(deftest a-done-child-clears-the-way-a-later-failure-would-book
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {'booker booker 'winner winner 'failer failer})]
          (core/submit! eng '(booker) {})
          (await (core/tick! eng))
          (is (= [:c :c] (map :way (:ways @world)))))))))
