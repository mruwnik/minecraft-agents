(ns engine.job-api-test
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.job-api :as api]
            [engine.events :as events]
            [engine.test-util :as tu]))
(defn setup []
  (let [p (tu/fake {})
        jobs {'wait {:args {:ms {:default 1000}} :check (constantly true)
                     :round (fn ^:async round [ctx] (await (ctx/act ctx :wait #js {:ms 1000})) :continue)}
              'done {:args {} :check (constantly true) :round (fn [_] :done)}
              'fail {:args {} :check (constantly true) :round (fn [_] (throw (js/Error. "test failure")))}}
        eng (core/create {:primitives p :jobs jobs :triggers {} :dir (tu/tmp-dir)
                          :events (events/make {:stdout? false})})]
    {:eng eng :p p :jobs jobs}))
(defn command [eng id op extra]
  (api/mutate! eng (merge {:op op :request-id id :generation-id (:generation-id (core/state eng))} extra)))
(deftest native-spec-validation-dedupe-conflict-and-bounded-ledger
  (let [{:keys [eng]} (setup)
        good '(done)
        response (command eng "request-1" :submit {:spec good})]
    (is (= "j1" (get-in response [:job :id])))
    (is (= true (:duplicate (command eng "request-1" :submit {:spec good}))))
    (is (= ["j1"] (:list (core/state eng))))
    (is (= :request-id-conflict (:reason (command eng "request-1" :submit {:spec '(wait)}))))
    (is (= :bad-spec (:reason (command eng "bad" :submit {:spec '(unknown)}))))
    (is (nil? (get-in (core/state eng) [:job-requests :records "bad"])))
    (is (= :generation-mismatch (:reason (api/mutate! eng {:op :submit :request-id "old" :generation-id "wrong" :spec good}))))
    (doseq [i (range 140)] (command eng (str "request-" (+ i 2)) :submit {:spec good}))
    (is (= api/max-requests (count (get-in (core/state eng) [:job-requests :records]))))
    (is (= :job-not-found (:reason (command eng "cancel-missing" :cancel {:id "j999"}))))
    (is (= 3 (count (:items (api/list-jobs eng 4 3)))))
    (command eng "cancel-original" :cancel {:id "j1"})
    ;; This request remains outside the bounded window here; a separate retained ID below covers current status.
    (let [submitted (command eng "retained" :submit {:spec good})]
      (command eng "cancel-retained" :cancel {:id (get-in submitted [:job :id])})
      (is (= :absent (get-in (command eng "retained" :submit {:spec good}) [:job :status]))))
    (core/shutdown! eng)))
(deftest ledger-survives-restore-and-pending-is-not-executed-twice
  (let [{:keys [eng p jobs]} (setup)
        request {:op :submit :request-id "stable" :generation-id (:generation-id (core/state eng)) :spec '(done)}]
    (api/mutate! eng request)
    (api/remember! eng "pending" {:status :pending :signature (api/fingerprint (assoc request :request-id "pending"))})
    (let [again (core/create {:primitives p :jobs jobs :triggers {} :dir (:dir eng)
                             :events (events/make {:stdout? false})})]
      (is (= true (:duplicate (api/mutate! again request))))
      (is (= :request-uncertain (:reason (api/mutate! again (assoc request :request-id "pending")))))
      (is (= ["j1"] (:list (core/state again))))
      (core/shutdown! again))
    (core/shutdown! eng)))
(deftest manual-lease-allows-queueing-but-refuses-interrupt
  (let [{:keys [eng]} (setup)]
    (reset! (:manual eng) {:who "driver" :token "manual"})
    (is (true? (:ok (command eng "queue" :submit {:spec '(done)}))))
    (is (= :manual-control (:reason (command eng "interrupt" :interrupt {:spec '(done)}))))
    (is (= {:who "driver" :token "manual"} @(:manual eng)))
    (core/shutdown! eng)))
(deftest failed-interrupt-parks-with-attention-and-predecessor-resumes
  (async done
    (tu/run-async done
      (fn ^:async run []
        (let [{:keys [eng]} (setup)]
          (command eng "original" :submit {:spec '(wait)})
          (let [running (core/tick! eng)]
            (command eng "interrupt" :interrupt {:spec '(fail)})
            (await running))
          (await (core/tick! eng))
          (is (contains? (:failed (core/state eng)) "j2"))
          (is (seq (core/outstanding eng)))
          (is (= "j1" (:resume (core/state eng))))
          (let [resumed (core/tick! eng)]
            (is (= "j1" (:id (core/running eng))))
            (command eng "cancel-first" :cancel {:id "j1"})
            (await resumed))
          (is (= ["j2"] (:list (core/state eng))))
          (is (true? (:ok (command eng "retry-failed" :retry {:id "j2"}))))
          (is (empty? (core/outstanding eng)))
          (command eng "cancel-failed" :cancel {:id "j2"})
          (is (empty? (:list (core/state eng))))
          (core/shutdown! eng))))))
(deftest submit-takes-front-hold-backoff-and-who-added-it
  (let [{:keys [eng]} (setup)]
    (command eng "first" :submit {:spec '(wait)})
    (let [r (command eng "second" :submit {:spec '(done) :front? true :hold? true :backoff {:after 5} :by "steward"})
          id (get-in r [:job :id])]
      (is (true? (:ok r)))
      (is (= [id "j1"] (:list (core/state eng))))
      (is (= {:hold? true :backoff {:after 5} :by "steward"}
             (select-keys (get-in (core/state eng) [:instances id]) [:hold? :backoff :by]))))
    (is (= :agent (get-in (core/state eng) [:instances "j1" :by])) "who defaults to :agent")
    (is (true? (:duplicate (command eng "second" :submit {:spec '(done) :front? true :hold? true :backoff {:after 5} :by "steward"}))))
    (is (= :request-id-conflict (:reason (command eng "second" :submit {:spec '(done)}))) "the options are part of the request")
    (core/shutdown! eng)))
(deftest bad-submit-options-are-refused-before-anything-changes
  (let [{:keys [eng]} (setup)]
    (are [extra reason] (= reason (:reason (command eng (str (random-uuid)) :submit (merge {:spec '(done)} extra))))
      {:front? "yes"} :bad-field
      {:hold? 1} :bad-field
      {:backoff {:after 0}} :bad-backoff
      {:by 7} :bad-by)
    (is (= [] (:list (core/state eng))))
    (is (empty? (get-in (core/state eng) [:job-requests :records])))
    (core/shutdown! eng)))
