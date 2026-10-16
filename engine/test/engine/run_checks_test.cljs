(ns engine.run-checks-test
  "A job's safe? and fit? exports are asked with can-run?, before a job starts and on every resume: as a checker fn or
  as a quoted condition form. A wait costs no failure; a child's wait is its parent's, and ends a reflex or exclusive root."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.job-api :as api]
            [engine.memory :as mem]
            [engine.test-util :as tu]))

(def world (atom {}))

(defn setup [jobs & [triggers fake]]
  (reset! world {})
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        eng (core/create {:primitives (tu/fake (or fake {})) :dir (tu/tmp-dir) :now #(deref clock) :jobs jobs
                          :triggers (or triggers {})
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :seen seen}))

(defn gate [k reason] (fn [_] (if (get @world k) (contract/ok :open "open") (contract/later reason (name reason)))))

(defn count-round [k] (fn [c] (swap! world update k (fnil inc 0)) (if (:end @world) (ctx/end c :done nil) :continue)))

(defn leaf [check-key] {check-key (gate :go :dark) :round (count-round :leaf-rounds)})

(defn parent [check-key]
  {:round (fn ^:async parent-round [c]
            (let [e (await (ctx/call c :leaf (leaf check-key) {}))]
              (swap! world assoc :after-child e)
              (if (= :continue e) :continue (ctx/end c :done nil))))})

(deftest a-queued-job-waits-and-a-started-one-yields-with-the-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [k [:safe? :fit?]]
          (let [{:keys [eng]} (setup {'root (assoc (leaf k) :round (count-round :rounds))})
                id (core/submit! eng '(root) {})]
            (await (core/tick! eng))
            (is (nil? (:rounds @world)) (str k " not started"))
            (is (= :dark (:reason (core/waiting eng id))) (str k " waits with the reason"))
            (is (= [id] (:list (core/state eng))))
            (swap! world assoc :go true)
            (await (core/tick! eng))
            (is (= 1 (:rounds @world)) (str k " runs once it holds"))
            (swap! world dissoc :go)
            (await (core/tick! eng))
            (is (= 1 (:rounds @world)) (str k " yields when it stops holding"))
            (is (= :dark (:reason (core/waiting eng id))))
            (is (= [id] (:list (core/state eng))) "still listed: a wait is no failure")))))))

(deftest an-export-may-be-a-quoted-condition-form
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [job {:safe? '(daytime) :round (count-round :rounds)}
              {:keys [eng]} (setup {'root job} nil {:time 15000})
              id (core/submit! eng '(root) {})]
          (await (core/tick! eng))
          (is (nil? (:rounds @world)))
          (is (= :night (:reason (core/waiting eng id)))))
        (let [{:keys [eng]} (setup {'root {:fit? '(daytime) :round (count-round :rounds)}} nil {:time 6000})]
          (core/submit! eng '(root) {})
          (await (core/tick! eng))
          (is (= 1 (:rounds @world))))))))

(deftest a-child-wait-is-its-parents-wait
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [k [:safe? :fit?]]
          (let [{:keys [eng]} (setup {'root (parent k)})
                id (core/submit! eng '(root) {})]
            (await (core/tick! eng))
            (is (nil? (:leaf-rounds @world)))
            (is (= :continue (:after-child @world)))
            (is (= :dark (:reason (core/waiting eng id))))
            (swap! world assoc :go true)
            (await (core/tick! eng))
            (is (= 1 (:leaf-rounds @world)))))))))

(deftest a-child-wait-under-a-reflex-root-ends-the-reflex
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [triggers {:r {:name :r :job '(root) :persistence :retry :when (fn [_ _ _] true)}}
              {:keys [eng seen]} (setup {'root (parent :safe?)} triggers)]
          (core/register-reflex! eng {:trigger :r})
          (await (core/tick! eng))
          (is (nil? (:leaf-rounds @world)))
          (is (= [:declined] (mapv #(get-in % [:data :outcome] (:outcome %)) (filter #(= [:reflex :ended] [(:source %) (:kind %)]) @seen)))))))))

(deftest a-child-wait-under-an-exclusive-root-ends-the-run-with-its-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'root (parent :safe?)})]
          (api/mutate! eng {:op :exclusive :spec '(root) :by "claude" :request-id "r1"
                            :generation-id (:generation-id (core/state eng))})
          (await (core/tick! eng))
          (is (nil? (:leaf-rounds @world)))
          (is (nil? (core/exclusive-job eng)))
          (is (= [:dark] (mapv :reason (filter #(= :stopped (:kind %)) @seen)))))))))

(deftest a-trigger-fires-only-when-its-jobs-checks-pass
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [k [:can-start? :safe? :fit?]]
          (reset! world {})
          (let [triggers {:r {:name :r :job '(reflex) :persistence :retry :when (fn [_ _ _] true)}}
                {:keys [eng seen]} (setup {'reflex (assoc (leaf k) :round (count-round :reflex-rounds))
                                           'other {:round (count-round :other-rounds)}}
                                          triggers)]
            (core/submit! eng '(other) {})
            (core/register-reflex! eng {:trigger :r})
            (await (core/tick! eng))
            (is (nil? (:reflex-rounds @world)) (str k " held"))
            (is (= 1 (:other-rounds @world)) (str k " cuts nothing: the listed job runs"))
            (is (empty? (filter #(= [:reflex :fired] [(:source %) (:kind %)]) @seen)))
            (is (= :dark (:reason (core/held-trigger eng :r))) (str k " shows held with the reason"))
            (is (= 1 (count (filter #(= [:reflex :held] [(:source %) (:kind %)]) @seen))))
            (await (core/tick! eng))
            (is (= 1 (count (filter #(= [:reflex :held] [(:source %) (:kind %)]) @seen))) "told once")
            (swap! world assoc :go true)
            (await (core/tick! eng))
            (is (= 1 (:reflex-rounds @world)) (str k " fires once the check passes"))
            (is (nil? (core/held-trigger eng :r)))))))))

(defn raw-mem [eng root slots] (mem/job-mem (mem/view (:store eng)) root slots))

(defn run-parent
  "Submit (root args) over a parent whose child leaf is gated by key; one tick."
  [jobs args]
  (let [{:keys [eng] :as s} (setup jobs)
        id (core/submit! eng (list 'root args) {})]
    (assoc s :id id)))

(deftest checks-flow-down-per-key
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng id]} (run-parent {'root (parent :safe?)} {:checks {:safe? '(always)}})]
          (await (core/tick! eng))
          (is (= 1 (:leaf-rounds @world)) "the caller's :safe? replaces the child's own")
          (is (= {:safe? '(always)} (get-in (raw-mem eng id [:leaf]) [:contract :call :checks])) "frozen as the form"))
        (reset! world {})
        (let [{:keys [eng id]} (run-parent {'root (parent :safe?)} {:checks {:fit? '(always)}})]
          (await (core/tick! eng))
          (is (nil? (:leaf-rounds @world)) "the other key leaves the child's :safe? alone")
          (is (= :dark (:reason (core/waiting eng id)))))))))

(deftest job-safe-on-a-child-call-keeps-the-childs-own
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [root {:round (fn ^:async r [c]
                             (swap! world assoc :after-child
                                    (await (ctx/call c :leaf (leaf :safe?) {:checks {:safe? '(job-safe)}})))
                             :continue)}
              {:keys [eng id]} (run-parent {'root root} {:checks {:safe? '(always)}})]
          (await (core/tick! eng))
          (is (nil? (:leaf-rounds @world)))
          (is (= :dark (:reason (core/waiting eng id))))
          (swap! world assoc :go true)
          (await (core/tick! eng))
          (is (= 1 (:leaf-rounds @world))))))))

(deftest a-jobs-defaults-may-carry-checks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {'root (assoc (leaf :safe?) :defaults {:checks {:safe? '(always)}}
                                                 :round (count-round :rounds))})]
          (core/submit! eng '(root) {})
          (await (core/tick! eng))
          (is (= 1 (:rounds @world))))))))

(deftest a-bad-checks-form-is-refused-at-submit
  (let [{:keys [eng]} (setup {'root (leaf :safe?)})
        submit #(api/mutate! eng {:op :submit :spec %1 :by "claude" :request-id %2
                                  :generation-id (:generation-id (core/state eng))})]
    (are [form] (and (= :bad-args (:reason (submit (list 'root {:checks {:safe? form}}) (str (hash form)))))
                     (empty? (:list (core/state eng))))
      '(nonsense)
      '(health))
    (is (= :bad-spec (:reason (submit '(root {:checks {:other (always)}}) "k2"))) "the shape is checked too")
    (is (true? (:ok (submit '(root {:checks {:safe? (and (job-safe) (not (threatened)))}}) "k3"))))))
