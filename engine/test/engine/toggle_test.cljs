(ns engine.toggle-test
  "jobs.access.toggle against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.job-api :as job-api]
            [engine.job-api :as job-api]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.access.toggle :as toggle]))

(defn setup
  ([world] (setup world (tu/legacy-capture-sink)))
  ([world [seen sink]]
   (let [clock (atom 1000000)
         p (tu/fake world)
         eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                           :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
     {:eng eng :p p :seen seen})))

(defn ^:async run-until-empty [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (await (core/tick! eng))
          (recur (inc i))))))

(defn ^:async child-outcome
  "Run job with args as the child of a recording parent until the list is empty, at most n ticks; the child's result, or {:waiting w} when the child declined and its parent waits."
  [eng job args n]
  (let [out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid job args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
    (let [id (core/submit! eng '(recording-parent) {})]
      (await (run-until-empty eng n))
      (if-let [w (:waiting (job-api/summary eng id))]
        {:waiting w}
        @out))))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn props [p pos] (js->clj (.-properties (.blockAt p (clj->js pos))) :keywordize-keys true))
(defn kinds [seen kind] (filterv #(= kind (:kind %)) @seen))

(def job 'jobs.access.toggle)
(def at {:x 3 :y 64 :z 0})
(def ground (into {} (for [x (range -5 20) z (range -5 20)] [(str x ",63," z) "stone"])))

(defn world
  "Ground, the block name at 3,64,0 with its state, the body at x z."
  ([name state] (world name state 3 2))
  ([name state x z]
   {:self {:pos {:x x :y 64 :z z}}
    :blocks (assoc ground "3,64,0" name)
    :states (if state {"3,64,0" state} {})}))

(defn ^:async run [w args]
  (let [{:keys [eng p seen]} (setup w)
        result (await (child-outcome eng job args 40))]
    {:result result :p p :seen seen}))

(defn head [m ks] (select-keys m ks))

;; ------------------------------------------------------------------ reaching the wanted state

(deftest a-block-is-brought-to-the-wanted-state-with-one-click
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[name from want key value] [["oak_fence_gate" {:open false} :open :open true]
                                            ["oak_fence_gate" {:open true} :closed :open false]
                                            ["oak_door" {:open false} :open :open true]
                                            ["spruce_trapdoor" {:open true} :closed :open false]
                                            ["copper_door" {:open false} :open :open true]
                                            ["lever" {:powered false} :on :powered true]
                                            ["lever" {:powered true} :off :powered false]
                                            ["stone_button" {:powered false} :press :powered true]]]
          (let [{:keys [result p seen]} (await (run (world name from) {:pos at :state want}))]
            (is (= {:status :done :reason :changed :block name :wanted want} (head result [:status :reason :block :wanted])) name)
            (is (= value (key (props p at))) name)
            (is (= 1 (count (calls p "useOn"))) (str name ": one click"))
            (is (= 1 (count (kinds seen :toggle.done))) name)))))))

(deftest a-block-already-in-the-wanted-state-is-not-clicked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[name from want] [["oak_fence_gate" {:open true} :open]
                                  ["oak_door" {:open false} :closed]
                                  ["oak_door" nil :closed]
                                  ["lever" {:powered true} :on]
                                  ["lever" nil :off]
                                  ["stone_button" {:powered true} :press]]]
          (let [{:keys [result p]} (await (run (world name from) {:pos at :state want}))]
            (is (= {:status :done :reason :already} (head result [:status :reason])) (str name want))
            (is (empty? (calls p "useOn")) (str name want ": no click"))))))))

(deftest a-far-body-walks-into-reach-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [result p]} (await (run (world "oak_fence_gate" {:open false} 3 14) {:pos at :state :open}))]
          (is (= :changed (:reason result)))
          (is (true? (:open (props p at))))
          (is (seq (tu/walk-calls p))))))))

(deftest one-call-walks-into-reach-and-clicks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (world "oak_fence_gate" {:open false} 3 14))
              result (await (child-outcome eng job {:pos at :state :open} 1))]
          (is (= :changed (:reason result)) "one round walks and clicks")
          (is (true? (:open (props p at)))))))))

(deftest the-position-may-be-a-vector-and-the-state-a-string
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [result p]} (await (run (world "lever" {:powered false}) {:pos [3 64 0] :state "on"}))]
          (is (= :changed (:reason result)))
          (is (true? (:powered (props p at)))))))))

(deftest euclidean-distance-in-reach-arrives-without-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (assoc (world "oak_fence_gate" {:open false} 7896.33 7592.32) :blocks (assoc (:blocks (world "oak_fence_gate" {:open false})) "7897,64,7595" "oak_fence_gate") :states (assoc {} "7897,64,7595" {:open false}))
              {:keys [result p]} (await (run w {:pos {:x 7897 :y 64 :z 7595} :state :open}))]
          (is (= :changed (:reason result)) "should change, not loop forever")
          (is (empty? (tu/walk-calls p)) "should not walk when within reach by Euclidean distance")
          (is (= 1 (count (calls p "useOn"))) "should click exactly once"))))))

;; ------------------------------------------------------------------ declined before any walk or click

(deftest a-request-the-hand-cannot-meet-is-declined-without-a-click-or-a-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[name from args reason] [["iron_door" {:open false} {:pos at :state :open} :needs-redstone]
                                         ["iron_trapdoor" {:open false} {:pos at :state :open} :needs-redstone]
                                         ["stone" nil {:pos at :state :open} :not-toggleable]
                                         ["air" nil {:pos at :state :open} :no-block]
                                         ["lever" {:powered false} {:pos at :state :open} :bad-state]
                                         ["oak_door" {:open false} {:pos at :state :on} :bad-state]
                                         ["oak_door" {:open false} {:pos at :state :press} :bad-state]
                                         ["oak_button" {:powered false} {:pos at :state :on} :bad-state]
                                         ["oak_door" {:open false} {:pos at :state :ajar} :bad-state]
                                         ["oak_door" {:open false} {:pos at} :bad-args]
                                         ["oak_door" {:open false} {:state :open} :bad-args]]]
          (let [{:keys [result p seen]} (await (run (world name from 3 14) args))]
            (is (= {:status :declined :reason reason} (head result [:status :reason])) (str name args))
            (is (empty? (calls p "useOn")) (str name args))
            (is (empty? (tu/walk-calls p)) (str name args ": no walk"))
            (is (= 1 (count (kinds seen :toggle.declined))) (str name args))))))))

(deftest an-unloaded-cell-is-declined-not-loaded
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [result p]} (await (run (assoc (world "oak_door" {:open false}) :unloaded ["3,64,0"]) {:pos at :state :open}))]
          (is (= :not-loaded (get-in result [:waiting :reason])) "the child declines: its parent waits")
          (is (empty? (calls p "useOn"))))))))

(deftest the-body-standing-in-a-door-is-not-shut-in-but-may-open-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [shut (await (run (world "oak_door" {:open true} 3 0) {:pos at :state :closed}))
              open (await (run (world "oak_door" {:open false} 3 0) {:pos at :state :open}))
              upper (await (run (-> (world "oak_door" {:open true} 3 0)
                                   (assoc-in [:blocks "3,65,0"] "oak_door")
                                   (assoc-in [:states "3,65,0"] {:open true}))
                               {:pos (assoc at :y 65) :state :closed}))]
          (is (= :standing-in (get-in shut [:result :waiting :reason])))
          (is (= :standing-in (get-in upper [:result :waiting :reason])) "the door's upper half")
          (is (empty? (calls (:p shut) "useOn")))
          (is (= :changed (:reason (:result open)))))))))

(deftest naming-the-upper-half-of-a-door-whose-lower-half-is-at-the-heads-is-declined-standing-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (-> (world "oak_door" {:open true} 3 0)
                    (update :blocks dissoc "3,64,0")
                    (update :states dissoc "3,64,0")
                    (assoc-in [:blocks "3,65,0"] "oak_door")
                    (assoc-in [:states "3,65,0"] {:open true :half "lower"})
                    (assoc-in [:blocks "3,66,0"] "oak_door")
                    (assoc-in [:states "3,66,0"] {:open true :half "upper"}))
              {:keys [result p]} (await (run w {:pos (assoc at :y 66) :state :closed}))]
          (is (= :standing-in (get-in result [:waiting :reason])))
          (is (empty? (calls p "useOn"))))))))

;; ------------------------------------------------------------------ gave up after one click

(deftest a-click-that-changes-nothing-is-given-up-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup (world "oak_door" {:open false :locked true}) (tu/capture-sink))
              result (await (child-outcome eng job {:pos at :state :open} 40))
              warns (filterv #(= :toggle.gave-up (:kind %)) @seen)]
          (is (= {:status :gave-up :reason :unchanged :wanted :open} (head result [:status :reason :wanted])))
          (is (= 1 (count (calls p "useOn"))) "one click, no retry")
          (is (= 1 (count warns)))
          (is (= :unchanged (:reason (:data (first warns))))))))))

(deftest a-block-somebody-else-flipped-just-before-the-click-is-named-wrong-way
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the door reads shut, somebody opens it, the click then shuts it again: it moved, but not to what was wanted
        (let [{:keys [eng p]} (setup (world "oak_door" {:open false}))
              real-use (.-useOn p)
              state (atom :first)
              _ (set! (.-useOn p) (fn [token a]
                                    (let [click #(.call real-use p token a)]
                                      (if (= :first @state)
                                        (do (reset! state :done)
                                            (.then (click) click))
                                        (click)))))
              result (await (child-outcome eng job {:pos at :state :open} 40))]
          (is (= {:status :gave-up :reason :wrong-way} (head result [:status :reason])))
          (is (false? (:open (props p at)))))))))

(deftest a-hand-that-cannot-be-emptied-is-given-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup (world "oak_door" {:open false}))
              _ (set! (.-useOn p) (fn [_ _] (js/Promise.resolve #js {:status "no-room"})))
              result (await (child-outcome eng job {:pos at :state :open} 40))]
          (is (= {:status :gave-up :reason :no-room} (head result [:status :reason])))
          (is (= 1 (count (kinds seen :toggle.gave-up)))))))))

(deftest a-block-the-body-cannot-walk-to-is-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (assoc (world "oak_door" {:open false} 3 14) :unreachable ["3,64,0"])
              {:keys [result p seen]} (await (run w {:pos at :state :open}))]
          (is (= {:status :gave-up :reason :unreachable} (head result [:status :reason])))
          (is (empty? (calls p "useOn")))
          (is (= 1 (count (kinds seen :toggle.gave-up)))))))))

(deftest a-walk-that-arrives-out-of-reach-by-the-jobs-own-measure-still-clicks-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (world "oak_fence_gate" {:open false} 3 4))
              result (await (child-outcome eng job {:pos at :state :open} 40))]
          (is (= :changed (:reason result)))
          (is (= 1 (count (calls p "useOn")))))))))

(defn too-far-first!
  "Make the fake's first n clicks answer out of reach (as the live reach check does for a body at the edge of :reach);
  returns the atom counting the clicks."
  [p n]
  (let [real-use (.-useOn p)
        clicks (atom 0)]
    (set! (.-useOn p) (fn [token a]
                        (if (<= (swap! clicks inc) n)
                          (js/Promise.resolve #js {:status "unreachable" :reason "too-far" :distance 4.53})
                          (.call real-use p token a))))
    clicks))

(deftest a-click-out-of-reach-walks-closer-and-clicks-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (world "oak_fence_gate" {:open true} 3 4))
              clicks (too-far-first! p 1)
              result (await (child-outcome eng job {:pos at :state :closed :reach 4} 60))]
          (is (= {:status :done :reason :changed} (head result [:status :reason])))
          (is (false? (:open (props p at))))
          (is (= 2 @clicks) "one click out of reach, one after the walk closer")
          (is (seq (tu/walk-calls p)) "the body walked closer")
          (is (<= (js/Math.abs (- (.. p self -pos -z) 0)) 3) "within reach - 1 of the block"))))))

(deftest one-call-walks-closer-after-a-click-out-of-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (world "oak_fence_gate" {:open true} 3 4))
              clicks (too-far-first! p 1)
              result (await (child-outcome eng job {:pos at :state :closed :reach 4} 1))]
          (is (= {:status :done :reason :changed} (head result [:status :reason])))
          (is (= 2 @clicks)))))))

(deftest a-click-still-out-of-reach-after-walking-closer-twice-is-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup (world "oak_fence_gate" {:open true} 3 4))
              clicks (too-far-first! p 99)
              result (await (child-outcome eng job {:pos at :state :closed :reach 4} 80))]
          (is (= {:status :gave-up :reason :unreachable} (head result [:status :reason])))
          (is (= 3 @clicks) "the first click and one after each of two walks closer")
          (is (= 1 (count (kinds seen :toggle.gave-up)))))))))

(deftest the-job-is-registered-with-doc-and-args
  (let [j (get registry/jobs job)]
    (is (string? (:doc j)))
    (is (= 3 (get-in j [:args :reach :default])))
    (is (some? toggle/doc))))
