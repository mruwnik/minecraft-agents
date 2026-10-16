(ns engine.toggle-test
  "jobs.access.toggle against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.expr :as expr]
            [engine.test-util :as tu :refer [run-until-empty]]
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

(defn ^:async child-outcome
  "Run job with args as the child of a recording parent until the list is empty, at most n ticks; the child's outcome."
  [eng job args n]
  (let [out (atom :not-done)
        parent (tu/recording-parent job args out)
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
    (core/submit! eng '(recording-parent) {})
    (await (run-until-empty eng n))
    @out))

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
            (is (empty? (kinds seen :toggle.done)) name)))))))

(deftest a-click-whose-state-was-undone-by-another-actor-is-already-there
  (let [done? (:done? (get registry/jobs job))
        ctx-of (fn [props mem]
                 {:primitives (tu/fake (world "lever" props)) :root "j1" :slots [] :args {:pos at :state :on}
                  :view (fn [] {:now 0 :data {:entries {:job/j1 [{:t 0 :data mem}]}}})})
        reason (fn [props mem] (:reason (nth (done? (ctx-of props mem)) 2)))]
    (is (= :changed (reason {:powered true} {:clicked {:was {:powered false} :now {:powered true}}})))
    (is (= :already (reason {:powered true} {:clicked {:was {:powered true} :now {:powered false}}})) "flipped on by another actor after the click turned it off")
    (is (= :already (reason {:powered true} {})))))

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
            (is (= :done (:status result)) (str name want))
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
                                         ["oak_door" {:open false} {:pos at} :bad-args]
                                         ["oak_door" {:open false} {:state :open} :bad-args]]]
          (let [{:keys [result p seen]} (await (run (world name from 3 14) args))]
            (is (= {:status :stopped :reason reason} (head result [:status :reason])) (str name args))
            (is (= (if (= :no-block reason) :condition-failed :impossible) (get-in result [:ending :kind])) (str name args))
            (is (empty? (calls p "useOn")) (str name args))
            (is (empty? (tu/walk-calls p)) (str name args ": no walk"))
            (is (empty? (kinds seen :toggle.declined)) (str name args))))))))

(deftest the-start-check-settles-what-the-block-and-the-args-never-allow
  (doseq [[name from args expected] [["iron_door" {:open false} {:pos at :state :open} [:never :needs-redstone]]
                                     ["stone" nil {:pos at :state :open} [:never :not-toggleable]]
                                     ["lever" {:powered false} {:pos at :state :open} [:never :bad-state]]
                                     ["oak_door" {:open false} {:pos at} [:never :bad-args]]
                                     ["air" nil {:pos at :state :open} [:later :no-block]]]]
    (let [c {:primitives (tu/fake (world name from 3 14)) :args args}]
      (is (= expected (tu/kind-reason ((:can-start? (get registry/jobs job)) c))) (str name args))))
  (is (= :ok (first ((:can-start? (get registry/jobs job))
                     {:primitives (tu/fake (world "oak_door" {:open false} 3 14)) :args {:pos at :state :open}})))))

(deftest a-state-no-block-has-is-refused-at-submit
  (is (re-find #"jobs.access.toggle :state must be one of .*, got :ajar"
               (expr/problem registry/jobs (list 'jobs.access.toggle {:pos at :state :ajar})))))

(defn waiting-reason [seen] (:reason (first (kinds seen :waiting))))

(deftest an-unloaded-cell-waits-not-loaded
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen p]} (await (run (assoc (world "oak_door" {:open false}) :unloaded ["3,64,0"]) {:pos at :state :open}))]
          (is (= :not-loaded (waiting-reason seen)))
          (is (empty? (calls p "useOn"))))))))

(defn column-free? [p {:keys [x z]}]
  (let [pos (.-pos (.self p))]
    (not (and (= x (js/Math.floor (.-x pos))) (= z (js/Math.floor (.-z pos)))))))

(deftest the-body-standing-in-a-door-steps-out-before-it-shuts-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [shut (await (run (world "oak_door" {:open true} 3 0) {:pos at :state :closed}))
              upper (await (run (-> (world "oak_door" {:open true} 3 0)
                                   (assoc-in [:blocks "3,65,0"] "oak_door")
                                   (assoc-in [:states "3,65,0"] {:open true}))
                               {:pos (assoc at :y 65) :state :closed}))
              lower-at-head (await (run (-> (world "oak_door" {:open true} 3 0)
                                            (update :blocks dissoc "3,64,0")
                                            (update :states dissoc "3,64,0")
                                            (assoc-in [:blocks "3,65,0"] "oak_door")
                                            (assoc-in [:states "3,65,0"] {:open true :half "lower"})
                                            (assoc-in [:blocks "3,66,0"] "oak_door")
                                            (assoc-in [:states "3,66,0"] {:open true :half "upper"}))
                                        {:pos (assoc at :y 66) :state :closed}))
              open (await (run (world "oak_door" {:open false} 3 0) {:pos at :state :open}))]
          (doseq [[nm r] [["door" shut] ["upper half" upper] ["lower half at the head" lower-at-head]]]
            (is (= :changed (:reason (:result r))) nm)
            (is (nil? (waiting-reason (:seen r))) nm)
            (is (column-free? (:p r) at) (str nm ": the body ends out of the column"))
            (is (= 1 (count (calls (:p r) "useOn"))) nm))
          (is (= :changed (:reason (:result open)))))))))

(defn holes [w cells] (update w :blocks #(apply dissoc % (map (fn [[x z]] (str x ",63," z)) cells))))

(deftest the-step-out-of-a-door-cell-takes-only-a-cell-with-a-floor
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (world "oak_door" {:open true} 3 0)
              one (await (run (holes w [[4 0] [2 0] [3 1]]) {:pos at :state :closed}))
              none (await (run (holes w [[4 0] [2 0] [3 1] [3 -1]]) {:pos at :state :closed}))
              end (.-pos (.self (:p one)))]
          (is (= :changed (:reason (:result one))))
          (is (= [3 -1] [(js/Math.floor (.-x end)) (js/Math.floor (.-z end))]) "steps onto the one cell with a floor")
          (is (= {:status :stopped :reason :unreachable} (head (:result none) [:status :reason])))
          (is (empty? (calls (:p none) "useOn"))))))))

;; ------------------------------------------------------------------ gave up after one click

(deftest a-click-that-changes-nothing-is-given-up-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup (world "oak_door" {:open false :locked true}) (tu/capture-sink))
              result (await (child-outcome eng job {:pos at :state :open} 40))
              warns (filterv #(= :toggle.gave-up (:kind %)) @seen)]
          (is (= {:status :stopped :reason :unchanged :wanted :open} (head result [:status :reason :wanted])))
          (is (= :nothing-possible (get-in result [:ending :kind])))
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
          (is (= {:status :stopped :reason :wrong-way} (head result [:status :reason])))
          (is (false? (:open (props p at)))))))))

(deftest a-block-the-body-cannot-walk-to-is-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (assoc (world "oak_door" {:open false} 3 14) :unreachable ["3,64,0"])
              {:keys [result p seen]} (await (run w {:pos at :state :open}))]
          (is (= {:status :stopped :reason :unreachable} (head result [:status :reason])))
          (is (= :nothing-possible (get-in result [:ending :kind])))
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
          (is (= {:status :stopped :reason :unreachable} (head result [:status :reason])))
          (is (= 3 @clicks) "the first click and one after each of two walks closer")
          (is (= 1 (count (kinds seen :toggle.gave-up)))))))))

(deftest the-job-is-registered-with-doc-and-args
  (let [j (get registry/jobs job)]
    (is (string? (:doc j)))
    (is (= 3 (get-in j [:args :reach :default])))
    (is (some? toggle/doc))))

;; ------------------------------------------------------------------ a dark block read back after the click

(defn make-dark
  "Perception's dark rule: the cell at pos is not made out (unknown), however the world reads."
  [p pos]
  (let [sensed (.-sensedAt p)]
    (aset p "sensedAt" (fn [q] (if (= (js->clj q :keywordize-keys true) pos) #js {:unknown true} (sensed q))))))

(deftest a-lever-in-the-dark-that-flipped-is-changed-not-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (world "lever" {:powered false} 3 4))
              _ (make-dark p at)
              result (await (child-outcome eng job {:pos at :state :on :reach 5} 40))]
          (is (= {:status :done :reason :changed} (head result [:status :reason])) (pr-str result))
          (is (true? (:powered (props p at)))))))))
