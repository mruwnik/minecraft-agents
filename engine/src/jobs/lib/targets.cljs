(ns jobs.lib.targets
  "The nearest of many targets by walking cost, for jobs that choose one of several (a tree, an ore block, ...).
  One planner search over the set of goals (planner query.goals: the goal test is any target's goal area, the heuristic
  the least over the targets), from the cell the body stands in. It stays within the walker's abilities
  (executor/planner-limits), over the walks' wide box, at most max-nodes nodes. A target walled off, or across a gap
  no move crosses, is passed over for a farther one the body can walk to.
  Bounded and resumable like go-to's search (jobs.lib.walk/run-search!). One call runs at most budget expansions
  and walk/round-ms. A search not over answers :searching and goes on at the next call from the same cell to the same
  targets (kept per body and tag, for at most walk/search-max-age-ms)."
  (:require [engine.path.executor :as executor]
            [engine.path.planner-tuned :as planner]
            [jobs.lib.walk :as walk]))

(def max-nodes
  "The node cap of one search over a set of targets: a few rounds of walk/round-budget. A set none of which is reached by
  then answers :none :budget (every target walled off floods all the land the box holds, ~1 s on the bench)."
  20000)

(def max-targets
  "Targets one search takes, the first of those given (callers give them nearest first): the heuristic looks at each."
  32)

(def proof-reasons
  "Planner reasons of a search that ran out of land to search: no target is reachable within the box and abilities."
  #{:exhausted :box :ladder-gap :air})

(defonce ^{:doc "The unfinished search of each [body tag]: {:key :t :plan}. key is [body cell, targets, range]; t when
  it began (ms); plan the planner's create-plan."}
  searches (atom {}))

(defn query
  "The planner query from the body's cell to within range of any of targets ({:x :y :z} cells)."
  [c targets range]
  (let [{:keys [x y z]} (first targets)
        ^js q (walk/plan-query c [x y z] range)]
    (set! (.-goals q) (to-array (map (fn [t] #js {:kind "near" :x (:x t) :y (:y t) :z (:z t) :range range}) targets)))
    q))

(defn new-search [c pw targets range key]
  {:key key :t (js/Date.now)
   :plan (planner/create-plan (.-snapshot pw) (query c targets range)
                              (walk/plan-options pw walk/default-weight
                                                 (executor/planner-limits (walk/body-policy c) (walk/solid-fn pw))
                                                 (assoc walk/wide-box :maxNodes max-nodes)))})

(defn answer
  "The answer of a search that is over, from its planner result r: {:status :found :target :index :cost} (cost in
  seconds of walking, risk counted as the planner does), or {:status :none :reason :proved}: proved when the search ran
  out of land with no frontier into unloaded land (no target is reachable for this walker), false when it ran out of nodes."
  [targets ^js r]
  (if (= "found" (.-status r))
    (let [i (.-goal r)
          ^js cost (.. r -path -cost)]
      {:status :found :target (nth targets i) :index i :cost (+ (.-seconds cost) (* 2 (.-risk cost)))})
    (let [reason (keyword (.-reason r))]
      {:status :none :reason reason :proved (and (contains? proof-reasons reason) (nil? (.-frontier r)))})))

(defn ^:async nearest!
  "The target of targets ({:x :y :z} cells, nearest first; the first max-targets are searched) the body reaches soonest
  by walking to within range of it: {:status :found :target :index :cost}, {:status :searching} (the search goes on at
  the next call), or {:status :none :reason :proved} (answer). One target is not searched ({:status :found :target t
  :index 0}): the walk to it decides. opts {:tag :budget}: tag keeps searches of different callers of one body apart
  (default :default), budget the expansions of one call (default walk/round-budget)."
  ([c targets range] (nearest! c targets range nil))
  ([c targets range {:keys [tag budget] :or {tag :default budget walk/round-budget}}]
   (let [targets (vec (take max-targets targets))
         pw (walk/path-world (:primitives c))]
     (cond
       (empty? targets) {:status :none :reason :no-targets :proved true}
       (= 1 (count targets)) {:status :found :target (first targets) :index 0}
       (nil? pw) {:status :none :reason :no-path-world :proved false}
       :else
       (let [who [(walk/body-name c) tag]
             k [(walk/body-cell c) targets range]
             kept (get @searches who)
             search (if (and (= k (:key kept)) (< (- (js/Date.now) (:t kept)) walk/search-max-age-ms))
                      kept
                      (new-search c pw targets range k))
             ^js p (:plan search)
             t0 (js/performance.now)]
         (loop [used walk/chunk-expansions]
           (walk/stop-if-cut! c)
           (cond
             ^boolean (.step p walk/chunk-expansions)
             (do (swap! searches dissoc who)
                 (answer targets (.result p)))

             (or (>= used budget) (>= (- (js/performance.now) t0) walk/round-ms))
             (do (swap! searches assoc who search)
                 {:status :searching})

             :else (do (await (walk/yield!))
                       (recur (+ used walk/chunk-expansions))))))))))
