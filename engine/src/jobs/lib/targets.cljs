(ns jobs.lib.targets
  "The nearest of many targets by walking cost, for jobs that choose one of several (a tree, an ore block, ...).
  One planner search over the set of goals (planner query.goals: the goal test is any target's goal area, the heuristic
  the least over the targets), from the cell the body stands in. It stays within the walker's abilities
  (executor/planner-limits), over the walks' wide box, at most max-nodes nodes. A target walled off, or across a gap
  no move crosses, is passed over for a farther one the body can walk to.
  Bounded and resumable like go-to's search (jobs.lib.walk.search/run-search!). One call runs at most budget expansions
  and wsearch/round-ms. A search not over answers :searching and goes on at the next call from the same cell to the same
  targets (kept per body and tag, for at most wsearch/search-max-age-ms)."
  (:require [engine.path.executor :as executor]
            [engine.path.planner-tuned :as planner]
            [jobs.lib.walk :as walk]
            [jobs.lib.walk.world :as wworld]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.search :as wsearch]))

(def max-nodes
  "The node cap of one search over a set of targets: a few rounds of wsearch/round-budget. A set none of which is reached by
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
  "The planner query from the body's cell to within range of any of targets ({:x :y :z (:range)} cells; a target's own
  :range replaces range)."
  [c targets range]
  (let [{:keys [x y z] :as t} (first targets)
        ^js q (wplan/plan-query c [x y z] (or (:range t) range))]
    (set! (.-goals q) (to-array (map (fn [t] #js {:kind "near" :x (:x t) :y (:y t) :z (:z t) :range (or (:range t) range)}) targets)))
    q))

(defn new-search
  "A search over targets of the world costed as go-to's plans are (wworld/costed-world: known dangers and darkness; opts
  :dangers and :dark, default true)."
  [c pw targets range key {:keys [dangers dark] :or {dangers true dark true}}]
  {:key key :t (js/Date.now)
   :plan (let [policy (wworld/body-policy c)]
           (planner/create-plan (.-snapshot pw) (query c targets range)
                                (wplan/with-drops
                                  (wplan/plan-options (wworld/costed-world c pw {:dangers? dangers :dark? dark}) walk/default-weight
                                                      (executor/planner-limits policy (wworld/solid-fn pw))
                                                      (assoc wplan/wide-box :maxNodes max-nodes))
                                  policy (wworld/landing-seen (:primitives c)))))})

(defn answer
  "The answer of a search that is over, from its planner result r: {:status :found :target :index :cost} (cost in
  seconds of walking, risk and darkness counted as the planner does), or {:status :none :reason :proved}: proved when the search ran
  out of land with no frontier into unloaded land (no target is reachable for this walker), false when it ran out of nodes."
  [targets ^js r]
  (if (= "found" (.-status r))
    (let [i (.-goal r)
          ^js cost (.. r -path -cost)]
      {:status :found :target (nth targets i) :index i :cost (+ (.-seconds cost) (* 2 (.-risk cost)) (.-darkSeconds cost))})
    (let [reason (keyword (.-reason r))]
      {:status :none :reason reason :proved (and (contains? proof-reasons reason) (nil? (.-frontier r)))})))

(defn ^:async nearest!
  "The target of targets ({:x :y :z} cells, nearest first; the first max-targets are searched) the body reaches soonest
  by walking to within range of it: {:status :found :target :index :cost}, {:status :searching} (the search goes on at
  the next call), or {:status :none :reason :proved} (answer). One target is not searched ({:status :found :target t
  :index 0}): the walk to it decides. A target may carry its own :range. opts {:tag :budget :dangers :dark}: tag keeps
  searches of different callers of one body apart (default :default), budget the expansions of one call (default
  wsearch/round-budget), dangers and dark (default true) whether known dangers and darkness are costed (new-search)."
  ([c targets range] (nearest! c targets range nil))
  ([c targets range {:keys [tag budget] :or {tag :default budget wsearch/round-budget} :as opts}]
   (let [targets (vec (take max-targets targets))
         pw (wworld/path-world (:primitives c))]
     (cond
       (empty? targets) {:status :none :reason :no-targets :proved true}
       (= 1 (count targets)) {:status :found :target (first targets) :index 0}
       (nil? pw) {:status :none :reason :no-path-world :proved false}
       :else
       (let [who [(wworld/body-name c) tag]
             k [(wworld/body-cell c) targets range]
             kept (get @searches who)
             search (if (and (= k (:key kept)) (< (- (js/Date.now) (:t kept)) wsearch/search-max-age-ms))
                      kept
                      (new-search c pw targets range k opts))
             ^js p (:plan search)
             t0 (js/performance.now)]
         (loop [used wplan/chunk-expansions]
           (wplan/stop-if-cut! c)
           (cond
             ^boolean (.step p wplan/chunk-expansions)
             (do (swap! searches dissoc who)
                 (answer targets (.result p)))

             (or (>= used budget) (>= (- (js/performance.now) t0) wsearch/round-ms))
             (do (swap! searches assoc who search)
                 {:status :searching})

             :else (do (await (wplan/yield!))
                       (recur (+ used wplan/chunk-expansions))))))))))
