(ns jobs.movement.ride-rail
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.primitive :as primitive]
            [engine.settings :as settings]
            [jobs.lib.build.plan :as plan]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.cond :as cond]
            [jobs.lib.cost :as cost]
            [jobs.lib.look :as look]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.ready :as ready]
            [jobs.lib.sense :as sense]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]
            [jobs.lib.vehicle :as vehicle]
            [jobs.lib.world :as known]
            [plan.rail :as rail]))

(def doc
  "Ride the rail line a plan wants (:plan, optionally only its :part) in a minecart from the end nearer the body to the other end.
  Boards a minecart standing free on the start rail, else puts a carried one there (jobs.items.use), else fetches one (jobs.lib.need,
  the call's :ways), and gets in (jobs.movement.mount, which walks into reach). Then it rides in strokes of the paddle primitive:
  each looks at the next rail ahead of the cart and holds forward for one stroke (a still cart takes the rider's forward input as
  a push, so it starts on the two plain rails before the launch group; a moving cart ignores it). A stroke the cart moved in is
  followed by another. The cart standing still in the far end's rail cell is the arrival; standing still anywhere else after a
  pushed stroke ends the ride :stalled. Then it gets off (jobs.movement.leave-vehicle) and takes back a cart it put down itself
  (jobs.combat.attack, then jobs.items.collect-drops); a cart found on the rail stays at the far end.
  Needs: the plan; its line judged sound (plan.rail/judge-line over what the body knows: a cell never seen is a break, :unloaded;
  jobs.build.rail-line builds and judges a line); a minecart (fetched by default).
  Conditions (a listed call waits on them): :plan (missing or unreadable plan), :not-sound (data :breaks, the judge's), :aboard-other
  (on another vehicle), :need (no minecart and only :carried ways). Impossible: :bad-args, :not-a-line (the plan's rail cells are no
  single line).
  Ends done (data {:from :to :cart id}), or nothing-possible :stalled (data :pos, where the cart stopped; the body has got off and
  taken back its own cart), :no-cart (a cart put down is not seen on the rail), :board-failed / :leave-failed (the child's ending is
  the :because), :failed (a stroke the primitive refused, data :primitive), or the fetch's ending. A restart re-reads the world: aboard it rides on, at the far end on foot it gets off and takes
  back its cart.")

(a/defargs args
  {:plan {:doc "id of the rail line's plan (jobs.build.rail-line builds it)" :spec a/name? :default nil}
   :part {:doc "only the rail cells of this part" :spec a/name? :default nil}})

(def target [:plan])

(a/defargs settings
  {::job-time-s {:default 600 :doc "Seconds a ride may take, boarding and getting off included." :spec (a/num-in 0 nil)}})

(def defaults {:spend {:job-time ::job-time-s}})

(def cart-speed "Blocks a second a cart runs at the top speed of a powered line (vanilla: 0.4 a tick)." 8)
(def board-s "Seconds boarding and getting off take, walks aside, for the quote." 6)

(def safe? ready/threatened)
(def fit? ready/fit?)

;; ------------------------------------------------------------------ the line

(defn line
  "{:ps [[x y z] ...] :cells judged} of the plan's rail line in track order, or {:trouble {:reason :kind :text}}."
  [c]
  (let [{:keys [plan part]} (:args c)
        p (:primitives c)
        answer (known/plan c plan)
        cells (when (and answer (not (:broken answer)))
                (plan/judged p answer part (set (keys (storage/carried-totals (u/inventory p)))) nil))
        chain (when (seq cells) (rail/line cells))
        trouble (plan/plan-trouble answer cells)]
    (cond
      trouble {:trouble {:reason :plan :kind :later :text trouble}}
      (:error chain) {:trouble {:reason :not-a-line :kind :never :why (:error chain)
                                :text (str "its rail cells are not one line: " (name (:error chain)))}}
      :else {:ps (mapv :pos chain) :cells cells})))

(defn breaks
  "The judge's breaks of the line as the body knows it (a cell never seen is :unloaded: a ride cannot count on it)."
  [c cells]
  (:breaks (rail/judge-line cells (partial plan/world-block (:primitives c)))))

(defn xyz [[x y z]] {:x x :y y :z z})

(defn ends
  "[from to] of the line ps: the end nearer the body first."
  [c ps]
  (let [me (u/self-pos c)
        d #(u/dist me (vehicle/centre (xyz %)))]
    (if (<= (d (first ps)) (d (peek ps))) [(first ps) (peek ps)] [(peek ps) (first ps)])))

(defn route
  "{:from :to :ps (from first)}, the ends kept in job memory once chosen, so a restart rides on the same way."
  [c ps]
  (let [{:keys [from to]} (ctx/mem c)
        [from to] (if (and from to) [from to] (ends c ps))]
    {:from from :to to :ps (if (= from (first ps)) ps (vec (rseq ps)))}))

;; ------------------------------------------------------------------ the cart

(defn cell-of [pos] (mapv #(js/Math.floor %) [(:x pos) (:y pos) (:z pos)]))

(defn in-cell? [pos [x _ z]] (let [[px _ pz] (cell-of pos)] (and (= px x) (= pz z))))

(defn carts
  "The minecarts in sight, as entity maps."
  [c]
  (look/seen-entities (:primitives c) {:names ["minecart"]}))

(defn cart-by-id [c id] (first (filter #(= id (:id %)) (carts c))))

(defn free-cart-at
  "A minecart nobody rides standing in the rail cell, or nil."
  [c cell]
  (first (filter #(and (in-cell? (:pos %) cell) (empty? (:passengers %))) (carts c))))

(defn ridden [c] (vehicle/vehicle-of (.self (:primitives c))))

(defn carried? [c] (pos? (get (need/counts c ["minecart"]) "minecart" 0)))

;; ------------------------------------------------------------------ checks

(def bad-args (checks/args-fit "needs a :plan" #(string? (:plan (:args %)))))

(defn line-ok
  "Later :plan / never :not-a-line for a plan that is no line; later :not-sound while the line known has breaks."
  [c]
  (let [{:keys [trouble cells]} (line c)
        bs (when cells (breaks c cells))
        unseen (when (every? #(= :unloaded (:why %)) bs) (distinct (map :pos bs)))]
    (cond
      (= :never (:kind trouble)) (contract/never (:reason trouble) (:text trouble) :why (:why trouble))
      trouble (contract/later (:reason trouble) (:text trouble))
      (seq unseen) (sense/look-or-later c :not-sound (str "cells of the line were not seen: " (pr-str (take 4 unseen))) unseen)
      (seq bs) (contract/later :not-sound (str "the line is not sound: " (pr-str (take 4 bs))) :breaks bs
                               :hint "build and judge it with build.rail-line")
      :else (contract/ok :line "a sound line"))))

(defn aboard-other
  "Later :aboard-other while the body rides anything but a minecart."
  [c]
  (let [v (ridden c)]
    (if (and v (not= "minecart" (:name v)))
      (contract/later :aboard-other (str "aboard another vehicle, " (:name v)) :vehicle (:name v))
      (contract/ok :vehicle "on foot or in a minecart"))))

(def can-start? (cond/and bad-args line-ok aboard-other))
(def can-run? aboard-other)

(defn done?
  "Done once the cart has stood still at the far end, the body is off and its own cart is taken back."
  [c]
  (let [{:keys [arrived placed stalled from to cart]} (ctx/mem c)]
    (if (and arrived (not stalled) (nil? (ridden c)) (nil? placed))
      (contract/ok :arrived "rode the line" :units :rides :done 1 :left 0 :result {:from from :to to :cart cart})
      (contract/later :riding "not at the far end yet" :units :rides :done 0 :left 1))))

(defn quote-of [c]
  (let [{:keys [ps]} (line c)
        most (or (get-in (ctx/frozen-call c) [:spend :job-time]) (settings/get settings ::job-time-s))
        ride-s (/ (count ps) cart-speed)
        walk-s (if (seq ps) (/ (u/dist (u/self-pos c) (vehicle/centre (xyz (first (ends c ps))))) cost/walk-speed) 0)
        mean (min most (+ board-s ride-s walk-s))]
    (cond-> {:uses {:job-time [(min mean board-s) mean most]}}
      (seq ps) (assoc :moves-to (let [[x y z] (second (ends c ps))] [(+ x 0.5) y (+ z 0.5)])))))

;; ------------------------------------------------------------------ steps

(defn ^:async board!
  "Get into a cart on the start rail: :again once a step was made, :continue while a child waits, else the ending."
  [c {:keys [from]}]
  (let [found (free-cart-at c from)
        placed-id (:placed (ctx/mem c))
        placed (when placed-id (cart-by-id c placed-id))]
    (cond
      (or found placed)
      (let [id (:id (or placed found))
            m (await (ctx/call c :mount 'jobs.movement.mount {:id id}))]
        (cond
          (= :continue m) :continue
          (= :done (:kind m)) (do (ctx/update-mem! c assoc :cart id) :again)
          :else (ctx/end c :nothing-possible :board-failed {:id id :because m})))

      (carried? c)
      (let [e (await (ctx/call c :put 'jobs.items.use {:on (xyz from) :item "minecart"}))
            cart (free-cart-at c from)]
        (cond
          (= :continue e) :continue
          cart (do (ctx/update-mem! c assoc :placed (:id cart)) :again)
          :else (ctx/end c :nothing-possible :no-cart {:pos from :because e})))

      :else
      (let [e (await (need/item! c "minecart" 1))]
        (cond
          (= :continue e) :continue
          (= :done (:kind e)) :again
          :else e)))))

(defn ahead
  "The rail cell after the one the cart stands in, toward the far end; the far end when the cart is off the line."
  [ps pos]
  (let [i (first (keep-indexed #(when (in-cell? pos %2) %1) ps))]
    (get ps (inc (or i -1)) (peek ps))))

(defn ^:async stroke!
  "One stroke of the ride: :again, or the ending."
  [c {:keys [ps to]}]
  (let [r0 (await (ctx/act c :paddle #js {:ticks 0}))
        pos (u/pos-of (.-pos r0))
        at-end? (in-cell? pos to)
        [x y z] (ahead ps pos)
        _ (when-not at-end? (await (ctx/act c :look (clj->js {:pos {:x (+ x 0.5) :y (+ (:y pos) 0.5) :z (+ z 0.5)}}))))
        r (await (ctx/act c :paddle (clj->js {:ticks (primitive/js-setting "vehicle/paddle-max-ticks") :forward (not at-end?)})))
        still? (zero? (.-moved r))
        now (u/pos-of (.-pos r))]
    (cond
      (not= "ok" (.-status r)) (ctx/end c :nothing-possible :failed {:primitive (.-status r)})
      (and still? (in-cell? now to)) (do (ctx/update-mem! c assoc :arrived true) :again)
      (and still? (not at-end?)) (do (ctx/update-mem! c assoc :stalled (cell-of now) :arrived true) :again)
      :else :again)))

(defn ^:async get-off! [c]
  (let [e (await (ctx/call c :leave 'jobs.movement.leave-vehicle {}))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) :again
      :else (ctx/end c :nothing-possible :leave-failed {:because e}))))

(defn ^:async take-back!
  "Break the cart this call put down and pick up what falls: :again, :continue while a child waits."
  [c id]
  (let [cart (cart-by-id c id)
        r (if cart (await (child/run!* c :break 'jobs.combat.attack {:targets [id] :radius 8})) :gone)]
    (if (= :continue r)
      :continue
      (let [d (await (child/run!* c :collect 'jobs.items.collect-drops {:radius 4 :near (or (:pos cart) (u/self-pos c))}))]
        (if (= :continue d)
          :continue
          (do (ctx/update-mem! c dissoc :placed) :again))))))

(defn ^:async step [c]
  (let [{:keys [ps trouble]} (line c)
        {:keys [arrived placed stalled]} (ctx/mem c)]
    (if trouble
      (ctx/end c :condition-failed (:reason trouble) {:text (:text trouble)})
      (let [{:keys [from to] :as way} (route c ps)
            _ (ctx/update-mem! c assoc :from from :to to)
            v (ridden c)]
        (cond
          (and v arrived) (await (get-off! c))
          (and arrived placed) (await (take-back! c placed))
          (and arrived stalled) (ctx/end c :nothing-possible :stalled {:pos stalled})
          arrived (or (ctx/done c) :again)
          v (await (stroke! c way))
          :else (await (board! c way)))))))

(defn ^:async round [c]
  (await (pace/steps! c #(step c))))
