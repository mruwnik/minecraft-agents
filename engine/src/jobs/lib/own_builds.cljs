(ns jobs.lib.own-builds
  "What the body counts as its own: cells covered by its plans, the wall of the own pen it stands in, blocks go-to put
  back or its scaffold ledger holds. Read by go-to's escalations, reach and blocks.dig."
  (:require [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.cost :as cost]
            [jobs.lib.escape :as escape]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.own-pen :as own-pen]
            [jobs.lib.reach :as reach]
            [jobs.lib.stair-memory :as smem]
            [jobs.lib.util :as u]
            [jobs.lib.world :as known]))

(defn feet-cell [c]
  (let [{:keys [x y z]} (reach/standing-cell (:primitives c))]
    [x y z]))

(defn own-plan-fn
  "(own-plan cell): the id of the plan this body made that covers cell, else nil."
  [c]
  (let [footprints (known/footprints c)
        own (access/own-plans c)]
    (fn [cell] (some own [(get footprints (vec cell))]))))

(defn plan-around-fn
  "(plan-around cell): the id of a plan this body made whose cells' bounding box holds cell, else nil."
  [c]
  (let [own (access/own-plans c)
        boxes (reduce (fn [m [cell id]]
                        (if-not (own id)
                          m
                          (update m id (fn [[lo hi]] (if lo [(mapv min lo cell) (mapv max hi cell)] [cell cell])))))
                      {} (known/footprints c))]
    (fn [cell] (some (fn [[id [lo hi]]] (when (every? true? (map <= lo cell hi)) id)) boxes))))

(defn with-own-pen
  "c with the own pen holding livestock the body stands in (jobs.lib.own-pen/shut-in, read on first use) as :own-pen: the
  fns below built from it share one read."
  [c]
  (assoc c :own-pen (own-pen/shut-in c (feet-cell c))))

(defn pen-of [c] (or (:own-pen c) (own-pen/shut-in c (feet-cell c))))

(defn own-build-fn
  "(own-build cell): :plan when a plan this body made covers cell, :pen when it is the wall of the own pen holding
  livestock the body stands in (pen-of), else nil."
  [c]
  (let [own-plan (own-plan-fn c)
        pen-wall? (:wall? (pen-of c))]
    (fn [cell]
      (cond (own-plan cell) :plan
            (pen-wall? cell) :pen))))

(defn own-fn
  "(own? cell block) of a way that may cut the body's own builds: own-placed's, or a cell of its own builds (own-build-fn)."
  [c own-placed]
  (let [own-build (own-build-fn c)]
    (fn [cell n] (or (own-placed cell n) (some? (own-build cell))))))

(def placed-policy
  "Body memory policy of :escalation-placed, the cells go-to put a block back in: later escalations may dig them."
  {:cap 100 :ttl (* 6 60 60 1000)})

(defn own-placed-fn
  "ledger/own-placed of c's memory, after the ledger's keep-alive!."
  [c]
  (ledger/entries c)
  (ledger/own-placed (ctx/view c)))

(defn own-dig-fn
  "(own? cell block) of a call that digs: own-placed-fn's, the body's own builds too when it may cut them (own-cut?)."
  [c own-cut?]
  (cond->> (own-placed-fn c) own-cut? (own-fn c)))

(defn not-diggable
  "The block at cell when an escalation's stair child may not cut it (escape/diggable?: not a protected
  block), else nil. For a cell first seen while digging; the zone, claim and footprint rules are the dig's own."
  [block-at cell]
  (when-not (escape/diggable? block-at (constantly true) cell)
    (block-at cell)))

(defn may-dig-fn
  "(may-dig? cell): no other owner's zone, claim or plan footprint refuses a dig there (jobs.lib.tidy/refusal), it is
  not a shelter block the call keeps for a hostile (mem :seal-kept) nor a plug of the body's against water (as its
  stairs, stair-memory/kept-plugs: the water behind it runs down the way), and unless own? the body's own builds
  (own-build-fn) are spared: a way through them is priced (own-cut-fn) and chosen apart."
  ([c] (may-dig-fn c false))
  ([c own?]
   (let [own-build (if own? (constantly nil) (own-build-fn c))
         ;; tidy/refusal's input read once: a route search asks of hundreds of cells
         rules (delay (access/rules-input c {:ignore-zones? false}))
         kept (into (:seal-kept (ctx/mem c) #{}) (smem/kept-plugs c))]
     (fn [cell]
       (and (not (contains? kept (vec cell)))
            (nil? (own-build cell))
            (or (boolean (:ignore-zones? (:args c))) (nil? (access/trespass-refusal @rules :dig (zipmap [:x :y :z] cell)))))))))

(defn own-cut-fn
  "(cut-s cell): seconds cutting cell costs on top of its dig when it is the body's own build (own-build-fn): what the
  cut destroys, put back whatever the block (jobs.lib.cost/destroyed-fn, own?) and, for the wall of its stocked
  pen, what the herd may lose through the gap while it stands open (jobs.lib.cost/herd-loss over the dig and the put-back);
  0 for any other cell."
  [c]
  (let [p (:primitives c)
        own-build (own-build-fn c)
        destroyed (cost/destroyed-fn (fn [[x y z]] (u/sensed p {:x x :y y :z z})))
        herd (delay (map :kind @(:herd (pen-of c))))]
    (fn [cell]
      (if-let [kind (own-build cell)]
        (let [n (u/seen-name p (zipmap [:x :y :z] cell))
              back (destroyed n cell true)]
          (+ back (if (= :pen kind) (cost/herd-loss @herd (+ back (or (some-> n cost/dig-cost) 0))) 0)))
        0))))
