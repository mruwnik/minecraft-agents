(ns jobs.movement.go-to.holes
  "go-to's bookkeeping of the holes its escalations dig and the blocks it places: what the body may dig, what is its own,
  the cells a child planned to cut and the ledger entries that put them back."
  (:require [jobs.lib.block-sets :as bs]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as b]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.ways :as cw]
            [jobs.lib.escape :as escape]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.own-pen :as own-pen]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.util :as u]
            [jobs.lib.world :as known]
            [jobs.movement.go-to.result :as end]))

(defn own-plan-fn
  "(own-plan cell): the id of the plan this body made that covers cell, else nil."
  [c]
  (let [footprints (known/footprints c)
        own (access/own-plans c)]
    (fn [cell] (some own [(get footprints (vec cell))]))))

(defn own-build-fn
  "(own-build cell): :plan when a plan this body made covers cell, :pen when it is the wall of the own pen holding
  livestock the body stands in (jobs.lib.own-pen, read on first use), else nil."
  ([c] (own-build-fn c (own-pen/shut-in c (end/feet-cell c))))
  ([c pen]
   (let [own-plan (own-plan-fn c)
         pen-wall? (:wall? pen)]
     (fn [cell]
       (cond (own-plan cell) :plan
             (pen-wall? cell) :pen)))))

(defn may-dig-fn
  "(may-dig? cell): no other owner's zone, claim or plan footprint refuses a dig there (jobs.lib.tidy/refusal), it is
  not a shelter block the call keeps for a hostile (mem :seal-kept), and unless own? the body's own builds
  (own-build-fn) are spared: a way through them is priced (own-cut-fn) and chosen apart."
  ([c] (may-dig-fn c false))
  ([c own?]
   (let [own-build (if own? (constantly nil) (own-build-fn c))
         ;; tidy/refusal's input read once: a route search asks of hundreds of cells
         rules (delay (access/rules-input c {:ignore-zones? false}))
         kept (:seal-kept (ctx/mem c) #{})]
     (fn [cell]
       (and (not (contains? kept (vec cell)))
            (nil? (own-build cell))
            (or (boolean (:ignore-zones? (:args c))) (nil? (access/trespass-refusal @rules :dig (zipmap [:x :y :z] cell)))))))))

(defn own-cut-fn
  "(cut-s cell): seconds cutting cell costs on top of its dig when it is the body's own build (own-build-fn): putting it
  back (jobs.lib.cost.ways/put-back-cost, the block's worth when it drops something else) and, for the wall of its stocked
  pen, what the herd may lose through the gap while it stands open (jobs.lib.cost/herd-loss over the dig and the put-back);
  0 for any other cell."
  [c]
  (let [p (:primitives c)
        pen (own-pen/shut-in c (end/feet-cell c))
        own-build (own-build-fn c pen)
        herd (delay (map :kind @(:herd pen)))]
    (fn [cell]
      (if-let [kind (own-build cell)]
        (let [n (u/seen-name p (zipmap [:x :y :z] cell))
              lost (if (or (nil? n) (some #{n} (b/drops-of p n))) 0 (:value (cost/item-value [{:name n :count 1}])))
              back (cw/put-back-cost lost)]
          (+ back (if (= :pen kind) (cost/herd-loss @herd (+ back (or (some-> n cost/dig-cost) 0))) 0)))
        0))))

(defn own-fn
  "(own? cell block) of a way that may cut the body's own builds: own-placed's, or a cell of its own builds (own-build-fn)."
  [c own-placed]
  (let [own-build (own-build-fn c)]
    (fn [cell n] (or (own-placed cell n) (some? (own-build cell))))))

(def placed-policy
  "Body memory policy of :escalation-placed, the cells go-to put a block back in: later escalations may dig them."
  {:cap 100 :ttl (* 6 60 60 1000)})

(defn own-placed-fn
  "(own? cell block): go-to itself placed block at cell (a put-back), or the body's scaffold ledger holds it there
  (a pillar, a plug: its own temporary block; an :intent entry counts when the cell holds the intended item, as
  jobs.lib.ledger/decide reads it), so it is not someone's build."
  [c]
  (let [put-back (set (map (comp (juxt :cell :block) :data) (ctx/entries c :escalation-placed)))
        entries (into {} (map (juxt :cell identity)) (ledger/open-entries (ctx/view c)))]
    (fn [cell block]
      (or (contains? put-back [(vec cell) block])
          (when-let [e (get entries (vec cell))]
            (case (:state e)
              :intent (= :placed (ledger/decide e (constantly block)))
              (and (#{:placed :removing} (:state e)) (= block (:item e)))))))))

(defn forget-dug-own!
  "Drop from the scaffold ledger each entry of dug ({:cell :block}) whose block it was and whose cell is air now: a
  block built there later is someone else's."
  [c dug]
  (let [p (:primitives c)
        l (ledger/open-entries (ctx/view c))
        gone (set (for [{:keys [cell block]} dug
                        :let [e (ledger/entry-at l (vec cell))]
                        :when (and e (= block (:item e)) (bs/air (u/seen-name p (zipmap [:x :y :z] cell))))]
                    (vec cell)))]
    (when (seq gone)
      (ledger/remember! c (remove (comp gone :cell) l)))))

(defn note-placed! [c cell]
  (when-let [block (u/seen-name (:primitives c) (zipmap [:x :y :z] cell))]
    (ctx/remember! c :escalation-placed {:cell (vec cell) :block block} placed-policy)))


(defn planned-cells
  "The solid cells escalation e from feet would cut, [{:cell :block}]: the stair's cuts or clear-path's door, those
  the body has sensed (the child notes what it digs of the rest)."
  [p {:keys [step heading steps]} feet]
  (let [block-at (escape/block-at-of p)
        unseen? (escape/unseen-of p)
        cells (case step
                :stair (escape/stair-cuts feet (escape/heading-dirs heading) steps)
                :clear-path (:cells (escape/door block-at unseen? feet (escape/heading-dirs heading) (escape/max-door)))
                nil)]
    (vec (for [cell cells :let [n (block-at cell)] :when (and (escape/solid? n) (not (unseen? cell)))] {:cell cell :block n}))))

(defn own-holes
  "The ledger entries (jobs.lib.tidy :tidy, body memory) of cells this go-to's escalations dug, lowest first."
  [c]
  (->> (tidy/entries c)
       (filter #(and (:escalation %) (= (:root c) (:job %)) (= (:id c) (:go-to %))))
       (sort-by (comp second :cell))
       vec))

(defn hole-tag
  "What marks a hole as this go-to's in the tidy ledger; its stair child writes it (:note) as it digs; clear-path's are noted by go-to after each round and in its cleanup.
  :mend while the escalation cuts the body's own builds (:own-cut): every hole of it is put back, own ground or not."
  [c]
  (cond-> {:escalation true :go-to (:id c)} (:own-cut (:escalation (ctx/mem c))) (assoc :mend true)))

(defn note-holes!
  "Write every cell of dug ({:cell :block}) that is air now and not yet noted to the ledger, as a :dig entry with
  :escalation true and :any-of, the items that put it back (the block or what it drops). Body memory outlives the
  round, a cut, a cancel and a restart: jobs.survival.restore-broken puts back what go-to did not."
  [c dug]
  (let [own-plan (own-plan-fn c)]
    (doseq [{:keys [cell block]} (distinct dug)]
      (when-let [plan (own-plan cell)]
        (ctx/warn-once! c [:dug-own cell] :go-to.dug-own-build
                        {:cell (vec cell) :block block :plan plan :text (str "dug " block " at " (pr-str (vec cell)) " in its own plan " plan)}))
      (escape/note-hole! c (hole-tag c) cell block))))
