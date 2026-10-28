(ns jobs.farm.find-spot
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [jobs.lib.torch :as torch]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.cost :as cost]
            [jobs.lib.ready :as ready]
            [jobs.lib.world :as known]
            [jobs.lib.pace :as pace]
            [plan.shape :as shape]))

(def meta {:tags [:goal]})

(def doc
  "Pick where a farm patch would go: the footprint of :plan (a plan id), else :w x :h. Ranks patches by their price in seconds
  (jobs.lib.patch score-patch). It tries patches nearest-first and stops once ::limit spots are found that no farther ring can beat
  (the best score a farther ring could have, less its walk, is no more than the worst kept). A patch of 8 or more is tried at every fourth corner. Reads blocks only: it never digs or marks
  anything. A scan reads at most jobs.lib.patch's ::max-reads blocks, then settles for the best spots so far. It never walks to the spot: a
  parent does.
  The search is one jobs.explore.search child with :patch, its walks priced against a 3-day food reserve (cost/food-search-worth): it looks round and scans the area round :center (or the body); while no patch
  is found it walks that job's legs and scans where each ends (with :center, the centre again: spots stay within :range of it), and stops at the first patch.
  With a :plan the patch is as big as the plan's cells in x and z, the farmland score counts only when the plan has crops or farmland, and
  the plan's own cells are not in the way.
  Ends with data {:spot pos :spots [...]}: done with the spots, or nothing-possible with cause and :why :distance (no leg left), :legs
  (the search's max legs walked), :stuck (three walks in a row failed), :time (the call's :job-time spent) or :no-plan (the plan is
  missing or unreadable), the search's :text and :best, the patch fewest cells short of a spot
  {:pos :w :h :short :unseen :no-floor :foreign :steep :dark :unfarmable :far :away} (cells with no seen ground, water/lava/ice on top,
  another's; off height, too dark or untillable where the crops do not pay for the fix; :far, short 0, a patch whose crops pay for
  all but the walk). A leg go-to gives up for the body's own shelter under a dark sky waits :dark (jobs.explore.search).
  :spots best first up to ::limit, each {:pos :w :h :score :worth :costs :level :work :step :water-share :sky :away :y :farmable :dark?}; :step is
  the largest height difference between two neighbouring cells of the patch. A pos is the north-west (minimum x and z) corner at
  ground level of the :w x :h patch, not its centre; :spot is the first spot's pos. :away is the distance from the patch's centre
  to :center (the body's cell without one), not from the corner.
  Score = :worth less :costs, seconds: worth is the crops of its cells, cost/sown-worth of wheat on wet or dry farmland, or
  the :worth arg shared over the cells, a dry one at the same share of a wet one (a :plan with no crops or farmland is worth the :worth arg; without it, an order with no cap, its spots rank by their costs alone); costs are :earthwork (cost/earthwork-s of :work, the blocks the cells are off the
  commonest ground height y), the :walk from :center (cost/walk-cost of :away), and, each only where it pays: torches for cells
  under crop light 9 (cost/light-cost, a trip when none is carried; :dark? true, jobs.farm.grow hangs them), water brought to dry
  cells (a trip a source and the irrigate step), a top a hoe cannot till dug out and filled with dirt. A patch scoring 0 or less
  is no spot. :level is the % of cells at y; open sky is reported in :sky, not scored. :farmable is the % of its cells whose
  ground is farmland or a hoe makes farmland of (dirt, grass, path, coarse or rooted dirt).
  A patch is left out when a column of it is unseen, lies under another plan's footprint, or in a zone or claim of another owner (the body's
  own zones are fine); :max-step drops patches with a bigger step.
  Needs nothing; it reads only what the body has seen and looks around at each place it scans.")

(a/defargs args
  {:plan {:doc "a plan id: the patch is the size of its cells; nil: :w x :h" :spec a/name? :default nil}
   :w {:doc "patch width (x), 1 to 16; without a :plan" :spec (a/int-in 1 16) :default 5}
   :h {:doc "patch height (z), 1 to 16; without a :plan" :spec (a/int-in 1 16) :default 5}
   :range {:doc "patches lie within this many blocks of :center in x and z, 1 to 48" :spec (a/int-in 1 48) :default 24}
   :center {:doc "where to search from {:x :y :z}; nil is the body's cell" :spec ::a/pos :default nil}
   :worth {:doc "seconds the patch is worth to the caller when every cell grows on wet farmland; nil: its crops' own worth (no crops: no cap)" :spec (a/num-in 0 nil) :default nil}
   :max-step {:doc "drop patches whose largest step between neighbouring cells is over this; nil: keep all" :spec (a/int-in 0 16) :default nil}})

(a/defargs settings
  {::depth {:default 12 :doc "How far above and below the centre's y to look for ground." :spec (a/int-in 1 16)}
   ::limit {:default 3 :doc "How many spots to keep; the scan stops once that many good ones are found." :spec (a/int-in 1 8)}
   ::job-time-s {:default 300 :doc "Seconds the call may take, exploring included (a guess until measured)." :spec (a/num-in 0 nil)}})

(def target [:center :plan :w :h])

(def defaults {:spend {:job-time ::job-time-s}})
(def safe?
  "Scanning needs only no danger near; the exploring walks are go-to children, which check their own light."
  (with-meta ready/threatened
    {::ready/exposure (fn [_] (contract/ok :survey "the scan walks nowhere; a leg's go-to child judges its own exposure"))}))
(def fit? (ready/fit-for :work))

(defn done?
  "Ok once spots are found; else later."
  [c]
  (let [spots (:spots (ctx/mem c))
        data [:units :spots :done (count spots) :left (if (seq spots) 0 1)
              :result (if (seq spots) {:spot (:pos (first spots)) :spots spots} {})]]
    (if (seq spots)
      (apply contract/ok :found (str "find-spot: " (count spots) " spots found") data)
      (apply contract/later :searching (str "find-spot: " (count spots) " spots found") data))))

(defn quote-of [_c]
  {:uses {:job-time [0.1 1 20]}})

(defn footprint
  "The patch a plan's cells need: {:w :h :farmland?} (x and z extent of the cells; farmland when one is a crop or farmland), nil for no cells."
  [cells]
  (when (seq cells)
    (let [xs (map #(get (:pos %) 0) cells)
          zs (map #(get (:pos %) 2) cells)]
      {:w (inc (- (apply max xs) (apply min xs)))
       :h (inc (- (apply max zs) (apply min zs)))
       :farmland? (boolean (some #(re-find #"farmland|crop " (shape/want-text (:want %))) cells))})))

(defn patch-of
  "The :patch arg of the search for c's args a, or nil when the plan is missing, unreadable or empty."
  [c a]
  (let [fp (if (:plan a)
             (let [pl (known/plan c (:plan a))]
               (when-not (known/unreadable? pl) (footprint (:cells pl))))
             (select-keys a [:w :h]))]
    (when fp
      (merge fp (select-keys a [:plan :range :max-step :worth]) {:torches (torch/torches-held (:primitives c))}
             {:depth (settings/get settings ::depth) :limit (settings/get settings ::limit)}))))

(defn ^:async step
  "One search child call; its first patch ends the job."
  [c]
  (let [a (merge (settings/job-defaults (quote jobs.farm.find-spot) args) (:args c))]
    (if-let [patch (patch-of c a)]
      (let [e (await (child/run!* c :search 'jobs.explore.search (cond-> {:patch patch :worth (cost/food-search-worth cost/food-reserve-points)} (:center a) (assoc :center (:center a)))))]
        (if (= :continue e)
          :continue
          (let [res (:data e)
                spots (some :spots (:found res))]
            (if (seq spots)
              (do (ctx/update-mem! c assoc :spots spots)
                  (ctx/end c :done nil {:spot (:pos (first spots)) :spots spots}))
              (let [why (or (:why res) :time)]
                (ctx/end c :nothing-possible why (cond-> {:spot nil :why why} (:text res) (assoc :text (:text res)) (:best res) (assoc :best (:best res)))))))))
      (ctx/end c :nothing-possible :no-plan {:spot nil :why :no-plan :text (str "plan " (:plan a) " is missing, unreadable or has no cells")}))))

(defn ^:async round
  "The whole attempt: the search child."
  [c]
  (await (pace/steps! c (fn ^:async s [] (await (step c))))))
