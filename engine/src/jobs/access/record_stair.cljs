(ns jobs.access.record-stair
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.stair-memory :as smem]
            [jobs.lib.access :as access]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.ready :as ready]
            [jobs.lib.reach :as reach]
            [plan.shape :as shape]))

(def doc
  "Remember the stair the body stands on as one of its own (memory :stair-way), reading the cells it has seen: a run of
  stands, each one block along :heading's axis and one up or down from the last, with a solid floor, air for the feet and
  head and air over the head (the cells jobs.access.stair cuts). The run through the body's cell is taken both ways to its
  ends; of the four lines through the cell the longest wins. The floors under every stand of the run become one entry,
  joined with the earlier entries that hold any of them, so a stair cut before the memory kept it whole (or dug by hand) is
  known to gather.mine (:stay-level breach, way-floor guard) and go-to's own way up again. Nothing is dug, placed or walked.

  :heading limits the lines to its axis. The body first looks at the cells beyond each end of the runs; a cell not seen ends a run there.

  Ends :done with data {:steps :top :bottom :floors} (the floors now in the entry; recording a stair already known changes
  nothing), or condition-failed :no-stair when no run of at least ::min-steps steps passes through the body's cell.")

(a/defargs args
  {:heading {:doc ":north :east :south or :west: look along that axis only; nil: both axes" :spec #{:north :east :south :west} :default nil}})

(a/defargs settings
  {::job-time-s {:default 30 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}
   ::min-steps {:default 2 :doc "Steps a run needs to count as a stair, not a hop." :spec (a/int-in 1 nil)}
   ::max-looks {:default 40 :doc "Looks at cells beyond the ends of the runs before it reads what it has seen." :spec (a/int-in 0 nil)}
   ::max-steps {:default 400 :doc "Steps read along a line each way before it is cut off." :spec (a/int-in 1 nil)}})

(def target [])

(def safe? ready/ok)

(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s}})

(defn stand?
  "Whether cell is a stand: a solid seen floor under it, air in it and over it."
  [block-at cell]
  (and (rules/solid-floor? block-at (shape/add cell [0 -1 0]))
       (rules/air (block-at cell))
       (rules/air (block-at (shape/add cell [0 1 0])))))

(defn step?
  "Whether from -> to is a step of a stair: both stands, and the air over the head of to that a stair cuts."
  [block-at from to]
  (and (stand? block-at from) (stand? block-at to)
       (rules/air (block-at (shape/add (if (> (to 1) (from 1)) from to) [0 2 0])))))

(defn run-along
  "The stands reached from cell by steps of d, nearest first."
  [block-at cell d limit]
  (loop [cur cell out []]
    (let [nxt (shape/add cur d)]
      (if (and (< (count out) limit) (step? block-at cur nxt))
        (recur nxt (conj out nxt))
        out))))

(defn lines-through
  "The stand runs through cell, one per axis and slope, top stand first: [{:heading :stands}]. A run along one axis
  and slope read the other way is the same run, so each is taken once."
  [block-at cell headings limit]
  (for [h headings
        rise [-1 1]
        :let [[dx dz] (shape/headings h)
              d [dx rise dz]
              back (run-along block-at cell (mapv - d) limit)
              fwd (run-along block-at cell d limit)
              stands (vec (concat (reverse back) [cell] fwd))]]
    {:heading h :d d :stands (if (neg? rise) stands (vec (reverse stands)))}))

(defn best-run
  "The longest of lines, or nil when it has fewer than min-steps steps."
  [lines min-steps]
  (let [best (apply max-key #(count (:stands %)) lines)]
    (when (and best (>= (dec (count (:stands best))) min-steps)) best)))

(defn run-of
  "The best run through the body's cell (see best-run), or nil."
  [c]
  (let [p (:primitives c)
        headings (case (:heading (:args c)) nil [:east :south] (:east :west) [:east] [:south])]
    (best-run (lines-through (access/seen-at p) (reach/standing-vec p) headings (settings/get settings ::max-steps))
              (settings/get settings ::min-steps))))

(defn floors-of [run] (mapv #(shape/add % [0 -1 0]) (:stands run)))

(defn frontier
  "The cells of the steps just beyond each end of lines (the stand, its floor, the cells over it, the air over the lower
  stand): what a look must have shown before the run is read."
  [lines]
  (distinct
   (for [{:keys [d stands]} lines
         [e sign] [[(first stands) 1] [(first stands) -1] [(peek stands) 1] [(peek stands) -1]]
         :let [n (shape/add e (mapv #(* sign %) d))
               lower (if (< (n 1) (e 1)) n e)]
         cell [n (shape/add n [0 -1 0]) (shape/add n [0 1 0]) (shape/add lower [0 2 0])]]
     cell)))

(defn ^:async see-ends!
  "Look at the unseen cells beyond the ends of the runs through the body's cell, nearest first, each once, at most
  ::max-looks looks (a cell behind rock stays unseen)."
  [c]
  (let [p (:primitives c)
        feet (reach/standing-vec p)
        headings (case (:heading (:args c)) nil [:east :south] (:east :west) [:east] [:south])]
    (loop [tried #{} n 0]
      (let [lines (lines-through (access/seen-at p) feet headings (settings/get settings ::max-steps))
            todo (remove tried (filter #(dig-look/unknown? p %) (frontier lines)))]
        (when (and (seq todo) (< n (settings/get settings ::max-looks)))
          (await (dig-look/look-at! c (first todo)))
          (recur (conj tried (first todo)) (inc n)))))))

(defn ^:async round [c]
  (let [feet (reach/standing-vec (:primitives c))
        _ (await (see-ends! c))
        run (run-of c)]
    (cond
      (nil? run)
      (ctx/end c :condition-failed :no-stair {:at feet :why (str "no stair of " (settings/get settings ::min-steps) " steps or more through " (pr-str feet) " in the cells seen")})
      :else
      (let [stands (:stands run)
            floors (floors-of run)
            all (smem/remember-way! c (first floors) floors)
            data {:steps (dec (count stands)) :top (first stands) :bottom (peek stands) :floors (count all)}]
        (ctx/end c :done nil (assoc data :text (str "recorded the stair from " (pr-str (first stands)) " to " (pr-str (peek stands)) ", " (:steps data) " steps")))))))

(defn done?
  "Done once one :stair-way entry holds every floor of the run through the body's cell."
  [c]
  (let [run (run-of c)
        known (some-> run floors-of set)]
    (if (and known (some #(every? (set (:floors (:data %))) known) (ctx/entries c :stair-way)))
      (contract/ok :already "the stair is recorded" :units :stairs :done 1 :left 0)
      (contract/later :not-yet "the stair is not recorded" :units :stairs :done 0 :left 1))))

(defn quote-of [_c] {:uses {:job-time [0.1 0.5 (settings/get settings ::job-time-s)]}})
