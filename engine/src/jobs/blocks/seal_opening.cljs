(ns jobs.blocks.seal-opening
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.checks :as checks]
            [jobs.lib.choose :as choose]
            [jobs.lib.dig-look :as look]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.need :as need]
            [jobs.lib.next-call :as next-call]
            [jobs.lib.pace :as pace]
            [jobs.lib.ready :as ready]
            [jobs.lib.seal :as seal]
            [jobs.lib.util :as u]
            [jobs.lib.waits :as waits]))

(def doc
  "Seal where the body's own passages broke into open ground it did not dig: a stair or tunnel that opened into a cave.
  The own cells are those of the stairs and tunnels it cut (jobs.lib.ledger/own-cells); an opening is a seen open cell
  (air, cave_air, water or replaceable) beside an own cell within :radius of :at that is not an own cell and that no
  open way within the radius (and a few cells beyond) links to the body's cells or to the cells beside the ends it
  leaves its passages by (ledger/way-ends: the highest step of a stair, the first cell of a tunnel): those stay open,
  they are the way back. Unseen cells beside own cells are looked at once each. The openings are filled by a
  jobs.blocks.fill-cells child (it walks into reach, places under the zone, claim and plan rules unless :ignore-zones?,
  and fetches blocks by the call's :ways); it never digs.

  Needs own cells within the radius: none is impossible :no-own-cells at the start. done? is :ok when no opening is
  left and every unseen cell beside an own cell was looked at. Ends :done {:sealed n} (0: nothing was open), else
  with the fill's kind and cause (condition-failed :need when blocks run out, :refused, nothing-possible :failed-steps
  ...) or nothing-possible :not-filled (a fill that placed nothing), data {:sealed n :left [cells]} and :next (the fetch
  that unblocks it, then the call again).")

(a/defargs args
  {:at {:doc "the centre of the search, [x y z] or {:x :y :z}; nil: where the body stands" :spec a/position? :default nil}
   :radius {:doc "blocks (along each axis) round :at within which own cells are sealed" :spec (a/int-in 1 32) :default 8}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 600 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}})

(def margin "Cells beyond the radius the way back is followed, so the ground beside a stair's top just outside the radius is not taken for an opening." 3)

(def faces [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])

(def target [:at])

(def ways need/fetch-ways)

(def defaults {:use {:blocks :build} :spend {:job-time ::job-time-s}})

(def fit? ready/ok)

(defn within? [centre r cell] (every? #(<= (js/Math.abs %) r) (map - cell centre)))

(defn beside [cell] (map #(mapv + cell %) faces))

(defn openings
  "{:open [cells] :unseen [cells]}: the open cells beside the own cells (a set) within r of centre that no open way of other
  cells within r + margin links to the seeds (where the body may walk back), and the cells there kind-at cannot name.
  kind-at: cell -> jobs.lib.seal/kind of the seen block, nil when not seen."
  [kind-at own seeds centre r]
  (let [kind-at (memoize kind-at)
        box? #(within? centre (+ r margin) %)
        way? #(and (box? %) (not (own %)) (= :open (kind-at %)))
        sides (sort (distinct (for [cell own :when (within? centre r cell)
                                    side (beside cell) :when (not (own side))]
                                side)))
        candidates (filterv #(= :open (kind-at %)) sides)
        back (loop [todo (filterv way? seeds) seen (set todo)]
               (if-let [cell (and (not-every? seen candidates) (peek todo))]
                 (let [more (remove seen (filter way? (beside cell)))]
                   (recur (into (pop todo) more) (into seen more)))
                 seen))]
    {:open (filterv (complement back) candidates)
     :unseen (filterv #(nil? (kind-at %)) sides)}))

(defn centre [c] (u/cell-vec (or (a/cell (:at (:args c))) (u/self-pos c))))

(defn seeds
  "The cells the way back starts from: the body's feet and head and the cells beside the ends of its passages."
  [c]
  (let [feet (u/cell-vec (u/self-pos c))]
    (into [feet (update feet 1 inc)] (mapcat beside) (ledger/way-ends c))))

(defn own-near [c] (set (filter #(within? (centre c) (:radius (:args c)) %) (ledger/own-cells c))))

(defn found
  "openings for the call: its own cells, seeds, :at and :radius."
  [c]
  (let [p (:primitives c)
        kind-at #(some-> (u/seen-name p (a/cell %)) seal/kind)]
    (openings kind-at (ledger/own-cells c) (seeds c) (centre c) (:radius (:args c)))))

(defn to-look
  "The unseen cells beside own cells not looked at yet and still unknown to the body (loaded, never sensed)."
  [c {:keys [unseen]}]
  (let [looked (:looked (ctx/mem c) #{})]
    (filterv #(and (not (looked %)) (look/unknown? (:primitives c) %)) unseen)))

(def safe? (ready/safe-at-build (ready/near-unseen-here #(a/cell (centre %)))))

(defn can-start?
  [c]
  (if (empty? (own-near c))
    (contract/never :no-own-cells (str "the body cut no stair or tunnel within " (:radius (:args c)) " of " (pr-str (centre c))))
    (contract/ok :own-cells "own passages near")))

(defn done?
  "Ok when own cells are near, no opening is left and no unseen cell beside them is still to look at."
  [c]
  (let [f (found c)
        left (count (:open f))
        sealed (:sealed (ctx/mem c) 0)]
    (checks/goal (and (seq (own-near c)) (zero? left) (empty? (to-look c f)))
                 [:sealed (if (pos? sealed) (str "sealed " sealed " cells") "no opening is left")]
                 [:open (str left " openings are left")]
                 {:units :cells :done sealed :left left :result {:sealed sealed}})))

(defn quote-of
  [c]
  (let [n (count (:open (found c)))
        t (* 3 n)]
    {:uses {:job-time [0 t (* 3 t)]} :done [0 n n]}))

(defn fill-args [c cells]
  (cond-> {:cells cells :use {:blocks (vec (or (ctx/use c :blocks) (choose/filler-names c :build)))} :ways (need/pass-ways c)}
    (:ignore-zones? (:args c)) (assoc :ignore-zones? true)))

(defn ^:async step!
  [c]
  (let [f (found c)
        look (to-look c f)]
    (if (seq look)
      (do (await (look/look-unknown! c look))
          (ctx/update-mem! c update :looked (fnil into #{}) look)
          :again)
      (let [e (waits/waiting-out c (await (ctx/call c :fill 'jobs.blocks.fill-cells (fill-args c (:open f)))))
            placed (if (map? e) (get-in e [:data :mended] 0) 0)]
        (when (pos? placed) (ctx/update-mem! c update :sealed (fnil + 0) placed))
        (cond
          (= :continue e) :continue
          (and (= :done (:kind e)) (pos? placed)) :again
          :else (let [data {:sealed (:sealed (ctx/mem c) 0) :left (:open (found c))}]
                  (if (= :done (:kind e))
                    (ctx/end c :nothing-possible {:reason :not-filled :text "the fill placed nothing and openings are left"} data)
                    (contract/with-because (ctx/end c (:kind e) (:cause e) data) e))))))))

(defn ^:async round [c]
  (next-call/on-ending c (await (pace/steps! c #(step! c))) 'jobs.blocks.seal-opening args))
