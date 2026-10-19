(ns jobs.survival.shut-in-night
  "What jobs.survival.shut-in adds for :for :night: the roof of a closed room mended with one block (the :room way), the
  :shut-in-failed memory of a way that failed on a site, the :shelter entry and the sealed/unsealed events."
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.blocks :as lb]
            [jobs.lib.choose :as choose]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.pen-gate :as pen-gate]
            [jobs.lib.shelter :as sh]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in-cells :as dig-cells]))

(a/defargs settings
  {::futile-radius {:default 8 :doc "Blocks around a site where a way failed that count as the same site." :spec (a/int-in 0 nil)}
   ::take-back-gate-range {:default 2 :doc "A cheap seal block this near a door or fence gate some plan wants is taken back with the dear ones: it would box the gate in." :spec (a/int-in 0 nil)}})

(defn futile-radius [] (settings/get settings ::futile-radius))

(def failed-policy {:cap 5 :ttl 600000})

(defn night? [c] (= :night (:for (:args c))))

(defn blocks
  "The blocks the call may place: its :use :blocks, else (for the night) the shelter fillers, else every :build filler."
  [c]
  (or (not-empty (ctx/use c :blocks)) (when (night? c) (choose/fillers :shelter))))

(defn failed-near
  "The :shut-in-failed entry data for way within futile-radius of the body, nil when none."
  [c way]
  (let [here (u/self-pos c)]
    (some #(let [{:keys [pos] w :way :as data} (:data %)] (when (and (= way w) (<= (u/dist here pos) (futile-radius))) data))
          (ctx/entries c :shut-in-failed))))

(defn fail!
  "A way ended other than :done on this site: remembered for the night, except for a lack of blocks or tool (its quote says so)."
  [c way reason]
  (when (and (night? c) (not (#{:no-blocks :no-tool} reason)))
    (ctx/remember! c :shut-in-failed {:pos (sh-sense/feet (:primitives c)) :way way :reason reason} failed-policy)))

(defn mend-cell
  "In a closed room with a door whose roof has a hole over the body: the cell to mend (dig-cells/room-plug), when a block is carried."
  [c]
  (when (seq (lb/carried c (blocks c)))
    (dig-cells/room-plug (:primitives c) (sh-sense/feet (:primitives c)) (:roof-height (:args c)))))

(defn ^:async mend!
  "The :room way: one block in the hole of the roof over the body. Resolves to :continue while a mob in the cell is waited out,
  else an ending-like {:kind :done|:nothing-possible :data {:placed [{:cell :item}] :text}}."
  [c]
  (let [item (lb/pick c (blocks c))
        plug (mend-cell c)]
    (if (or (nil? item) (nil? plug))
      {:kind :nothing-possible :cause :failed :data {:text "cannot mend the roof: no block or no hole"}}
      (let [r (await (tidy/place! c plug item true))
            status (.-status r)
            at (u/cell-vec plug)]
        (cond
          (#{"placed" "occupied"} status)
          (do (dig-cells/forget-wait! c :mob-since)
              (when (= "placed" status)
                (ledger/remember! c (-> (ledger/intend (ledger/open-entries (ctx/view c)) {:cell at :item item :before "air" :job (:id c) :purpose :shelter})
                                        (ledger/confirm at))))
              {:kind :done :data {:placed (if (= "placed" status) [{:cell at :item item}] [])}})
          (dig-cells/keep-waiting! c :mob-since r) :continue
          :else {:kind :nothing-possible :cause :failed :data {:text (str "cannot mend the roof: " status (dig-cells/mob-text r)) :status status}})))))

(defn near-gate?
  "Whether cell {:x :y :z} lies within ::take-back-gate-range of a door or fence gate some plan wants (jobs.lib.pen-gate)."
  [c {:keys [x y z]}]
  (boolean (some (fn [[gx gy gz]] (<= (u/dist {:x x :y y :z z} {:x gx :y gy :z gz}) (settings/get settings ::take-back-gate-range)))
                 (keys (pen-gate/opening-index (:world (:engine c)))))))

(defn open-text [cells]
  (str "open cells " (pr-str (mapv (juxt :x :y :z) cells))))

(defn spent-text [spent]
  (str/join ", " (map (fn [[item n]] (str n " " item)) (sort-by (comp - val) spent))))

(defn end!
  "The night's end of a call: the :shelter entry {:pos :state :built} (:sealed false when the world shows the body open;
  :roof the cell placed above it; :door the cells of one side placed, or of the pocket's way in; :start the cell dug from, or
  the top of the shaft walled at the bottom of; :take-back the placed {:cell :item} dearer than :take-back-value and any near
  a planned gate, which dig-in-leave/leave! digs again by day; :room true for a mended roof), and the shut-in.sealed / .unsealed event.
  way is the way that ended the call, data its result ({:placed :roof :start :spent}), sealed? whether the world shows the body shut in."
  [c way {:keys [placed roof start spent]} sealed?]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        cells (set (map (comp u/cell-pos :cell) placed))
        prev (:data (ctx/latest c :shelter))
        resealed (= feet (:pos prev))
        roof (case way :walls (update feet :y + 2) :room (when-let [r (first cells)] r) roof)
        door (case way
               :walls (dig-cells/door cells feet)
               :pocket (not-empty (mapv u/cell-pos (map :cell placed)))
               nil)
        start (or (case way :walls (:shaft-start (ctx/mem c)) :pit start nil) (when resealed (:start prev)))
        door (or door (when resealed (:door prev)))
        cheap (set (choose/cheap-blocks (keys spent) (:take-back-value (:args c))))
        take-back (->> (concat (when resealed (:take-back prev))
                               (for [{:keys [cell item]} placed :when (or (not (cheap item)) (near-gate? c (u/cell-pos cell)))] {:cell (u/cell-pos cell) :item item}))
                       (reduce #(assoc %1 (:cell %2) %2) {}) vals vec)
        cells-text (pr-str (mapv u/cell-vec cells))]
    (when (and (seq placed) (not sealed?))
      (let [open (dig-cells/open-cells p feet)]
        (ctx/emit! c :shut-in.unsealed :warn
                   {:pos feet :placed (mapv :cell placed) :open open
                    :text (str "NOT sealed in: placed " (count placed) " blocks at " cells-text
                               ", but the world still shows " (open-text open) (when (empty? open) " (no roof)"))})))
    (when (and (seq placed) sealed?)
      (ctx/emit! c :shut-in.sealed (if resealed :warn :info)
                 {:pos feet :placed (mapv :cell placed) :resealed resealed :spent spent
                  :text (str (cond (= :room way) "mended the roof of a closed room over the body: "
                                   resealed "sealed the shelter again: "
                                   :else "sealed in for the night: ")
                             "placed " (count placed) " blocks (" (spent-text spent) ") at " cells-text
                             ". At night an open shelter is closed again; it is left by day")}))
    (ctx/remember! c :shelter (cond-> {:pos feet :state :built}
                                (not sealed?) (assoc :sealed false)
                                (contains? cells roof) (assoc :roof roof)
                                door (assoc :door door)
                                start (assoc :start start)
                                (seq take-back) (assoc :take-back take-back)
                                (= :room way) (assoc :room true))
                   sh/shelter-policy)))
