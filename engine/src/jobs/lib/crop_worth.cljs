(ns jobs.lib.crop-worth
  "What the crops of a field are worth to a pass, shared by jobs.farm.tend and jobs.farm.harvest: the seen water that wets a
  field (waters), the rate a crop grows at (growth-rate), whether a cell holds a ripe crop now (ripe-now) and the worth of
  the harvests and replantings of a field's read cells (harvest-worth), unseen cells at the measured prior (cost/field-share)."
  (:require [jobs.lib.check-cache :as check-cache]
            [jobs.lib.cost :as cost]
            [jobs.lib.crops :as crops]
            [jobs.lib.farm :as lib-farm]
            [jobs.lib.look :as look]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]))

(defn waters
  "The seen water as a set of [x y z] near the cells; a cell poured is water only once it is seen as such. A start check (a ctx
  without a token) reads it once per memory epoch (once per cache window without a perception): a check asks for it several times a tick."
  [c cs]
  (let [p (:primitives c)
        me (u/self-pos c)
        cell (mapv js/Math.floor [(:x me) (:y me) (:z me)])
        epoch (sense/known-memory p)
        reach (min 64 (+ (lib-farm/water-reach) 1 (apply max 0 (map #(u/dist me %) cs))))
        read #(into #{}
                    (map (fn [{:keys [x y z]}] [x y z]))
                    (map :pos (look/seen-blocks p {:names ["water"] :radius reach :max 4096 :live? true})))]
    (cond (:token c) (read)
          epoch (check-cache/keyed c ::waters [reach cell epoch] read)
          :else (check-cache/cached c [::waters reach cell] read))))

(defn growth-rate
  "Stages a second the crop at pos grows (crops/stages-per-s): wet while water seen in ws (a set of [x y z]) reaches the
  ground under it, else dry (the worse case when its reach is not seen), at its light as seen."
  [p ws {:keys [x y z] :as pos}]
  (crops/stages-per-s (lib-farm/hydrated? ws x (dec y) z)
                      (when (sh-sense/seen-cell? p pos)
                        (some-> (sh-sense/block-light-at p pos) (max (or (sh-sense/sky-light-at p pos) 0))))))

(defn ripe-now
  "[crop chance] of the crop cell at pos (crop: the one wanted there, nil for any) holding a ripe crop now: in view 1 or 0;
  remembered out of view, cost/ripe-chance since it was seen at its growth-rate, counting only while within the server's simulation distance (cost/growing-s); another block [nil 0]; nil when never seen."
  [p ws pos crop]
  (let [b (u/sensed p pos)]
    (when-not (or (nil? b) (true? (.-unknown b)))
      (let [name (.-name b)
            age (or (.-age b) 0)]
        (cond
          (not (and (crops/ripe-age name) (or (nil? crop) (= crop name)))) [nil 0]
          (false? (.-visible b)) [name (cost/ripe-chance name age (cost/growing-s (/ (or (.-ageMs b) 0) 1000) (let [me (.-pos (.self p))] (js/Math.hypot (- (.-x me) (:x pos)) (- (.-z me) (:z pos))))) (growth-rate p ws pos))]
          :else [name (if (>= age (crops/ripe-age name)) 1 0)])))))


(defn harvest-worth
  "{:worth :ripe} of the rows [[pos crop read]] (read: ripe-now of the cell, nil never seen; crop nil: any) over water ws.
  - Harvest: each seen cell's ripe chance times its crop's worth (cost/crop-worth), and with replant? its replanting
    (cost/sown-worth at the cell's growth-rate).
  - A cell never seen has the share cost/field-share :ripe of the seen cells over the measured prior, at the least crop
    worth when the cell may hold any crop.
  :ripe is the crops expected ripe, seen and unseen."
  [p ws rows replant?]
  (let [seen (keep #(nth % 2) rows)
        unseen (remove #(nth % 2) rows)
        n (count seen)
        least #(apply min (map cost/crop-worth (if % [%] (keys crops/ripe-age))))
        replant #(if replant? (cost/sown-worth % (growth-rate p ws %2)) 0)
        ripe-seen (reduce + 0 (map (fn [[name chance]] (if name chance 0)) seen))
        ripe-share (cost/field-share :ripe ripe-seen n)]
    {:worth (+ (reduce + 0 (map (fn [[pos _ [name chance]]] (if name (* chance (+ (cost/crop-worth name) (replant name pos))) 0)) rows))
               (reduce + 0 (map (fn [[pos crop]] (* ripe-share (+ (least crop) (if crop (replant crop pos) 0)))) unseen)))
     :ripe (+ ripe-seen (* ripe-share (count unseen)))}))
