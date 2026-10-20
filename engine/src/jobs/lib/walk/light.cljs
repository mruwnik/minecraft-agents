(ns jobs.lib.walk.light
  "A walk's torch for a floor the body cannot see: place one at the step's takeoff, look until the floor shows."
  (:require [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.look :as look]
            [jobs.lib.placement :as placement]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.torch :as torch]
            [jobs.lib.util :as u]))

(def torch-placed-policy "Body memory of the torches walks placed (:torch-placed {:cell :by}), for a tidy-up." {:cap 64 :ttl :forever})

(defn ^:async see-lit!
  "Look at cell [x y z] (a floor a torch lights) until the body sees it (a full-effort sight pass, then see, the gate's: a pass takes in
  any point of the floor's top, a glance only its centre), at most light-wait-ms: true when seen."
  [c see wait-ms [x y z :as cell]]
  (await (ctx/act c :look (clj->js {:pos {:x (+ x 0.5) :y (+ y 0.5) :z (+ z 0.5)}})))
  (loop [waited 0]
    (look/see! c (look/search-effort))
    (cond
      (some? (see cell)) true
      (>= waited wait-ms) false
      :else (do (await (ctx/act c :wait #js {:ms 100}))
                (recur (+ waited 100))))))

(defn may-light?
  "The rules' verdict on a torch at cell [x y z] (jobs.lib.access/may? :sow: the body's own cells allowed): no other
  owner's zone or claim, none under :ignore-zones?."
  [c cell]
  (access/may? c :sow cell))

(defn ^:async light!
  "Light the floor of step k (done {:status :light :step k :cells [floor]}, the gate's) with a torch from the inventory
  (or the off hand) at its takeoff (jobs.lib.torch/torch-at: a wall torch at the head, else a floor torch at the feet,
  each where may-light?; placed through jobs.lib.tidy/place!), then look until the floor shows: nil to walk on, else
  the walk's done map: {:status :no-torch} with none held, {:status :replan :why :no-light :cells [takeoff]} when the
  torch could not go there, {:status :replan :why :unseen :no-light [takeoff]} when the floor stayed unseen."
  [c steps {:keys [step cells at]} see wait-ms]
  (let [p (:primitives c)
        tk (nth steps (dec step))
        g (nth steps step)
        feet [(:x tk) (:y tk) (:z tk)]
        floor (first cells)
        block-at (fn [[x y z]] (u/seen-name p {:x x :y y :z z}))
        no-light (fn [why] {:status :replan :why :no-light :learned true :cells [feet] :at at :step step :detail why})
        verdicts (into {} (map (fn [cell] [cell (may-light? c cell)])) [feet (update feet 1 inc)])
        refusal (when-not (some :ok (vals verdicts)) (:reason (get verdicts feet)))]
    (cond
      (zero? (torch/torches-held p)) {:status :no-torch :cells [feet] :at at :step step}
      refusal (no-light refusal)
      (not (await (torch/off-hand-to-inventory! c))) (no-light :off-hand-full)
      :else
      (let [choice (torch/torch-at [(js/Math.sign (- (:x g) (:x tk))) (js/Math.sign (- (:z g) (:z tk)))] feet
                                   (placement/eye (u/self-pos c)) block-at (comp :ok verdicts))
            cell (:cell choice)
            there? #(torch/torch-blocks (block-at cell))
            r (when (and cell (not (there?)))
                (await (tidy/place! c (zipmap [:x :y :z] cell) "torch" false (placement/js-click (:click choice)))))
            placed? (= "placed" (some-> r .-status))]
        (cond
          (:refused choice) (no-light (:refused choice))
          (not (or placed? (there?))) (no-light (keyword (.-status r)))
          :else
          (do (when placed?
                (ctx/remember! c :torch-placed {:cell cell :by (:id c)} torch-placed-policy)
                (ctx/emit! c :walk.torch-placed :info {:cell cell :floor floor :text (str "placed a torch at " (pr-str cell) " to see the floor at " (pr-str floor))}))
              (when-not (await (see-lit! c see wait-ms floor))
                {:status :replan :why :unseen :learned true :cells [floor] :no-light [feet] :at at :step step})))))))
