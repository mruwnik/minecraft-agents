(ns jobs.lib.threats
  "Mobs that chase: how far each follows its target (vanilla follow_range), and the :threat entries a flight leaves in
  body memory. One :threat entry per mob fled per flight {:mob :id :uuid :key :pos (last known) :ended}, kept
  threat-policy. The third flight from one mob (same :key, uuid else id) within the ttl warns hostile.chased.
  planner-dangers: the cells of sensed mobs and deaths by terrain go-to's searches keep off (the planner's
  options.dangers). known-threats: every mob the body knows may hurt it, seen, fled from or met at a death;
  threats-of gives each its reach (jobs.lib.threat-field) for jobs.lib.cost/death-chance, tolls-of prices each one's field
  for the planner (options.tolls, threat-tolls)."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [engine.ctx :as ctx]
            [engine.entity-observations :as obs]
            [engine.memory :as mem]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.death-spot :as death-spot]
            [jobs.lib.hide-spot :as hide-spot]
            [jobs.lib.threat-field :as field]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::chased-flights {:default 3 :doc "Flights from one mob within the ttl that tell the agent." :spec (a/int-in 1 nil)}
   ::fled-memory-ms {:default 180000 :doc "Milliseconds after a flight during which a mob it fled from still counts as near the place it was last known (errands that walk to a spot hold off), and the body is not sent out on a comfort errand." :spec (a/int-in 0 nil)}
   ::max-threats {:default 8 :doc "The most known threats (known-threats) a price reads, the nearest first." :spec (a/int-in 1 nil)}})

(def follow-range threat/follow-range)

(def max-follow-range threat/max-follow-range)

(def threat-policy {:cap 20 :ttl (* 5 60 1000)})

(defn fled-memory-ms [] (settings/get settings ::fled-memory-ms))

(defn fresh-threats
  "The :threat entries of view written within ::fled-memory-ms."
  [view]
  (filterv #(< (- (:now view) (:t %)) (fled-memory-ms)) (mem/entries view :threat)))

(defn listed-threats
  "The :threat entries among entries whose mob the client still tracks by id (a mob that blew up or died is gone; a
  hidden one is still listed): a filter only, so a spot is never read for a mob that is not there."
  [p entries]
  (let [listed (set (map #(.-id %) (array-seq (.entities p #js {:radius 1000 :kind "hostile" :max 1000}))))]
    (filterv #(contains? listed (:id (:data %))) entries)))

(defn fled-recently?
  "Whether a flight left a :threat entry within ::fled-memory-ms for a mob the client still lists (a killed one is no danger)."
  [p view]
  (boolean (seq (listed-threats p (fresh-threats view)))))

(defn fled-near?
  "Whether the field (jobs.lib.threat-field) of a listed mob fled from in the last ::fled-memory-ms, from where it was
  last known, holds the cell of spot {:x :y :z}."
  [c spot]
  (boolean (some #(when-let [at (danger-q/rough-pos (:data %))] (field/reaches? c {:mob (:mob (:data %)) :pos at} spot))
                 (listed-threats (:primitives c) (fresh-threats (ctx/view c))))))

(defn chased-flights [] (settings/get settings ::chased-flights))

(defn place-of
  "Where the body knows mob e of primitives p to be: {:pos} for one it has seen, else (heard only) what a sound tells,
  {:direction :band :from} (obs/rough-hearing from the body's place), never the exact place."
  [p e]
  (if (danger-q/seen-only? e)
    {:pos (u/pos-of (.-pos e))}
    (let [from (u/pos-of (.-pos (.self p)))]
      (assoc (obs/rough-hearing from (u/pos-of (.-pos e))) :from from))))

(def rough-pos danger-q/rough-pos)

(defn mob-key [e] (or (.-uuid e) (.-id e)))

(defn flights
  "How many :threat entries within the ttl name the mob key k."
  [c k]
  (count (filter #(= k (:key (:data %))) (ctx/entries c :threat))))

(defn remember!
  "Write the :threat entry for a mob fled ({:mob :id :uuid :ended and :pos, or :direction :band :from}); at the chased-flights-th from it, warn
  hostile.chased."
  [c {:keys [mob id uuid ended] :as t}]
  (let [k (or uuid id)
        place (select-keys t [:pos :direction :band :from])]
    (ctx/remember! c :threat (merge {:mob mob :id id :uuid uuid :key k :ended ended} place) threat-policy)
    (let [n (flights c k)]
      (when (= (chased-flights) n)
        (ctx/emit! c :hostile.chased :warn
                   (merge {:mob mob :id id :flights n}
                          (select-keys place [:pos :direction :band])
                          {                    :text (str "the " mob " " id " has chased the body " n " times in "
                               (/ (:ttl threat-policy) 60000) " min")}))))))

;; ---------------------------------------------------------------- dangers for the planner

(defn body-of [p]
  (let [self (.self p)]
    {:health (.-health self) :food (.-food self) :equipment (cost/equipment-of (.-equipment self))
     :weapon (combat/best-weapon p combat/default-weapons) :pos (u/pos-of (.-pos self))}))

(defn sensed-mobs
  "The real dangers primitives p senses (jobs.lib.danger/dangers within its known-scan: seen or heard, with a way to the
  body or a line of fire; never x-ray; danger-opts as danger-q/danger-in?), as [{:key :name :chaser (cost/mob-kind) :pos}]."
  [p danger-opts]
  (mapv (fn [e] {:key (mob-key e) :name (.-name e) :chaser (cost/mob-kind e) :pos (danger-q/mob-pos p e)})
        (danger-q/dangers p (danger-q/known-scan) {:ranged-radius (danger-q/known-scan)} danger-opts)))

(defn remembered-spots
  "The places of the :threat entries of c whose mob the client still tracks, as {:pos ..} data with a :pos (a mob
  that blew up or died is gone; a hidden one is still listed), for known-threats."
  [c]
  (keep #(let [d (:data %)] (when-let [at (rough-pos d)] (assoc d :pos at)))
        (listed-threats (:primitives c) (ctx/entries c :threat))))

(defn planner-dangers
  "The planner's options.dangers for the body of c (JS array, nil for none): the cell of each sensed mob (sensed-mobs,
  nearest first), no way through it (:block; its price is its field's tolls, threat-tolls), and each uncleared death by
  terrain (death/terrain-danger: the cells that killed are near its spot); at most cost/max-dangers."
  [c]
  (let [p (:primitives c)
        here (u/pos-of (.-pos (.self p)))
        ds (concat (for [{:keys [name pos]} (sort-by #(u/dist here (:pos %)) (sensed-mobs p {:way? false}))]
                     {:x (:x pos) :y (:y pos) :z (:z pos) :close 0 :radius 0 :rate 0 :mob name :block true})
                   (for [{:keys [pos cause weight mob?]} (death-spot/uncleared c) :when (not mob?)]
                     (cost/terrain-danger {:pos pos :cause cause :weight weight})))]
    (when (seq ds) (clj->js (vec (take cost/max-dangers ds))))))

;; ---------------------------------------------------------------- the threats every price reads

(defn max-threats [] (settings/get settings ::max-threats))

(defn known-threats
  "The mobs c's body knows may hurt it, [{:mob :pos :weight :key :chaser :fled :remembered}], at most ::max-threats, the nearest
  the body first: one sensed now (sensed-mobs, weight 1), :fled a :threat flight's last place (remembered-spots, 1; one whose
  :key is sensed now is left out: the sensed place wins), and, :remembered, a recent death or near-death by a mob at its
  spot (death-spot/recent, weighing death-spot/present-share of its fading weight; a walker's only while not seen-clear?, a water
  mob's whatever the light: it lives in the water, not in the dark) and a :hurt entry by a water mob (fading over the
  :hurt memory's ttl). One mob at one cell counts once, at its greatest weight."
  [c]
  (let [p (:primitives c)
        now (ctx/now c)
        ttl (:ttl (or (mem/policy (ctx/view c) :hurt) mem/default-policy))
        fade (fn [age] (if (number? ttl) (cost/death-weight age ttl) (cost/death-weight age)))
        sensed (sensed-mobs p {:way? false})
        now-keys (set (map :key sensed))
        here (u/pos-of (.-pos (.self p)))]
    (->> (concat (for [{:keys [key name chaser pos]} sensed] {:mob name :pos pos :weight 1 :key key :chaser chaser})
                 (for [{:keys [mob pos key]} (remembered-spots c) :when (not (contains? now-keys key))] {:mob mob :pos pos :weight 1 :key key :fled true})
                 (for [{:keys [cause pos weight mob?] :as d} (death-spot/recent c)
                       :when (and mob? (or (field/water-mobs cause) (not (death-spot/seen-clear? c d))))]
                   {:mob cause :pos pos :weight (death-spot/present-share c (assoc d :mob cause)) :remembered true})
                 (for [{:keys [t data]} (ctx/entries c :hurt)
                       :let [cause (death-spot/hurt-cause data) w (fade (- now t))]
                       :when (and (field/water-mobs cause) (:pos data) (pos? w))]
                   {:mob cause :pos (:pos data) :weight w :remembered true}))
         (filter #(and (string? (:mob %)) (:pos %)))
         (group-by (juxt :key field/field-key))
         vals
         (map #(apply max-key :weight %))
         (sort-by #(u/dist here (:pos %)))
         (take (max-threats))
         vec)))

(defn hide-threats
  "The mobs of the recent hides of c (:hid entries) at their spots, [{:mob :pos :weight :remembered}]: a stream that
  hunted the body there, weighing death-spot/present-share of its fade over hide-spot ::memory-ms."
  [c]
  (let [now (ctx/now c)
        ttl (settings/get hide-spot/settings ::hide-spot/memory-ms)]
    (vec (for [{:keys [t data]} (ctx/entries c :hid)
               :let [w (cost/death-weight (- now t) ttl)]
               :when (and (pos? w) (:pos data))
               mob (:mobs data)]
           {:mob mob :pos (:pos data) :weight (death-spot/present-share c {:mob mob :pos (:pos data) :weight w :age-ms (- now t)}) :remembered true}))))

(defn with-reach
  "threats ({:mob :pos}), each with its :reach (threat-field/reach-fn of its field): what death-chance reads."
  [c threats]
  (mapv #(assoc % :reach (field/reach-fn (field/field-of c %))) threats))

(defn threats-of
  "known-threats of c (those keep? holds of), with-reach."
  ([c] (threats-of c any?))
  ([c keep?] (with-reach c (filter keep? (known-threats c)))))

(defn death-body
  "The body of c as death-chance and death-cost read it: cost/death-body and body-of."
  [c]
  (let [p (:primitives c)] (merge (cost/death-body p (ctx/view c)) (body-of p))))

(defn danger-near-fn
  "A fn of a cell pos: the nearest known threat (threats-of c) the body would flee whose field weighs the cell in full
  (within threat-field ::close steps of its way), as {:mob :mob-pos :mob-distance}, else nil. Threats are read once."
  [c]
  (let [body (body-of (:primitives c))
        ts (filterv #(= :flee (cost/stance body (:mob %))) (threats-of c))]
    (fn [{:keys [x y z] :as pos}]
      (->> ts
           (keep (fn [{:keys [mob reach] :as t}]
                   (when (= 1 (reach (js/Math.floor x) (js/Math.floor y) (js/Math.floor z)))
                     {:mob mob :mob-pos (select-keys (:pos t) [:x :y :z]) :mob-distance (js/Math.round (u/dist pos (:pos t)))})))
           (sort-by :mob-distance)
           first))))

(defn tolls-of
  "go-to's tolls ([{:x :y :z :factor}]) for threats ({:mob :pos :weight}) of c: every cell of each one's field costs
  cost/threat-factor (the mob, its weight x the cell's) more a second; the dearest of one kind of mob where it is known
  twice, the kinds summed, at most cost/threat-factor-cap."
  [c threats]
  (let [body (body-of (:primitives c))
        cap (cost/threat-factor-cap body)
        by-cell (js/Map.)]
    (doseq [{:keys [mob weight] :as t} threats
            :let [f (cost/threat-factor body mob weight)]
            :when (pos? f)
            ^js cell (:cells (field/field-of c t))]
      (let [x (aget cell 0) y (aget cell 1) z (aget cell 2)
            k (str x "," y "," z)
            ^js row (or (.get by-cell k) (let [r #js {:x x :y y :z z :mobs {}}] (.set by-cell k r) r))]
        (set! (.-mobs row) (update (.-mobs row) mob (fnil max 0) (* f (aget cell 3))))))
    (mapv (fn [^js r] {:x (.-x r) :y (.-y r) :z (.-z r) :factor (min cap (reduce + 0 (vals (.-mobs r))))})
          (es6-iterator-seq (.values by-cell)))))

(defn threat-tolls
  "tolls-of the known-threats of c: go-to's price of every known mob (the planner's options.tolls). Read once per
  check-cache window for the same threats (threat-field/shared: one vector, so walk.world/with-tolls merges it once)."
  [c]
  (let [ts (known-threats c)]
    (if (empty? ts)
      []
      (field/shared c [::threat-tolls (mapv (fn [t] [(field/field-key t) (/ (js/Math.round (* 20 (:weight t))) 20)]) ts)]
                    #(tolls-of c ts)))))
