(ns jobs.lib.threats
  "Mobs that chase: how far each follows its target (vanilla follow_range), and the :threat entries a flight leaves in
  body memory. One :threat entry per mob fled per flight {:mob :id :uuid :key :pos (last known) :ended}, kept
  threat-policy. The third flight from one mob (same :key, uuid else id) within the ttl warns hostile.chased.
  planner-dangers: the known dangers go-to's searches cost (the planner's options.dangers), costed by
  jobs.lib.cost/danger-list. water-tolls: the water a mob that lives in water was met in, the whole body of it, costed
  by jobs.lib.cost/habitat-factor (the planner's options.tolls)."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [engine.ctx :as ctx]
            [engine.entity-observations :as obs]
            [engine.memory :as mem]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.death-spot :as death-spot]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::default-follow-range {:default 16 :doc "Blocks a mob of unknown kind notices the body from." :spec (a/int-in 1 nil)}
   ::chased-flights {:default 3 :doc "Flights from one mob within the ttl that tell the agent." :spec (a/int-in 1 nil)}
   ::fled-memory-ms {:default 180000 :doc "Milliseconds after a flight during which a mob it fled from still counts as near the place it was last known (errands that walk to a spot hold off), and the body is not sent out on a comfort errand." :spec (a/int-in 0 nil)}
   ::fled-radius {:default 40 :doc "Blocks around a fled mob's last known place that an errand's walk treats as dangerous." :spec (a/num-in 0 nil)}
   ::sensed-radius {:default 24 :doc "Blocks out to which a sensed real danger is costed." :spec (a/int-in 1 nil)}
   ::water-cells {:default 8000 :doc "The most seen water cells one water mob's body of water is costed over (water-tolls; the planner looks each up per node)." :spec (a/int-in 1 nil)}})

(def follow-ranges
  "Blocks a mob keeps chasing its target out to (vanilla follow_range attribute)."
  {"zombie" 35 "husk" 35 "drowned" 35 "zombie_villager" 35 "zombified_piglin" 35
   "enderman" 64 "pillager" 32 "vindicator" 12 "evoker" 12 "blaze" 48 "ghast" 100})

(defn default-follow-range [] (settings/get settings ::default-follow-range))

(defn follow-range [mob-name] (get follow-ranges mob-name (default-follow-range)))

(def max-follow-range (apply max (vals follow-ranges)))

(def threat-policy {:cap 20 :ttl (* 5 60 1000)})

(defn fled-memory-ms [] (settings/get settings ::fled-memory-ms))

(defn fled-radius [] (settings/get settings ::fled-radius))

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
  "Whether a listed mob fled from in the last ::fled-memory-ms was last known within ::fled-radius of spot {:x :y :z}."
  [p view spot]
  (boolean (some #(some-> (danger-q/rough-pos (:data %)) (u/dist spot) (<= (fled-radius))) (listed-threats p (fresh-threats view)))))

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

(defn sensed-radius [] (settings/get settings ::sensed-radius))

(defn body-of [p]
  (let [self (.self p)]
    {:health (.-health self) :equipment (cost/equipment-of (.-equipment self))
     :weapon (combat/best-weapon p combat/default-weapons) :pos (u/pos-of (.-pos self))}))

(defn sensed-mobs
  "The real dangers primitives p senses within sensed-radius (jobs.lib.danger/dangers: seen or heard, with a way to the body
  or a line of fire; never x-ray; danger-opts as danger-q/danger-in?), as [{:key :name :pos}]."
  [p danger-opts]
  (mapv (fn [e] {:key (mob-key e) :name (.-name e) :pos (danger-q/mob-pos p e)})
        (danger-q/dangers p (sensed-radius) {:ranged-radius (sensed-radius)} danger-opts)))

(defn remembered-spots
  "The places of the :threat entries of c whose mob the client still tracks, as {:pos ..} data with a :pos (a mob
  that blew up or died is gone; a hidden one is still listed), for known-dangers."
  [c]
  (keep #(let [d (:data %)] (when-let [at (rough-pos d)] (assoc d :pos at)))
        (listed-threats (:primitives c) (ctx/entries c :threat))))

(defn known-dangers
  "danger-list of the body of primitives p: sensed-mobs and the remembered :threat entries' data."
  ([p remembered] (known-dangers p remembered nil))
  ([p remembered opts] (cost/danger-list (body-of p) (sensed-mobs p {:way? false}) remembered opts)))

(defn death-dangers
  "The uncleared recent deaths of c (death-spot/uncleared) as remembered dangers [{:key :mob :pos :weight :terrain?}]: the plan keeps away from where the body died lately, at a cost: the spot stays a cell a walk to the body's pile may enter."
  [c]
  (map-indexed (fn [i {:keys [pos cause weight mob?]}] {:key (str "death-" i) :mob cause :pos pos :weight weight :block false :terrain? (not mob?)}) (death-spot/uncleared c)))

(defn planner-dangers
  "The planner's options.dangers for the body of c (JS array, nil for none): known-dangers with the :threat spots its
  flights left and the spots of its recent deaths."
  [c]
  (let [ds (known-dangers (:primitives c) (concat (remembered-spots c) (death-dangers c)) (cost/danger-opts))]
    (when (seq ds) (clj->js ds))))

(defn danger-near-fn
  "A fn of a cell pos: the nearest known mob (known-dangers of c: sensed or remembered) the body would flee that has pos
  within its full-cost radius, as {:mob :mob-pos :mob-distance}, else nil. Dangers are read once."
  [c]
  (let [p (:primitives c)
        body (body-of p)
        ds (known-dangers p (remembered-spots c) (cost/danger-opts))]
    (fn [pos]
      (->> ds
           (keep (fn [{:keys [mob close] :as d}]
                   (let [dist (u/dist pos d)]
                     (when (and (<= dist close) (= :flee (cost/stance body mob)))
                       {:mob mob :mob-pos (select-keys d [:x :y :z]) :mob-distance (js/Math.round dist)}))))
           (sort-by :mob-distance)
           first))))

;; ---------------------------------------------------------------- water a mob lives in

(def water-mobs "Mobs that live in water: they stay in the body of water they were met in, not round a point." #{"drowned" "guardian" "elder_guardian"})

(defn water-mob-spots
  "Where c's body met a mob that lives in water, [{:mob :pos :weight}]: one sensed now (weight 1), a :threat flight's
  last place (1), a recent death by one (death-spot/recent, its fading weight; seeing the spot lit does not clear it,
  the mob lives in the water, not in the dark) and a :hurt entry by one (fading over the :hurt memory's ttl)."
  [c]
  (let [p (:primitives c)
        now (ctx/now c)
        ttl (:ttl (or (mem/policy (ctx/view c) :hurt) mem/default-policy))
        fade (fn [age] (if (number? ttl) (cost/death-weight age ttl) (cost/death-weight age)))]
    (concat (for [{:keys [name pos]} (sensed-mobs p {:way? false}) :when (water-mobs name)] {:mob name :pos pos :weight 1})
            (for [{:keys [mob pos]} (remembered-spots c) :when (water-mobs mob)] {:mob mob :pos pos :weight 1})
            (for [{:keys [cause pos weight]} (death-spot/recent c) :when (water-mobs cause)] {:mob cause :pos pos :weight weight})
            (for [{:keys [t data]} (ctx/entries c :hurt)
                  :let [cause (death-spot/hurt-cause data) w (fade (- now t))]
                  :when (and (water-mobs cause) (:pos data) (pos? w))]
              {:mob cause :pos (:pos data) :weight w}))))

(defn water? [p x y z] (= "water" (u/seen-name p {:x x :y y :z z})))

(defn water-surface
  "[x y z] of the top seen water cell of the column at or next to pos (the cell, up to 2 below it, or a side neighbour's:
  a body hit from the water stood on its shore), nil when no water is seen there."
  [p {:keys [x y z]}]
  (let [x (js/Math.floor x) y (js/Math.floor y) z (js/Math.floor z)]
    (some (fn [[dx dz]]
            (when-let [wy (some #(when (water? p (+ x dx) % (+ z dz)) %) [y (dec y) (- y 2)])]
              [(+ x dx) (loop [wy wy] (if (water? p (+ x dx) (inc wy) (+ z dz)) (recur (inc wy)) wy)) (+ z dz)]))
          [[0 0] [1 0] [-1 0] [0 1] [0 -1]])))

(defn water-body
  "The seen water cells [x y z] of the body of water whose top cell is surface: the columns whose cell at its level is seen
  water, side by side from it, each from that level down while seen water; at most n cells, the nearest columns first."
  [p [sx sy sz] n]
  (loop [queue #queue [[sx sz]] seen #{[sx sz]} cells (transient [])]
    (if (or (empty? queue) (>= (count cells) n))
      (persistent! cells)
      (let [[x z] (peek queue)
            column (take-while #(water? p x % z) (range sy (- sy 384) -1))
            more (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]]
                       :let [k [(+ x dx) (+ z dz)]]
                       :when (and (not (contains? seen k)) (water? p (first k) sy (second k)))]
                   k)]
        (recur (into (pop queue) more) (into seen more) (reduce #(conj! %1 [x %2 z]) cells (take (- n (count cells)) column)))))))

(defn water-tolls
  "go-to's tolls ([{:x :y :z :factor}]) for the bodies of water c's body met a water mob in (water-mob-spots): every seen
  cell of each (water-body, at most ::water-cells) costs habitat-factor more a second, the dearest mob where two share
  a cell. Read once per check-cache window for the same spots."
  [c]
  (let [p (:primitives c)
        spots (water-mob-spots c)]
    (if (empty? spots)
      []
      (check-cache/cached c [::water-tolls (mapv (fn [{:keys [mob pos weight]}] [mob (update-vals pos js/Math.floor) (/ (js/Math.round (* 20 weight)) 20)]) spots)]
                          (fn []
                            (let [body (body-of p)
                                  n (settings/get settings ::water-cells)
                                  factors (reduce (fn [m {:keys [mob pos weight]}]
                                                    (let [f (cost/habitat-factor body mob weight)]
                                                      (if-let [top (when (pos? f) (water-surface p pos))]
                                                        (reduce #(update %1 %2 (fnil max 0) f) m (water-body p top n))
                                                        m)))
                                                  {} spots)]
                              (mapv (fn [[[x y z] f]] {:x x :y y :z z :factor f}) factors)))))))
