(ns jobs.lib.threats
  "Mobs that chase: how far each follows its target (vanilla follow_range), and the :threat entries a flight leaves in
  body memory. One :threat entry per mob fled per flight {:mob :id :uuid :key :pos (last known) :ended}, kept
  threat-policy. The third flight from one mob (same :key, uuid else id) within the ttl warns hostile.chased.
  planner-dangers: the cells of sensed mobs and deaths by terrain go-to's searches keep off (the planner's
  options.dangers). known-threats: every mob the body knows may hurt it, seen, fled from, met at a death or seen and lost;
  threats-of gives each its reach (jobs.lib.threat-field) for jobs.lib.cost/death-chance, tolls-of prices each one's field
  for the planner (options.tolls, threat-tolls)."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [engine.ctx :as ctx]
            [engine.entity-observations :as obs]
            [engine.memory :as mem]
            [engine.path.planner.base :as base]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.death :as death]
            [jobs.lib.cost.mob-toll :as mob-toll]
            [jobs.lib.cost.risk :as risk]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.cost.tolls :as cost-tolls]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.death-spot :as death-spot]
            [jobs.lib.hide-spot :as hide-spot]
            [jobs.lib.hostile-loss :as hostile-loss]
            [jobs.lib.threat-field :as field]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::chased-flights {:default 3 :doc "Flights from one mob within the ttl that tell the agent." :spec (a/int-in 1 nil)}
   ::fled-memory-ms {:default 180000 :doc "Milliseconds after a flight during which a mob it fled from still counts as near the place it was last known (errands that walk to a spot hold off), and the body is not sent out on a comfort errand." :spec (a/int-in 0 nil)}
   ::max-threats {:default 8 :doc "The most known threats (known-threats) a price reads, the nearest first." :spec (a/int-in 1 nil)}
   ::negligible-toll {:default 0.001 :doc "Seconds more a second near it under which a sighting is dropped (sightings): at its weight it costs no body this much, so a cell's walk time moves by less than this share." :spec (a/num-in 0 nil)}})

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
  "Write the :threat entry for a mob fled ({:mob :id :uuid :ended, :held and :chaser (cost/mob-kind) as seen, and :pos, or
  :direction :band :from}); at the chased-flights-th from it, warn hostile.chased."
  [c {:keys [mob id uuid ended held chaser] :as t}]
  (let [k (or uuid id)
        place (select-keys t [:pos :direction :band :from])]
    (ctx/remember! c :threat (merge (cond-> {:mob mob :id id :uuid uuid :key k :ended ended}
                                              (some? held) (assoc :held held) (some? chaser) (assoc :chaser chaser)) place) threat-policy)
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

(defn mob-entry [p e] {:key (mob-key e) :name (.-name e) :held (.-held e) :chaser (cost/mob-kind e) :pos (danger-q/mob-pos p e)})

(defn sensed-mobs
  "The real dangers primitives p senses (jobs.lib.danger/dangers within its known-scan: seen or heard, with a way to the
  body or a line of fire; never x-ray; danger-opts as danger-q/danger-in?), as [{:key :name :held :chaser (cost/mob-kind) :pos}]."
  [p danger-opts]
  (mapv #(mob-entry p %)
        (danger-q/dangers p (danger-q/known-scan) {:ranged-radius (danger-q/known-scan)} danger-opts)))

(defn remembered-spots
  "The places of the :threat entries of c whose mob the client still tracks, as {:pos ..} data with a :pos (a mob
  that blew up or died is gone; a hidden one is still listed), for known-threats."
  [c]
  (keep #(let [d (:data %)] (when-let [at (rough-pos d)] (assoc d :pos at)))
        (listed-threats (:primitives c) (ctx/entries c :threat))))

(defn sightings
  "The hostiles c's body saw and lost (the perception's fadedMobs; none without one), [{:mob :pos :key :weight
  :remembered :sighted}], weight death-spot/sighting-share; none whose place the body has seen clear since (death-spot/seen-clear?),
  nor any whose weight costs under ::negligible-toll at the dearest a mob can cost any body (cost/threat-factor-cap)."
  [c]
  (when-let [f (.-fadedMobs (:primitives c))]
    (for [^js m (array-seq (.call f (:primitives c)))
          :let [s {:mob (.-name m) :pos (u/pos-of (.-pos m)) :age-ms (.-ageMs m)}]
          :when (not (death-spot/seen-clear? c (assoc s :mob? true)))
          :let [w (death-spot/sighting-share c s)]
          :when (<= (settings/get settings ::negligible-toll) (* w (cost/threat-factor-cap)))]
      {:mob (:mob s) :pos (:pos s) :key (or (.-uuid m) (.-id m)) :weight w :remembered true :sighted true})))

(defn planner-dangers
  "The planner's options.dangers for the body of c (JS array, nil for none): the cell of each sensed mob (sensed-mobs,
  nearest first), no way through it (:block; its price is its field's tolls, threat-tolls), and each uncleared death by
  terrain (death/terrain-danger: the cells that killed are near its spot); at most cost/max-dangers."
  [c]
  (let [p (:primitives c)
        here (u/pos-of (.-pos (.self p)))
        ds (concat (for [{:keys [name pos]} (sort-by #(u/dist here (:pos %)) (sensed-mobs p {:way? false :any-range? true}))]
                     {:x (:x pos) :y (:y pos) :z (:z pos) :close 0 :radius 0 :rate 0 :mob name :block true})
                   (for [{:keys [pos cause weight mob?]} (death-spot/uncleared c) :when (not mob?)]
                     (cost/terrain-danger {:pos pos :cause cause :weight weight})))]
    (when (seq ds) (clj->js (vec (take cost/max-dangers ds))))))

;; ---------------------------------------------------------------- the threats every price reads

(defn max-threats [] (settings/get settings ::max-threats))

(defn known-threats
  "The mobs c's body knows may hurt it, [{:mob :pos :weight :key :chaser :fled :remembered}], at most ::max-threats, the nearest
  the body first: one sensed now (sensed-mobs, weight 1) or felt hitting it (danger/felt-hostiles, 1, where the blow came from), :fled a :threat flight's last place (remembered-spots, 1; one whose
  :key is sensed now is left out: the sensed place wins), and, :remembered, a recent death or near-death by a mob at its
  spot (death-spot/recent, weighing death-spot/present-share of its fading weight; a walker's only while not seen-clear?, a water
  mob's whatever the light: it lives in the water, not in the dark) and a :hurt entry by a water mob (fading over the
  :hurt memory's ttl), and, :remembered :sighted, a mob seen and lost (sightings) not sensed or fled from now. One mob
  at one cell counts once, at its greatest weight."
  [c]
  (let [p (:primitives c)
        now (ctx/now c)
        ttl (:ttl (or (mem/policy (ctx/view c) :hurt) mem/default-policy))
        fade (fn [age] (if (number? ttl) (cost/death-weight age ttl) (cost/death-weight age)))
        sensed (into (sensed-mobs p {:way? false :any-range? true})
                     (map #(mob-entry p %))
                     (danger-q/felt-hostiles p (ctx/view c)))
        now-keys (set (map :key sensed))
        fled (vec (remove #(contains? now-keys (:key %)) (remembered-spots c)))
        known-keys (into now-keys (map :key) fled)
        here (u/pos-of (.-pos (.self p)))]
    (->> (concat (for [{:keys [key name held chaser pos]} sensed] {:mob name :held held :pos pos :weight 1 :key key :chaser chaser})
                 (for [{:keys [mob held chaser pos key]} fled] {:mob mob :held held :chaser chaser :pos pos :weight 1 :key key :fled true})
                 (remove #(contains? known-keys (:key %)) (sightings c))
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
  "The body of c as the answers' prices read it, as respond's (hostile-loss/body-at), with body-of."
  [c]
  (let [p (:primitives c)] (merge (hostile-loss/body-at c (ctx/view c)) (body-of p))))

(defn threat-id
  "What a threat's stance and toll are kept under: its field-key and the :held :key :chaser tolls-of reads."
  [t]
  [(field/field-key t) (select-keys t [:held :key :chaser])])

(defn stances
  "{threat-id {:stance :price :near}} of threats ts for c's body (death-body): mob-toll/answered-together of each met at
  its place with those of ts that come into it before the answer to it alone ends (mob-toll/joins?), as respond meets the
  mobs near the body together; read once a world epoch for the same threats and body (threat-field/shared)."
  [c ts]
  (let [body (dissoc (death-body c) :pos)]
    (field/shared c [::stances (mapv threat-id ts) body]
                  (fn []
                    (let [ms (mapv mob-toll/answer-mob ts)]
                      (into {} (map-indexed
                                (fn [i t]
                                  (let [at (:pos t)
                                        alone (mob-toll/answered-together body [(ms i)] at)
                                        with (when alone
                                               (keep-indexed (fn [j u] (when (and (not= i j) (mob-toll/joins? u (:pos (nth ts j)) at (:near alone))) u)) ms))]
                                    [(threat-id t) (if (seq with) (mob-toll/answered-together body (into [(ms i)] with) at) alone)]))
                                ts))))
                  (constantly 0))))

(defn notice-at
  "A fn of a cell x y z: what threat t's meet rate is times there (death/notice-factor from its place; 1 when its class
  has none)."
  [t]
  (or (death/notice-factor (threat/kind (cost/mob-of-threat t)) (:pos t)) (constantly 1)))

(defn capped-sum
  "Seconds a second of tolls [rest death] (mob-toll/threat-parts) cost together: their rests summed, at most cap, and
  their deaths summed on top."
  [cap parts]
  (+ (min cap (reduce + 0 (map first parts))) (reduce + 0 (map second parts))))

(defn danger-near-fn
  "A fn of a cell pos: the nearest known threat (threats-of c) the body would flee (stances) whose field weighs the cell in
  full (within threat-field ::close steps of its way), as {:mob :mob-pos :mob-distance :toll :stay-loss}, else nil. :toll is
  what each second walked there costs more: mob-toll/threat-parts of every such threat at its weight and its notice-at
  the cell, summed, the rest at most cost/threat-factor-cap, the death on top (capped-sum). :stay-loss, a fn of seconds, is what standing there that long costs:
  cost/stand-loss of them all, each at its weight and distance from pos. Threats are read once."
  [c]
  (let [body (death-body c)
        cap (cost/threat-factor-cap)
        ts (threats-of c)
        st (stances c ts)
        ts (->> ts
                (filter #(= :flee (:stance (st (threat-id %)) :flee)))
                (mapv #(assoc % :parts (mob-toll/threat-parts (cost/mob-of-threat %) (:weight %) (st (threat-id %))) :notice (notice-at %)))
                (filterv #(pos? (reduce + (:parts %)))))]
    (fn [{:keys [x y z] :as pos}]
      (let [fx (js/Math.floor x) fy (js/Math.floor y) fz (js/Math.floor z)
            in (filterv (fn [{:keys [reach]}] (= 1 (reach fx fy fz))) ts)]
        (when (seq in)
          (let [t (first (sort-by #(u/dist pos (:pos %)) in))
                mobs (mapv (fn [{:keys [mob chaser weight] :as m}]
                             {:name mob :distance (u/dist pos (:pos m)) :speed (cost/chase-speed (or chaser mob)) :weight weight})
                           in)]
            {:mob (:mob t) :mob-pos (select-keys (:pos t) [:x :y :z]) :mob-distance (js/Math.round (u/dist pos (:pos t)))
             :toll (capped-sum cap (map (fn [{:keys [parts notice]}] (mapv #(* % (notice fx fy fz)) parts)) in))
             :stay-loss (fn [seconds] (cost/stand-loss body (:equipment body) mobs pos seconds))}))))))

(defn stay-danger-fn
  "(fn [pos stay-s worth]) of stay-danger over one reading of c's threats, for a caller pricing many positions."
  [c]
  (let [near (danger-near-fn c)]
    (fn [pos stay-s worth]
      (when-let [{:keys [stay-loss] :as d} (near pos)]
        (let [stay-s (or stay-s (risk/work-s :click))
              risk (stay-loss stay-s)]
          (when-not (cost/affords? risk worth) (assoc (dissoc d :toll :stay-loss) :stay-s stay-s :risk risk :worth worth)))))))

(defn stay-danger
  "The nearest known threat the body would flee whose field holds pos (danger-near-fn), with :stay-s, :risk and :worth,
  when standing there stay-s seconds (nil: a click's; :risk, its :stay-loss) costs more than worth; else nil. The one
  price of staying to work in a threat's field, walked to (go-to) or in reach (jobs.lib.near/go-near!)."
  [c pos stay-s worth]
  ((stay-danger-fn c) pos stay-s worth))

(defn round-weight
  "A threat's weight to 1/20: what tolls-of prices and threat-tolls keys by, so a table is a function of its key."
  [weight]
  (/ (js/Math.round (* 20 weight)) 20))

(defn table-key
  "What a tolls-of table of threats is a function of besides their stances: per threat its threat-id and the weight to 1/20."
  [threats]
  (mapv (fn [t] (conj (threat-id t) (round-weight (:weight t)))) threats))

(defn body-inputs
  "[health equipment weapon] of c's body: what tolls-of reads of it besides the stances (cost/threat-factor, threat-factor-cap)."
  [c]
  (let [{:keys [health equipment weapon]} (body-of (:primitives c))]
    [health equipment weapon]))

(defn stances-of
  "stances of threats ts among known threats all ({:mob :pos}, ts among them): each met with all, so a threat left out of
  ts still joins those in it."
  [c all ts]
  (select-keys (stances c all) (map threat-id ts)))

(defn toll-inputs
  "What tolls-of reads of threats ({:mob :pos :weight}) of known threats all (stances-of): a map of their table-key less
  the weight to the highest weight (round-weight), the body-inputs and the stances' prices (toll-inputs-no-worse?)."
  [c all threats]
  (conj (into [(reduce (fn [m k] (update m (pop k) (fnil max 0) (peek k))) {} (table-key threats))] (body-inputs c))
        (update-vals (stances-of c all threats) :price)))

(defn toll-inputs-no-worse?
  "Whether tolls priced from toll-inputs was are no cheaper than from now: the same threats, none at a higher weight or
  a dearer answer, the same armour and weapon, health no lower. A walk's kept search goes on only then; a weight that
  fades only cheapens, and health regained only cheapens."
  [now was]
  (or (= now was)
      (let [[ts h eq w st] now [ts0 h0 eq0 w0 st0] was
            price #(or (get %1 %2) ##Inf)]
        (and (= [eq w] [eq0 w0]) (= (set (keys ts)) (set (keys ts0))) (every? (fn [[k wt]] (<= wt (ts0 k))) ts)
             (every? #(<= (price st %) (price st0 %)) (keys ts))
             (number? h) (number? h0) (>= h h0)))))

(defn tolls-of
  "go-to's tolls (cost.tolls/packed, seqs as {:x :y :z :factor} each) for threats ({:mob :pos :weight}) of c: every cell of each one's field costs
  mob-toll/threat-parts (its stance's price, its weight to 1/20 x the cell's x its notice-at the cell) more a second; the dearest of one mob where it is known twice (a
  kind with no :key counts as one mob), mobs of one :key each summed (capped-sum at cost/threat-factor-cap)."
  ([c threats] (tolls-of c threats (stances c threats)))
  ([c threats st]
  (let [cap (cost/threat-factor-cap)
        by-cell (js/Map.)]
    (doseq [{:keys [mob weight key] :as t} threats
            :let [f (mob-toll/threat-parts (cost/mob-of-threat t) (round-weight weight) (st (threat-id t)))
                  notice (notice-at t)]
            :when (pos? (reduce + f))
            ^js cell (:cells (field/field-of c t))]
      (let [x (aget cell 0) y (aget cell 1) z (aget cell 2)
            k (base/cell-key x y z)
            ^js row (or (.get by-cell k) (let [r #js {:x x :y y :z z :mobs (js/Map.)}] (.set by-cell k r) r))
            ^js mobs (.-mobs row)
            mk (str (or key mob))
            w (* (aget cell 3) (notice x y z))
            v (mapv #(* w %) f)]
        (when-not (>= (reduce + (or (.get mobs mk) [0 0])) (reduce + v)) (.set mobs mk v))))
    (let [^js rows (js/Array.from (.values by-cell))]
      (cost-tolls/packed (.-length rows)
                    (fn [i xs ys zs factors]
                      (let [^js r (aget rows i)]
                        (aset xs i (.-x r)) (aset ys i (.-y r)) (aset zs i (.-z r))
                        (aset factors i (capped-sum cap (es6-iterator-seq (.values (.-mobs r))))))))))))

(defn shared-tolls
  "tolls-of threats ts of c, each at its stance among known threats all (stances-of), read once per check-cache window for
  the same threats and body-inputs (threat-field/shared: one table, so walk.world/with-tolls merges it once)."
  [c all ts]
  (if (empty? ts)
    []
    (let [st (stances-of c all ts)]
      (field/shared c [::threat-tolls (table-key ts) (body-inputs c) (update-vals st :price)]
                    #(tolls-of c ts st)
                    count))))

(defn threat-tolls
  "shared-tolls of the known-threats of c (those keep? holds of): go-to's price of every known mob (the planner's
  options.tolls)."
  ([c] (threat-tolls c any?))
  ([c keep?] (let [all (known-threats c)] (shared-tolls c all (filterv keep? all)))))
