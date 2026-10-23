(ns jobs.survival.respond.cover
  "Where a flight goes and how it covers itself (jobs.survival.respond-flight): the home, the glance round, the target away from the hostiles, a seal or side pocket where the body stands."
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.combat :as combat]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.look :as look]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]
            [jobs.survival.respond.state :as flight]
            [jobs.survival.respond.goal :as respond.goal]
            [jobs.lib.rooms :as room-lib]
            [jobs.survival.respond.refuge :as refuge]
            [jobs.lib.flee-search :as search]
            [jobs.lib.flee-walk :as walk]
            [jobs.survival.respond.options :as cornered]
            [jobs.survival.respond.cornered :as pick]))

(a/defargs settings
  {::home-range {:default 64 :doc "Blocks within which the latest :bed or :home entry is a home a flight runs or leans to." :spec (a/num-in 0 nil)}
   ::way-reach {:default 14 :doc "Blocks within which the stairs the body cut and the places its recent walks began are a way out of a flight." :spec (a/num-in 0 nil)}})
(defn home-range [] (settings/get settings ::home-range))
(defn way-reach [] (settings/get settings ::way-reach))

(defn home-pos
  "The position of the latest :bed or :home entry within home-range blocks of the body, or nil."
  [c]
  (let [pos (->> [(ctx/latest c :bed) (ctx/latest c :home)]
                 (remove nil?)
                 (sort-by :t >)
                 first
                 :data
                 :pos)]
    (when (and pos (<= (u/dist pos (u/self-pos c)) (home-range))) pos)))

(defn refuges
  "The closed rooms to flee to: the latest :home or :bed (home-pos) and those behind the doors the body has seen
  (jobs.lib.rooms/rooms)."
  [c]
  (room-lib/rooms c (home-pos c)))

(def glance-turns
  "Degrees from the way away a flight turns to look before it chooses: the view cone (about 100 degrees wide) then
  takes in the ground ahead and to either side."
  [0 90 -90])


(defn unseen-before-wall?
  "Whether a cell the body has not sensed lies in the columns along d from the first, up to glance-reach or the first
  column whose seen feet or head cell blocks the way (the cells behind it cannot change the walk, nor can the floor
  under it)."
  [p from d y]
  (loop [k 1]
    (when (<= k (cornered/glance-reach))
      (let [[x z] (walk/column-along from d k)
            open-floor? (walk/floorless? (u/seen-name p {:x x :y (dec y) :z z}))
            unknown (some #(dig-look/unknown? p [x % z]) (range (if open-floor? (- y 2) (dec y)) (+ y 2)))
            blocked? (some (fn [yy] (and (not (dig-look/unknown? p [x yy z]))
                                         (not (walk/passable? (u/seen-name p {:x x :y yy :z z})))))
                           [y (inc y)])]
        (cond
          blocked? false
          unknown true
          :else (recur (inc k)))))))

(defn ^:async glance-away!
  "Turn to look along each glance-turns direction from dir ([ux uz]) with a cell the body has not sensed in its first
  glance-reach columns (feet, head, floor, and under an open floor the cell below; not past a seen wall), at the feet
  cell glance-reach blocks along, as a player turns round before running: at most three looks, a sight pass after
  each, none at a cell already glanced this flight."
  [c dir]
  (let [p (:primitives c)
        from (u/self-pos c)
        y (js/Math.floor (:y from))]
    (loop [[turn & more] glance-turns]
      (when turn
        (let [d (walk/rotate dir turn)
              [gx gz] (walk/column-along from d (cornered/glance-reach))
              cell [gx y gz]]
          (when (and (not (some #{cell} (:glanced (ctx/mem c)))) (unseen-before-wall? p from d y))
            (ctx/update-mem! c update :glanced #(vec (take-last (cornered/glance-memory) (conj (vec %) cell))))
            (await (dig-look/look-at! c cell))
            (look/see! c))
          (recur more))))))

(defn cover-fn
  "(feet cell -> whether it is out of every seen ranged threat's line of fire and of every spot (the unseen shooters,
  flight/fire-spots), over the cells the body has seen: an unseen cell stops no arrow), nil when no seen threat is
  ranged and no spot is given (a heard one's rough spot is no line of fire)."
  ([p threats] (cover-fn p threats nil))
  ([p threats spots]
   (let [ranged (filter #(and (combat/ranged? %) (danger-q/seen-only? %)) threats)
         at (into (mapv #(danger-q/mob-pos p %) ranged) spots)]
     (when (seq at)
       (let [arrow-at (fn [x y z] (reach/arrow-kind-of (u/seen-block p {:x x :y y :z z})))]
         #(reach/out-of-fire? arrow-at at %))))))

(defn trail-floors
  "The floors [x y z] under the places the body's recent walks (memory :moved) began: the way it came, as the stairs it cut are."
  [c]
  (set (for [{{:keys [from]} :data} (ctx/entries c :moved)
             :when from]
         [(js/Math.floor (:x from)) (dec (js/Math.floor (:y from))) (js/Math.floor (:z from))])))

(defn seen-stand?
  "Whether the floor [x y z] and the two cells over it are seen: a leg to a stand the body has not seen makes go-to search
  unseen ground until its node budget is gone."
  [p [x y z]]
  (every? #(u/seen-block p {:x x :y % :z z}) [y (inc y) (+ y 2)]))

(defn way-up
  "Underground, the feet cell on a stair this body cut (ledger/ways-of) or where a recent walk of its began (trail-floors),
  that leads up and away from threats (walk/way-target), else nil. Only stands the body has seen count."
  [c threats]
  (let [p (:primitives c)
        floors (into (ledger/ways-of c) (trail-floors c))]
    (when (and (seq threats) (sh-sense/buried? p))
      (walk/way-target (u/self-pos c) (mapv #(danger-q/mob-pos p %) threats)
                       (filter #(seen-stand? p %) floors) (way-reach)))))

(defn bystander-spots
  "The places of the hostiles the body knows within their follow range of it (threats/follow-range: they would notice it) that are not chasing it (not in chasing, not killed): a flight step does not end nearer to one of them, so the body does not run from one mob into another."
  [c chasing]
  (let [p (:primitives c)
        skip (into (set (map #(.-id %) chasing)) (flight/dead-ids c))]
    (->> (flight/known-chasers p)
         (remove #(skip (.-id %)))
         (filter #(<= (danger-q/mob-distance p %) (threats/follow-range (.-name %))))
         (map #(danger-q/mob-pos p %)))))

(defn ^:async judge!
  "The flight's goal, homes and flee search (respond.goal/judge!) after a glance along the way away (glance-away!):
  homes the first of rooms (refuges) and home-pos, none with a creeper within blast reach (blast?), kept as :homes."
  [c covered? rooms blast?]
  (await (glance-away! c (search/away (u/self-pos c) (respond.goal/chasers c))))
  (let [homes (if blast? [] (remove nil? [(:start (first rooms)) (home-pos c)]))]
    (assoc (await (respond.goal/judge! c homes covered?)) :homes homes)))

(defn ^:async look-ahead!
  "judged again (respond.goal/judge! over its :homes) after a look at the unseen cells its routes stopped before
  (flee-search/unseen-ahead; each once a flight, as glances are); nil when there is none to look at."
  [c covered? judged]
  (when-let [cells (seq (remove (set (:glanced (ctx/mem c))) (search/unseen-ahead (:ends judged))))]
    (ctx/update-mem! c update :glanced #(vec (take-last (cornered/glance-memory) (into (vec %) cells))))
    (await (dig-look/look-unknown! c cells))
    (assoc (await (respond.goal/judge! c (:homes judged) covered?)) :homes (:homes judged))))

(defn ^:async flight-target!
  "The walk target away from threats: the stairs up (way-up), else the step to the end flee-search/choose takes of
  judged's search (judge!; a :retreat.search debug event lists them): out of a ranged chaser's line of fire first, then a safe home (judged's :home), then an open end by
  score, a pocket only in a closed cave; with none, after a look at the unseen cells the routes stopped before
  (look-ahead!), the same over the search run again; nil when cornered. Left out: ends under rock below the flight's floor
  (flight/floor; not out of the line of fire, nor on stairs the body cut), ends go-to refused, routes near a :hazard,
  wet ends while a dry one is left (none with a drowned among threats); then ends toward a hostile not chasing
  (bystander-spots) and flight/avoid-ends, each while another end is left. judged and covered? given so the caller
  shares them; dodge-shot! judges here."
  ([c threats] (let [covered? (cover-fn (:primitives c) threats (flight/fire-spots c))]
                 (await (flight-target! c threats covered? (await (judge! c covered? (refuges c) false))))))
  ([c threats covered? judged]
   (if-let [way (way-up c threats)]
     way
     (let [p (:primitives c)
           from (u/self-pos c)
           block-at (cornered/block-at-fn p)
           bystanders (bystander-spots c threats)
           floor (flight/floor c)
           known (ledger/ways-of c)
           opts {:refused (:refused (ctx/mem c))
                 :never? (fn [end] (and (< (:y end) floor) (walk/roofed? block-at end)
                                        (not (and covered? (covered? end)))
                                        (not (contains? known [(:x end) (dec (:y end)) (:z end)]))))
                 :hazards (keep (comp :pos :data) (ctx/entries c :hazard))
                 :wet? #(walk/wet? block-at (fn [cell] (:waterlogged? (u/seen-facts p cell))) %)
                 :dry-only? (boolean (some #(= "drowned" (.-name %)) threats))
                 :avoid (cons (fn [end] (some #(< (u/dist end %) (u/dist from %)) bystanders)) (flight/avoid-ends c))
                 :prefer? (when-let [home (:home judged)] #(= home (:pos (:target %))))}
           e (or (search/choose (:ends judged) opts)
                 (when-let [again (await (look-ahead! c covered? judged))]
                   (search/choose (:ends again) (assoc opts :prefer? (when-let [home (:home again)] #(= home (:pos (:target %))))))))
           round1 #(/ (js/Math.round (* 10 %)) 10)]
       (ctx/emit! c :retreat.search :debug
                  {:chasers (mapv (fn [{:keys [name pos]}] [name (mapv #(js/Math.round (% pos)) [:x :y :z])]) (respond.goal/chasers c))
                   :from (mapv js/Math.floor (search/cell-of from))
                   :picked (some-> e :end search/cell-of)
                   :ends (mapv (fn [e] (cond-> (select-keys e [:open? :caught? :cover? :unseen])
                                         true (assoc :at (search/cell-of (:end e)) :score (round1 (:score e)) :gain (round1 (:gain e)) :s (round1 (:s e))
                                                     :arrive (round1 (:arrive e)))
                                         (:pos (:target e)) (assoc :kind (:kind (:target e)))))
                               (:ends judged))})
       (:step e)))))

(defn ^:async take-cover!
  "Plug in (every way: walls, pit, pocket; refuge), unless that failed this sweep: its step's result, nil when it cannot be taken."
  [c]
  (await (refuge/plug-option! c :plug refuge/all-ways)))

(defn ^:async dig-pocket!
  "Underground with no stair to take, a side pocket in the rock beats a hop across the open cave: its step's result,
  nil when it is not possible. One dig attempt a flight (a hop does not clear that); a pocket under way is finished."
  [c]
  (let [{:keys [pocket-tried]} (ctx/mem c)]
    (cond
      (pick/plug-continues? c) (await (refuge/plug-on! c))
      pocket-tried nil
      :else (do (ctx/update-mem! c assoc :pocket-tried true)
                (await (refuge/plug! c :pocket #{:pocket}))))))

(defn close-danger?
  "Whether a creeper chases the body or a melee threat is within cover-gap: a seal or pit beside it is no cover (a blast
  breaks the wall, a mob stands in the cell to fill)."
  [p threats]
  (boolean (some #(or (combat/creeper? %)
                      (and (not (combat/ranged? %)) (<= (danger-q/mob-distance p %) (cornered/cover-gap))))
                 threats)))

(defn exposed-low?
  "Whether the body is at or under flight/::hide-health with a ranged threat's line of fire on it (covered? false at its feet),
  or, a ranged threat seen, a plug begun where it stands is not finished (refuge/plugging-here?: its first blocks may
  be all that stop the arrows)."
  [c covered?]
  (boolean (and covered?
                (or (refuge/plugging-here? c)
                    (and (<= (.-health (.self (:primitives c))) (flight/hide-health))
                         (not (covered? (sh-sense/feet (:primitives c)))))))))
