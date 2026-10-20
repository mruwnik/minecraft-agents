(ns jobs.survival.retreat-cover
  "Where a flight goes and how it covers itself (jobs.survival.retreat): the home, the glance round, the target away from the hostiles, a seal or side pocket where the body stands."
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.look :as look]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]
            [jobs.survival.retreat-flight :as flight]
            [jobs.survival.retreat-goal :as retreat-goal]
            [jobs.lib.rooms :as room-lib]
            [jobs.survival.retreat-refuge :as refuge]
            [jobs.survival.retreat-walk :as walk]
            [jobs.survival.retreat-cornered :as cornered]
            [jobs.survival.retreat-cornered.pick :as pick]))

(a/defargs settings
  {::home-range {:default 64 :doc "Blocks within which the latest :bed or :home entry is a home a flight runs or leans to." :spec (a/num-in 0 nil)}
   ::swim-factor {:default 0 :doc "Blocks of walking a block of swimming costs a flight; 0: jobs.lib.cost/swim-factor of the boots." :spec (a/num-in 0 nil)}
   ::way-reach {:default 14 :doc "Blocks within which the stairs the body cut are a way out of a flight." :spec (a/num-in 0 nil)}})
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
  takes in every turn walk/choose-target tries."
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

(defn swim-factor
  "The flight's price of a swim block: ::swim-factor, else jobs.lib.cost/swim-factor of the boots worn."
  [c]
  (let [f (settings/get settings ::swim-factor)]
    (if (pos? f) f (cost/swim-factor (.-equipment (.self (:primitives c)))))))

(defn lost-spots
  "The places of the mobs a flight lost within lost-memory-ms (:threat entries, other than the chasers now on), as walk
  positions: a creeper three times, so the way away leans from it."
  [c chasing]
  (let [ids (set (map #(.-id %) chasing))]
    (->> (threats/listed-threats (:primitives c) (ctx/since c :threat (- (ctx/now c) (cornered/lost-memory-ms))))
         (map :data)
         (remove #(contains? ids (:id %)))
         (mapcat (fn [d] (when-let [pos (or (:pos d) (danger-q/rough-pos d))]
                           (repeat (if (= "creeper" (:mob d)) 3 1) pos)))))))

(defn way-up
  "Underground, the feet cell on a stair this body cut (ledger/ways-of) that leads up and away from threats
  (walk/way-target), else nil."
  [c threats]
  (let [p (:primitives c)]
    (when (and (seq threats) (sh-sense/buried? p))
      (walk/way-target (u/self-pos c) (mapv #(danger-q/mob-pos p %) threats) (ledger/ways-of c) (way-reach)))))

(defn ^:async flight-target!
  "The walk target away from threats (walk/choose-target over what the body sees, after glance-away!; cover from the
  ranged ones first, a swim slow), leaning toward the first of rooms (refuges), nil when none. covered? as cover-fn
  (given so the caller shares one)."
  ([c threats] (await (flight-target! c threats (cover-fn (:primitives c) threats (flight/fire-spots c)) (refuges c))))
  ([c threats covered? rooms]
   (if-let [way (way-up c threats)]
     way
   (let [p (:primitives c)
         from (u/self-pos c)
         threat-pos (into (into (mapv #(danger-q/mob-pos p %) threats) (lost-spots c threats)) (flight/fire-spots c))
         refuge (:start (first rooms))
         judged (retreat-goal/judge c (remove nil? [refuge (home-pos c)]))
         goal (retreat-goal/usable judged)
         home (or refuge (:home judged) goal (home-pos c))
         flank? (and (some? (or refuge goal)) (cost/outruns-at? (room-lib/food p) (walk/flank-turn)))]
     (await (glance-away! c (walk/direction from threat-pos home flank?)))
     (walk/choose-target (cornered/block-at-fn p) from threat-pos home (keep (comp :pos :data) (ctx/entries c :hazard))
                         (flight/walk-step) {:swim-factor (swim-factor c) :covered? covered? :flank? flank?
                          :avoid (flight/avoid-ends c)
                          :refused (:refused (ctx/mem c))
                          :waterlogged? #(:waterlogged? (u/seen-facts p %))
                          :floor (flight/floor c)
                          :known (ledger/ways-of c)})))))

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
