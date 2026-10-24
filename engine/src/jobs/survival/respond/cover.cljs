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
            [jobs.lib.walk.world :as wworld]
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

(defn blast
  "The creepers among threats within blast reach (flight/blaster?)."
  [p threats]
  (filterv #(flight/blaster? p %) threats))

(defn survey
  "The closed rooms a flight step races for (room-lib/survey from home-pos): up to room-lib/judge-per-step door sides
  judged afresh, none with a melee threat within cover-gap; nil with a creeper of threats within blast reach (blast).
  keep? as room-lib/survey's: the flee price (caught-chance!) keeps nothing, so it surveys what the flight's step then does."
  [c threats keep?]
  (let [p (:primitives c)]
    (when (empty? (blast p threats))
      (room-lib/survey c (home-pos c)
                       (if (some #(and (not (combat/ranged? %)) (<= (danger-q/mob-distance p %) (cornered/cover-gap))) threats)
                         0
                         (room-lib/judge-per-step))
                       keep?))))

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

(defn homes
  "The homes a flight runs to: the first of rooms (survey's) and home-pos, none with a creeper within blast reach (blast?)."
  [c rooms blast?]
  (if blast? [] (remove nil? [(:start (first rooms)) (home-pos c)])))

(defn ^:async judge!
  "The flight's goal, homes (homes) and flee search (respond.goal/judge!) after a glance along the way away
  (glance-away!), the homes kept as :homes."
  [c covered? rooms blast?]
  (await (glance-away! c (search/away (u/self-pos c) (respond.goal/chasers c))))
  (let [hs (homes c rooms blast?)]
    (assoc (await (respond.goal/judge! c hs covered?)) :homes hs)))

(defn ^:async caught-chance!
  "{:chance :seconds :cornered?} of the flight's next step from where the body stands (flee-search/caught-chance), for
  the fight-or-flee weighing, from what that step judges by: its chasers (flight/look over the memory flee! resumes,
  flight/resume-flight), their cover-fn and blast, its rooms (survey, kept nowhere), its stair up (way-up: a seen way,
  flee-search/way-up-chance, no search), else its search (respond.goal/judge! over homes), choose options
  (respond.goal/choose-opts) and cells looked at (memory :glanced), no glance taken. Kept (memory :caught-chance) for a
  flee step's seconds (flee-search ::step-s) while its cell, its looks, its chasers and its stair up stay the same."
  [c]
  (let [p (:primitives c)
        now (ctx/now c)
        {:keys [glanced] kept :caught-chance :as mem} (ctx/mem c)
        threats (:on (flight/look c (flight/resume-flight mem now)))
        up (way-up c threats)
        in {:cell (respond.goal/cell (u/self-pos c)) :glanced glanced :chasers (mapv #(.-id %) threats) :up up}]
    (if (and (= in (:in kept)) (< (- now (:t kept)) (* 1000 (settings/get search/settings ::search/step-s))))
      (:chance kept)
      (let [counts (wworld/way-counts p)
            chance (if up
                     (search/way-up-chance counts)
                     (let [covered? (cover-fn p threats (flight/fire-spots c))
                           judged (await (respond.goal/judge! c (homes c (:rooms (survey c threats false)) (seq (blast p threats))) covered?))]
                       (search/caught-chance (:ends judged) counts (respond.goal/choose-opts c threats covered? (:home judged)) glanced)))]
        (ctx/update-mem! c assoc :caught-chance {:in in :t now :chance chance})
        chance))))

(defn ^:async look-ahead!
  "judged again (respond.goal/judge! over its :homes) after a look at the first unseen cells of its routes not looked at
  this flight (flee-search/unseen-ahead); nil when there is none to look at."
  [c covered? judged]
  (when-let [cells (search/unseen-ahead (:ends judged) (:glanced (ctx/mem c)))]
    (ctx/update-mem! c update :glanced #(vec (take-last (cornered/glance-memory) (into (vec %) cells))))
    (await (dig-look/look-unknown! c cells))
    (assoc (await (respond.goal/judge! c (:homes judged) covered?)) :homes (:homes judged))))

(defn ^:async flight-target!
  "The walk target away from threats: the stairs up (way-up), else the step to the end flee-search/choose takes of
  judged's search (judge!; a :retreat.search debug event lists them): out of a ranged chaser's line of fire first, then a safe home (judged's :home), then an open end by
  score, a pocket only in a closed cave; with none, or when its step is into a cell the body has not seen
  (flee-search/into-unseen?), after a look at the routes' first unseen cells (look-ahead!), the same over the search
  run again (nothing left to look at: the step as it was); nil when cornered. Left out: ends under rock below the flight's floor
  (flight/floor; not out of the line of fire, nor on stairs the body cut), ends go-to refused, routes near a :hazard,
  wet ends while a dry one is left (none with a drowned among threats); then ends toward a hostile not chasing
  (bystander-spots) and flight/avoid-ends, each while another end is left. judged and covered? given so the caller
  shares them; dodge-shot! judges here."
  ([c threats] (let [covered? (cover-fn (:primitives c) threats (flight/fire-spots c))]
                 (await (flight-target! c threats covered? (await (judge! c covered? (:rooms (survey c threats true)) (seq (blast (:primitives c) threats))))))))
  ([c threats covered? judged]
   (if-let [way (way-up c threats)]
     way
     (let [p (:primitives c)
           from (u/self-pos c)
           opts-for #(respond.goal/choose-opts c threats covered? (:home %))
           first-pick (search/choose (:ends judged) (opts-for judged))
           again (when (or (nil? first-pick) (search/into-unseen? first-pick)) (await (look-ahead! c covered? judged)))
           e (if again
               (search/choose (:ends again) (opts-for again))
               first-pick)
           round1 #(/ (js/Math.round (* 10 %)) 10)]
       (ctx/emit! c :retreat.search :debug
                  {:chasers (mapv (fn [{:keys [name pos]}] [name (mapv #(js/Math.round (% pos)) [:x :y :z])]) (respond.goal/chasers c))
                   :from (mapv js/Math.floor (search/cell-of from))
                   :picked (some-> e :end search/cell-of)
                   :ends (mapv (fn [e] (cond-> (select-keys e [:open? :caught? :cover? :unseen])
                                         true (assoc :at (search/cell-of (:end e)) :score (round1 (:score e)) :gain (round1 (:gain e)) :s (round1 (:s e))
                                                     :arrive (round1 (:arrive e)))
                                         (:pos (:target e)) (assoc :kind (:kind (:target e)))))
                               (:ends (or again judged)))})
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
