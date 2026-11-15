(ns jobs.survival.respond.cover
  "Where a flight goes and how it covers itself (jobs.survival.respond-flight): the home, the glance round, the target away from the hostiles, a seal or side pocket where the body stands."
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
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]
            [jobs.survival.hide-out :as hide-out]
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
  {::way-reach {:default 14 :doc "Blocks within which the stairs the body cut and the places its recent walks began are a way out of a flight." :spec (a/num-in 0 nil)}})
(defn way-reach [] (settings/get settings ::way-reach))

(defn home-pos
  "The position of the latest :bed or :home entry at any distance, or nil: the flight's search prices the run to it
  (flee-search, its own node cap), and survey ranks it among the rooms by distance."
  [c]
  (->> [(ctx/latest c :bed) (ctx/latest c :home)]
       (remove nil?)
       (sort-by :t >)
       first
       :data
       :pos))

(defn blast
  "The creepers among threats within blast reach (flight/blaster?)."
  [p threats]
  (filterv #(flight/blaster? p %) threats))

(declare cover-fn)

(defn fire-on
  "The shooters' fire on the body where it stands, as cost/refuge-search-pays? prices a search by it: {:exposed? (its feet
  cell out of cover, respond.goal/fire-price's covered?)}; nil with no shooter in sight (fire-price nil)."
  [c threats]
  (let [p (:primitives c)]
    (when-let [{:keys [covered?]} (respond.goal/fire-price c threats (cover-fn p threats (flight/fire-spots c)))]
      {:exposed? (not (covered? (sh-sense/feet p)))})))

(defn hold-for-flee!
  "room-lib/hold-for-flee! for the home leg of the flee search this step (the goal only as the fallback) or its ring's
  reach (flee-search/reach from the body), whichever is longer."
  [c]
  (let [p (:primitives c)]
    (room-lib/hold-for-flee! c (home-pos c) (respond.goal/active c)
                             (search/reach (u/self-pos c) (respond.goal/chasers c) (.-food (.self p))))))

(defn survey
  "The closed rooms a flight step races for (room-lib/survey from home-pos, threats' lead or the shooters' fire, fire-on,
  pricing its door sides judged afresh), its searches part of the step's one (room-lib/step-spent-s); nil with a creeper
  of threats within blast reach (blast). keep? as room-lib/survey's: the flee price (caught-chance!) keeps only the home's judgement."
  [c threats keep?]
  (when (empty? (blast (:primitives c) threats))
    (let [r (room-lib/survey c (home-pos c) {:threats threats :keep? keep? :fire (fire-on c threats) :spent-s (room-lib/step-spent-s c)})]
      (room-lib/spent! c (- (:spent-s r) (:ways-s r)) :survey-floods)
      (room-lib/spent! c (:spent-s r) :survey-ways)
      r)))

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
  each, none at a cell already glanced this flight; none once the decision's deadline has passed (room-lib/decision-over?)."
  [c dir]
  (let [p (:primitives c)
        from (u/self-pos c)
        y (js/Math.floor (:y from))]
    (loop [[turn & more] glance-turns]
      (when (and turn (not (room-lib/decision-over? c)))
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
  that leads up and away from threats (walk/way-target) and that no chaser (respond.goal/chasers, calm ones left out)
  gets to a block of first (flee-search/way-beaten?), else nil. Only stands the body has seen count."
  [c threats]
  (let [p (:primitives c)
        from (u/self-pos c)
        floors (into (ledger/ways-of c) (trail-floors c))
        target (when (and (seq threats) (sh-sense/buried? p))
                 (walk/way-target from (mapv #(danger-q/mob-pos p %) threats) (filter #(seen-stand? p %) floors) (way-reach)))]
    (when (and target (not (search/way-beaten? (search/with-ways c (remove :calm? (respond.goal/chasers c))) from (.-food (.self p)) target)))
      target)))

(defn homes
  "The homes a flight runs to: the first of rooms (survey's) and home-pos, none with a creeper within blast reach (blast?)."
  [c rooms blast?]
  (if blast? [] (remove nil? [(:start (first rooms)) (home-pos c)])))

(defn dead-end-covers
  "The covers a dead end of the flee search is priced with (respond.goal/judge!): the cornered step's where the body
  stands (cornered-covers, against the hostiles it weighs, from the ways' specs when given), built only when the search
  needs them (flee-search/price-all!). Not home: the dead end lies away from it."
  ([c] (dead-end-covers c nil))
  ([c specs] (fn [] (cornered/cornered-covers c (cornered/near-hostiles c) false specs))))

(defn ^:async judge!
  "The flight's goal, homes (homes) and flee search (respond.goal/judge!) after a glance along the way away
  (glance-away!), the homes kept as :homes. Right after a back-off (memory :backed-off, taken here: one step) and while a
  cornered step would still weigh one (respond.options/back-off-open?), its search plans the back-off's ring
  (respond.options/back-off-reach) first: a body pressed so close backs off again, and the ring's plans come before the
  meter runs out on the far ring. Elsewhere the far ring comes first and a cornered step searches the back-off itself."
  [c threats covered? rooms blast?]
  (await (glance-away! c (search/away (u/self-pos c) (respond.goal/chasers c))))
  (let [hs (homes c rooms blast?)
        near (when (and (:backed-off (ctx/mem c)) (cornered/back-off-open? c (respond.goal/chasers c) (cornered/near-hostiles c)))
               (cornered/back-off-reach))]
    (ctx/update-mem! c dissoc :backed-off)
    (assoc (await (respond.goal/judge! c threats hs (when-not blast? (:start (first rooms))) covered? (dead-end-covers c) near)) :homes hs)))

(defn building-looks
  "[{:at :seconds :closed :refuge-s}]: the looks through a door into a building survey s dropped :unseen, the home's or
  another's (room-lib/door-glances), each what it may show: the chance the room beyond the door side is closed
  (cost/closed-chance of its boundary's seen and unseen cells, reach-room/boundary) and then the run from the door into it
  at a flight's pace; its seconds room-lib/glance-s. Part of the step's one search, before the flee search: a door's
  flood only while it is kept or pays as survey's door sides do (room-lib/look-s, cost/refuge-search-pays? against threats' lead
  at the body and that door, the run it saves over survey's ways in won, and fire), the home's always, as survey judges it. respond prices each against the answers
  without it."
  [c s threats]
  (let [p (:primitives c)
        m (room-lib/survey-meter c)
        me (u/self-pos c)
        food (room-lib/food p)
        pace (cost/flee-pace food)
        here (room-lib/here-s p threats)
        best (reduce min ##Inf (keep (fn [{w :way-in :as r}] (when (and w (room-lib/race-won? p r w threats)) (:steps w))) (:rooms s)))
        fire (fire-on c threats)
        pays? (fn [{:keys [pos d]}]
                (let [blocks (u/dist me d)]
                  (cost/refuge-search-pays? (reach/spent-s) (room-lib/look-s p pos) here (room-lib/lead-s p [(room-lib/mid d)] threats blocks)
                                                  (cost/run-saved-s blocks best food) fire)))
        r (binding [reach/*search* m]
            (vec (for [g (room-lib/door-glances c s)
                       :when (or (:home? g) (zero? (room-lib/look-s p (:pos g))) (pays? g))
                       :let [{:keys [seen unseen]} (reach-room/boundary p (:pos g))]]
                   {:at g :seconds (room-lib/glance-s c g) :closed (cost/closed-chance seen (count unseen))
                    :refuge-s (/ (u/dist (room-lib/mid (:d g)) (room-lib/mid (:pos g))) pace)})))]
    (room-lib/spent-meter! c m :building-looks)
    r))

(defn ^:async caught-chance-once!
  "{:chance :seconds :cornered?} of the flight's next step from where the body stands (flee-search/caught-chance), for
  the fight-or-flee weighing, from what that step judges by: its chasers (flight/look over the memory flee! resumes,
  flight/resume-flight), their cover-fn and blast, its rooms (survey, kept nowhere; with no refuge, :looks, building-looks'), its stair up (way-up: a seen way,
  flee-search/way-up-chance, no search), else its search (respond.goal/judge! over homes), choose options
  (respond.goal/choose-opts) and cells looked at (memory :glanced), no glance taken; :way-scan the way counts it priced
  with (wworld/way-scan, for jobs.lib.walk.way-log). Kept (memory :caught-chance) for a
  flee step's seconds (flee-search ::step-s) while its cell, its looks, its chasers and its stair up stay the same.
  specs: the covers' specs the decision priced, its dead ends' covers (dead-end-covers)."
  ([c] (await (caught-chance-once! c nil)))
  ([c specs]
  (let [p (:primitives c)
        now (ctx/now c)
        {:keys [glanced] kept :caught-chance :as mem} (ctx/mem c)
        threats (:on (flight/look c (flight/resume-flight mem now)))
        up (way-up c threats)
        in {:cell (respond.goal/cell (u/self-pos c)) :glanced glanced :chasers (mapv #(.-id %) threats) :up up}]
    (if (and (= in (:in kept)) (< (- now (:t kept)) (* 1000 (settings/get search/settings ::search/step-s))))
      (:chance kept)
      (let [scan (wworld/way-scan p)
            counts (if scan (select-keys scan [:k :n]) {:k 0 :n 0})
            chance (if up
                     (search/way-up-chance counts)
                     (let [covered? (cover-fn p threats (flight/fire-spots c))
                           blast? (seq (blast p threats))
                           s (survey c threats false)
                           rooms (:rooms s)
                           ;; before the flee search: the step's one search, its home floods first
                           looks (when (and s (not-any? combat/creeper? threats)) (building-looks c s threats))
                           judged (await (respond.goal/judge! c threats (homes c rooms blast?) (when-not blast? (:start (first rooms))) covered? (dead-end-covers c specs) nil))
                           r (search/caught-chance (:ends judged) counts (respond.goal/choose-opts c threats covered? (respond.goal/closed-home judged)) glanced)]
                       (cond-> r (and (seq looks) (not (:refuge-s r))) (assoc :looks looks))))
            chance (cond-> chance (and chance scan) (assoc :way-scan scan) (:refuge-s chance) (assoc :hide-s (hide-out/quiet-s)))]
        (ctx/update-mem! c assoc :caught-chance {:in in :t now :chance chance})
        chance)))))

(defn ^:async caught-chance!
  "caught-chance-once!, after the glance along the way away the flight's step takes first (glance-away!, as judge!), and one
  look at the cells its flight would look at first (:look): a way that may be there is priced after the glance the flight
  takes anyway, not before it. One pass: the price asked again after the look is
  the answer, whatever it still wants to look at (cells past the glance memory would cycle for ever), and the answer too
  when the decision's deadline has passed: the look and the second pricing are left out."
  ([c] (await (caught-chance! c nil)))
  ([c specs]
   (await (glance-away! c (search/away (u/self-pos c) (respond.goal/chasers c))))
   (let [r (await (caught-chance-once! c specs))]
     (if-let [cells (when-not (room-lib/decision-over? c) (:look r))]
       (do (ctx/update-mem! c update :glanced #(vec (take-last (cornered/glance-memory) (into (vec %) cells))))
           (await (sense/look-at-unknown! c cells dig-look/max-floor-looks))
           (await (caught-chance-once! c specs)))
       r))))

(defn look-price
  "{:s :saving :looks} of the flight's look at cells: s the seconds of its looks (cost/look-s) plus the shooters' fire on
  the body over them where it stands uncovered (respond.goal/fire-price's :risk); saving what the look may spare: the
  dead end pick (choose's, nil none) pays for the unseen (:dead-end), with no pick the cheapest cornered answer at a
  dead end of judged's ends (:trapped)."
  [c threats covered? judged pick cells]
  (let [looks (min (count cells) dig-look/max-floor-looks)
        secs (cost/look-s looks)
        {:keys [risk] here-covered? :covered?} (respond.goal/fire-price c threats covered?)]
    {:looks looks
     :s (+ secs (if (and risk (not (here-covered? (look/cell-of (u/self-pos c))))) (risk secs) 0))
     :saving (if pick (:dead-end pick 0) (or (some->> (seq (keep :trapped (:ends judged))) (reduce min)) 0))}))

(defn ^:async look-ahead!
  "judged again (respond.goal/judge! over its :homes) after a look at the first unseen cells of its routes not looked at
  this flight (flee-search/unseen-ahead), each with a sight pass (sense/look-at-unknown!: the search plans over memory,
  so memory takes the whole view first), taken only while its look-price is under what it may spare (debug
  retreat.look-ahead either way); nil when there is none to look at or the look does not pay."
  [c threats covered? judged pick]
  (when-let [cells (search/unseen-ahead (:ends judged) (:glanced (ctx/mem c)))]
    (let [{:keys [s saving] :as price} (look-price c threats covered? judged pick cells)
          took? (< s saving)]
      (ctx/emit! c :retreat.look-ahead :debug (assoc price :took? took? :cells (vec (take (:looks price) cells))))
      (when took?
        (ctx/update-mem! c update :glanced #(vec (take-last (cornered/glance-memory) (into (vec %) cells))))
        (await (sense/look-at-unknown! c cells dig-look/max-floor-looks))
        (assoc (await (respond.goal/judge! c threats (:homes judged) (:room judged) covered? (dead-end-covers c) nil)) :homes (:homes judged))))))

(defn ^:async flight-target!
  "The walk target away from threats: the stairs up (way-up), else the step to the end flee-search/choose takes of
  judged's search (judge!; a :retreat.search debug event lists them): the best score less marks (a step or end in a ranged chaser's line of fire, a safe home's credit), a pocket only in a closed cave; with none, or when its step is into a cell the body has not seen
  (flee-search/into-unseen?), after a look at the routes' first unseen cells (look-ahead!; not when the search was cut,
  flee-search/cut?), the same over the search run again (nothing left to look at: the step as it was); nil when cornered
  or the search was cut before an end was chosen. Left out: ends under rock below the flight's floor
  (flight/floor; not out of the line of fire, nor on stairs the body cut), ends go-to refused, routes near a :hazard,
  wet ends while a dry one is left (none with a drowned among threats); then ends toward a hostile not chasing
  (bystander-spots) and flight/avoid-ends, each while another end is left. judged and covered? given so the caller
  shares them; dodge-shot! judges here."
  ([c threats] (let [covered? (cover-fn (:primitives c) threats (flight/fire-spots c))]
                 (await (flight-target! c threats covered? (await (judge! c threats covered? (:rooms (survey c threats true)) (seq (blast (:primitives c) threats))))))))
  ([c threats covered? judged]
   (if-let [way (way-up c threats)]
     way
     (let [p (:primitives c)
           from (u/self-pos c)
           opts-for #(respond.goal/choose-opts c threats covered? (respond.goal/closed-home %))
           first-pick (search/choose (:ends judged) (opts-for judged))
           ;; a search its meter cut has more to plan at the next step's: no look stands in for that
           again (when (and (not (search/cut? (:ends judged))) (or (nil? first-pick) (search/into-unseen? first-pick)))
                   (await (look-ahead! c threats covered? judged first-pick)))
           e (if again
               (search/choose (:ends again) (opts-for again))
               first-pick)
           round1 #(/ (js/Math.round (* 10 %)) 10)]
       (ctx/emit! c :retreat.search :debug
                  {:chasers (mapv (fn [{:keys [name pos speed]}] (cond-> [name (mapv #(js/Math.round (% pos)) [:x :y :z])] speed (conj (/ (js/Math.round (* 10 speed)) 10)))) (respond.goal/chasers c))
                   :from (mapv js/Math.floor (search/cell-of from))
                   :picked (some-> e :end search/cell-of)
                   :ends (mapv (fn [e] (cond-> (select-keys e [:open? :caught? :cover? :unseen])
                                         true (assoc :at (search/cell-of (:end e)) :score (round1 (:score e)) :gain (round1 (:gain e)) :s (round1 (:s e))
                                                     :arrive (round1 (:arrive e)) :dead-end (:dead-end e 0))
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
