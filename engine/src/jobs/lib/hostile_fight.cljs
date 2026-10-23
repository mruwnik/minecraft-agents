(ns jobs.lib.hostile-fight
  "The hostile mode of combat.attack (:targets :hostiles): which dangers near it fights, and when a held or leashed fight stops."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.access :as access]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.look :as look]
            [jobs.lib.flee-walk :as walk]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.body :as body]
            [jobs.lib.breath :as breath]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.shore :as shore]
            [engine.game :as game]
            [jobs.lib.price :as price]
            [jobs.lib.reach :as reach]
            [jobs.lib.threats :as threats]
            [jobs.lib.walk-risk :as risk]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::return-s {:default 600 :doc "Seconds a flight from a mob counts as a return of its kind: the next flight from one is priced to come again (flee-answer :returns)." :spec (a/num-in 0 nil)}
   ::hold-s {:default 15 :doc "Seconds a held fight waits without a swing for a mob to come within reach before it declines." :spec (a/num-in 0 nil)}
   ::chase-timeout-s {:default 10 :doc "Seconds one walk toward a hostile may take: it moves, so the walk aims again at where it is now this often." :spec (a/num-in 0 nil)}
   ::point-blank {:default 1.5 :doc "Blocks within which a hostile (a shooter too, not a creeper or an enderman) is in the body's own cell or beside it: a cornered body fights a shooter this close instead of hiding from it." :spec (a/num-in 0 nil)}
   ::back-step {:default 6 :doc "Blocks the body backs off from a creeper after each hit: its fuse runs on, so it ends out of the blast's reach (about 6)." :spec (a/num-in 1 nil)}
   ::felt-s {:default 3 :doc "Seconds back the mob blows the body felt set the rate a fight is priced at (fight-answer)." :spec (a/num-in 0 nil)}
   ::leash {:default 20 :doc "Blocks from where a hostile fight started beyond which a hostile is not chased." :spec (a/num-in 0 nil)}})

(defn hostiles? [c] (= :hostiles (:targets (:args c))))

(defn chase-timeout-s [] (settings/get settings ::chase-timeout-s))

(defn hold-ms [] (* 1000 (settings/get settings ::hold-s)))


(defn start-of
  "Where the fight started: the first step's position (the current one before it)."
  [c]
  (or (:start (ctx/mem c)) (u/self-pos c)))

(defn in-leash?
  "Whether hostile e stands within the leash of the start point."
  [c e]
  (<= (u/dist (start-of c) (u/pos-of (.-pos e))) (settings/get settings ::leash)))

(defn in-range
  "The hostiles within :radius (ranged ones within :ranged-radius, a heard one by its band): the visible ones nearest
  first, then the hidden melee ones (a ranged mob without a line of fire, by jobs.lib.danger/danger? as the hostile-near
  trigger, is dropped). The killed and the :skip ids are left out, and so is a calm enderman: a swing provokes it."
  [c]
  (let [{:keys [radius ranged-radius skip]} (:args c)
        ranged-radius (or ranged-radius radius)
        dead (into (set skip) (:killed (ctx/mem c)))
        all (->> (danger-q/known-hostiles (:primitives c) radius {:ranged-radius (max radius ranged-radius)})
                 (remove #(contains? dead (.-id %)))
                 (remove danger-q/calm-enderman?)
                 (remove #(and (combat/ranged? %) (not (danger-q/danger? (:primitives c) %)))))]
    (into (filterv #(.-visible %) all) (remove #(.-visible %)) all)))

(defn point-blank?
  "Whether hostile e is within ::point-blank of the body and is no creeper or enderman."
  [c e]
  (and (not (combat/creeper? e)) (not (combat/enderman? e))
       (<= (danger-q/mob-distance (:primitives c) e) (settings/get settings ::point-blank))))

(defn targets
  "The hostiles in range the body has seen, within the leash and not given up on, nearest first; a creeper only when it is
  the one hostile in range (fought hit-and-back): with company it is left to blow where the fight is (fight-answer)."
  [c]
  (let [given-up (:given-up (ctx/mem c) {})
        hs (in-range c)
        lone? (= 1 (count hs))]
    (->> hs (remove #(and (combat/creeper? %) (not lone?))) (filter danger-q/seen-only?) (filter #(in-leash? c %)) (remove #(contains? given-up (.-id %))))))

(defn heard
  "The first hostile in range the body only hears, or nil."
  [c]
  (first (remove danger-q/seen-only? (in-range c))))

(defn dark-around?
  "Whether mob e stands in or beside unlit space: its cell, the four cells next to it and the one 2 past it from the body,
  those open or not sensed, include one the body sees as dark (look/dark-fn: a cell not sensed counts as dark underground).
  A mob in the dark is hardly seen, so what shows is the lit edge of the dark it stands at."
  [c e]
  (let [p (:primitives c)
        pos (u/pos-of (.-pos e))
        me (u/self-pos c)
        [x y z] (mapv #(js/Math.floor %) [(:x pos) (:y pos) (:z pos)])
        away (fn [d] (let [v (- (d pos) (d me))] (cond (> v 0.5) 2 (< v -0.5) -2 :else 0)))
        cells [[x y z] [(inc x) y z] [(dec x) y z] [x y (inc z)] [x y (dec z)] [(+ x (away :x)) y (+ z (away :z))]]]
    (boolean (when-let [dark (look/dark-fn p)]
               (some (fn [[cx cy cz]]
                       (and (let [n (u/seen-name p {:x cx :y cy :z cz})] (or (nil? n) (bs/passable-or-water? n)))
                            (= 1 ((:at dark) cx cy cz))))
                     cells)))))

(defn held?
  "Whether the call waits for target instead of walking to it: :hold, a creeper (it comes, the body hits and backs off), or in a :hostiles fight a melee mob in or beside unlit space
  (a player does not chase into unlit space: more may be there, unseen)."
  [c target]
  (boolean (and (not (combat/ranged? target))
                (or (:hold (:args c)) (combat/creeper? target) (and (hostiles? c) (dark-around? c target))))))

(defn held-idle?
  "Whether a held fight has waited ::hold-s since its last swing (or its start) with no mob coming within reach."
  ([c] (held-idle? c (:hold (:args c))))
  ([c held?]
   (let [m (ctx/mem c)]
     (and held?
          (> (- (ctx/now c) (or (:last-attack m) (:started-at m))) (hold-ms))))))

(defn struck
  "What the fight counted per mob, as {id {:name :hits :health :blocked :blocked-health :quiet}} (the last three only
  when set), for a parent that weighs how near each mob is to dying and for the next call's carry-over."
  [m]
  (into {} (for [id (distinct (concat (keys (:hits m)) (keys (:fails m)) (keys (:quiet m))))]
             [id (cond-> {:name (get-in m [:names id]) :hits (get-in m [:hits id] 0)}
                   (number? (get-in m [:health id])) (assoc :health (get-in m [:health id]))
                   (pos? (get-in m [:fails id] 0)) (assoc :blocked (get-in m [:fails id]) :blocked-health (get-in m [:fail-health id]))
                   (pos? (get-in m [:quiet id] 0)) (assoc :quiet (get-in m [:quiet id])))])))

(defn carry-over
  "The attack memory that seeds a caller's next call after a fight that ended with hostiles left: the ending data's
  kills, given-up ids, names and per-mob counts (:struck); nil for an ending without :struck (a refused start)."
  [data]
  (when-let [struck (:struck data)]
    (let [pick (fn [k] (into {} (for [[id e] struck :when (some? (get e k))] [id (get e k)])))]
      {:killed (:killed data) :given-up (:given-up data) :names (:names data)
       :hits (pick :hits) :health (pick :health) :quiet (pick :quiet)
       :fails (pick :blocked) :fail-health (pick :blocked-health)})))

(defn mob-of
  "What fight-damage needs of hostile e: its name, distance (jobs.lib.danger/mob-distance: a heard one by its band) and
  the hits the attack has landed on it (struck, its :struck entry, or nil)."
  [p struck e]
  (cond-> {:name (.-name e) :held (.-held e) :distance (danger-q/mob-distance p e) :hits (:hits struck 0)}
    (number? (:health struck)) (assoc :health (:health struck))))

(defn fight-of
  "The fight with hostiles and the best weapon carried, as jobs.lib.cost/fight-damage takes it; struck-of maps a mob id
  to what the attack counted on it (nil: none)."
  [c hostiles struck-of]
  (let [p (:primitives c)]
    {:weapon (combat/best-weapon p (combat/weapons c))
     :equipment (cost/equipment-of (.-equipment (.self p)))
     :mobs (mapv #(mob-of p (struck-of (.-id %)) %) hostiles)}))

(defn incoming
  "{:rate hp a second, :hardest hp} of the mob blows the body felt within ::felt-s (its hurt memory), none before
  `since` (ms): their sum over the time since the first (at least a second), and the largest one; zeros without any."
  ([c] (incoming c 0))
  ([c since]
   (let [now (ctx/now c)
         hits (body/mob-hits-since (ctx/view c) (max since (- now (* 1000 (settings/get settings ::felt-s)))))
         amounts (map :amount hits)]
     {:rate (if (seq hits) (/ (reduce + amounts) (max 1 (/ (- now (:t (first hits))) 1000))) 0)
      :hardest (reduce max 0 amounts)})))

(defn way-mobs
  "The hostiles known (seen, or heard at their rough spot) within price ::way-radius of the way to the nearest of near,
  not in near: a fight walked into there meets them too."
  [p near]
  (when-let [t (first near)]
    (let [ids (set (map #(.-id %) near))]
      (remove #(ids (.-id %)) (danger-q/hostiles-near-way p (danger-q/mob-pos p t) (settings/get price/settings ::price/way-radius) {})))))

(defn walk-hp
  "hp the walk to the nearest of near costs: go-to's own route price (risk/estimate, its hostiles left out: they are in the
  fight) plus the drops of the straight way there over the ground the body has seen, its unseen steps at the planner's
  unknown prices (cost/unseen-route-hp)."
  [c near]
  (if-let [t (first near)]
    (let [p (:primitives c)
          goal (danger-q/mob-pos p t)
          {:keys [hp]} (risk/estimate c goal {:catchers? false})
          kind-at (reach/seen-lookup p reach/kind-of :ground)
          route (cost/straight-route kind-at (u/pos-of (.-pos (.self p))) goal)]
      (+ hp
         (cost/route-fall-damage (cost/equipment-of (.-equipment (.self p))) route)
         (cost/unseen-route-hp kind-at route)))
    0))

(defn body-of
  "What the answers' prices read of the body of c: health, absorption, food and what a death costs it (cost/death-body)."
  [c]
  (let [p (:primitives c)
        self (.self p)]
    (assoc (cost/death-body p (ctx/view c))
           :health (.-health self) :absorption (.-absorption self) :food (or (.-food self) 20))))

(defn blasting
  "The creepers known within ::price/way-radius of the way to the nearest of near (none of near) that come within their
  swell start of the body or that mob before a fight of seconds ends, at their chase speed: the fight walks into their blast."
  [p near seconds]
  (when-let [t (first near)]
    (let [ids (set (map #(.-id %) near))
          t-pos (danger-q/mob-pos p t)
          comes? (fn [cr] (let [at (danger-q/mob-pos p cr)
                                d (min (danger-q/mob-distance p cr) (u/dist at t-pos))]
                            (<= (/ (max 0 (- d cost/swell-start)) (cost/chase-speed (cost/mob-kind cr))) seconds)))]
      (->> (danger-q/hostiles-near-way p t-pos (settings/get price/settings ::price/way-radius) {})
           (filter combat/creeper?)
           (remove #(ids (.-id %)))
           (filter comes?)))))

(def swing-reach "Blocks from its eyes within which a player's swing lands on a mob's box (vanilla)." 3)

(defn swing-gap
  "Blocks from the eyes at ex ey ez to the box of a mob whose feet are at pos (taken as a body's box)."
  [ex ey ez {:keys [x y z]}]
  (let [gap (fn [v lo hi] (max 0 (- lo v) (- v hi)))]
    (js/Math.hypot (gap ex (- x game/hitbox-half) (+ x game/hitbox-half))
                   (gap ey y (+ y game/body-height))
                   (gap ez (- z game/hitbox-half) (+ z game/hitbox-half)))))

(defn reaches?
  "Whether the body can get within swing-reach of hostile e: one only heard or a flyer (it comes to the body) does; else
  the body's eyes are in reach now, or some cell in reach holds the body (on a floor, or afloat: water with no water
  over it) and a walk or surface swim gets there (jobs.lib.reach/forward over the mob lookup: an unseen cell is ground;
  a search that runs out counts as one)."
  [c e]
  (let [p (:primitives c)
        at (danger-q/mob-pos p e)
        me (u/self-pos c)
        kind-at (reach/mob-lookup p)
        near? (fn [x y z] (<= (swing-gap (+ x 0.5) (+ y game/eye-height) (+ z 0.5) at) swing-reach))
        holds? (fn [x y z] (and (reach/standable? kind-at x y z)
                                (not (and (keyword-identical? :water (kind-at x y z)) (keyword-identical? :water (kind-at x (inc y) z))))))
        [mx my mz :as cell] (reach/cell-of at)
        r (js/Math.ceil swing-reach)]
    (boolean
     (or (not (danger-q/seen-only? e))
         (combat/flyer? e)
         (<= (swing-gap (:x me) (+ (:y me) game/eye-height) (:z me) at) swing-reach)
         (and (some (fn [[x y z]] (and (near? x y z) (holds? x y z)))
                    (for [x (range (- mx r) (+ mx r 1)) z (range (- mz r) (+ mz r 1)) y (range (- my r 2) (+ my r 1))] [x y z]))
              (not= :closed (reach/search-steps (partial reach/forward kind-at) (reach/cell-of me) cell near?)))))))

(declare hit-and-back-ok? flee-answer)

(defn fight-answer
  "The answer of fighting near (nearest first) for cost/decide: the walk there (walk-hp) and the fight with near and the
  mobs by its way (way-mobs), at least the mob blows felt lately (incoming :rate) over its length, :hit the hardest blow
  felt lately or of its mobs. A mob the body cannot get within a swing of (reaches?) is not killed (cost/fight-answer):
  the fight is followed at once by the walk away from those it leaves (flee-answer, cost/followed). A lone creeper is
  fought hit-and-back (nil when hit-and-back-ok? fails); any other creeper near, or by the way in time (blasting), is
  not fought: it blows where the fight is, beside its first mob, so its blast at its distance from that mob (at most its
  swell start) is added, in :blasts."
  [c near struck-of]
  (let [p (:primitives c)
        lone-creeper? (and (= 1 (count near)) (combat/creeper? (first near)))
        fought (if lone-creeper? near (remove combat/creeper? near))
        others (remove combat/creeper? (way-mobs p fought))]
    (when (and (seq fought) (or (not lone-creeper?) (hit-and-back-ok? c near)))
      (let [es (vec (concat fought others))
            fight (update (fight-of c es struck-of) :mobs #(mapv (fn [m e] (assoc m :reach? (reaches? c e))) % es))
            left (keep (fn [[m e]] (when (false? (:reach? m)) e)) (map vector (:mobs fight) es))
            stats (cost/armour-stats (:equipment fight))
            {:keys [rate hardest]} (incoming c)
            a (cost/fight-answer fight)
            seconds (+ (:seconds a) (/ (danger-q/mob-distance p (first fought)) cost/walk-speed))
            creepers (if lone-creeper? [] (concat (filter combat/creeper? near) (blasting p near seconds)))
            spot (danger-q/mob-pos p (first fought))
            blasts (map #(cost/blast-hurt stats (min cost/swell-start (u/dist (danger-q/mob-pos p %) spot))) creepers)]
        (cond-> {:damage (+ (walk-hp c fought) (max (:damage a) (* rate (:seconds a))) (reduce + 0 blasts))
                 :hit (max (:hit a) hardest)
                 :blasts (vec (filter pos? blasts))
                 :seconds seconds
                 :creepers (vec creepers)}
          (seq left) (cost/followed (flee-answer c left nil nil)))))))

(defn mob-near
  "Hostile e as the answers take a mob: {:name :held :distance :speed (its chase speed) :follow (its follow range)}."
  [p e]
  {:name (.-name e) :held (.-held e) :distance (danger-q/mob-distance p e) :speed (cost/chase-speed (cost/mob-kind e)) :follow (threats/follow-range (.-name e))})

(defn swim-out
  "{:swim blocks :land blocks :chance c} of a body in water swimming to the nearest dry stand joined to it by water and air
  (shore/swim-to-land: the way's length, a stand with land to walk on beyond the mobs' reach over a ledge) and the land it can walk on from there; with no stand in the joined water the swim is the farthest it reaches and the land 0; :chance the chance more land lies where nothing was seen (shore/land-run); nil on land. One flood a place, kept ::check-cache/ttl-ms."
  [c mobs]
  (let [p (:primitives c)]
    (when (breath/in-water? p)
      (let [me (u/self-pos c)
            reach (reduce max 0 (map cost/hit-range mobs))]
        (check-cache/cached
         c [:swim-out (u/cell-vec me) reach]
         (fn []
           (let [{:keys [stand land] :as found} (shore/swim-to-land p me reach)]
             (if (or (nil? stand) (zero? land))
               (assoc (select-keys found [:swim :land :chance]) :stand? (some? stand))
               (let [{:keys [run chance]} (shore/land-run p stand ##Inf)]
                 {:swim (:swim found) :land run :chance chance :stand? true})))))))))

(defn returns
  "Flights from mobs named one of names within ::return-s, by the :hostile entries (c is a ctx): one for each other run
  that fled (its :run, the run's :run-at in memory); the run's own change to flight is no return."
  [c names]
  (let [since (- (ctx/now c) (* 1000 (settings/get settings ::return-s)))
        names (set names)
        own (:run-at (ctx/mem c))]
    (->> (ctx/entries c :hostile)
         (filter #(and (> (:t %) since) (names (get-in % [:data :mob])) (= :flee (get-in % [:data :decision]))))
         (keep-indexed (fn [i e] (let [run (get-in e [:data :run])] (cond (nil? run) [:entry i] (not= own run) run))))
         distinct
         count)))

(defn flee-answer
  "The answer of a walk away from near (cost/flee-answer: each at its chase speed and follow range, the flights from
  such mobs lately its returns, later the answer after it, in water first the swim-out), its damage at least felt (hp a
  second felt since the flight began, nil none) over the walk; nil when a mob keeps up."
  [c near later felt]
  (let [p (:primitives c)
        self (.self p)
        mobs (mapv #(mob-near p %) near)
        swim (swim-out c mobs)
        a (cost/flee-answer {:food (or (.-food self) 20) :equipment (cost/equipment-of (.-equipment self))
                             :mobs mobs
                             :swim (:swim swim) :land (:land swim) :open (:chance swim)
                             :ashore (when (and (:chance swim) (:stand? swim)) (cost/fight-answer (fight-of c near (constantly nil))))
                             :returns (returns c (map #(.-name %) near))
                             :later later})]
    (cond-> a
      (and a felt) (update :damage max (* felt (/ (:seconds a) 2))))))

(defn weigh
  "cost/decide over answers ({key answer-or-nil}) for the body of c where it stands, with :answers; nil when none is open."
  [c answers]
  (some-> (cost/decide (body-of c) answers (u/self-pos c)) (assoc :answers answers)))

(defn answer
  "weigh fighting near (fight-answer) against walking away from them (flee-answer): {:answer :fight|:flee :prices :answers}."
  [c near struck-of]
  (let [fight (fight-answer c near struck-of)]
    (weigh c {:fight fight :flee (flee-answer c near fight nil)})))

(defn too-far-to-chase?
  "Whether a ranged target is better left than walked to with other hostiles in range: answer over them all says :flee."
  [c target]
  (let [hs (in-range c)]
    (boolean (and (combat/ranged? target)
                  (some #(not= (.-id %) (.-id target)) hs)
                  (= :flee (:answer (answer c (cons target (remove #(= (.-id %) (.-id target)) hs)) (constantly nil))))))))

(defn back-cell
  "The cell n blocks (at most) from from in the open direction that ends farthest from threat, gaining at least a block on
  it; nil when none does. block-at: cell -> block name (as respond.options/block-at-fn)."
  [block-at from threat n]
  (let [now (u/dist from threat)]
    (->> (range 0 360 45)
         (keep #(peek (walk/walk-cells block-at from (walk/rotate (or (walk/unit (- (:x from) (:x threat)) (- (:z from) (:z threat))) [1 0]) %) n)))
         (filter #(>= (u/dist % threat) (inc now)))
         (sort-by #(- (u/dist % threat)))
         first)))

(defn back-step [] (settings/get settings ::back-step))

(defn back-cell-of
  "The cell the body backs off to from creeper over what it senses (back-cell), nil when none."
  [c creeper]
  (let [at (access/sensed-at (:primitives c) access/hidden-guess)]
    (back-cell (fn [{:keys [x y z]}] (at [x y z])) (u/self-pos c) (u/pos-of (.-pos creeper)) (back-step))))

(defn hit-and-back-ok?
  "Whether the body can fight hostiles hit-and-back: exactly one, a creeper, a sword or axe carried and an open cell to
  back off to. Ends the fight when it stops holding."
  [c hostiles]
  (let [p (:primitives c)
        creeper (first hostiles)]
    (boolean (and (= 1 (count hostiles))
                  (combat/creeper? creeper)
                  (contains? weapon/weapon-damage-by-name (combat/best-weapon p (combat/weapons c)))
                  (back-cell-of c creeper)))))

(defn wins?
  "Whether fighting hostiles leaves :reserve health (jobs.lib.danger/fight-reserve; fight-answer's damage): the one weighing the flight's
  cornered options share. Never against a creeper, nor against none, nor against an
  enderman: only thorns armour with tamed wolves would win, and the body cannot see wolves."
  ([c hostiles] (wins? c hostiles (constantly nil)))
  ([c hostiles struck-of]
   (boolean (and (seq hostiles)
                 (not-any? combat/creeper? hostiles)
                 (not-any? combat/enderman? hostiles)
                 (cost/leaves-reserve? {:health (.-health (.self (:primitives c))) :reserve (danger-q/fight-reserve)
                                        :damage (:damage (fight-answer c hostiles struck-of))})))))
