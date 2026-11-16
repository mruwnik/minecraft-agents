(ns jobs.survival.respond.options
  "The cornered options of a flight (jobs.survival.respond-flight): the fight, the back-off and the stand (jobs.survival.stand-in), and the settings of the retreat jobs."
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [jobs.lib.blocks :as lb]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.hurt :as hurt]
            [jobs.lib.body :as body]
            [jobs.lib.child :as child]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.danger :as cost-danger]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.shelter.clock :as clock]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.memory-age :as memory-age]
            [engine.perception.rays :as rays]
            [jobs.lib.seal :as seal]
            [jobs.lib.escape :as escape]
            [jobs.lib.hostile-fight :as hostile]
            [jobs.lib.hostile-loss :as hostile-loss]
            [jobs.lib.walk-pick :as pick]
            [jobs.lib.look :as look]
            [jobs.lib.pillar :as pl]
            [jobs.lib.ready :as ready]
            [jobs.lib.reach :as reach]
            [jobs.lib.result :as result]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.util :as u]
            [jobs.lib.shot :as shot]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.respond.state :as flight]
            [jobs.survival.respond.goal :as respond.goal]
            [jobs.lib.rooms :as room-lib]
            [jobs.survival.respond.pillar :as pillar]
            [jobs.survival.respond.refuge :as refuge]
            [jobs.lib.flee-search :as search]
            [jobs.lib.walk.world :as wworld]
            [jobs.survival.hide-out :as hide-out]
            [jobs.survival.shut-in :as shut-in]
            [jobs.survival.wall-in :as wall-in]))

(def tool-weapons
  "Item name substrings a cornered body with no weapon and no seal swings: any of them beats the fist."
  ["_pickaxe" "_shovel" "_hoe"])



(a/defargs settings
  {::back-off-reach {:default 2 :doc "Blocks off the ring a back-off step's flee search plans its ends on (jobs.lib.flee-search: each heading's end within a block of it)." :spec (a/num-in 1 nil)}
   ::fist-sweeps {:default 1 :doc "Times a body with no weapon, every cornered option failed, looks for a way to walk before it punches a hostile it would not beat; 0: it punches at once." :spec (a/int-in 0 nil)}
   ::hit-reach {:default 3 :doc "Blocks within which a melee hostile counts as the one that hit the body." :spec (a/num-in 0 nil)}
   ::glance-reach {:default 3 :doc "Blocks along a direction to the feet cell a glance looks at." :spec (a/int-in 0 nil)}
   ::glance-memory {:default 24 :doc "Most glanced cells a flight remembers." :spec (a/int-in 0 nil)}
   ::cover-gap {:default 5 :doc "Blocks within which a melee chaser keeps a low body from taking cover where it stands." :spec (a/num-in 0 nil)}
   ::close-radius {:default 8 :doc "Blocks within which a hostile that attacks at close range (a creeper too, not a ranged mob or an enderman) bounds a flee step to close-leg-s, decided at the step's start." :spec (a/num-in 0 nil)}
   ::close-leg-s {:default 1 :doc "Seconds a flee step may search or walk while a close-range hostile is within close-radius, or the body was hurt: a body that stands searching there is hit for the whole leg." :spec (a/num-in 0 nil)}
   ::hurt-window-s {:default 5 :doc "Seconds after the body lost health in which a flee step is bounded to close-leg-s, even with no hostile seen in reach (an unseen attacker hits as hard)." :spec (a/num-in 0 nil)}
   ::fire-hits {:default 1 :doc "Hits by a ranged mob (the damage's attacker, or an arrow) within hurt-window-s that put a cornered body under fire: cover comes before any fight, and a shooter it has not seen is never weighed as beaten. 0: never." :spec (a/int-in 0 nil)}
   ::shot-hits {:default 2 :doc "Hits by a ranged mob within hurt-window-s that make a walking body take cover (seal, or break the line of fire) instead of walking on under fire it cannot answer. 0: it keeps walking." :spec (a/int-in 0 nil)}
   ::cover-tries {:default 2 :doc "Cornered steps under fire in which cover (seal, back off) is tried before an armed body fights a melee mob pressing it: a mob standing in the cells to wall keeps every seal failing, and each back-off step makes the covers open again." :spec (a/int-in 1 nil)}
   ::beaten-hits {:default 2 :doc "Hits by a melee mob near (the damage names one within close-radius; not a creeper or an enderman) within hurt-window-s since the flight's first leg that show the flight is failing: the body fights it first, armed or not. 0: never." :spec (a/int-in 0 nil)}
   ::press-gap {:default 2 :doc "Blocks within which a melee mob (not a creeper or an enderman) makes a cornered armed body fight before it builds a seal or pillar, hit or not: the mob strikes before a block is placed and stands in the cells to fill." :spec (a/num-in 0 nil)}
   ::arrival-s {:default 4 :doc "Seconds within which a melee mob (not a creeper or an enderman) reaches a cornered armed body: it digs no pit or pocket, which take longer than the mob needs while the mob hits the digger. 0: never." :spec (a/num-in 0 nil)}
   ::pillar-mob-fails {:default {:beside {:k 4 :n 4} :clear {:k 4 :n 8}}
                       :spec (a/map-of #{:beside :clear} (a/map-with {:k (a/int-in 0 nil) :n (a/int-in 0 nil)}))
                       :doc "Of n pillars of a flight (built, or failed for a mob: a refused block or a knockback with a hostile near the cell, or a mob in the cell), k failed so, one per situation (a flight's feet cell and class), by the melee hostiles when it began (respond.pillar/mob-class): :beside, one whose box overlaps a cell the pillar fills, :clear none. The trial and probe bodies' logs on 2026-10-28 (a refusal is physics given the class, which staged scenes do not skew). Recount: node tools/event-counts.mjs wall-places."}
   ::pillar-s {:default 1 :doc "Seconds a cornered pillar takes to lift the body out of a melee mob's reach (a jump and a place a block): the blows it costs then are weighed against the fight's." :spec (a/num-in 0 nil)}
   ::flight-timeout-s {:default 5 :doc "Seconds bounding one step of the flight: the mob moves, so the next step aims again." :spec (a/num-in 0 nil)}
   ::creeper-keep {:default 5 :doc "Blocks within which a creeper stops the fight walking toward a mob on its side: the fight swings from where the body stands, never a step nearer the creeper." :spec (a/num-in 0 nil)}})
(defn flight-timeout-s [] (settings/get settings ::flight-timeout-s))
(defn creeper-keep [] (settings/get settings ::creeper-keep))

(defn fist-sweeps [] (settings/get settings ::fist-sweeps))
(defn hit-reach [] (settings/get settings ::hit-reach))

(defn hits-total
  "The hits an attack ending's (or prior's) :struck counts over all mobs."
  [struck] (reduce + 0 (map :hits (vals struck))))

(declare near-hostiles)

(defn ^:async fight!
  "Fight back with the best of weapons (the fist when none is carried) whatever
  the health, kept while the hostile stays close: :again, nil when the fight
  cannot reach any hostile, or every target is already dead (the option failed this flight, else the flight spins without time passing; hits new in this call reset the sweep count). reach-only?: only a hostile within hit-reach is
  swung at, none walked to. A fight that ends other than done keeps its memory (its given-up counts) for the next."
  [c weapons reach-only?]
  (let [radius (danger-q/hostile-radius)
        radius (if reach-only? (min radius (hit-reach)) radius)
        ranged-radius (if reach-only? radius :fire-range)]
    (ctx/update-mem! c assoc :cornered true)
    (ctx/update-mem! c #(cond-> % (:cornered (:children %)) (update-in [:children :cornered] dissoc :start)))
    (let [hits-before (hits-total (:struck (:fight-prior (ctx/mem c))))
          r (await (ctx/call c :cornered 'jobs.combat.attack
                             {:targets :hostiles :radius radius :ranged-radius ranged-radius :hold true :use {:tools (mapv name weapons)}
                              :skip (flight/dead-ids c) :prior (:fight-prior (ctx/mem c)) :checks {:safe? '(always) :fit? '(always)}}))
          ended (when (contract/ending? r) r)]
      (ctx/update-mem! c update :dead #(into (vec %) (:killed (:data ended))))
      (if (and ended (or (not= :done (:kind ended)) (= :already (:cause ended))))
        (do (when (:struck (:data ended)) (ctx/update-mem! c assoc :fight-prior (select-keys (:data ended) [:killed :given-up :names :struck])))
            (flight/tried! c :fight)
            (when (> (hits-total (:struck (:data ended))) hits-before)
              (ctx/update-mem! c assoc :sweeps 0))
            nil)
        :again))))


;; ------------------------------------------------------------------ cornered: the safest option

(defn outrun?
  "Whether the body gets away on foot from every one of hostiles that attacks at close range (jobs.lib.cost/outruns?,
  at its food). Creepers, endermen and ranged mobs have their own rules."
  [c hostiles]
  (cost/outruns? (room-lib/food (:primitives c))
                 (->> hostiles
                      (remove #(or (combat/creeper? %) (combat/enderman? %) (combat/ranged? %)))
                      (map cost/mob-kind))))

(defn near
  "The hostiles that count: the hostile-near trigger's dangers (danger/hostile-dangers with the args of its register
  entry, ready/trigger-args), so the call answers what fired it and ends once none is left."
  [c]
  (danger-q/hostile-dangers (:primitives c) (ctx/view c) (ready/trigger-args c)))

(defn near-hostiles
  "The hostiles a cornered body weighs: within :radius, ranged ones within :ranged-radius, the dead skipped."
  [c]
  (let [radius (danger-q/hostile-radius)
        ]
    (flight/near-known (:primitives c) (set (flight/dead-ids c)) radius :fire-range)))

(defn angry-enderman?
  "Whether one of hostiles is an enderman that is angry: the client flags it so (a stare or a blow provokes it), or an
  enderman hurt the body this flight. A calm one is walked away from like any mob, never sealed against, however near.
  Once found angry it stays so for the flight while one is known within follow range (the client's flag does not hold,
  and the enderman may teleport out of the near radius: the body must not leave the water it stepped into)."
  [c hostiles]
  (let [p (:primitives c)
        hurt? (boolean (some-> (:flight-start (ctx/mem c)) (->> (body/hit-by-since? (ctx/view c) "enderman"))))
        angry? (boolean (or (and (or hurt? (:angry-enderman (ctx/mem c))) (some combat/enderman? (concat hostiles (flight/known-chasers p))))
                            (some #(and (combat/enderman? %) (not (danger-q/calm-enderman? %))) hostiles)))]
    (when angry? (ctx/update-mem! c assoc :angry-enderman true))
    angry?))

(defn hitters
  "Of hostiles, the melee ones (not a creeper or an enderman) within hit-reach."
  [c hostiles]
  (filterv #(and (not (combat/ranged? %)) (not (combat/creeper? %)) (not (combat/enderman? %))
                 (<= (danger-q/mob-distance (:primitives c) %) (hit-reach)))
           hostiles))

(defn close-radius [] (settings/get settings ::close-radius))

(defn hurt-by
  "The causes of a :hurt memory entry (the raw body event: its attacker's name, else the damage's cause)."
  [e]
  (hurt/causes-of (:data e)))

(defn hit-on-the-run!
  "Whether a melee chaser hit the body on a leg walked since the previous flight step (ran! notes the leg's start; blows
  before it, in a search or a fight, say nothing):
  health lost with a hitter near, or a hurt entry naming a melee hostile within close-radius (a fed body heals the hit
  within a second, so health alone misses it). A chaser that hits a running body cannot be outrun. Noted in job memory (:hit-on-the-run), with the health, for the cornered
  options' odds."
  [c]
  (let [p (:primitives c)
        health (.-health (.self p))
        {:keys [step-at ran-at ran-health]} (ctx/mem c)
        now (ctx/now c)
        near (near-hostiles c)
        names (->> near
                   (filter #(and (not (combat/ranged? %)) (not (combat/creeper? %)) (not (combat/enderman? %))
                                 (<= (danger-q/mob-distance p %) (close-radius))))
                   (map #(.-name %))
                   set)
        ran? (and step-at ran-at (>= ran-at step-at))
        entries (when ran? (ctx/since c :hurt ran-at))
        named? (and ran? (some #(some names (hurt-by %)) entries))
        mob-or-unlogged? (or (empty? entries) (some #(hurt/mob-hit? (:data %)) entries))
        hit? (boolean (and ran? (or (and ran-health (< health ran-health) (seq (hitters c near)) mob-or-unlogged?) named?)))]
    (ctx/update-mem! c assoc :step-at now :hit-on-the-run hit?)
    hit?))

(def cover-options
  "The cornered options that shut the body in, each with the jobs.survival.shut-in ways it builds."
  {:plug [:walls] :plug-dig [:pit :pocket]})

(defn hurt-window-s [] (settings/get settings ::hurt-window-s))

(defn shots-in
  "Shots in one :hurt entry: its hits when the entry is a shot, else the shot packets among its merged damages (a skeleton's arrow and a zombie's blow in one loss)."
  [e]
  (if (shot/shot? e)
    (:hits (:data e) 1)
    (count (filter #(shot/shot? {:data %}) (:damages (:data e))))))

(defn quiet-s "Seconds a hidden body keeps its refuge after the last danger (hide-out ::quiet-s), for the job that may not require hide-out itself." [] (hide-out/quiet-s))

(defn pillar-cover
  "The cover {:open-s :seconds :fails :reached :rise :look-s :blocked} of a pillar against hostiles (cost/cover-answer): in a melee mob's reach for ::pillar-s,
  then the hide-out's quiet on top (the open seconds only for the blocks not yet under the body: pillar/lifted), all of it in a shooter's line of fire or a creeper's sight; it fails for a mob in its cell by
  ::pillar-mob-fails of the class the body stands in (wall-in/mob-fails, respond.pillar/mob-class), with the refusals this
  flight saw added (flight/with-seen-fails), or for a mob climbing to its top (respond.pillar/pillar-climb), and its
  failures here (flight/fails-here); begun by plan!'s look at the unsensed cells of its rise (cost/look-s), rock there by
  :blocked (respond.pillar/rise-look);
  nil when no pillar fits here or a climber or a flyer is near (both reach its top). A shot felt within ::hurt-window-s keeps the whole of its seconds in the line of fire (cornered-covers prices the shooter)."
  [c hostiles]
  (when-let [climb (and (pillar/pillar-item c) (not-any? #(or (combat/climber? %) (combat/flyer? %)) hostiles)
                        (pillar/climb-of c #(room-lib/survey-over? c)))]
    (let [h (- (pillar/pillar-height) (pillar/lifted c))
          s (* (settings/get settings ::pillar-s) (/ h (pillar/pillar-height)))
          seconds (+ s (hide-out/quiet-s))
          {:keys [looks blocked]} (pillar/rise-look (:primitives c) (pl/feet-cell c))
          refused (wall-in/mob-fails (flight/with-seen-fails c :pillar (settings/get settings ::pillar-mob-fails))
                                     (= :beside (pillar/mob-class (:primitives c) (pl/feet-cell c))))]
      {:open-s (if (or (some #(or (combat/ranged? %) (combat/creeper? %)) hostiles)
                       (some #(pos? (shots-in %)) (ctx/since c :hurt (- (ctx/now c) (* 1000 (hurt-window-s))))))
                 seconds s)
       :seconds seconds
       :fails (flight/fails-here c :pillar (- 1 (* (- 1 refused) (- 1 climb))))
       :reached (boolean (some combat/enderman? hostiles)) :rise [s h]
       :look-s (cost/look-s looks) :blocked blocked})))

(defn covers-in
  "What shut-in/covers reads past the ways: the body's feet cell, the cells the hostiles near fill and their kinds, its
  inventory, the options tried, the args the ways take and the remembered and raw world (their change counters)."
  [c]
  (let [p (:primitives c)
        hs (danger-q/known-hostiles p (danger-q/hostile-radius) {})]
    {:feet (sh-sense/feet p)
     :mobs (set (mapcat #(seal/hitbox-cells (danger-q/mob-pos p %)) hs))
     :kinds (sort (map #(.-name %) hs))
     :items (mapv (juxt :name :count) (u/inventory p))
     :tried (:tried (ctx/mem c))
     :args (select-keys (:args c) [:for :ignore-zones? :trespass? :on-lava])
     :memory (some-> (aget p "perception") :st memory-age/epoch)
     :epoch (some-> (look/raw-of p) rays/epoch-of)}))

(defn covers
  "shut-in/covers of ways where the body stands, each way's kept (memory ::covers) for check-cache/ttl-ms while what
  they read stays the same (covers-in): one decision prices them once for its answers, its flee search's dead end and
  its cornered step. Each way's :fails counts its failures where the body stands (flight/fails-here), and :dig-s the dig
  away out of it from the nearest hostile (hide-out/dig-out-s; the pillar has none). A way not kept is quoted only
  while the decision's stages before the flee search are in time (room-lib/survey-over?); the ways left (meta :cut)
  are quoted by the next call."
  [c ways]
  (let [p (:primitives c)
        in (covers-in c)
        now (ctx/now c)
        kept (::covers (ctx/mem c))
        kept (if (and (= in (:in kept)) (<= 0 (- now (:t kept)) (check-cache/ttl-ms))) kept {:in in :t now :asked #{} :v {}})
        missing (remove (:asked kept) ways)
        [kept cut] (loop [kept kept [w & more :as left] missing]
                     (if (or (nil? w) (room-lib/survey-over? c))
                       [kept (vec left)]
                       (recur (-> kept (update :v merge (shut-in/covers c [w])) (update :asked conj w)) more)))]
    (when (not= (count cut) (count missing)) (ctx/update-mem! c assoc ::covers kept))
    (let [nearest (some->> (seq (danger-q/known-hostiles p (danger-q/hostile-radius) {})) (apply min-key #(danger-q/mob-distance p %)))
          dig-s (hide-out/dig-out-s p (some->> nearest (danger-q/mob-pos p)))]
      (with-meta (into {} (map (fn [[w spec]] [w (cond-> (update spec :fails #(flight/fails-here c w (or % 0))) dig-s (assoc :dig-s dig-s))])) (select-keys (:v kept) ways))
        {:cut cut}))))

(defn exits-of
  "{:dawn-s} every cover's spec carries (cost/cover-answer's exits): the mean seconds of night left."
  [c]
  {:dawn-s (second (get-in (clock/night-left-quote c) [:uses :job-time]))})

(defn cornered-covers
  "{option cover} of the cornered options not yet tried that shut the body in or lift it: :plug the walls, :plug-dig
  the cheaper of a pit and a pocket (none at home), :pillar (pillar-cover). given: the ways' covers to read them from
  (default covers) and the pillar's (respond-to-hostile/cover-specs)."
  ([c hostiles home?] (cornered-covers c hostiles home? nil))
  ([c hostiles home? given]
  (let [open? #(not (flight/tried? c %))
        ways (mapcat (fn [[option ways]] (when (and (open? option) (not (and home? (= :plug-dig option)))) ways)) cover-options)
        specs (if given (select-keys given ways) (covers c ways))
        equipment (cost/equipment-of (.-equipment (.self (:primitives c))))
        mobs (hostile-loss/mobs-of (:primitives c) (ctx/view c) hostiles)
        body (assoc (hostile/body-of c) :meet (mapv #(.-name %) hostiles))
        at (u/self-pos c)
        answer #(cost/cover-answer {:equipment equipment :mobs mobs} (merge (exits-of c) %))
        death (cost/meet-death-fn body at)
        price #(cost/answer-price* body (answer %) at death)
        cheaper (fn [ways] (some->> (keep specs ways) seq (apply min-key price)))
        pillar (when (open? :pillar) (if given (:pillar given) (pillar-cover c hostiles)))]
    (update-vals (cond-> {}
                   (cheaper (:plug cover-options)) (assoc :plug (cheaper (:plug cover-options)))
                   (cheaper (:plug-dig cover-options)) (assoc :plug-dig (cheaper (:plug-dig cover-options)))
                   pillar (assoc :pillar pillar))
                 #(merge (exits-of c) %)))))

(defn break-out-s
  "Seconds the body takes to break out of the seal of a refuge that failed where it stands: the cheapest block to dig
  (cost/dig-cost) of the flight's plug cells, else of the walls round the feet; a cell it does not see is the block it
  placed there, else rock (the walk's guess for an unseen cell). 0 when it is not so shut in."
  [c]
  (if-not (refuge/failed-refuge-here? c)
    0
    (let [p (:primitives c)
          {:keys [x y z]} (sh-sense/feet p)
          placed (into {} (map (juxt :cell :item)) (refuge/plug-entries c))
          cells (or (seq (keys placed)) [[(inc x) y z] [(dec x) y z] [x y (inc z)] [x y (dec z)]])]
      (or (some->> cells (keep #(cost/dig-cost (or (u/seen-name p (zipmap [:x :y :z] %)) (placed %) "stone"))) seq (apply min)) 0))))

(defn leaving
  "answers after break-s seconds of breaking out of a failed refuge's seal: the options in leaves (those that take the body
  out of its cell) begun after the break (cost/leave-answer: the blows of mobs over it), the plugs gone (refuge/plug-option!
  refuses a plug there), the rest (a hold in place) as they were."
  [equipment mobs break-s leaves answers]
  (if (pos? break-s)
    (reduce (fn [as k] (cond-> as (contains? as k) (update k #(cost/leave-answer {:equipment equipment :mobs mobs :seconds break-s} %))))
            (dissoc answers :plug :plug-dig)
            leaves)
    answers))

(defn ^:async fight-walk-q
  "go-to's quote of the walk a cornered fight makes (fight!: :hold, so a melee mob is waited for, never walked to): to the
  first mob of hostiles it goes for that the body has seen (hostile/fought, hostile/sighted?) when that one shoots; nil
  when the fight walks nowhere."
  [c hostiles]
  (let [p (:primitives c)
        t (first (filter hostile/sighted? (hostile/fought p hostiles)))]
    (when (and t (combat/ranged? t))
      (await (pick/quote! c (hostile/fight-walk-args p t))))))

(defn cornered-answers
  "{option answer} of the cornered options open to the body against hostiles: :fight, the covers (cornered-covers),
  with a back-off step to take (back-off-of's :target; nil: none or tried), :back-off the walk away
  (hostile/cornered-answers) caught as its search says (:caught), and with swim? (afloat, a drowned near) :swim-out the
  swim to dry land and walk on (hostile/flee-answer, caught at the prior: no search runs from the shore); with enderman?
  (an angry one near) :water and :roof not yet tried, and with a creeper near :stand, a hide's seconds (hide-out/hide-s, the window its stay chance is
  counted over) held where hostiles but the enderman reach the body (cost/hold-answer), each whole hide this flight held
  (memory :held-s) counted as one the danger outlasted: the hold lasts while it is the cheapest. The stand is priced
  against every hostile that hits a body standing there: each melee mob by its arrival, each creeper's blast, each
  shooter with a line of fire (danger?) or not seen (hostile-loss/unseen-shooters). home?: the body is in its home room; walk-q: go-to's quote of the walk the fight makes (fight-walk-q)."
  ([c hostiles home? back-off swim? enderman?] (cornered-answers c hostiles home? back-off swim? enderman? nil))
  ([c hostiles home? back-off swim? enderman? walks & [walk-q]]
  (let [a (hostile/cornered-answers c hostiles (cornered-covers c hostiles home?) (some? (:target back-off)) (:caught back-off) walk-q)
        p (:primitives c)
        hold (fn [o mobs] (cost/hold-answer {:equipment (cost/equipment-of (.-equipment (.self p))) :mobs mobs
                                            :walk-s (get walks o 0) :then (vec (remove nil? (vals (dissoc a :stand))))
                                            :seconds (hide-out/hide-s) :stayed (quot (:held-s (ctx/mem c) 0) (hide-out/hide-s))}))
        mobs (mapv #(hostile/mob-near p %) hostiles)]
    (->> (cond-> (dissoc a :flee)
      (contains? a :stand) (assoc :stand (hold :stand (into (mapv #(hostile/mob-near p %) (filter #(or (not (combat/ranged? %)) (danger-q/danger? p %)) hostiles))
                                                            (hostile-loss/unseen-shooters p (ctx/view c) hostiles))))
      (contains? a :flee) (assoc :back-off (:flee a))
      swim? (assoc :swim-out (hostile/flee-answer c hostiles (:fight a) nil nil))
      enderman? (into (for [o [:water :roof] :when (not (flight/tried? c o))] [o (hold o mobs)])))
         (leaving (cost/equipment-of (.-equipment (.self p))) mobs (break-out-s c)
                  (into #{:fight :back-off :swim-out :pillar} (filter #(pos? (get walks % 0))) [:water :roof :stand]))))))

(defn glance-reach [] (settings/get settings ::glance-reach))

(defn glance-memory [] (settings/get settings ::glance-memory))

(defn arriving
  "Of hostiles, the melee ones (not a creeper or an enderman) that reach the body within s seconds (::arrival-s) at their chase speed."
  ([c hostiles] (arriving c hostiles (settings/get settings ::arrival-s)))
  ([c hostiles s]
   (filterv #(and (not (combat/ranged? %)) (not (combat/creeper? %)) (not (combat/enderman? %))
                  (<= (/ (danger-q/mob-distance (:primitives c) %) (cost-danger/chase-speed (cost/mob-kind %))) s))
            hostiles)))

(defn cover-gap [] (settings/get settings ::cover-gap))


(defn press-gap [] (settings/get settings ::press-gap))

(defn close-leg-s [] (settings/get settings ::close-leg-s))


(defn sighted-gaps
  "{id [name blocks]} of the near hostiles the body sees now (not remembered): the gaps a step's closing rate is read from."
  [c]
  (into {} (for [e (near-hostiles c) :when (and (not (true? (.-remembered e))) (danger-q/seen-only? e))]
             [(.-id e) [(.-name e) (danger-q/mob-distance (:primitives c) e)]])))

(defn ran!
  "Note that the body starts a leg of the flight now: the time and health (memory :ran-at, :ran-health) hit-on-the-run! counts blows from;
  the flight's first leg's time (:legs-at) beaten? counts them from."
  [c]
  (let [now (ctx/now c)]
    (ctx/update-mem! c #(-> % (assoc :ran-at now :ran-health (.-health (.self (:primitives c)))) (update :legs-at (fnil identity now))))))

(defn ^:async leg-step!
  "One flee step to target as a jobs.movement.run-away child of at most leg-s seconds (y-margin (16) blocks past body and target bound its route): true when the body arrived or got on a leg nearer, false when go-to gave up, waited or was declined (mem :leg-timed-out: it ran out of the leg's seconds unhurt).
  A target with a judged route (:route meta) is walked along that route only (go-to's :route), to within range of the target
  or of the route's end; a route go-to cannot walk (:off-route) puts the target in mem :refused, so the next step picks
  another judged option, never a way go-to plans on its own.
  Emits debug retreat.step-speed {:slot :blocks :seconds :bps :food :chasers [[name id gap]]}, :chasers the hostiles seen as it starts (sighted-gaps; the body faces away from them as it walks)."
  ([c slot target range leg-s] (await (leg-step! c slot target range leg-s (respond.goal/leg-margin))))
  ([c slot target range leg-s y-margin]
  (let [from (u/self-pos c)
        gaps0 (sighted-gaps c)
        t0 (ctx/now c)
        health0 (.-health (.self (:primitives c)))
        route (:route (meta target))
        end (peek route)
        _ (ran! c)
        r (await (ctx/call c slot 'jobs.movement.run-away
                           (cond-> {:pos target :range (if end (max range (js/Math.ceil (u/dist target end))) range) :leg-s leg-s}
                             y-margin (assoc :y-margin y-margin)
                             route (assoc :route route))))
        res (when (contract/ending? r) (:data r))
        _ (when (#{:goal-not-standable :off-route} (result/why r)) (ctx/update-mem! c update :refused (fnil conj #{}) (mapv target [:x :y :z])))
        s (/ (- (ctx/now c) t0) 1000)
        d (u/dist from (u/self-pos c))
        _ (ctx/update-mem! c assoc :leg-timed-out (and (= :limit-reached (:kind r)) (>= (.-health (.self (:primitives c))) health0)))]
    (ctx/emit! c :retreat.step-speed :debug {:slot slot :blocks (/ (js/Math.round (* 10 d)) 10) :seconds (/ (js/Math.round (* 10 s)) 10)
                                             :bps (when (pos? s) (/ (js/Math.round (* 10 (/ d s))) 10))
                                             :food (room-lib/food (:primitives c))
                                             :chasers (vec (for [[id [n g]] gaps0] [n id (/ (js/Math.round (* 10 g)) 10)]))})
    (boolean (or (:arrived res) (:nearer res))))))

(defn step! [c slot target range] (leg-step! c slot target range (flight-timeout-s)))

(defn back-off-reach [] (settings/get settings ::back-off-reach))

(defn back-off-open?
  "Whether a cornered step weighs a back-off from chasers ({:name :pos}) among hostiles: one is left to back off from,
  the back-off was not tried this flight and every close-range one is outrun (outrun?)."
  [c chasers hostiles]
  (boolean (and (seq chasers) (not (flight/tried? c :back-off)) (outrun? c hostiles))))

(defn back-off-of
  "The back-off step of near, the scored ends of a flee search's ring ::back-off-reach off (flee-search/search!'s :near,
  or its ends when that ring is its only one): {:target :caught}, target the best scored end that gains on the chasers
  over standing still, on a route none catches, clear of :hazard places (nil: none, no back-off), caught the chance the
  step is caught (flee-search/caught-chance at the way counts, wworld/way-scan), the one its price reads."
  [c near]
  (let [hazards (keep (comp :pos :data) (ctx/entries c :hazard))
        target (some->> near
                        (remove :caught?)
                        (filter #(pos? (:gain %)))
                        (remove #(search/hazard-near? hazards (:cells %)))
                        seq
                        (apply max-key :score)
                        :end)
        counts (or (some-> (wworld/way-scan (:primitives c)) (select-keys [:k :n])) {:k 0 :n 0})]
    {:target target
     :caught (when target (search/caught-chance near counts {:hazards hazards} (:glanced (ctx/mem c))))}))

(defn ^:async back-off-search!
  "back-off-of a flee search from chasers ({:name (mob-kind) :pos}) whose ring is back-off-reach off, part of the step's
  one search (room-lib/step-meter): for a step whose search planned no near ring."
  [c chasers]
  (let [m (room-lib/step-meter c)
        ends (await (search/search! c chasers [] {:reach (back-off-reach) :meter m}))]
    (room-lib/spent-meter! c m :back-off)
    (ctx/update-mem! c flight/note-search (search/starved? ends))
    (back-off-of c ends)))

(defn ^:async back-off!
  "Step back to target (back-off-of's): :again, nil when the walk is blocked or a chaser hit the body on it (the option is tried then:
  a chaser that keeps up is not outrun, and the next step prices the covers and the fight instead). A step that moved
  leaves memory :backed-off for the next step's search (respond.cover/judge!)."
  [c target]
  (let [health0 (.-health (.self (:primitives c)))
        moved? (await (step! c :back-off target 0))]
    (when moved? (ctx/update-mem! c assoc :backed-off true))
    (if (and moved? (>= (.-health (.self (:primitives c))) health0))
      :again
      (do (flight/tried! c :back-off) (if moved? :again nil)))))

(defn ^:async hold!
  "One hold (a pause of flight/wait-ms, why), kept in memory :held-s: :again. The next step prices the hold anew, so it
  ends when another answer is cheaper."
  [c why]
  (let [s (/ (flight/wait-ms) 1000)]
    (await (child/pause! c s why))
    (ctx/update-mem! c update :held-s (fnil + 0) s)
    :again))

(defn recently-hurt?
  "Whether the body lost health within hurt-window-s."
  [c]
  (pos? (ctx/count-in c :hurt (* 1000 (hurt-window-s)))))

(defn stand-leg-s
  "Seconds one leg to a place to stand may take: close-leg-s once the body was hurt (hurt?), else flight-timeout-s; a search that stands still is hit for its whole leg."
  [hurt?]
  (if hurt? (close-leg-s) (flight-timeout-s)))

(defn ^:async stand!
  "Walk to and stand in a place of kind where (jobs.survival.stand-in: :water, :roof or :dry), a leg a step: :again while it
  walks, and once it stands there a pause of flight/wait-ms (why; none without a why), kept in memory :held-s: :again,
  the next step prices the hold anew. nil when there is none or the walk is blocked (the option is tried! then)."
  [c option where why]
  (let [e (await (ctx/call c option 'jobs.survival.stand-in {:where where :leg-s (stand-leg-s (recently-hurt? c))}))]
    (cond
      (= :continue e) :again
      (not= :done (:kind e)) (do (flight/tried! c option) nil)
      why (await (hold! c why))
      :else :again)))

(defn ^:async blocked!
  "Every option failed: forget them all, warn once a flight, pause a moment (why cornered) and go on: the next step
  tries them again from the start (the danger still stands); no pause while the body is being hit or a creeper is near (it closes in
  and blows while the body stands, so the next step must try the fight or a walk at once). The second sweep in a row ends the flight :cannot-escape; a sweep after a starved flee search (flight/count-sweep) does not count."
  [c why]
  (ctx/update-mem! c #(-> % (dissoc :tried) flight/count-sweep))
  (when-not (:blocked-warned (ctx/mem c))
    (ctx/update-mem! c assoc :blocked-warned true)
    (ctx/emit! c :retreat_blocked :warn {:text (str why "; every escape failed, trying them again")}))
  (when-not (or (recently-hurt? c) (some combat/creeper? (near-hostiles c)))
    (await (child/pause! c (/ (flight/wait-ms) 1000) "cornered")))
  (if (flight/cannot-escape? (ctx/mem c)) (flight/end-flight! c :cannot-escape) :again))
