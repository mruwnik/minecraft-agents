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
            [jobs.lib.block-sets :as bs]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.memory-age :as memory-age]
            [engine.perception.rays :as rays]
            [jobs.lib.seal :as seal]
            [jobs.lib.escape :as escape]
            [jobs.lib.hostile-fight :as hostile]
            [jobs.lib.look :as look]
            [jobs.lib.pillar :as pl]
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
   ::beaten-hits {:default 2 :doc "Hits by a melee mob near (the damage names one within close-radius; not a creeper or an enderman) within hurt-window-s that show the flight is failing: the body fights it first, armed or not. 0: never." :spec (a/int-in 0 nil)}
   ::press-gap {:default 2 :doc "Blocks within which a melee mob (not a creeper or an enderman) makes a cornered armed body fight before it builds a seal or pillar, hit or not: the mob strikes before a block is placed and stands in the cells to fill." :spec (a/num-in 0 nil)}
   ::arrival-s {:default 4 :doc "Seconds within which a melee mob (not a creeper or an enderman) reaches a cornered armed body: it digs no pit or pocket, which take longer than the mob needs while the mob hits the digger. 0: never." :spec (a/num-in 0 nil)}
   ::pillar-mob-fails {:default {:beside {:k 4 :n 4} :clear {:k 4 :n 8}}
                       :spec (a/map-of #{:beside :clear} (a/map-with {:k (a/int-in 0 nil) :n (a/int-in 0 nil)}))
                       :doc "Of n pillars of a flight (built, or failed with a refused place in the body's own cell), k failed so, by the melee hostiles when it began: :beside, one in or beside the body's cell (within 1 cell across and 1 up or down of its feet), :clear none. The trial and probe bodies' logs on 2026-10-28 (a refusal is physics given the class, which staged scenes do not skew). Recount: node tools/event-counts.mjs wall-places."}
   ::pillar-s {:default 1 :doc "Seconds a cornered pillar takes to lift the body out of a melee mob's reach (a jump and a place a block): the blows it costs then are weighed against the fight's." :spec (a/num-in 0 nil)}
   ::hold-cap {:default 60 :doc "Holds (one wait-ms each) a flight makes in water or under a roof against an angry enderman before it tries the next option (seal, pit, pocket, fight); 0: none." :spec (a/int-in 0 nil)}
   ::refuge-gap {:default 10 :doc "Blocks within which a lone melee hostile the body cannot outfight is pillared against (else sealed against) before any flee step, when blocks are carried; 0 never." :spec (a/num-in 0 nil)}
   ::flight-timeout-s {:default 5 :doc "Seconds bounding one step of the flight: the mob moves, so the next step aims again." :spec (a/num-in 0 nil)}
   ::enderman-close {:default 3 :doc "Blocks within which an enderman counts as angry though it has not hurt the body yet (a calm one does not come this near)." :spec (a/num-in 0 nil)}
   ::creeper-keep {:default 5 :doc "Blocks within which a creeper stops the fight walking toward a mob on its side: the fight swings from where the body stands, never a step nearer the creeper." :spec (a/num-in 0 nil)}})
(defn flight-timeout-s [] (settings/get settings ::flight-timeout-s))
(defn enderman-close [] (settings/get settings ::enderman-close))
(defn creeper-keep [] (settings/get settings ::creeper-keep))

(defn fist-sweeps [] (settings/get settings ::fist-sweeps))
(defn hit-reach [] (settings/get settings ::hit-reach))

(defn hits-total
  "The hits an attack ending's (or prior's) :struck counts over all mobs."
  [struck] (reduce + 0 (map :hits (vals struck))))

(defn ^:async fight!
  "Fight back with the best of weapons (the fist when none is carried) whatever
  the health, kept while the hostile stays close: :again, nil when the fight
  cannot reach any hostile, or every target is already dead (the option failed this flight, else the flight spins without time passing; hits new in this call reset the sweep count). reach-only?: only a hostile within hit-reach is
  swung at, none walked to. A fight that ends other than done keeps its memory (its given-up counts) for the next."
  [c weapons reach-only?]
  (let [radius (danger-q/hostile-radius)
        ranged-radius (danger-q/ranged-radius)
        radius (if reach-only? (min radius (hit-reach)) radius)
        ranged-radius (if reach-only? radius ranged-radius)]
    (ctx/update-mem! c assoc :cornered true)
    (ctx/update-mem! c #(cond-> % (:cornered (:children %)) (update-in [:children :cornered] dissoc :start)))
    (let [hits-before (hits-total (:struck (:fight-prior (ctx/mem c))))
          r (await (ctx/call c :cornered 'jobs.combat.attack
                             {:targets :hostiles :radius radius :ranged-radius ranged-radius :use {:tools (mapv name weapons)}
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
  "The hostiles that count: ones the body has seen (jobs.lib.danger/known-hostiles), melee within :radius and ranged
  within :ranged-radius. One behind a wall cannot reach or shoot the body, so
  it is left alone, as is one with no walkable way to the body (jobs.lib.danger) unless it hit the body lately (danger/hit-ids),
  as the hostile-near trigger does."
  [c]
  (danger-q/dangers (:primitives c) (danger-q/hostile-radius) {:ranged-radius (danger-q/ranged-radius)} {:hit-ids (danger-q/hit-ids (ctx/view c))}))

(defn near-hostiles
  "The hostiles a cornered body weighs: within :radius, ranged ones within :ranged-radius, the dead skipped."
  [c]
  (let [radius (danger-q/hostile-radius)
        ranged-radius (danger-q/ranged-radius)]
    (flight/near-known (:primitives c) (set (flight/dead-ids c)) radius ranged-radius)))

(defn angry-enderman?
  "Whether one of hostiles is an enderman that is angry (the client's flag), hurt the body this flight (its health is under the flight's start) or
  stands within enderman-close: a calm one is walked away from like any mob, never sealed against. Once found angry it stays so for the flight
  while one is known within follow range (the client's flag does not hold, and the enderman may teleport out of the near radius: the body must not leave the water it stepped into)."
  [c hostiles]
  (let [p (:primitives c)
        hurt? (boolean (some-> (:flight-start (ctx/mem c)) (->> (body/mob-hit-since? (ctx/view c)))))
        angry? (boolean (or (and (:angry-enderman (ctx/mem c)) (some combat/enderman? (concat hostiles (flight/known-chasers p))))
                            (some #(and (combat/enderman? %) (or hurt? (true? (.-angry %)) (<= (danger-q/mob-distance p %) (enderman-close)))) hostiles)))]
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
  "Whether a melee chaser hit the body since the previous flight step: health lost with a hitter near, or a hurt entry
  naming a melee hostile within close-radius (a fed body heals the hit within a second, so health alone misses it). A chaser
  that hits it cannot be outrun. Noted in job memory (:hit-on-the-run), with the health, for the cornered options' odds."
  [c]
  (let [p (:primitives c)
        health (.-health (.self p))
        {:keys [step-health step-at]} (ctx/mem c)
        now (ctx/now c)
        near (near-hostiles c)
        names (->> near
                   (filter #(and (not (combat/ranged? %)) (not (combat/creeper? %)) (not (combat/enderman? %))
                                 (<= (danger-q/mob-distance p %) (close-radius))))
                   (map #(.-name %))
                   set)
        named? (and step-at (some #(some names (hurt-by %)) (ctx/since c :hurt step-at)))
        entries (when step-at (ctx/since c :hurt step-at))
        mob-or-unlogged? (or (empty? entries) (some #(hurt/mob-hit? (:data %)) entries))
        hit? (boolean (or (and step-health (< health step-health) (seq (hitters c near)) mob-or-unlogged?) named?))]
    (ctx/update-mem! c assoc :step-health health :step-at now :hit-on-the-run hit?)
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

(defn pillar-cover
  "The cover {:open-s :seconds :fails} of a pillar against hostiles (cost/cover-answer): in a melee mob's reach for ::pillar-s,
  then the hide-out's quiet on top, all of it in a shooter's line of fire; it fails for a mob in its cell by
  ::pillar-mob-fails of the class the body stands in (wall-in/mob-fails, beside? within 1 cell), with the refusals this
  response saw added (flight/with-seen-fails);
  nil when no pillar fits here, a climber is near or a shot was felt within ::hurt-window-s (the top stays in that line of fire)."
  [c hostiles]
  (when (and (pillar/pillar-item c) (not-any? combat/climber? hostiles)
             (not-any? #(pos? (shots-in %)) (ctx/since c :hurt (- (ctx/now c) (* 1000 (hurt-window-s)))))
             (pillar/pillar-fits? (:primitives c) (pl/feet-cell c)))
    (let [s (settings/get settings ::pillar-s)
          seconds (+ s (hide-out/quiet-s))]
      {:open-s (if (some combat/ranged? hostiles) seconds s) :seconds seconds
       :fails (wall-in/mob-fails (flight/with-seen-fails c :pillar (settings/get settings ::pillar-mob-fails)) (wall-in/beside? c 1))})))

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
  its cornered step."
  [c ways]
  (let [in (covers-in c)
        now (ctx/now c)
        kept (::covers (ctx/mem c))
        kept (if (and (= in (:in kept)) (<= 0 (- now (:t kept)) (check-cache/ttl-ms))) kept {:in in :t now :asked #{} :v {}})
        missing (remove (:asked kept) ways)
        kept (cond-> kept (seq missing) (-> (update :v merge (shut-in/covers c missing)) (update :asked into missing)))]
    (when (seq missing) (ctx/update-mem! c assoc ::covers kept))
    (select-keys (:v kept) ways)))

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
        mobs (mapv #(hostile/mob-near (:primitives c) %) hostiles)
        body (hostile/body-of c)
        at (u/self-pos c)
        answer #(cost/cover-answer {:equipment equipment :mobs mobs} %)
        death (delay (cost/death-price body (hostile/again-answers c hostiles) at))
        price #(cost/answer-price* body (answer %) at @death)
        cheaper (fn [ways] (some->> (keep specs ways) seq (apply min-key price)))
        pillar (when (open? :pillar) (if given (:pillar given) (pillar-cover c hostiles)))]
    (cond-> {}
      (cheaper (:plug cover-options)) (assoc :plug (cheaper (:plug cover-options)))
      (cheaper (:plug-dig cover-options)) (assoc :plug-dig (cheaper (:plug-dig cover-options)))
      pillar (assoc :pillar pillar)))))

(defn cornered-answers
  "{option answer} of the cornered options open to the body against hostiles: :fight, the covers (cornered-covers) and,
  unless the back-off was tried, :back-off the walk away (hostile/cornered-answers); home?: the body is in its home room."
  [c hostiles home?]
  (let [a (hostile/cornered-answers c hostiles (cornered-covers c hostiles home?) (not (flight/tried? c :back-off)))]
    (cond-> (dissoc a :flee) (contains? a :flee) (assoc :back-off (:flee a)))))

(defn fight-wins?
  "Whether fighting hostiles is the cheapest of the cornered answers (cornered-answers, jobs.lib.hostile-fight/wins?)."
  ([c hostiles] (fight-wins? c hostiles false))
  ([c hostiles home?]
   (hostile/wins? c hostiles (cornered-answers c hostiles home?))))

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

(defn refuge-gap [] (settings/get settings ::refuge-gap))

(defn press-gap [] (settings/get settings ::press-gap))

(defn close-leg-s [] (settings/get settings ::close-leg-s))


(defn sighted-gaps
  "{id [name blocks]} of the near hostiles the body sees now (not remembered): the gaps a step's closing rate is read from."
  [c]
  (into {} (for [e (near-hostiles c) :when (and (not (true? (.-remembered e))) (danger-q/seen-only? e))]
             [(.-id e) [(.-name e) (danger-q/mob-distance (:primitives c) e)]])))

(defn ^:async leg-step!
  "One flee step to target as a jobs.movement.run-away child of at most leg-s seconds (y-margin (16) blocks past body and target bound its route): true when the body arrived or got on a leg nearer, false when go-to gave up, waited or was declined (mem :leg-timed-out: it ran out of the leg's seconds unhurt).
  Emits debug retreat.step-speed {:slot :blocks :seconds :bps :food :chasers [[name id gap]]}, :chasers the hostiles seen as it starts (sighted-gaps; the body faces away from them as it walks)."
  ([c slot target range leg-s] (await (leg-step! c slot target range leg-s (respond.goal/leg-margin))))
  ([c slot target range leg-s y-margin]
  (let [from (u/self-pos c)
        gaps0 (sighted-gaps c)
        t0 (ctx/now c)
        health0 (.-health (.self (:primitives c)))
        r (await (ctx/call c slot 'jobs.movement.run-away
                           (cond-> {:pos target :range range :leg-s leg-s} y-margin (assoc :y-margin y-margin))))
        res (when (contract/ending? r) (:data r))
        _ (when (= :goal-not-standable (result/why r)) (ctx/update-mem! c update :refused (fnil conj #{}) (mapv target [:x :y :z])))
        s (/ (- (ctx/now c) t0) 1000)
        d (u/dist from (u/self-pos c))
        _ (ctx/update-mem! c assoc :leg-timed-out (and (= :limit-reached (:kind r)) (>= (.-health (.self (:primitives c))) health0)))]
    (ctx/emit! c :retreat.step-speed :debug {:slot slot :blocks (/ (js/Math.round (* 10 d)) 10) :seconds (/ (js/Math.round (* 10 s)) 10)
                                             :bps (when (pos? s) (/ (js/Math.round (* 10 (/ d s))) 10))
                                             :food (room-lib/food (:primitives c))
                                             :chasers (vec (for [[id [n g]] gaps0] [n id (/ (js/Math.round (* 10 g)) 10)]))})
    (boolean (or (:arrived res) (:nearer res))))))

(defn step! [c slot target range] (leg-step! c slot target range (flight-timeout-s)))

(defn ^:async back-off!
  "Step back from chasers ({:name (mob-kind) :pos}): to the best scored end of a flee search (flee-search/search!)
  ::back-off-reach off that gains on them over standing still, on a route none catches, clear of :hazard places:
  :again, nil when there is none or the walk is blocked."
  [c chasers]
  (let [hazards (keep (comp :pos :data) (ctx/entries c :hazard))
        ends (await (search/search! c chasers [] {:reach (settings/get settings ::back-off-reach)}))
        target (some->> ends
                        (remove :caught?)
                        (filter #(pos? (:gain %)))
                        (remove #(search/hazard-near? hazards (:cells %)))
                        seq
                        (apply max-key :score)
                        :end)]
    (when target
      (if-not (await (step! c :back-off target 0))
        (do (flight/tried! c :back-off) nil)
        :again))))

(defn ^:async hold!
  "One hold (a pause of flight/wait-ms) in a spot an enderman cannot use: :again, or nil once the flight has held
  hold-cap times (the option is then tried! and the next one of the order comes)."
  [c option why]
  (if (>= (:holds (ctx/mem c) 0) (settings/get settings ::hold-cap))
    (do (flight/tried! c option) nil)
    (do (ctx/update-mem! c update :holds (fnil inc 0))
        (await (child/pause! c (/ (flight/wait-ms) 1000) why))
        :again)))

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
  walks, and once it stands there a short hold (hold!, why; no hold without a why). nil when there is none or the walk is
  blocked (the option is tried! then)."
  [c option where why]
  (let [e (await (ctx/call c option 'jobs.survival.stand-in {:where where :leg-s (stand-leg-s (recently-hurt? c))}))]
    (cond
      (= :continue e) :again
      (not= :done (:kind e)) (do (flight/tried! c option) nil)
      why (await (hold! c option why))
      :else :again)))

(defn ^:async blocked!
  "Every option failed: forget them all, warn once a flight, pause a moment (why cornered) and go on: the next step
  tries them again from the start (the danger still stands); no pause while the body is being hit or a creeper is near (it closes in
  and blows while the body stands, so the next step must try the fight or a walk at once). The second sweep in a row ends the flight :cannot-escape."
  [c why]
  (ctx/update-mem! c #(-> % (dissoc :tried) flight/count-sweep))
  (when-not (:blocked-warned (ctx/mem c))
    (ctx/update-mem! c assoc :blocked-warned true)
    (ctx/emit! c :retreat_blocked :warn {:text (str why "; every escape failed, trying them again")}))
  (when-not (or (recently-hurt? c) (some combat/creeper? (near-hostiles c)))
    (await (child/pause! c (/ (flight/wait-ms) 1000) "cornered")))
  (if (flight/cannot-escape? (ctx/mem c)) (flight/end-flight! c :cannot-escape) :again))
