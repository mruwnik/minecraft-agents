(ns jobs.survival.respond-to-hostile
  (:require [engine.hurt :as hurt]
            [jobs.lib.ready :as ready]
            [jobs.survival.block-arrow-gap :as gap]
            [jobs.survival.respond-flight :as flight]
            [jobs.survival.respond.cover :as cover]
            [engine.args :as a]
            [jobs.lib.checks :as checks]
            [jobs.lib.body :as body]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.hostile-fight :as hostile]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.pace :as pace]
            [jobs.lib.shot :as shot]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.goal :as goal]
            [jobs.survival.respond.doors :as doors]
            [jobs.survival.shut-in :as shut-in]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.threats :as threats]
            [jobs.lib.look :as look]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.lib.walk-pick :as pick]
            [jobs.lib.walk.way-log :as way-log]))

(def doc
  "A hostile is near: fight it (jobs.combat.attack) when the odds are fair, else flee; one whole attempt per round, deciding
  afresh before each call of the child. The flight (jobs.survival.respond-flight) is this job's own step loop: it makes every
  choice (room, door, walk, cover, shut-in, pillar, back-off, fist) and each way is its own call (the fight choice is jobs.lib.cost, see the README row).
  Needs: a real danger (as the hostile-near trigger, jobs.lib.danger, in sight) within the danger ::hostile-radius setting (::ranged-radius for ranged mobs);
  none near is done at once. The call's :use :tools are the weapons, handed to both children.
  Returns: :done when no danger is near and the body holds no refuge, or when every danger is only heard and the fight declined
  (they are muted, the ending names the mob), or when a fight held for a seen mob that neither came nor hurt the body (it is watched: muted while it comes no nearer than danger/answered-within and the body is unhurt); :nothing-possible :retreat-failed (the flight's ending is the :because) when the
  flight ends so; :nothing-possible :no-response after ::max-unchanged calls in a row that change nothing; :limit-reached on its
  time budget. Never :continue. Writes one :hostile memory entry per encounter. A danger reflex: never backed off.")

(a/defargs settings
  {::stay-inside {:default true :doc "A body shut in a closed room or its own shelter fights from there and never steps out toward a mob; under any roof it does not flee for mobs it only hears (false: it may)." :spec boolean?}
   ::job-time-s {:default 480 :doc "Seconds a response to a hostile may take (a hide runs long)." :spec (a/num-in 0 nil)}
   ::hold-radius {:default 16 :doc "Hostiles known within this many blocks count for holding position in the dark." :spec (a/num-in 0 nil)}
   ::hit-window-s {:default 5 :doc "How recent a hurt keeps a fight from leaving heard or watched mobs, and makes a round look once more; the flight's hits within it are counted (:hits)." :spec (a/num-in 0 nil)}
   ::max-unchanged {:default 3 :doc "Calls of a child in a row that leave the body and the dangers as they were before the reflex stops :no-response." :spec (a/int-in 1 nil)}
   ::struck-s {:default 30 :doc "Seconds the hits the fight landed outlive the round (:struck-hits), so a refired reflex still counts them." :spec (a/num-in 0 nil)}
   ::struck-cap {:default 8 :doc "How many :struck-hits entries memory keeps." :spec (a/int-in 1 nil)}
   ::quote-mean-s {:default 20 :doc "Seconds an encounter is guessed to take, for the quote." :spec (a/num-in 0 nil)}})

(defn stay-inside? [] (settings/get settings ::stay-inside))
(defn hold-radius [] (settings/get settings ::hold-radius))
(defn hit-window-ms [] (* 1000 (settings/get settings ::hit-window-s)))
(defn max-unchanged [] (settings/get settings ::max-unchanged))
(defn struck-policy [] {:cap (settings/get settings ::struck-cap) :ttl (* 1000 (settings/get settings ::struck-s))})

(def target [])

(def defaults {:checks {:safe? '(always) :fit? '(always)} :spend {:job-time ::job-time-s} :use {:tools :attack}})

(def meta {:category :reflex})

(def safe? ready/ok)

(def fit? ready/ok)

(def hostile-policy {:cap 50 :ttl (* 60 60 1000)})

(defn near
  "The hostiles that count: ones the body has seen (jobs.lib.danger/known-hostiles), melee within :radius and ranged
  within :ranged-radius. One behind a wall cannot reach or shoot the body, so
  it is left alone, as is one with no walkable way to the body (jobs.lib.danger) unless it hit the body lately (danger/hit-ids),
  as the hostile-near trigger does."
  [c]
  (danger-q/dangers (:primitives c) (danger-q/hostile-radius) {:ranged-radius (danger-q/ranged-radius)} {:hit-ids (danger-q/hit-ids (ctx/view c))}))


(def other {:fight :flee :flee :fight})

(defn hold?
  "Whether the fight stays where it is: in the dark, with more than one hostile known (the ones in view are not all there are),
  or with any when the body is shut in a closed room or its own shelter and ::stay-inside is on."
  [c]
  (let [p (:primitives c)]
    (boolean (and (look/dark-here? p)
                  (or (> (count (danger-q/known-hostiles p (hold-radius) {})) 1)
                      (and (stay-inside?) (or (sh-sense/shut-in-room? p) (sh-inside/sheltered-in c))))))))

(defn hurt-lately?
  "Whether a mob may have hurt the body within hit-window-ms (an unseen one may be hitting it); hunger, fire or a fall do not count."
  [c]
  (body/mob-hit-since? (ctx/view c) (- (ctx/now c) (hit-window-ms))))

(defn stay-under-roof?
  "Whether the body keeps its roof: ::stay-inside on, a roof over the feet, and every danger near either only heard
  or a flyer (a phantom cannot get under a roof), none a creeper or an enderman. The fight then holds and the flight does not run."
  [c hs]
  (let [p (:primitives c)]
    (boolean (and (stay-inside?)
                  (seq hs)
                  (sh-sense/roofed? p (sh-sense/roof-height))
                  (every? #(or (combat/flyer? %) (not (danger-q/seen-only? %))) hs)
                  (not-any? #(or (combat/creeper? %) (combat/enderman? %)) hs)))))

(defn ^:async run-child
  "One call of the child for decision: :continue, :done, :declined (a fight that ended otherwise, its memory put back: the
  flight may run), or the ending of a flight that ended otherwise. The flight is this job's own steps (respond-flight/flee!)."
  [c decision hold]
  (let [_ (when (and (= :fight decision) (not= :fight (:last-run (ctx/mem c))))
            (ctx/update-mem! c #(cond-> % (:fight (:children %)) (update-in [:children :fight] dissoc :start))))
        _ (ctx/update-mem! c assoc :last-run decision)
        e (if (= :fight decision)
            (await (ctx/call c :fight 'jobs.combat.attack
                             {:targets :hostiles :radius (danger-q/hostile-radius) :ranged-radius (danger-q/ranged-radius)
                              :use {:tools (combat/weapons c)} :hold (or hold (hold? c)) :prior (:fight-prior (ctx/mem c))
                              :checks {:safe? '(always) :fit? '(always)}}))
            (await (flight/flee! c)))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) (do (when (= :flee decision) (ctx/update-mem! c assoc :flight-end (:data e))) :done)
      (= :fight decision) (do (ctx/update-mem! c assoc :declined (:cause e))
                              (when (:struck (:data e)) (ctx/update-mem! c assoc :fight-prior (select-keys (:data e) [:killed :given-up :names :struck])))
                              :declined)
      :else e)))

(defn changed-text
  "Pure: what changed since the earlier encounter entry prev with the same mob when the decision differs (else nil)."
  [prev {:keys [mob decision health]}]
  (when (and prev (= mob (:mob prev)) (not= decision (:decision prev)))
    (str "; earlier " (name (:decision prev)) " at " (:health prev) " hp, now " health " hp")))

(defn encounter-text [{:keys [mob decision pos direction band] :as entry} prev]
  (str (name decision) ": " mob
       (if pos
         (str " seen at " (pr-str (mapv #(js/Math.floor (% pos)) [:x :y :z])))
         (str " heard " (name band) ", " (name direction)))
       (changed-text prev entry)))

(defn log-encounter!
  "One :hostile entry and event for each change of decision in a run (a fight turned flight is said, with the health;
  the event has the answers' prices, seconds, and the cover way a flight begins with; way-log/log-priced! the way counts
  scan the flight was priced with, when it was). The entry's :run is the run's first decision time (hostile-fight/returns)."
  [c threat decision prices cover scan]
  (when-not (= decision (:logged (ctx/mem c)))
    (let [prev (:data (mem/latest (ctx/view c) :hostile))
          run (or (:run-at (ctx/mem c)) (ctx/now c))
          entry (assoc (threats/place-of (:primitives c) threat) :mob (.-name threat) :decision decision
                       :health (js/Math.round (.-health (.self (:primitives c)))) :run run)]
      (ctx/remember! c :hostile entry hostile-policy)
      (ctx/emit! c :hostile.decided :info (cond-> (assoc (dissoc entry :from :run) :text (encounter-text entry prev))
                                            prices (assoc :prices (update-vals prices js/Math.round))
                                            cover (assoc :cover cover)))
      (way-log/log-priced! c scan)
      (ctx/update-mem! c assoc :logged decision :run-at run))))

(defn fight-struck
  "What the fight counted per mob: the running child's (hostile/struck), else the last declined ending's."
  [c]
  (let [running (hostile/struck (get-in (ctx/mem c) [:children :fight]))]
    (if (seq running) running (get-in (ctx/mem c) [:fight-prior :struck]))))

(defn struck-of
  "The :struck entry of mob id: the fight child's own, else the one an earlier round left in memory (:struck-hits)."
  [c id]
  (or (get (fight-struck c) id)
      (get-in (mem/latest (ctx/view c) :struck-hits) [:data id])))

(defn keep-struck!
  "Writes the last fight ending's :struck map to memory, so a refire after a lost round keeps the hits."
  [c]
  (when-let [struck (seq (fight-struck c))]
    (ctx/remember! c :struck-hits (into {} struck) (struck-policy))))

(defn note-hits
  "Pure: the times (ms) of hits taken in the last window-ms (only a flight counts them), given the earlier ones, now and the health before and now."
  [hits now before after window-ms]
  (let [hits (filterv #(< (- now %) window-ms) hits)]
    (cond-> hits (< after before) (conj now))))

(defn mute-heard!
  "Mutes heard mobs (jobs.lib.body/heard-muted?) when every danger in hs is heard only, else lifts the mutes: a mob
  the body sees counts again."
  [c hs]
  (cond
    (empty? hs) nil
    (every? (complement danger-q/seen-only?) hs)
    (ctx/remember! c :heard-mute {:until (+ (ctx/now c) (body/next-mute-ms (ctx/view c)))} body/mute-policy)
    :else (ctx/forget-where! c :heard-mute (constantly true))))

(defn leave-heard?
  "Whether a fight that found no target leaves the dangers hs alone instead of fleeing: every one only heard (none
  seen, so none to walk to or hit), none a creeper, and the body not hurt within hit-window-ms (an unseen mob may be
  the one hitting it)."
  [c hs]
  (and (not-any? danger-q/seen-only? hs)
       (not-any? combat/creeper? hs)
       (empty? (:hits (ctx/mem c)))
       (not (hurt-lately? c))))

(defn look-unheard?
  "Whether the dangers hs are all heard only, the body not hurt lately and not yet looked around this run: it turns
  toward the sound before it leaves the mob alone, as a player would."
  [c hs]
  (boolean (and (seq hs)
                (not-any? danger-q/seen-only? hs)
                (not (hurt-lately? c))
                (not (look/surveyed? c)))))

(defn leave-heard!
  "Mutes heard mobs (mute-heard!) and returns {:left-heard ending-data} saying which one was left, so the job the reflex cut goes on."
  [c hs]
  (let [mute-s (/ (body/next-mute-ms (ctx/view c)) 1000)
        entry (threats/place-of (:primitives c) (first hs))
        mob (.-name (first hs))]
    (mute-heard! c hs)
    {:left-heard {:mob mob :mute-s mute-s
                  :text (str "the " mob " heard " (name (:band entry)) ", " (name (:direction entry))
                             " is not in sight and the body can afford the fight: no flight; heard mobs are left alone for "
                             mute-s " s, one that comes into sight is fought")}}))

(defn watchable?
  "Pure: whether the dangers hs may be watched instead of answered: every one seen, none a creeper, shooter or fusing mob."
  [hs]
  (and (every? danger-q/seen-only? hs)
       (not-any? #(or (combat/creeper? %) (combat/ranged? %) (true? (.-fusing %))) hs)))

(defn leave-watched?
  "Whether a fight that held for a mob to come and gave up (:hold-timeout) leaves the dangers hs watched instead of fleeing:
  watchable?, the fight landed no hit and the body was not hurt within hit-window-ms."
  [c hs]
  (and (= :hold-timeout (:declined (ctx/mem c)))
       (watchable? hs)
       (empty? (:hits (ctx/mem c)))
       (empty? (ctx/since c :hurt (- (ctx/now c) (hit-window-ms))))))

(defn leave-watched!
  "Mutes the mobs hs as watched (jobs.lib.body/watched) and returns {:left-watched ending-data}, so the job the reflex cut goes on."
  [c hs]
  (let [mute-s (/ (body/next-mute-ms (ctx/view c) :watch-mute) 1000)
        mob (.-name (first hs))]
    (ctx/remember! c :watch-mute {:mobs (into {} (map (fn [h] [(.-id h) (danger-q/mob-distance (:primitives c) h)])) hs) :until (+ (ctx/now c) (* 1000 mute-s))} body/mute-policy)
    {:left-watched {:mob mob :mute-s mute-s
                    :text (str "the " mob " neither came nor hurt the body in the hold: no flight; it is watched for " mute-s
                               " s and answered again if it comes nearer or the body is hurt")}}))

(def cover-ways
  "The covers respond prices, each a way of jobs.survival.shut-in where the body stands (and :door, below)."
  [:walls :pit :pocket])

(defn cover-specs
  "{way cover} of shutting the body in against near by each way shut-in/covers finds possible here (the seconds a side
  stays open, the quote, the hide-out's quiet). The open doors of the closed (or not yet all seen) room the body stands in, when the flight would shut one (sealing, respond.doors/sealing-doors), are the cover :door (shut-in/door-cover), done by the flight's own door step.
  None with a creeper near (cost/cover-answer: its blast takes the walls), so its quotes are not asked for: they take
  seconds the fuse does not leave."
  [c near sealing]
  (when-not (some combat/creeper? near)
    (cond-> (shut-in/covers c cover-ways)
      sealing (assoc :door (shut-in/door-cover c (:n sealing) (map #(.-name %) near) (:unseen? sealing))))))

(defn cover-answers
  "{way answer} of the covers specs (cover-specs) against mobs [hostile/mob-near's] wearing equipment (cost/cover-answer:
  the blows while a side is open; then: what follows it, or the same cover once more)."
  [equipment mobs then specs]
  (into {} (keep (fn [[w cover]] (some->> (cost/cover-answer {:equipment equipment :mobs mobs :then then} cover) (vector w)))) specs))

(defn look-answer
  "{:answer :room :value :cost}: looking into the room first (respond.doors/room-look) when a cell of it is unseen: the
  room is closed by cost/closed-chance of its seen and unseen boundary cells, then the door shut as in a seen room, else
  the answers now without the door (cost/look-answer, cost/look-value); now-of: the same of mobs [hostile/mob-near's]
  come nearer during the look. nil when there is nothing to look at."
  [c near now now-of sealing then]
  (when-let [{:keys [seen unseen headings] :as room} (doors/room-look c sealing)]
    (let [p (:primitives c)
          body (hostile/body-of c)
          equipment (cost/equipment-of (.-equipment (.self p)))
          mobs (mapv #(hostile/mob-near p %) near)
          door-cover (shut-in/door-cover c (:n sealing) (map #(.-name %) near) false)
          closed (cost/closed-chance seen unseen)
          answer (cost/look-answer body {:equipment equipment :mobs mobs :looks (* 2 (count headings))
                                         :outcomes [[closed #(assoc (now-of %) :door (cost/cover-answer {:equipment equipment :mobs % :then then} door-cover))]
                                                    [(- 1 closed) #(dissoc (now-of %) :door)]]}
                                   (u/self-pos c))]
      (when answer
        (assoc (cost/look-value body now answer (u/self-pos c)) :answer answer :room room :closed closed)))))

(defn walk-felt
  "hp a second of the mob blows felt since the flight began (hostile/incoming), nil while not fleeing: a walk is priced at
  the blows it fails to leave, not at the fight's before it (they would flip the choice back)."
  [c]
  (let [m (ctx/mem c)]
    (when-let [start (and (= :flee (:decision m)) (:flight-start m))]
      (:rate (hostile/incoming c start)))))

(defn ^:async respond-with!
  "The rest of respond once it has weighed the answers: log the decision, run its child, the other one when it declines."
  [c near weighed decision cover creeper stay?]
  (log-encounter! c (if (and (= :flee decision) creeper) creeper (first near)) decision (:prices weighed) cover (:way-scan weighed))
  (when (and stay? (not (:stay-noted (ctx/mem c))))
    (ctx/update-mem! c assoc :stay-noted true)
    (ctx/emit! c :hostile.sealed-hold :info {:text "under a roof: the mobs near are heard only or flyers, so the body stays in" :mobs (mapv #(.-name %) near)}))
  (ctx/update-mem! c #(cond-> (assoc % :decision decision) cover (assoc :cover-way cover) (not cover) (dissoc :cover-way)))
  (let [result (await (run-child c decision stay?))]
    (keep-struck! c)
    (cond
      (not= :declined result) result
      (and (= :fight decision) (leave-heard? c near)) (leave-heard! c near)
      stay? result
      (and (= :fight decision) (leave-watched? c near)) (leave-watched! c near)
      :else (await (run-child c (other decision) false)))))

(defn ^:async respond [c near]
  (let [p (:primitives c)
        self (.self p)
        m (ctx/mem c)
        before (:last-health m (.-health self))
        entries (when-let [t (:last-at m)] (ctx/since c :hurt t))
        after (if (or (empty? entries) (some #(hurt/mob-hit? (:data %)) entries)) (.-health self) before)
        hits (note-hits (when (= :flee (:decision m)) (:hits m)) (ctx/now c) before after (hit-window-ms))
        _ (ctx/update-mem! c assoc :hits hits :last-health (.-health self) :last-at (ctx/now c))
        felt (walk-felt c)
        target (when-not (some combat/enderman? near) (first (hostile/fought near)))
        fight (when-not (some combat/enderman? near) (hostile/fight-answer c near #(struck-of c %) (when target (await (pick/quote! c (hostile/fight-walk-args p target))))))
        caught (when-not (some combat/enderman? near) (await (cover/caught-chance! c)))
        walk (when-not (some combat/enderman? near) (hostile/flee-answer c near fight felt caught))
        then (or (:then walk) (when fight [fight]))
        sealing (when-not (some #(or (combat/creeper? %) (combat/enderman? %)) near) (doors/sealing-doors c near))
        specs (when-not (some combat/enderman? near) (cover-specs c near sealing))
        equipment (cost/equipment-of (.-equipment self))
        covers (cover-answers equipment (mapv #(hostile/mob-near p %) near) then specs)
        now (merge covers {:fight fight :flee walk})
        now-of (fn [mobs] (merge (cover-answers equipment mobs then specs) {:fight fight :flee (when walk (hostile/flee-answer c near fight felt caught mobs))}))
        look (when (seq covers) (look-answer c near now now-of sealing then))
        weighed (some-> (hostile/weigh c (cond-> now look (assoc :look (:answer look)))) (assoc :way-scan (:way-scan caught)))
        stay? (and (not (hurt-lately? c)) (stay-under-roof? c near))
        decision (if (or stay? (= :fight (:answer weighed))) :fight :flee)
        cover (when (and (not stay?) (contains? covers (:answer weighed))) (:answer weighed))
        creeper (first (get-in weighed [:answers :fight :creepers]))]
    (if (and (= :look (:answer weighed)) (not stay?))
      (do (ctx/emit! c :hostile.look-first :info {:prices (update-vals (:prices weighed) js/Math.round) :value (:value look) :cost (:cost look)
                                                 :closed (:closed look) :sides (:sides (:room look))
                                                 :text (str "a side of the room is unseen: a look first (closed by chance " (.toFixed (:closed look) 2)
                                                            ", worth " (js/Math.round (:value look)) " s, costs " (.toFixed (:cost look) 1) " s)")})
          (await (doors/look-into-room! c (:room look))))
      (respond-with! c near weighed decision cover creeper stay?))))

(def cover-sides
  "Of the four sides of the body's cell, how many must be walled (feet and head cell solid) for it to hold cover: a
  doorway leaves one open."
  3)

(defn covered?
  "Whether the body is roofed and walled on cover-sides sides, as a cell with a doorway is (a canopy or overhang is not)."
  [p]
  (let [{:keys [x y z]} (sh-sense/feet p)
        walled? (fn [[dx dz]] (every? #(sh-sense/solid-at? p {:x (+ x dx) :y (+ y %) :z (+ z dz)}) [0 1]))]
    (and (sh-sense/roofed? p (sh-sense/roof-height))
         (<= cover-sides (count (filter walled? [[1 0] [-1 0] [0 1] [0 -1]]))))))

(defn gap-possible?
  "Whether the gap job could stop the arrows but for cover: only ranged mobs near, the gap job able to start without
  fetching, a tool that digs stone (the way out of a sealed doorway) carried, and the gap job not yet tried (tried?)."
  [c hs tried?]
  (boolean (and (not tried?)
                (every? combat/ranged? hs)
                (= :ok (first (gap/can-start? (assoc c :args {:ways #{:carried}}))))
                (tools/can-harvest? (:primitives c) "stone"))))

(defn gap-wanted?
  "Whether to stop arrows with a block rather than fight or flee: gap-possible? and the body in cover (covered?)."
  [c hs tried?]
  (boolean (and (gap-possible? c hs tried?) (covered? (:primitives c)))))

(defn cover-unseen?
  "Whether the gap job is possible and the body has not looked around yet while the cover it holds may be walls it has not seen
  (its view cone leaves the sides and back): it looks before it flees."
  [c hs tried?]
  (boolean (and (gap-possible? c hs tried?) (not (look/surveyed? c)) (not (covered? (:primitives c))))))

(defn ^:async gap!
  "One call of block-arrow-gap (never fetching): :continue or its ending's kind; nothing placed lets the round decide as usual.
  Tried once it has ended; a :continue (its look around) calls it again."
  [c]
  (let [e (await (ctx/call c :gap 'jobs.survival.block-arrow-gap {:ways #{:carried}}))]
    (when-not (= :continue e) (ctx/update-mem! c assoc :gap-tried true))
    (if (= :continue e) :continue (:kind e))))

(defn hiding?
  "Whether the body holds a refuge (sealed in, a pillar or a pit): a hostile it hides from is out of sight
  and has no way to the body, yet is still there."
  [c]
  (some? (:refuge (ctx/mem c))))


(defn done?
  "No real danger near, no refuge held and no flight on (a chaser or a fled mob its end still has to write)."
  [c]
  (let [n (count (near c))
        {:keys [fled chasers]} (ctx/mem c)
        flying? (or (seq fled) (seq chasers) (hiding? c))
        clear? (and (zero? n) (not flying?) (not (shot/open? (:primitives c) (ctx/view c))))]
    (checks/goal clear? [:clear "no danger near"] [(if flying? :fleeing :danger) (if flying? "a flight is on" (str n " dangers near"))]
                 {:units :hostiles :done (count fled) :left (+ n (count chasers))})))

(defn quote-of
  "Find-out: an encounter lasts as long as the danger stays."
  [_c]
  {:uses {:job-time [0 (settings/get settings ::quote-mean-s) ##Inf]}})

(defn signature
  "What a call may change: the body's cell, its health (a fight that costs health is a response), the ids of the
  dangers near and what the fight child counted (hits landed, swings without damage, blocked walks, kills)."
  [c hs]
  {:health (.-health (.self (:primitives c)))
   :cell (let [{:keys [x y z]} (u/self-pos c)] [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)])
   :ids (set (map #(.-id %) hs))
   :fight (select-keys (get-in (ctx/mem c) [:children :fight]) [:hits :quiet :fails :killed])
   :prior (select-keys (:fight-prior (ctx/mem c)) [:struck :given-up :killed])})

(defn fight-unsettled?
  "Whether a round that now sees no danger may be a blink: the fight child yielded :continue or the body was hurt within
  hit-window-ms (a mob knocked a cell off is still there). The round then looks again, up to ::max-unchanged times, before it ends."
  [c outcome]
  (boolean (or (= :continue outcome)
               (seq (ctx/since c :hurt (- (ctx/now c) (hit-window-ms)))))))

(defn ^:async round
  "One whole attempt: respond (or let the retreat hide on) while a danger is near; :nothing-possible when the retreat
  ends other than :done, or :no-response after max-unchanged calls in a row that changed nothing; never for the time a
  danger stays."
  [c]
  (when-not (contains? (ctx/mem c) :goal)
    (ctx/update-mem! c assoc :goal (goal/interrupted-goal (ctx/view c) (:root c))))
  (loop [unchanged 0 looks 0]
    (way-log/resolve! c)
    (let [hs (near c)
          before (signature c hs)
          outcome (cond
                    (hiding? c) (await (run-child c :flee false))
                    (empty? hs) (when (shot/open? (:primitives c) (ctx/view c)) (await (run-child c :flee false)))
                    (gap-wanted? c hs (:gap-tried (ctx/mem c))) (await (gap! c))
                    (cover-unseen? c hs (:gap-tried (ctx/mem c))) (await (look/look-around! c))
                    (look-unheard? c hs) (await (look/look-around! c))
                    :else (await (respond c hs)))
          after (near c)]
      (if (contract/ending? outcome)
        (do (mute-heard! c hs)
            (ctx/end c (:kind outcome) (if (= :nothing-possible (:kind outcome)) :retreat-failed (:cause outcome))
                     (cond-> {:flight-ended (:cause outcome)} (:text (:data outcome)) (assoc :text (:text (:data outcome))))))
        (cond
          (:left-heard outcome) (ctx/end c :done nil (:left-heard outcome))
          (:left-watched outcome) (ctx/end c :done nil (:left-watched outcome))
          (and (empty? after) (not (hiding? c)) (not (and (< looks (max-unchanged)) (fight-unsettled? c outcome)))) (ctx/end c :done nil (:flight-end (ctx/mem c)))
          (and (empty? after) (not (hiding? c))) (do (await (pace/pace! c)) (recur unchanged (inc looks)))
          :else
          (let [n (if (= before (signature c after)) (inc unchanged) 0)]
            (if (>= n (max-unchanged))
              (do (mute-heard! c hs)
                  (ctx/end c :nothing-possible :no-response {:text (str "a hostile is still near and " n " tries changed nothing")}))
              (do (await (pace/pace! c)) (recur n 0)))))))))
