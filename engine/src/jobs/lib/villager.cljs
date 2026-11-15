(ns jobs.lib.villager
  "Villagers in sight, as the village jobs (breed, feed, trade, roll) look for them. One search radius for all;
  a call overrides it with its :settings."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.primitive :as primitive]
            [engine.settings :as settings]
            [jobs.lib.child :as child]
            [jobs.lib.util :as u]
            [jobs.lib.look :as look]
            [jobs.lib.vantage :as vantage]
            [jobs.lib.shelter.sense :as sh-sense]))

(a/defargs settings
  {::search-radius {:default 48 :doc "Blocks within which villagers are looked for." :spec (a/int-in 1 96)}
   ::poll-s {:default 2 :doc "Seconds between looks while waiting for a villager's job or for day to look for a villager asleep out of view." :spec (a/int-in 1 60)}
   ::take-job-ticks {:default 1240 :doc "Game ticks of the villagers' walking hours (10-9000, 11000-12000) a jobless villager is waited for to take a free workstation (or to walk to it). The game's rule: a jobless villager looks for a free job site every 20-40 ticks and drops a claim on another site after 1200 ticks of walking to it, so past 1240 it holds a site it reaches (or ours is out of its way)." :spec (a/int-in 1 nil)}})

(defn poll-s [] (settings/get settings ::poll-s))

(defn take-job-ticks [] (settings/get settings ::take-job-ticks))

(def workstations
  "Profession -> its workstation block."
  {"librarian" "lectern" "farmer" "composter" "cleric" "brewing_stand" "armorer" "blast_furnace" "butcher" "smoker"
   "cartographer" "cartography_table" "fisherman" "barrel" "fletcher" "fletching_table" "leatherworker" "cauldron"
   "mason" "stonecutter" "shepherd" "loom" "toolsmith" "smithing_table" "weaponsmith" "grindstone"})

(defn search-radius [] (settings/get settings ::search-radius))

(defn seen
  "The villager entities in sight within the search radius."
  [p]
  (look/seen-objs p {:radius (search-radius) :names ["villager"]}))

(defn gone-cause
  "The cause for a villager not in view: :gone with a hint saying it is out of sight, not dead, and that villagers
  are in bed at night."
  [c]
  (let [night? (sh-sense/dark-outside? (:primitives c))]
    {:reason :gone
     :hint (str "no such villager in view within " (search-radius) " blocks (out of sight, not known dead"
                (when night? "; it is night and villagers sleep behind walls out of sight, try by day")
                ")")}))

(defn readable?
  "Whether the villager entity e can be traded with from where the body stands: the act's own test (the eye to the
  villager's middle within prim/attack-reach, engine/js/trade.mjs), so a villager on a bed or a slab one block off needs
  no walk."
  [c e]
  (let [me (u/self-pos c)
        at (u/pos-of (.-pos e))
        d (js/Math.hypot (- (:x at) (:x me)) (- (+ (:y at) (/ (or (.-height e) 1.95) 2)) (+ (:y me) game/eye-height)) (- (:z at) (:z me)))]
    (<= d (primitive/js-setting "prim/attack-reach"))))

(defn adult? [e] (not (.-baby e)))

(defn adults [p] (filter adult? (seen p)))

(def id?
  "Whether x can name a villager: its uuid (string) or its numeric entity id, as entity listings give it."
  (a/pred "a villager's uuid string or entity id" #(or (string? %) (integer? %))))

(defn ident
  "What names villager entity e to the jobs: its uuid when known, else its numeric entity id."
  [e]
  (or (.-uuid e) (.-id e)))

(defn find-by-uuid
  "The villager entity named by id (uuid or numeric entity id) in sight within the search radius, or nil."
  [p id]
  (first (filter #(or (= id (.-uuid %)) (= id (.-id %))) (seen p))))

(defn chosen
  "The uuid of the villager a job works with: the call's :villager, else the one picked first (kept in memory), else nil."
  [c]
  (or (:villager (:args c)) (:chosen (ctx/mem c))))

(defn pick!
  "Without :villager, choose the nearest adult villager in sight that ok? accepts and keep it, one whose profession reads
  as a job first (the profession is not always readable). The id or nil."
  [c ok?]
  (or (chosen c)
      (let [es (filter ok? (adults (:primitives c)))]
        (when-let [e (or (first (remove #(= "unemployed" (.-profession %)) es)) (first es))]
          (ctx/update-mem! c assoc :chosen (ident e))
          (ident e)))))

(defn ^:async find-in-view!
  "The entity of the villager pick! gives, looked for with a look round that stops at the first look that shows it
  (sensed only inside the view cone, so a test after the last look would miss it); nil when none shows. want: look/see!."
  [c ok? want]
  (let [p (:primitives c)
        f #(some->> (pick! c ok?) (find-by-uuid p))]
    (or (f) (await (look/look-around-until! c f (look/glance-effort) want)))))

(defn ^:async approach-remembered!
  "A villager named by id that is out of view: walk (go-to child, slot :memory) to where the body last saw it (the entity
  memory, else known: a place the caller saw it at), as a player goes back to the spot, once. :continue while the walk
  goes on, :again when it ended (the caller looks round for the villager), nil when the body knows no place for it, is
  already within reach of it or has walked there."
  ([c id reach] (approach-remembered! c id reach nil))
  ([c id reach known]
   (let [p (:primitives c)
         f (aget p "rememberedEntity")
         pos (when (and id (not (:memory-walked (ctx/mem c))))
               (or (some-> (when f (.call f p id)) (js->clj :keywordize-keys true)) known))]
     (when (and pos (not (u/within? (u/self-pos c) pos reach)))
       (let [r (await (child/call-walk* c :memory {:pos pos :range reach :ways #{:walk :clear-seal}}))]
         (if (= :continue r)
           :continue
           (do (ctx/update-mem! c assoc :memory-walked true) :again)))))))

(defn ^:async hear-step!
  "By day, a villager heard but not seen: one vantage step toward the sound and a look round there, as a player walks
  toward a voice (a sound names no villager, so the nearest heard one). :continue while the walk waits, :again after a
  step, nil when none is heard or the steps are spent."
  [c id]
  (let [rows (vantage/heard c ["villager"] (search-radius))
        r (when (seq rows) (await (vantage/step! c rows #(find-by-uuid (:primitives c) id))))]
    (case r :partial :continue nil nil :again)))

(defn ^:async search-out-of-view!
  "A villager named by id (or picked) that is out of view: walk to where the body last saw it (approach-remembered!),
  else at night, when villagers sleep out of sight, wait polls till day and look once more, else by day step toward
  where it is heard. :again, :continue, or nil when nothing is left to try (the caller ends it :gone)."
  [c id reach]
  (let [m (ctx/mem c)]
    (or (await (approach-remembered! c id reach))
        (cond
          (sh-sense/night? (:primitives c)) (do (ctx/update-mem! c assoc :night-waited true)
                                                (await (child/pause! c (poll-s) :night-villager-out-of-view))
                                                :continue)
          (:night-waited m) (do (ctx/update-mem! c dissoc :night-waited :memory-walked :vantage) :again)
          :else (await (hear-step! c id))))))

(defn walk-ticks-before
  "Ticks of villagers' walking hours (10-9000, 11000-12000) in the span from time 0 to time t, t any whole number of ticks."
  [t]
  (let [day (js/Math.floor (/ t 24000))
        m (- t (* day 24000))
        span (fn [from to] (max 0 (- (min m to) from)))]
    (+ (* day 9990) (span 10 9000) (span 11000 12000))))

(defn count-ticks!
  "Add to memory :job-ticks the game ticks (world age) of villagers' walking hours since the last count (the part of that
  span inside those hours, whatever the gap); the count it gives. Dissoc :job-at and :job-ticks to count afresh."
  [c]
  (let [self (.self (:primitives c))
        age (.-worldAge self)
        t (.-timeOfDay self)
        {:keys [job-at job-ticks]} (ctx/mem c)
        gap (if (and age job-at t) (max 0 (- age job-at)) 0)
        n (+ (or job-ticks 0) (- (walk-ticks-before t) (walk-ticks-before (- t gap))))]
    (ctx/update-mem! c assoc :job-at age :job-ticks n)
    n))

(defn job-sites
  "The job-site blocks the body has seen within the villager search radius, other than the workstation at pos: free
  ones a jobless villager may have claimed instead."
  [c pos]
  (->> (look/seen-blocks (:primitives c) {:names (set (vals workstations)) :radius (search-radius) :live? true})
       (remove #(= (select-keys (:pos %) [:x :y :z]) (select-keys pos [:x :y :z])))
       (mapv #(select-keys % [:name :pos]))))
