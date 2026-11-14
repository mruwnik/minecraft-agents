(ns jobs.lib.villager
  "Villagers in sight, as the village jobs (breed, feed, trade, roll) look for them. One search radius for all;
  a call overrides it with its :settings."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.child :as child]
            [jobs.lib.util :as u]
            [jobs.lib.look :as look]
            [jobs.lib.vantage :as vantage]
            [jobs.lib.shelter.sense :as sh-sense]))

(a/defargs settings
  {::search-radius {:default 48 :doc "Blocks within which villagers are looked for." :spec (a/int-in 1 96)}
   ::poll-s {:default 2 :doc "Seconds between looks while waiting for a villager's job or for day to look for a villager asleep out of view." :spec (a/int-in 1 60)}})

(defn poll-s [] (settings/get settings ::poll-s))

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
