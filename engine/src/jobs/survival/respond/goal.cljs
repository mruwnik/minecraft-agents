(ns jobs.survival.respond.goal
  "The :goal of a flight (jobs.survival.respond-flight): the destination of the walk it interrupted, when the flight walks to
  it and when it gives it up."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]
            [jobs.survival.respond.state :as flight]
            [jobs.lib.flee-search :as search]
            [jobs.lib.flee-walk :as walk]))

(a/defargs settings
  {::drift {:default 3 :doc "Blocks a walk to the goal may take the body farther from the goal's height than where that walk began: a leg that does ends the walk to the goal (a plan that ends under or over the goal, nearer only in x and z, is no way to it)." :spec (a/num-in 0 nil)}
   ::same-place {:default 8 :doc "Blocks from a safe home within which the goal counts as that home; a goal farther off gives way to the home." :spec (a/num-in 0 nil)}
   ::retry-s {:default 30 :doc "Seconds after a walk to the goal was given up (it lost ground on the hostiles, did not near the goal or left its height) before a flight, this one or the next, may walk to that goal again." :spec (a/num-in 0 nil)}
   ::near {:default 3 :doc "Blocks from the goal at which the flight no longer walks to it." :spec (a/num-in 0 nil)}
   ::leg-margin {:default 16 :doc "Blocks past body and goal, up and down, a leg to the goal may plan in the open." :spec (a/num-in 0 nil)}})
(defn near [] (settings/get settings ::near))
(defn leg-margin [] (settings/get settings ::leg-margin))
(defn drift [] (settings/get settings ::drift))
(defn same-place [] (settings/get settings ::same-place))
(defn retry-s [] (settings/get settings ::retry-s))

(defn goal-pos
  "The :goal arg ({:x :y :z} or [x y z]) as a position map, nil when none."
  [goal]
  (when goal
    (let [[x y z] (if (vector? goal) goal [(:x goal) (:y goal) (:z goal)])]
      {:x x :y y :z z})))

(defn active
  "The :goal as a position, nil when none or the flight has been there (:goal-reached)."
  [c]
  (when-not (:goal-reached (ctx/mem c)) (goal-pos (:goal (ctx/mem c)))))

(defn cell [pos] (mapv #(js/Math.floor (% pos)) [:x :y :z]))

(defn given-up?
  "Whether a walk to goal was given up within retry-s (off!: body memory :goal-off, so the next flight knows it too)."
  [c goal]
  (boolean (some #(= (cell goal) (:goal (:data %))) (ctx/entries c :goal-off))))

(defn chasers
  "The chasers a flight's search runs from, {:name (cost/mob-kind) :mob (its name) :pos :calm? (an enderman the client does
  not flag angry: it walks about, a flight from it is no race; an angry one is the cornered options')}: the hostiles the body senses or remembers
  within the longest follow range (this flight's dead skipped), the last known places of the mobs fled within
  threats/fled-memory-ms that it does not know now, and where shots came from (flight/fire-spots)."
  [c]
  (let [p (:primitives c)
        dead (set (flight/dead-ids c))
        known (remove #(dead (.-id %)) (flight/known-chasers p))
        ids (set (map #(.-id %) known))]
    (-> (mapv (fn [e] (cond-> {:name (cost/mob-kind e) :mob (.-name e) :pos (danger-q/mob-pos p e)}
                         (and (combat/enderman? e) (not (true? (.-angry e)))) (assoc :calm? true))) known)
        (into (comp (map :data) (remove #(ids (:id %)))
                    (keep #(when-let [pos (or (:pos %) (danger-q/rough-pos %))] {:name (:mob %) :mob (:mob %) :pos pos})))
              (threats/listed-threats p (threats/fresh-threats (ctx/view c))))
        (into (map (fn [pos] {:name nil :pos pos})) (flight/fire-spots c)))))

(defn safe-home
  "Pure: the first of homes (positions) whose walk, its end in search ends, is a safe run (flee-search/safe-why nil);
  nil when none."
  [ends homes]
  (first (filter #(when-let [e (search/named ends %)] (nil? (search/safe-why e))) homes)))

(defn home-why
  "Pure: why home (a position, or nil) is no safe run (flee-search/safe-why of its end in search ends; :no-way when it
  was not planned); nil when it is safe or there is no home."
  [ends home]
  (when home
    (if-let [e (search/named ends home)] (search/safe-why e) :no-way)))

(defn verdict
  "Pure: why a flight does not go to goal, its end e in the flight's search, nil when it may: :hostile-nearer when
  beaten? (a chaser gets there first even against a straight walk: flee-search/beaten-to?), flee-search/safe-why
  (:no-way: not planned whole, :past-hostile, :hostile-nearer); :died-there: within flight/::died-radius of a place the
  body died; :home-safer: home (safe-home) lies elsewhere (farther than same-place from it)."
  [goal e {:keys [home died beaten?]}]
  (cond
    beaten? :hostile-nearer
    (nil? e) :no-way
    (search/safe-why e) (search/safe-why e)
    (flight/near-died? died goal) :died-there
    (and home (> (u/dist home goal) (same-place))) :home-safer))

(declare under-floor? margin)

(defn ^:async judge!
  "What the flight makes of its goal and homes (positions) this step, from one flee search (flee-search/search!, from
  chasers, the goal and homes named, covered? as cover-fn): {:goal (active) :why (:given-up, else verdict; nil: it
  may go there) :home (safe-home of homes) :home-why (of the last of homes) :ends (the search's scored ends)}."
  [c homes covered?]
  (let [goal (active c)
        named (cond-> (mapv (fn [h] {:pos h :kind :home}) homes)
                (and goal (not (given-up? c goal)) (not (under-floor? c goal))) (conj {:pos goal :kind :goal}))
        chs (chasers c)
        ends (await (search/search! c chs named {:covered? covered? :y-margin (margin c)}))
        home (safe-home ends homes)]
    {:goal goal :home home :ends ends :home-why (home-why ends (last homes))
     :why (when goal
            (cond
              (given-up? c goal) :given-up
              (under-floor? c goal) :under-floor
              :else (verdict goal (search/named ends goal)
                             {:home home :died (flight/died-spots c)
                              :beaten? (search/beaten-to? (search/with-ways c chs) (u/self-pos c) (.-food (.self (:primitives c))) goal)})))}))

(defn ^:async caught-chance!
  "The chance a flight from where the body stands is caught (flee-search/caught-chance of a search from its chasers),
  for the fight-or-flee weighing; kept (memory :caught-chance) while the body stays in its cell for a flee step's
  seconds (flee-search ::step-s)."
  [c]
  (let [here (cell (u/self-pos c))
        now (ctx/now c)
        kept (:caught-chance (ctx/mem c))]
    (if (and (= here (:cell kept)) (< (- now (:t kept)) (* 1000 (settings/get search/settings ::search/step-s))))
      (:chance kept)
      (let [chance (search/caught-chance (await (search/search! c (chasers c) [] {:y-margin (margin c)})) (sh-sense/buried? (:primitives c)))]
        (ctx/update-mem! c assoc :caught-chance {:cell here :t now :chance chance})
        chance))))

(defn usable
  "The goal of judged (judge!) when nothing speaks against it, else nil."
  [{:keys [goal why]}]
  (when-not why goal))

(defn under-floor?
  "Whether pos lies below the flight's floor (flight/floor) under rock: the body is under ground, or a block it has
  seen roofs pos."
  [c pos]
  (let [p (:primitives c)
        cell (update-vals (select-keys pos [:x :y :z]) #(js/Math.floor %))]
    (boolean (and (< (:y pos) (flight/floor c))
                  (or (sh-sense/buried? p) (walk/roofed? #(u/seen-name p %) cell))))))

(defn margin
  "The :y-margin of a leg to the goal: under ground no deeper than walk/max-descent past body and goal, so the plan
  cannot dive through the caves below."
  [c]
  (if (sh-sense/buried? (:primitives c)) (walk/max-descent) (leg-margin)))

(defn off!
  "Give the walk to goal up for retry-s (body memory :goal-off) and say why (:lost-ground or :height) in a
  retreat.goal-off info."
  [c goal why]
  (ctx/update-mem! c dissoc :goal-y0)
  (ctx/remember! c :goal-off {:goal (cell goal) :why why} {:cap 4 :ttl (* 1000 (retry-s))})
  (ctx/emit! c :retreat.goal-off :info
             {:why why
              :text (str (case why
                           :lost-ground "the walk to the goal lost ground on the hostiles or did not near the goal"
                           :height "the walk to the goal left the goal's height or went down under rock")
                         ": away from them instead, the goal again in " (retry-s) " s")}))

(defn leg
  "The goal of judged (judge!), when nothing speaks against it, as a go-to target: the planner's danger costs route the
  leg round the hostiles. Once the body is within near of it the goal is done for the flight (:goal-reached): the
  flight goes on away from the chasers; without that it walked back and forth between the goal and the way away for ever."
  [c judged]
  (let [goal (usable judged)]
    (cond
      (nil? goal) nil
      (<= (u/dist (u/self-pos c) goal) (near)) (do (ctx/update-mem! c assoc :goal-reached true) nil)
      :else goal)))

(defn start-leg!
  "Note the feet height the walk to the goal began at (:goal-y0), once a walk."
  [c]
  (ctx/update-mem! c update :goal-y0 #(or % (:y (u/self-pos c)))))

(defn drifted?
  "Whether the body, after a leg to goal, is more than drift farther from the goal's height than where the walk to it
  began (:goal-y0), or stands under rock below the flight's floor."
  [c goal]
  (let [self (u/self-pos c)
        y0 (:goal-y0 (ctx/mem c) (:y self))
        off (fn [y] (js/Math.abs (- (:y goal) y)))]
    (or (> (off (:y self)) (+ (off y0) (drift)))
        (under-floor? c self))))
