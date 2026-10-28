(ns jobs.survival.respond.step
  "What a flight step does once respond-choose has picked an option: the walk away and what the step says."
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [clojure.set :as set]
            [clojure.string :as str]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.combat :as combat]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]
            [jobs.survival.respond.options :as cornered]
            [jobs.survival.respond.cornered :as pick]
            [jobs.survival.respond.cover :as cover]
            [jobs.survival.respond.state :as flight]
            [jobs.survival.respond.goal :as respond.goal]
            [jobs.survival.respond.pillar :as pillar]
            [jobs.survival.respond.refuge :as refuge]
            [jobs.lib.walk :as lwalk]))

(a/defargs settings
  {::eat-gap {:default 12 :doc "Blocks to the nearest hostile at which a bite is eaten per flee step." :spec (a/num-in 0 nil)}
   ::low-leg-s {:default 1.5 :doc "Seconds bounding a flight step while exposed low in a ranged chaser's line of fire: a search that stands still must not last." :spec (a/num-in 0 nil)}
   ::slow-step-cap {:default 2 :doc "Flee steps in a row that ran out of their seconds before the body moved (a slow start, nothing in the way) tried again before the way counts as blocked; as many in a row whose end go-to then found no floor under (the walk read it unseen, as rock) are chosen again without that end." :spec (a/int-in 0 nil)}
   ::stall-blocks {:default 1 :doc "A low step that moved the body less than this many blocks stood still: cover where it stands comes next." :spec (a/num-in 0 nil)}})
(defn eat-gap [] (settings/get settings ::eat-gap))
(defn low-leg-s [] (settings/get settings ::low-leg-s))
(defn slow-step-cap [] (settings/get settings ::slow-step-cap))
(defn stall-blocks [] (settings/get settings ::stall-blocks))

(defn close-hitters
  "Of the hostiles near, those that attack at close range (creepers included, not ranged mobs or endermen) within close-radius."
  [c]
  (filterv #(and (not (combat/ranged? %)) (not (combat/enderman? %))
                 (<= (danger-q/mob-distance (:primitives c) %) (cornered/close-radius)))
           (cornered/near-hostiles c)))


(defn ^:async eat-on-the-run!
  "One bite a flee step, with the nearest hostile at least eat-gap away; none for the rest of the flight once
  nothing is left to eat or a bite fails."
  [c threat]
  (when (and (not (:ate (ctx/mem c)))
             (>= (danger-q/mob-distance (:primitives c) threat) (eat-gap)))
    (let [r (await (ctx/call c :eat 'jobs.survival.eat {:until 20 :max-bites 1 :get-food? false}))]
      (when (and (contract/ending? r) (not= :done (:kind r)))
        (ctx/update-mem! c assoc :ate true)))))

(defn say-target!
  "Log (retreat.target) the cell a flee step walks to, the chasers it runs from and whether it is out of every ranged one's line of fire."
  [c target threats covered?]
  (let [cell (mapv #(js/Math.floor %) [(:x target) (:y target) (:z target)])
        from (mapv #(str (.-name %) " " (.-id %)) threats)]
    (ctx/emit! c :retreat.target :info {:target cell :from from :cover (boolean (and covered? (covered? target)))
                                        :text (str "flee step to " (str/join " " cell) " away from " (str/join ", " from))})))

(defn say-choice!
  "Log (retreat.choice info), when it changes, what the flight chose among: the hostiles it knows and where, each
  closed room (survey) and why it is not run to (verdicts; nil: it is), the cells that are no closed room, the home,
  the goal and why it is dropped (judged), and where the flight leans. blast: the creepers within blast reach
  (flight/blaster?), for which no room was judged."
  [c survey verdicts judged blast]
  (let [p (:primitives c)
        at (fn [pos] (when pos (mapv #(js/Math.floor (% pos)) [:x :y :z])))
        rooms (mapv (fn [{:keys [room why inside?]}]
                      {:at (at (:start room)) :own? (boolean (:home? room)) :why (if inside? :inside why)})
                    verdicts)
        goal (at (:goal judged))
        blast (mapv #(str (.-name %) " " (.-id %)) blast)
        lean (at (or (:start (first (:rooms survey))) (:home judged) (respond.goal/usable judged) (when (empty? blast) (cover/home-pos c))))
        choice {:blast blast :rooms rooms :dropped (:dropped survey) :home (at (cover/home-pos c)) :home-why (when (cover/home-pos c) (:home-why judged)) :goal goal :goal-why (:why judged) :lean lean}]
    (when (not= choice (:choice-said (ctx/mem c)))
      (ctx/update-mem! c assoc :choice-said choice)
      (ctx/emit! c :retreat.choice :info
                 (assoc choice
                        :hostiles (mapv #(str (.-name %) " " (.-id %) " " (at (danger-q/mob-pos p %))) (flight/known-chasers p))
                        :text (str "flee choice: " (if lean (str "leans to " lean) "no refuge or home to run to, runs straight away from the hostiles")
                                   (when (:home-why choice) (str "; home " (at (cover/home-pos c)) " dropped (" (name (:home-why choice)) ")"))
                                   (when goal (str "; goal " goal (when (:why judged) (str " dropped (" (name (:why judged)) ")"))))
                                   (when (seq blast) (str "; no room is a refuge within blast reach of " (str/join ", " blast)))
                                   (when (seq rooms)
                                     (str "; rooms " (str/join ", " (map #(str (:at %) " " (name (or (:why %) :run))) rooms))))
                                   (when (seq (:dropped survey))
                                     (str "; no room " (str/join ", " (map #(str (:at %) " " (name (:why %))) (:dropped survey)))))))))))

(defn ^:async dodge-shot!
  "A shot from a shooter not seen (jobs.lib.shot/open?): one flee step to a cell out of its line of fire
  (cover/flight-target! over the shot spots) :again; the cornered options (cover first) when no open way leads out."
  [c]
  (let [spots (flight/fire-spots c)
        target (await (cover/flight-target! c []))]
    (if-not target
      (await (pick/cornered! c "shot at by a shooter not seen, no open way out of its line" false))
      (let [_ (ctx/emit! c :retreat.target :info {:target (mapv #(js/Math.floor %) [(:x target) (:y target) (:z target)]) :from [] :shot-spots (count spots)
                                                  :text "flee step out of the line of an unseen shooter"})
            moved (await (cornered/leg-step! c :step target 1 (cornered/close-leg-s)))]
        (if moved :again (await (pick/cornered! c "shot at by a shooter not seen, the step out of its line is blocked" false)))))))

(defn ^:async walk-away!
  "The flee step when the body outruns the chasers: eat a bite, then take the stairs up, the goal leg or the flight's
  target (cover/flight-target! over judged, cover/judge!; stuck: the cornered fallback, called with its reason; a target out of the line of fire is walked onto and centred on, not near), or cover where it stands, or dig a side pocket first."
  [c threats judged covered? stuck]
  (let [p (:primitives c)
        threat (first threats)
        _ (await (eat-on-the-run! c threat))
        stair (cover/way-up c threats)
        leg (when-not stair (respond.goal/leg c judged))
        gap0 (flight/nearest-gap p threats)
        goal-gap0 (when leg (u/dist (u/self-pos c) leg))
        target (or leg (await (cover/flight-target! c threats covered? judged)))
        grown? (and (zero? (:regrown (ctx/mem c) 0))
                    (seq (set/difference (set (map #(.-id %) (flight/look! c))) (set (map #(.-id %) threats)))))
        low? (cover/exposed-low? c covered?)
        cover (cond
                grown? (do (ctx/update-mem! c update :regrown (fnil inc 0)) :again)
                (and low?
                     (or (pick/plug-continues? c) (not (and target (covered? target))) (pos? (:stalled (ctx/mem c) 0)))
                     (not (cover/close-danger? p threats)))
                (await (cover/take-cover! c))
                (and target (not stair) (sh-sense/buried? p) (not (cover/close-danger? p threats)))
                (await (cover/dig-pocket! c)))]
    (cond
      (some? cover) cover
      (nil? target) (await (stuck "no open way away from the hostile"))
      :else
      (let [_ (when (:cornered (ctx/mem c)) (ctx/update-mem! c dissoc :cornered))
            _ (say-target! c target threats covered?)
            _ (when leg (respond.goal/start-leg! c))
            from (u/self-pos c)
            refused0 (:refused (ctx/mem c))
            cover? (and covered? (covered? target))
            moved (await (cornered/leg-step! c :step target (if cover? 0 1)
                                    (cond (or (seq (close-hitters c)) (cornered/recently-hurt? c)) (cornered/close-leg-s)
                                          low? (low-leg-s)
                                          :else (cornered/flight-timeout-s))
                                    (if leg (respond.goal/margin c) (respond.goal/leg-margin))))
            ;; go-to counts a body on the cell's edge as there, its box still in the line of fire
            _ (when (and moved cover? (not= (select-keys target [:x :y :z]) (sh-sense/feet p)))
                (await (lwalk/centre! c (+ (:x target) 0.5) (+ (:z target) 0.5))))]
        (ctx/update-mem! c #(if (and low? (< (u/dist from (u/self-pos c)) (stall-blocks)))
                              (update % :stalled (fnil inc 0))
                              (dissoc % :stalled)))
        (cond
          (and leg (not (and moved
                             (>= (or (flight/nearest-gap p threats) js/Infinity) (dec (or gap0 0)))
                             (< (u/dist (u/self-pos c) leg) (- goal-gap0 0.5)))))
          (do (respond.goal/off! c leg :lost-ground) :again)
          (and leg (respond.goal/drifted? c leg)) (do (respond.goal/off! c leg :height) :again)
          (and (not moved) (not= refused0 (:refused (ctx/mem c))) (< (:refused-steps (ctx/mem c) 0) (slow-step-cap)))
          (do (ctx/update-mem! c update :refused-steps (fnil inc 0)) :again)
          (and (not moved) (:leg-timed-out (ctx/mem c)) (< (:slow-steps (ctx/mem c) 0) (slow-step-cap)))
          (do (ctx/update-mem! c update :slow-steps (fnil inc 0)) :again)
          (not moved) (await (stuck "the way away from the hostile is blocked"))
          :else
          (let [gap (flight/nearest-gap p threats)]
            (ctx/update-mem! c #(-> % (dissoc :tried :slow-steps :refused-steps :regrown) (flight/note-gap gap)))
            :again))))))
