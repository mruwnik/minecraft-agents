(ns world-test.caught-flights
  "tools/event-counts.mjs caught-flights: counts the flights behind jobs.lib.cost.caught ::flights from the trial bodies'
  event logs, and the fights of the same logs beside the price the engine gives them.
  An episode is a run of decisions (flee or fight) about one group of kinds (caught/groups), each within gap-ms of the
  one before; its health the first's, its :chasers the most of the group's mobs a decision names near (nil before the
  decisions named them), its :weapon the one its respond run equipped or wore at its end. Within gap-ms after its last
  decision it died of the group or was hurt by :hits of its blows.
  report: per group, the flights as ::flights counts them (a death by the blows that kill at its health, the group's blow
  at the counting difficulty, unarmoured) and their fit (caught/posterior: the blows, the chance caught at some healths);
  :fights, per weapon the zombie family's fights, their deaths and the deaths cost/fight-answer prices them at
  (unarmoured: the kit's armour is not in the logs); :by-chasers, the zombie family's flights caught (killed, or hurt
  caught-hits times) k of n by chasers.
  Pure part: episodes, report."
  (:require [jobs.lib.cost.answer :as answer]
            [jobs.lib.cost.caught :as caught]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.cost.weapon :as weapon]))

(def gap-ms 30000)
(def caught-hits {:zombie 2 :creeper 1 :bow 2 :spider 2})

(defn episodes
  "Rows [{:group :decision :t :health :chasers :weapon :mob :died? :hits}] of the episodes in records
  (world-test.event-counts/scan-line's, sorted by :t), of flee decisions or of decision's."
  ([records] (episodes records :flee))
  ([records decision]
   (mapcat (fn [[group family]]
             (let [eps (reduce (fn [eps {:keys [k t mob health near job] d :decision}]
                                 (if (and (= :decided k) (family mob) (= decision d))
                                   (let [e (peek eps)
                                         n (some->> near (filter family) count)
                                         more (fn [e] (cond-> e n (update :chasers (fnil max 0) n)))]
                                     (if (and e (<= (- t (:end e)) gap-ms))
                                       (conj (pop eps) (more (assoc e :end t)))
                                       (conj eps (more {:t t :end t :health health :job job :mob mob}))))
                                   eps))
                               [] records)
                   within (fn [{:keys [t end]} r] (<= t (:t r) (+ end gap-ms)))
                   weapon (fn [{:keys [job t]}] (when job (or (some #(when (and (= :equip (:k %)) (= job (:job %)) (<= t (:t %))) (:item %)) records)
                                                              (some #(when (and (= :ended (:k %)) (= job (:job %)) (<= t (:t %))) (:wear %)) records))))]
               (for [e eps :when (:health e)]
                 {:group group :decision decision :t (:t e) :health (:health e) :chasers (:chasers e) :weapon (weapon e) :mob (:mob e)
                  :died? (boolean (some #(and (= :died (:k %)) (family (:cause %)) (within e %)) records))
                  :hits (reduce + 0 (keep #(when (and (= :hurt (:k %)) (some family (:causes %)) (within e %)) (or (:hits %) 1)) records))})))
           caught/groups)))

(defn caught? [{:keys [group died? hits]}] (or died? (<= (caught-hits group 2) hits)))

(def blow-of "The mob whose blow a group's deaths are counted in." {:zombie "zombie" :bow "skeleton" :spider "spider"})

(defn armed? [{:keys [weapon mob]}] (and (contains? weapon/weapon-damage-by-name weapon) (not= "drowned" mob)))

(defn r3 [x] (/ (js/Math.round (* 1000 x)) 1000))

(defn flights
  "::flights counts {:lived [[health blows n]] :died [[health blows n]]} of a group's flight rows, a death's blows
  ceil(health / hit), hit the group's blow at the ::threat/difficulty setting."
  [rows hit]
  (let [tally (fn [f rs] (->> rs (map f) frequencies (map (fn [[[h b] n]] [h b n])) (sort-by (juxt first second)) vec))
        h #(r3 (:health %))]
    {:lived (tally (juxt h :hits) (remove :died? rows))
     :died (tally (juxt h #(js/Math.ceil (/ (:health %) hit))) (filter :died? rows))}))

(defn fight-deaths
  "{weapon {:n :deaths :priced}} of fight rows: the deaths, and the sum of each fight's answer-death-chance by
  cost/fight-answer of its mob beside an unarmoured body at its health with its weapon."
  [rows]
  (into (sorted-map)
        (for [[w rs] (group-by #(or (:weapon %) "fist") rows)]
          [w {:n (count rs) :deaths (count (filter :died? rs))
              :priced (r3 (reduce + (map (fn [{:keys [health mob weapon]}]
                                           (let [body {:health health :food 20 :level 0 :carried 0}]
                                             (answer/answer-death-chance body (answer/fight-answer {:weapon weapon :health health :mobs [{:name mob :distance 0}]}) nil)))
                                         rs)))}])))

(defn report
  "{:flights {group counts} :fit {group {:blows :shape :chance {health chance}}} :fights :by-chasers} of flee rows and
  fight rows (episodes'), at the ::threat/difficulty setting."
  [rows fights]
  (let [counts-of (fn [rows] (into (sorted-map) (for [[g rs] (group-by :group rows) :when (blow-of g)]
                                                  [g (flights rs (threat/difficulty-scale (get threat/mob-hit (blow-of g))))])))
        fit-of (fn [counts] (into (sorted-map) (for [[g c] counts :let [{:keys [blows shape]} (caught/posterior c)]]
                                                 [g {:blows (r3 blows) :shape (some-> shape r3) :chance (into (sorted-map) (for [h [2 5 10 15 20]] [h (r3 (caught/chance c h))]))}])))
        counts (counts-of rows)
        armed (counts-of (filter armed? fights))]
    {:flights counts
     :fit (fit-of counts)
     :fights (fight-deaths (filter #(= :zombie (:group %)) fights))
     :fight-counts armed
     :fight-fit (fit-of armed)
     :by-chasers (into (sorted-map) (for [[c rs] (group-by :chasers (filter #(and (= :zombie (:group %)) (:chasers %)) rows))]
                                      [c {:k (count (filter caught? rs)) :n (count rs)}]))}))
