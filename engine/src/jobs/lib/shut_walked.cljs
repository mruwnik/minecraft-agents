(ns jobs.lib.shut-walked
  "The :walked kind of jobs.maintenance.shut: doors, gates and trapdoors a walk left open (see that job's doc). `ops` is what the job's loop calls."
  (:require [jobs.lib.ready :as ready]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [jobs.lib.checks :as checks]
            [jobs.lib.util :as u]
            [jobs.lib.pass :as pass]
            [jobs.lib.doors :as doors]
            [jobs.lib.access :as access]
            [jobs.lib.shut-walk :as walk]
            [jobs.lib.world :as known]))

(a/defargs settings
  {::unrecorded? {:default false :doc "Also shut wooden doors of the body's own building seen standing open that no walk recorded opening." :spec boolean?}
   ::home-radius {:default 8 :doc "With ::unrecorded?, a door this close to the :home place is the body's own, in blocks." :spec (a/num-in 0 nil)}
   ::attempts {:default 5 :doc "Runs whose clicks failed before a block is dropped for good." :spec (a/int-in 1 nil)}
   ::s-per-door {:default 6 :doc "Seconds a quote counts for one door: the walk and the clicks." :spec (a/num-in 0 nil)}})
(defn home-radius [] (settings/get settings ::home-radius))
(defn attempts [] (settings/get settings ::attempts))

(defn radius [c] (or (:radius (:args c)) 16))

(defn cell-key [{:keys [x y z]}] [x y z])

(defn own-door?
  "Whether the door cell is in the footprint of a plan this body made, or within the home-radius setting (8) of its :home place."
  [c {:keys [x y z] :as cell}]
  (let [home (mem/place (ctx/view c) :home)]
    (boolean (or (some (access/own-plans c) [(get (known/footprints c) [x y z])])
                 (and home (<= (doors/distance {:x (:x home) :y (:y home) :z (:z home)} cell) (home-radius)))))))

(defn door-args [c] (assoc (:args c) :radius (radius c)))

(defn targets
  "The left-open blocks (doors/left-open as the body last saw them, any age; then doors/unrecorded-open) within :radius, minus the cells in left ({:cell [x y z]})."
  [c left & _]
  (let [given-up (set (map :cell left))
        args (door-args c)]
    (remove #(given-up (cell-key (:cell %)))
            (concat (doors/left-open (:primitives c) (ctx/view c) args 0 doors/seen-open-block)
                    (when (settings/get settings ::unrecorded?) (doors/unrecorded-open (:primitives c) (ctx/view c) (assoc args :own? #(own-door? c %))))))))

(defn give-up!
  "Record why the target stays open. :standing-in and a first few :shut-failed keep the entry,
  with its opening time (a failed shut counts in :fails; the trigger waits :open-s per fail before it tries again). A repeat failure that keeps its entry only gets an info."
  [c {:keys [cell by fails t]} reason]
  (let [k (cell-key cell)
        fails' (if (= :shut-failed reason) (inc fails) fails)
        keep? (case reason
                :standing-in true
                :shut-failed (< fails' (attempts))
                false)]
    (when-not (= :animal-in-the-way reason)
      (ctx/forget-where! c :opened #(= cell (:cell %)))
      (when keep?
        (ctx/remember! c :opened {:cell cell :by by :t (or t (ctx/now c)) :shut? true :fails fails'} pass/opened-policy)))
    (ctx/emit! c :shut.gave-up (if (or (= :animal-in-the-way reason) (and keep? (pos? fails))) :info :warn)
               {:openings :walked :cell k :reason reason
                :text (str "the block at " k " stays open: " (name reason)
                           (when (and (= :shut-failed reason) keep?) ", will try again later"))})
    {:cell k :reason reason}))

(defn ^:async shut!
  "Shut the block: nil when shut, else the reason it stays open."
  [c {:keys [cell column dist]}]
  (if (pass/in-column? column (doors/feet-cell (u/self-pos c)))
    :standing-in
    (if-let [why (when (> dist (pass/leftover-reach)) (await (walk/walk! c cell #{:walk :clear-seal})))]
      why
      (loop [n 1]
        (await (pass/shut-column! c column))
        (cond
          (not (doors/open-block (:primitives c) cell)) nil
          (< n (walk/tries)) (recur (inc n))
          (seq (pass/animals-in c column)) :animal-in-the-way
          :else :shut-failed)))))

(defn done?
  "Ok when no block seen left open stands within :radius and none was given up on; later with the recorded blocks never
  seen; else later, with the doors shut so far and the blocks left."
  [c]
  (let [open (targets c [])
        given-up (count (:left (ctx/mem c)))
        unseen (when (and (empty? open) (zero? given-up)) (doors/unseen-recorded (:primitives c) (ctx/view c) (door-args c)))
        shut (:shut (ctx/mem c) 0)]
    (if (seq unseen)
      (contract/later :gate-unseen "a block a walk opened was never seen" :look-at (vec unseen))
      (checks/goal (and (empty? open) (zero? given-up)) [:shut (str "shut " shut " left open by a walk")]
                   [:open (str (+ (count open) given-up) " blocks stand open")]
                   {:units :doors :done shut :left (+ (count open) given-up) :result {:shut shut :left []}}))))

(defn quote-of [c]
  (let [t (* (settings/get settings ::s-per-door) (count (targets c [])))]
    {:uses {:job-time [0 t (* 3 t)]}}))

(defn finish
  "[left text] for the ending: the recorded blocks never seen are left too."
  [c shut left]
  (let [left (into left (map (fn [cell] {:cell (cell-key cell) :reason :unseen})
                             (doors/unseen-recorded (:primitives c) (ctx/view c) (door-args c))))]
    [left (str "shut " shut " left open by a walk" (when (seq left) (str ", gave up on " (count left))))]))

(def ops
  {:targets targets :shut! shut! :give-up! give-up! :safe? ready/threatened :can-run? (fn [_] (contract/ok :openings "nothing to wait for"))
   :done? done? :quote quote-of :wait-first? (fn [_] false) :finish finish})
