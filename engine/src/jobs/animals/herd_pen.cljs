(ns jobs.animals.herd-pen
  "The pen and its gate for jobs.animals.herd: choosing the gate, the geometry of the walk through it, the waits and the gate toggles."
  (:require [jobs.lib.animals :as animals]
            [jobs.lib.click :as click]
            [engine.ctx :as ctx]
            [jobs.lib.declined :as declined]
            [jobs.lib.gate :as gate]
            [jobs.lib.look :as look]
            [jobs.lib.near :as near]
            [jobs.lib.pen :as pen]
            [jobs.lib.util :as u]
            [jobs.animals.herd-run :refer [adults animal-now approach-range box-centre cell cell-pos drop-gate! end! finish! gate-cell gate-open? hold-gate! in-pen-adults max-reopens near-pen orthogonal out-1 read-pen set-phase! settle-ms shut-failed!]]))

;; ------------------------------------------------------------------ the pen and its gate

(defn free-floor?
  "True when an animal can stand in cell: feet and head free, something to stand on."
  [p [x y z]]
  (let [at (fn [dy] (u/seen-name p {:x x :y (+ y dy) :z z}))
        [below feet head] (map at [-1 0 1])]
    (boolean (and below feet head (pen/passes? feet) (pen/passes? head) (not (pen/passes? below))))))

(defn gate-sides
  "[inside outside]: the pen cell beside gate g and the free floor straight across, or nil (a corner gate)."
  [p inside g]
  (let [[x y z] (cell g)]
    (first (for [[dx dz] orthogonal
                 :let [in [(- x dx) y (- z dz)] out [(+ x dx) y (+ z dz)]]
                 :when (and (contains? inside in) (not (contains? inside out)) (free-floor? p out))]
             [in out]))))

(defn choose-gate
  "{:gate pos :in cell :out cell} of the usable gate (:gate, else the nearest of the pen's), or nil."
  [c answer]
  (let [p (:primitives c)
        here (u/self-pos c)
        wanted (:gate (:args c))]
    (->> (:gates answer)
         (map :pos)
         (filter #(or (nil? wanted) (= (cell wanted) (cell %))))
         (keep (fn [g] (when-let [[in out] (gate-sides p (:inside answer) g)] {:gate g :in in :out out})))
         (sort-by #(u/dist here (:gate %)))
         first)))

(def min-depth
  "Pen cells in a straight line from in-1 the gate's axis needs: the body on in-5 puts a led animal 2.2 cells
  inside the gate cell; on in-4 it still overlaps it."
  5)

(def max-axis-depth "Pen cells of the axis the steps use." 6)

(def approach-cells "Cells out-1..out-4 outside the gate that must be free floor." 4)

(def overlap-margin "Added to half-width + 0.5 when asking whether an entity overlaps a cell." 0.1)

(def settled-dist
  "An animal this close (or closer) to the body is at rest behind it. A led animal stops 3.2 to 3.7 away,
  often 1 to 2 off the axis. Beyond 4.3 it is pinned. A pinned one is still under the 6 at which the lead
  drags it."
  4.3)

(def settled-move "An animal that moved less than this since the last look has stopped." 0.25)

(def pinned-ms "How long a far animal may stay far before it counts as pinned." 6000)

(def max-backs "Steps back a pinned animal gets before the walk out and retry." 2)

(def half-widths
  "Half the width of the mob's box, by name; mobs not listed use default-half-width."
  {"cow" 0.45 "mooshroom" 0.45 "sheep" 0.45 "goat" 0.45 "pig" 0.45 "chicken" 0.2})

(def default-half-width 0.45)

(defn half-width [mob] (get half-widths mob default-half-width))

(defn axis-dir
  "The unit step [dx dz] from the gate cell g to the pen cell in beside it."
  [g in]
  (let [[gx _ gz] g [ix _ iz] in]
    [(- ix gx) (- iz gz)]))

(defn shift
  "Cell moved k steps of d = [dx dz]."
  [[x y z] [dx dz] k]
  [(+ x (* k dx)) y (+ z (* k dz))])

(defn axis-cells
  "The cells of the line out -> gate -> in, in walking order: out-4 .. out-1 (out is out-1), the gate cell g,
  in-1 .. in-n (in is in-1) with n = depth at most max-axis-depth."
  [g in out depth]
  (let [d (axis-dir g in)]
    (vec (concat (map #(shift out d (- %)) (reverse (range approach-cells)))
                 [g]
                 (map #(shift in d %) (range (min depth max-axis-depth)))))))

(defn depth
  "How many pen cells lie in a straight line from in (in-1) along the gate's axis."
  [inside g in]
  (let [d (axis-dir g in)]
    (count (take-while inside (iterate #(shift % d 1) in)))))

(defn overlaps-cell?
  "True when an entity centre pos {:x :z} is within 0.5 + half-width + overlap-margin of the middle of cell on both
  axes (flat): its box touches the cell, or nearly."
  [{:keys [x z]} half-width [cx _ cz]]
  (let [limit (+ 0.5 half-width overlap-margin)]
    (and (<= (js/Math.abs (- x (+ cx 0.5))) limit)
         (<= (js/Math.abs (- z (+ cz 0.5))) limit))))

(defn settle-step
  "What a look at a led animal says: :settled (within settled-dist of the body and moved less than settled-move),
  :pinned (farther, and waited-ms has reached pinned-ms) or :wait."
  [dist moved waited-ms]
  (cond
    (and (<= dist settled-dist) (< moved settled-move)) :settled
    (and (> dist settled-dist) (>= waited-ms pinned-ms)) :pinned
    :else :wait))

(defn step-in-step
  "How the stepping goes on for a pinned animal: :back (fewer than max-backs backs made: the
  body steps back one position), :retry-from-out (backs made, not retried yet: walk out to out-4 and start over)
  or :give-up."
  [backs retried]
  (cond
    (< backs max-backs) :back
    (not retried) :retry-from-out
    :else :give-up))

(defn let-go-cell
  "The pen cell farthest (flat) from the gate cell g, off the axis through g and in when one is as far."
  [inside g in]
  (let [[gx _ gz] g
        [dx dz] (axis-dir g in)
        far (fn [[x _ z]] (+ (* (- x gx) (- x gx)) (* (- z gz) (- z gz))))
        off-axis? (fn [[x _ z]] (not (zero? (- (* (- x gx) dz) (* (- z gz) dx)))))]
    (first (sort-by (juxt (comp - far) #(if (off-axis? %) 0 1) identity) inside))))

(defn approach-free?
  "True when out-1..out-4 (out is out-1) are all free floor."
  [p g in out]
  (every? #(free-floor? p %) (take approach-cells (axis-cells g in out 0))))

(defn other-leaks
  "The leaks of the pen but the chosen gate standing open."
  [answer g]
  (filterv #(not= (cell (:pos %)) (cell g)) (:leaks answer)))

(def look-range "How near the body goes to a pen cell it has not seen before looking at it." 4)

(defn ^:async check-unknown!
  "The pen has cells the body has not seen (a gate in the dark decays): go near the nearest, look at it once, then read
  the pen again. A cell still unknown after the look ends :no-pen."
  [c answer]
  (let [here (u/self-pos c)
        centre (fn [{:keys [x y z]}] {:x (+ x 0.5) :y (+ y 0.5) :z (+ z 0.5)})
        unk (->> (:leaks answer) (filter #(= :unloaded (:why %))) (map :pos) (sort-by #(u/dist here (centre %))) first)
        looked (set (:looked-pen (ctx/mem c)))]
    (cond
      (nil? unk) (finish! c :no-pen)
      (not (u/within? here unk look-range))
      (if (= :blocked (await (near/go-near! c unk look-range {:doors :never :escalate false})))
        (finish! c :unreachable)
        :continue)
      (contains? looked (cell unk)) (finish! c :no-pen)
      :else (do (await (ctx/act c :look (clj->js {:pos (centre unk)})))
                (look/see! c)
                (ctx/update-mem! c update :looked-pen (fnil conj []) (cell unk))
                :continue))))

(defn survey-pen!
  "The pen is read in full: choose the gate and go to the leash phase, or end with the reason."
  [c answer]
  (let [{:keys [target]} (:args c)
        {:keys [gate in out] :as chosen} (choose-gate c answer)
        leaks (when chosen (other-leaks answer gate))
        inside (count (in-pen-adults c answer))]
    (cond
      (= :no-start (:reason answer)) (finish! c :no-pen)
      (nil? chosen) (finish! c :no-gate)
      (not (gate/allowed? c :herd.declined "herd" :place gate {:own-plans-ok? true})) (finish! c :refused)
      (not (approach-free? (:primitives c) (cell gate) in out)) (finish! c :no-gate {:why :no-approach})
      (seq leaks) (finish! c :leaky {:leaks (vec (take 12 leaks))})
      (>= inside target) (finish! c :full)
      (< (depth (:inside answer) (cell gate) in) min-depth) (finish! c :too-shallow)
      :else (do (ctx/update-mem! c assoc :phase :leash :gate gate :inside-cell (cell-pos in) :outside-cell (cell-pos out)
                                 :axis (axis-cells (cell gate) in out (depth (:inside answer) (cell gate) in)))
                  :continue))))

(defn ^:async survey! [c]
  (let [centre (box-centre (:box (:args c)))]
    (if (> (u/dist (u/self-pos c) centre) near-pen)
      (if (= :blocked (await (near/go-near! c centre approach-range {:doors :never :escalate false})))
        (finish! c :unreachable)
        :continue)
      (let [answer (read-pen c)]
        (if (= :unloaded (:reason answer))
          (await (check-unknown! c answer))
          (survey-pen! c answer))))))

(declare go!)

(defn ^:async step-out-of-gate!
  "The body stands in the gate cell: walk to the first cell outside it (the next round shuts)."
  [c]
  (case (await (go! c (nth (:axis (ctx/mem c)) out-1)))
    :arrived :continue
    :failed (end! c :unreachable)
    :continue))

(defn ^:async toggle-gate!
  [c state next reach then]
  (let [r (await (declined/call-child! c :gate 'jobs.access.toggle {:pos (:gate (ctx/mem c)) :state state :reach reach}))]
    (cond
      (not= :done r) (if (= :declined r) :declined :continue)
      (= :done (:status (ctx/child-result c :gate)))
      (do (when (= :closed state) (drop-gate! c))
          (set-phase! c next then))
      (= :open state) (end! c :gate-stuck)
      :else (shut-failed! c))))

(defn ^:async set-gate!
  "Put the gate into state (:open or :closed) with jobs.access.toggle (within :reach), then go to phase next, merging
  :then into the memory; :gate-stuck when it did not open, shut-failed! when it did not shut. The :gate-held entry is
  written before an open and dropped after a shut. A shut with the body in the gate cell first walks out of it."
  [c state next & [{:keys [reach then] :or {reach 3}}]]
  (when (= :open state) (hold-gate! c))
  (if (and (= :closed state) (click/standing-in? (u/self-pos c) (cell-pos (gate-cell c)) nil))
    (await (step-out-of-gate! c))
    (await (toggle-gate! c state next reach then))))

(defn outside-gate?
  "True when pos {:x :z} stands past the gate cell g on the side away from the pen cell in beside it (along the axis)."
  [g in {:keys [x z]}]
  (let [[gx _ gz] g
        [dx dz] (axis-dir g in)]
    (neg? (+ (* dx (- (js/Math.floor x) gx)) (* dz (- (js/Math.floor z) gz))))))

(defn body-outside? [c]
  (outside-gate? (gate-cell c) (cell (:inside-cell (ctx/mem c))) (u/self-pos c)))

(def max-leaves "How often a run that ends with the body on the pen side goes out before the shut." 2)

(defn body-in-pen?
  "True when the body stands on a cell of the box or on the gate cell: a shut from there would leave it inside."
  [c]
  (let [here (u/self-pos c)
        [gx _ gz] (gate-cell c)]
    (or (animals/in-box? (:box (:args c)) here)
        (and (= gx (js/Math.floor (:x here))) (= gz (js/Math.floor (:z here)))))))

(defn ^:async exit-dash!
  "The exit in one round: go-near! to out-1 with its own gate handling (open, pass, shut). Shut and outside: on to the
  census. Open and outside: the shut from outside (:shut-out). Still inside, or blocked: by :exit-open, the
  failure counted in :dash-tries."
  [c]
  (hold-gate! c)
  (let [r (await (near/go-near! c (cell-pos (nth (:axis (ctx/mem c)) out-1)) 0 {:escalate false}))
        outside? (body-outside? c)
        open? (gate-open? c)]
    (cond
      (and outside? (#{:there :partial} r) (not open?)) (do (drop-gate! c) (set-phase! c :census))
      (and outside? (#{:there :partial} r)) (set-phase! c :shut-out)
      :else (set-phase! c :exit-open {:dash-tries (inc (:dash-tries (ctx/mem c) 0))}))))

(defn shut-side-step
  "What follows a shut of the gate: :census when the body is outside, :reopen (the gate opened again, the body goes out)
  while fewer than max-reopens were made, else :stuck."
  [outside? reopens]
  (cond
    outside? :census
    (< reopens max-reopens) :reopen
    :else :stuck))

(defn shut-side!
  "The gate is shut after the exit: see shut-side-step; a body on the pen side opens the gate again (:exit-open)."
  [c]
  (let [reopens (:reopens (ctx/mem c) 0)]
    (case (shut-side-step (body-outside? c) reopens)
      :census (set-phase! c :census {:animal nil :stepped-back false :reopens nil})
      :reopen (set-phase! c :exit-open {:reopens (inc reopens)})
      :stuck (end! c :gate-stuck))))

(defn ^:async go!
  "One round of a go-to (range 0, :doors :never) to the cell: :arrived, :failed or nil while it goes on."
  [c cell]
  (let [r (await (ctx/call-child c :walk 'jobs.movement.go-to {:pos (cell-pos cell) :range 0 :doors :never :escalate false}))]
    (cond
      (not= :done r) nil
      (:arrived (ctx/child-result c :walk)) :arrived
      :else :failed)))

(defn ^:async go-then!
  "Walk to the cell, then to phase next (merging more); a blocked walk gives up :unreachable."
  [c cell next & [more]]
  (case (await (go! c cell))
    :arrived (set-phase! c next more)
    :failed (end! c :unreachable)
    :continue))

(defn flat-dist [a b]
  (js/Math.hypot (- (:x a) (:x b)) (- (:z a) (:z b))))

(defn cell-middle [[x _ z]] {:x (+ x 0.5) :z (+ z 0.5)})

(defn animal-pos [c]
  (some-> (animal-now c (:animal (ctx/mem c))) .-pos u/pos-of))

(defn ^:async walk-settle!
  "Walk to the cell, then look every 500 ms until the animal is at rest behind the body (settle-step). Nil while it
  goes on, then :settled (also when max-ms ran out), :pinned (too far for too long) or :unreachable."
  [c cell max-ms]
  (let [m (ctx/mem c)]
    (if-not (:walked m)
      (case (await (go! c cell))
        :arrived (do (ctx/update-mem! c assoc :walked true :settle-from (ctx/now c) :last-seen nil) nil)
        :failed :unreachable
        nil)
      (if-let [there (animal-pos c)]
        (let [moved (if-let [l (:last-seen m)] (flat-dist l there) 1.0)
              waited (- (ctx/now c) (:settle-from m))]
          (case (settle-step (flat-dist (u/self-pos c) there) moved waited)
            :settled :settled
            :pinned :pinned
            (if (>= waited max-ms)
              :settled
              (do (ctx/update-mem! c assoc :last-seen there)
                  (await (ctx/act c :wait #js {:ms settle-ms}))
                  nil))))
        :settled))))

(defn ^:async settle-at!
  "walk-settle! at axis cell i; (on-done :settled or :pinned), a blocked walk gives up :unreachable."
  [c i max-ms on-done]
  (let [r (await (walk-settle! c (nth (:axis (ctx/mem c)) i) max-ms))]
    (case r
      nil :continue
      :unreachable (end! c :unreachable)
      (on-done r))))

(defn ^:async look-wait! [c]
  (await (ctx/act c :wait #js {:ms settle-ms}))
  :continue)

(defn waited-ms
  "Ms since this phase's first look (the clock starts on the first call)."
  [c]
  (let [now (ctx/now c)
        from (or (:wait-from (ctx/mem c)) now)]
    (ctx/update-mem! c assoc :wait-from from)
    (- now from)))

(defn crowded-near?
  "True when an adult of :mob stands within r (flat) of the middle of cell."
  [c cell r]
  (boolean (some #(<= (flat-dist (u/pos-of (.-pos %)) (cell-middle cell)) r) (adults c))))

(defn on-gate?
  "True when an adult of :mob overlaps the gate cell."
  [c]
  (let [w (half-width (:mob (:args c)))
        g (gate-cell c)]
    (boolean (some #(overlaps-cell? (u/pos-of (.-pos %)) w g) (adults c)))))

(defn ^:async wait-clear!
  "on-clear when crowded? is false; else look again in 500 ms, and after limit-ms go on with one info herd.crowded-gate."
  [c crowded? limit-ms where on-clear]
  (cond
    (not crowded?) (await (on-clear))
    (>= (waited-ms c) limit-ms) (do (ctx/emit! c :herd.crowded-gate :info {:where where :text "the cells at the gate stayed crowded, going on"})
                                    (await (on-clear)))
    :else (await (look-wait! c))))

(defn ^:async shut-when-clear!
  "Shut the gate (:closed within reach, then to phase next with then) while no adult of :mob overlaps its cell, never
  while one does: looks every 500 ms, then (on-crowded) after limit-ms."
  [c reach limit-ms next then on-crowded]
  (cond
    (not (on-gate? c)) (await (set-gate! c :closed next {:reach reach :then then}))
    (>= (waited-ms c) limit-ms) (await (on-crowded))
    :else (await (look-wait! c))))
