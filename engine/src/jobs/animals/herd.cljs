(ns jobs.animals.herd
  (:require [engine.ctx :as ctx]
            [engine.jobs.animals :as animals]
            [engine.jobs.apiary :as apiary]
            [engine.jobs.pen :as pen]
            [engine.jobs.util :as u]
            [engine.jobs.watch :as watch]
            [engine.path.near :as near]
            [engine.triggers.pen-gate :as pg]))

(def doc
  "Bring grown animals of type :mob into the pen :box until it holds :target of them. Each animal goes
  on a lead and is walked through the gate one cell at a time (a long drag jams in a 1-wide gate).
  No food is needed.

  Declines while the box already holds :target adults. A started run always passes, so a cut job resumes.
  A body more than 16 blocks from the pen first walks within 12 of it.

  Checks before the first animal (each ends the run with its reason):
  - :no-pen: the pen cannot be read from :box.
  - :leaky: the pen has a leak other than the chosen gate (:leaks lists them).
  - :no-gate: no usable gate. The gate is :gate, else the pen's gate nearest the body. It needs pen floor on
    one side and free floor straight across (a corner gate has none). The four cells straight out from the gate
    must be free floor too (:why :no-approach).
  - :too-shallow: fewer than 5 pen cells in a line inward from the gate.
  - :full: adults already on the pen's cells make :target.

  One round trip per animal:
  - Leash the nearest adult outside the pen (jobs.animals.leash, :radius). Babies, animals in the pen,
    animals given up on and animals already fetched twice and still outside are skipped.
  - Approach the cell 4 out from the gate and wait until the animal rests (up to 8 s).
  - Wait up to 10 s for other adults to leave the cells inside the gate (one info herd.crowded-gate).
  - Line up on the cells 3, 2 and 1 out, open the gate (jobs.access.toggle), then step through the gate
    and 5 cells in. Each stop waits up to 6 s for the animal to rest.
  - An animal still too far behind is pinned. The body steps back one cell and tries again, twice. Then it
    walks out, shuts the gate and starts the trip over once. After that the animal is given up as :jammed and
    let go outside.
  - Shut the gate behind the animal once no adult overlaps its cell, from one cell back.
  - Walk to the pen cell farthest from the gate and let the animal go there (jobs.animals.unleash, lead
    picked up). It counts as brought when it stands on a pen cell. The body empties its hand so the animal
    does not follow the lead out.
  - Walk out. It waits up to 10 s while an adult is within 2 of the gate, then opens, passes and shuts the gate
    in one go. If the gate is left open with the body outside, it shuts it from there. If it stays open, one
    warn herd.gate-open. A shut that leaves the body inside opens the gate again and goes out, twice at most,
    then ends :gate-stuck.
  - Count the adults on the pen's cells before the exit and after the shut. Fewer is an escape (warn
    herd.escaped, counted in :escaped).

  While the gate is open a :gate-held memory entry {:cell [x y z]} is kept fresh, so the pen-gate trigger
  leaves it alone. It is dropped after each shut.

  Lead watching: an animal seen off this body's lead is given up on (:lead-broke), one not seen at all too
  (:lost). With nobody led, the open gate is shut, leads within 8 blocks are picked up
  (jobs.forestry.collect-drops) and leashing starts again once. A second time ends :lost. :timeout-s limits one
  animal from its lead on until it is let go. An end with an animal on the lead lets it go where the body
  stands and shuts the gate from the body's side.

  A round ends only in a safe state: gate shut, body outside the pen and the gate cell, nobody on the lead.
  Between the lead and the shut from outside the round goes on step by step, so no other job interleaves. A
  cut there is possible. A round that starts unsafe restarts from what it sees: led with the body in the pen
  steps on from the nearest axis cell, led with the body outside goes back and lines up again, nobody led
  with the body in the pen goes out, nobody led with the gate open shuts it.

  Ends with info herd.done and result {:reason :inside n :target :brought [keys] :given-up {key reason}
  :gate pos :escaped n}. :inside is counted on the pen's cells at the end. Every reason but :brought and
  :full also gives one warn herd.gave-up. Reasons: :brought (pen holds :target), :short (fewer came), :full,
  :no-pen, :leaky, :no-gate, :too-shallow, :unreachable (a walk was blocked), :gate-stuck, :lost, :timeout,
  or the leash reason when nobody could be led. The body ends outside with the gate shut.")

(def args
  {:mob {:doc "the animal's name, such as \"cow\"" :default nil}
   :box {:doc "the pen: {:min {:x :y :z} :max {:x :y :z}}, inclusive, the feet cells of its floor" :default nil}
   :target {:doc "grown animals of :mob the pen should hold" :default 2}
   :gate {:doc "the fence gate {:x :y :z} to bring them through; the pen's usable gate nearest the body when nil" :default nil}
   :radius {:doc "animals within this many blocks of the body are fetched" :default 24}
   :timeout-s {:doc "seconds one animal may take from its lead on until it is let go" :default 180}})

(def near-pen 16)
(def approach-range 12)
(def watch-radius 64)
(def release-radius 8)
(def regather-radius 8)
(def settle-ms 500)
(def orthogonal [[1 0] [-1 0] [0 1] [0 -1]])

(def leading
  "The phases in which the animal is on the lead and the walk is the body's own: watched before each step, bound
  by :timeout-s."
  #{:approach :clear-out :line-up :open :step :shut-behind :shut-deepest :shut-back :shut-click :retry-back :retry-shut
    :retry-out
    :give-up-walk :deep})

(def out-1 "Index in the axis cells of the cell outside the gate." 3)
(def gate-index "Index in the axis cells of the gate cell." 4)
(def in-1 "Index in the axis cells of the cell inside the gate." 5)
(def deepest "Index in the axis cells of the last position the body steps to (in-5)." 9)

(def settle-out-ms "How long the body waits for the animal to rest at a position outside the gate." 8000)
(def crowd-ms "How long the cells inside the gate may stay crowded before the body goes on." 10000)
(def shut-look-ms "How long a shut waits for the animals to clear the gate cell." 6000)
(def exit-shut-ms "How long the shut from outside waits before stepping back in." 2000)
(def crowd-out-radius 2.5)
(def crowd-in-radius 2.0)
(def held-policy {:cap 4 :ttl 60000})

(defn cell [{:keys [x y z]}] [x y z])

(defn cell-pos [[x y z]] {:x x :y y :z z})

(defn box-centre [{:keys [min max]}]
  {:x (/ (+ (:x min) (:x max) 1) 2) :y (:y min) :z (/ (+ (:z min) (:z max) 1) 2)})

(defn in-box?
  "True when pos stands on a feet cell of the box, floored as engine.jobs.pen/in-pen? floors it."
  [{:keys [min max]} {:keys [x y z]}]
  (let [fx (js/Math.floor x) fy (js/Math.floor (+ y 0.01)) fz (js/Math.floor z)]
    (and (<= (:x min) fx (:x max)) (<= (:y min) fy (:y max)) (<= (:z min) fz (:z max)))))

(defn view-radius
  "How far from the body animals are listed: the fetch radius, plus the way to the pen."
  [c]
  (let [{:keys [radius box]} (:args c)]
    (+ radius (u/dist (u/self-pos c) (box-centre box)) 8)))

(defn adults [c]
  (animals/adults (:primitives c) (:mob (:args c)) (view-radius c)))

(defn check
  "A started run always passes; else :mob and :box are given and the box holds fewer than :target adults."
  [c]
  (let [{:keys [mob box target]} (:args c)]
    (boolean (or (:started (ctx/mem c))
                 (and mob box (< (count (filter #(in-box? box (u/pos-of (.-pos %))) (adults c))) target))))))

(defn read-pen [c]
  (pen/check {:block-at (apiary/block-at-fn (:primitives c)) :box (:box (:args c))}))

(defn in-pen-adults
  "The adults of :mob standing on the pen's cells."
  [c answer]
  (filterv #(pen/in-pen? answer (u/pos-of (.-pos %))) (adults c)))

(defn animal-now [c k]
  (animals/find-by-key (:primitives c) (:mob (:args c)) watch-radius k))

(defn led-now
  "The keys of :led whose animal is on this body's lead now."
  [c]
  (filterv #(some-> (animal-now c %) animals/led-by-me?) (:led (ctx/mem c))))

(def transient-keys "Memory of one wait or walk, dropped when the phase or position moves on." [:walked :settle-from :last-seen :wait-from])

(defn set-phase!
  "Go to phase (the walk and wait state dropped), merging more into the memory."
  ([c phase] (set-phase! c phase nil))
  ([c phase more]
   (ctx/update-mem! c #(-> (apply dissoc % transient-keys) (merge more) (assoc :phase phase)))
   :continue))

(defn gate-cell [c] (cell (:gate (ctx/mem c))))

(defn gate-open? [c]
  (boolean (seq (pg/open-cells (apiary/block-at-fn (:primitives c)) [(gate-cell c)]))))

(defn same-cell? [g entry-cell] (= g (some-> entry-cell vec)))

(defn hold-gate!
  "Tell the pen-gate trigger the open gate is open on purpose: a fresh :gate-held entry, the old one replaced."
  [c]
  (let [g (gate-cell c)]
    (ctx/forget-where! c :gate-held #(same-cell? g (:cell %)))
    (ctx/remember! c :gate-held {:cell g :by (:id c)} held-policy)))

(defn drop-gate! [c]
  (let [g (gate-cell c)]
    (ctx/forget-where! c :gate-held #(same-cell? g (:cell %)))))

;; ------------------------------------------------------------------ the end

(defn finish!
  "Count the pen, emit the outcome, hand it to the parent and end. A nil reason is judged by the count."
  [c reason & [extra]]
  (let [m (ctx/mem c)
        target (:target (:args c))
        inside (set (map animals/key-of (in-pen-adults c (read-pen c))))
        brought (filterv inside (:brought m))
        reason (or reason
                   (cond (>= (count inside) target) :brought
                         (seq brought) :short
                         :else (or (:trouble m) :short)))
        result (merge {:reason reason :inside (count inside) :target target :brought brought
                       :given-up (:given-up m {}) :gate (:gate m) :escaped (:escaped-total m 0)}
                      extra)]
    (when (:gate m) (drop-gate! c))
    (ctx/emit! c :herd.done :info (assoc result :text (str "herd done: " (name reason) ", " (count inside) " of " target " in the pen")))
    (when-not (#{:brought :full} reason)
      (ctx/emit! c :herd.gave-up :warn {:reason reason :text (str "herding stopped: " (name reason))}))
    (ctx/result! c result)
    :done))

(defn end!
  "Give up with reason: the animal on the lead is let go where the body stands, then an open gate shut."
  [c reason]
  (ctx/update-mem! c assoc :ending reason)
  (cond
    (seq (led-now c)) (set-phase! c :let-go)
    (gate-open? c) (set-phase! c :shut-gate)
    :else (finish! c reason)))

(def max-reopens "How often a gate shut with the body inside is opened again for the body to go out." 2)

(def max-shut-retries "How often a gate that will not shut is tried again before the run gives up on it." 2)

(defn escaped-count
  "How many animals left the pen between two counts of the adults on its cells."
  [before after]
  (max 0 (- before after)))

(defn shut-failed!
  "The gate cannot be shut: an animal still led is let go first (phase :let-go, the ending :shut-failed unless one is
  set: :gate-stuck if the gate stays open, judged by the count once it shuts after all), then the shut is tried again
  from :shut-gate, max-shut-retries times; the next failure leaves the gate to the pen-gate trigger with one warn
  herd.gate-open naming it, and the run ends."
  [c]
  (let [m (ctx/mem c)
        retries (:shut-retries m 0)
        [x y z] (gate-cell c)]
    (cond
      (seq (led-now c)) (set-phase! c :let-go {:ending (or (:ending m) :shut-failed)})
      (< retries max-shut-retries) (set-phase! c :shut-gate {:shut-retries (inc retries)})
      :else (do (ctx/emit! c :herd.gate-open :warn {:gate (:gate m)
                                                    :text (str "the gate at " x " " y " " z " could not be shut and stays open")})
                (finish! c (if (= :shut-failed (:ending m)) :gate-stuck (:ending m)))))))

(defn after-shut!
  "The gate is shut (or was not open): end the run when it is ending (a :shut-failed ending is judged by the count, the
  gate having shut after all), else leash again."
  [c]
  (if-let [reason (:ending (ctx/mem c))]
    (finish! c (when-not (= :shut-failed reason) reason))
    (set-phase! c :regather)))

(defn census!
  "Right after the shut from outside: fewer adults on the pen's cells than before the exit are escapes, one warn
  herd.escaped and added to :escaped-total; then leash again."
  [c]
  (let [m (ctx/mem c)
        before (:pen-before m 0)
        after (count (in-pen-adults c (read-pen c)))
        escaped (escaped-count before after)]
    (when (pos? escaped)
      (ctx/emit! c :herd.escaped :warn {:escaped escaped :before before :after after
                                        :text (str escaped " animal(s) escaped through the gate during the exit: " before " in the pen before, " after " after")}))
    (ctx/update-mem! c assoc :escaped-total (+ (:escaped-total m 0) escaped) :pen-before nil)
    (if (:ending m)
      (after-shut! c)
      (set-phase! c :leash))))

(defn ^:async release-step!
  "One round of unleashing (jobs.animals.unleash by key, kept on one animal until that child is done, which
  includes picking its lead up), the next led animal not yet tried after it; on-done once none is left."
  [c on-done]
  (let [m (ctx/mem c)
        k (or (:releasing m) (first (remove (set (:release-tried m)) (led-now c))))]
    (if-not k
      (on-done)
      (do (ctx/update-mem! c assoc :releasing k)
          (when (= :done (await (ctx/call-child c :unleash 'jobs.animals.unleash
                                                {:mob (:mob (:args c)) :animal k :radius release-radius})))
            (ctx/update-mem! c #(-> % (update :release-tried (fnil conj []) k) (dissoc :releasing))))
          :continue))))

;; ------------------------------------------------------------------ the pen and its gate

(defn free-floor?
  "True when an animal can stand in cell: feet and head free, something to stand on."
  [p [x y z]]
  (let [at (fn [dy] (u/block-name p {:x x :y (+ y dy) :z z}))
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
  "How the stepping goes on for a pinned animal at position index: :back (fewer than max-backs backs made: the
  body steps back one position), :retry-from-out (backs made, not retried yet: walk out to out-4 and start over)
  or :give-up."
  [_index backs retried]
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

(defn ^:async survey! [c]
  (let [{:keys [box target]} (:args c)
        centre (box-centre box)]
    (if (> (u/dist (u/self-pos c) centre) near-pen)
      (if (= :blocked (await (near/walk-near! c centre approach-range {:doors :never})))
        (finish! c :unreachable)
        :continue)
      (let [answer (read-pen c)
            {:keys [gate in out] :as chosen} (choose-gate c answer)
            leaks (when chosen (other-leaks answer gate))
            inside (count (in-pen-adults c answer))]
        (cond
          (#{:no-start :unloaded} (:reason answer)) (finish! c :no-pen)
          (nil? chosen) (finish! c :no-gate)
          (not (approach-free? (:primitives c) (cell gate) in out)) (finish! c :no-gate {:why :no-approach})
          (seq leaks) (finish! c :leaky {:leaks (vec (take 12 leaks))})
          (>= inside target) (finish! c :full)
          (< (depth (:inside answer) (cell gate) in) min-depth) (finish! c :too-shallow)
          :else (do (ctx/update-mem! c assoc :phase :leash :gate gate :inside-cell (cell-pos in) :outside-cell (cell-pos out)
                                     :axis (axis-cells (cell gate) in out (depth (:inside answer) (cell gate) in))
                                     :wanted (- target inside))
                    :continue))))))

(defn ^:async set-gate!
  "Put the gate into state (:open or :closed) with jobs.access.toggle (within :reach), then go to phase next, merging
  :then into the memory; :gate-stuck when it did not open, shut-failed! when it did not shut. The :gate-held entry is
  written before an open and dropped after a shut."
  [c state next & [{:keys [reach then] :or {reach 3}}]]
  (when (= :open state) (hold-gate! c))
  (let [r (await (ctx/call-child c :gate 'jobs.access.toggle {:pos (:gate (ctx/mem c)) :state state :reach reach}))]
    (cond
      (not= :done r) (if (= :declined r) :declined :continue)
      (= :done (:status (ctx/child-result c :gate)))
      (do (when (= :closed state) (drop-gate! c))
          (set-phase! c next then))
      (= :open state) (end! c :gate-stuck)
      :else (shut-failed! c))))

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
    (or (in-box? (:box (:args c)) here)
        (and (= gx (js/Math.floor (:x here))) (= gz (js/Math.floor (:z here)))))))

(defn ^:async exit-dash!
  "The exit in one round: walk-near! to out-1 with its own gate handling (open, pass, shut). Shut and outside: on to the
  census. Open and outside: the shut from outside (:shut-out). Still inside, or blocked: by :exit-open, the
  failure counted in :dash-tries."
  [c]
  (hold-gate! c)
  (let [r (await (near/walk-near! c (cell-pos (nth (:axis (ctx/mem c)) out-1)) 0))
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
  (let [r (await (ctx/call-child c :walk 'jobs.movement.go-to {:pos (cell-pos cell) :range 0 :doors :never}))]
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

;; ------------------------------------------------------------------ the animals

(def max-tries 2)

(defn tired-keys
  "The keys of animals leashed max-tries times already (tries {key n}) and not in the pen (a set of keys)."
  [tries pen-keys]
  (into [] (comp (filter #(>= (val %) max-tries)) (map key) (remove pen-keys)) tries))

(defn skip-keys
  "Keys never to leash: babies, animals in the pen, the ones given up on, those already fetched twice and outside."
  [c]
  (let [{:keys [mob radius]} (:args c)
        answer (read-pen c)
        near (animals/herd (:primitives c) mob (+ radius near-pen))
        in-pen? #(pen/in-pen? answer (u/pos-of (.-pos %)))
        skip? #(or (true? (.-baby %)) (in-pen? %))
        pen-keys (into #{} (comp (filter in-pen?) (map animals/key-of)) near)]
    (-> (vec (keys (:given-up (ctx/mem c))))
        (into (comp (filter skip?) (map animals/key-of)) near)
        (into (tired-keys (:tries (ctx/mem c)) pen-keys)))))

(defn ^:async leash-next!
  "Lead one more animal and start its trip, or end when the pen holds enough or none more can be led."
  [c]
  (let [{:keys [mob radius target]} (:args c)
        m (ctx/mem c)]
    (if (>= (count (in-pen-adults c (read-pen c))) target)
      (finish! c nil)
      (let [r (await (ctx/call-child c :leash 'jobs.animals.leash {:mob mob :radius radius :skip (skip-keys c)}))
            res (when (= :done r) (ctx/child-result c :leash))]
        (cond
          (nil? res) :continue
          (= :leashed (:reason res))
          (set-phase! c :approach {:animal (:animal res) :led [(:animal res)] :animal-started (ctx/now c)
                                   :tries (update (:tries m) (:animal res) (fnil inc 0))
                                   :pos-index 0 :backs 0 :retried false :deepest-tried false})
          (and (empty? (:brought m)) (empty? (:given-up m))) (finish! c (:reason res))
          :else (finish! c nil))))))

(defn prune-led!
  "Drop from :led the animals no longer on this body's lead, booking each (:lead-broke seen, :lost not seen);
  how many were dropped."
  [c]
  (let [gone (keep (fn [k] (let [a (animal-now c k)]
                             (cond (nil? a) [k :lost]
                                   (not (animals/led-by-me? a)) [k :lead-broke])))
                   (:led (ctx/mem c)))]
    (when (seq gone)
      (ctx/update-mem! c #(-> %
                              (update :led (fn [led] (vec (remove (set (map first gone)) led))))
                              (update :given-up merge (into {} gone)))))
    (count gone)))

(defn regather-or-lose!
  "Nobody is led any more: shut the gate if it stands open, pick the leads up and start again once, else give up :lost."
  [c]
  (if (:regathered (ctx/mem c))
    (end! c :lost)
    (do (ctx/update-mem! c #(-> %
                                (assoc :regathered true :animal nil)
                                (update :given-up (fn [g] (into {} (remove (fn [[_ r]] (= :lead-broke r))) g)))))
        (set-phase! c (if (gate-open? c) :shut-gate :regather)))))

(defn ^:async regather! [c]
  (let [r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops {:radius regather-radius :filter ["lead"]}))]
    (if (= :done r) (set-phase! c :leash) :continue)))

;; ------------------------------------------------------------------ the trip

(defn released!
  "The led animal is let go: booked :brought when it stands on a pen cell, else given up :outside; on to phase next."
  [c next]
  (let [k (:animal (ctx/mem c))
        there (animal-pos c)
        in? (boolean (and there (pen/in-pen? (read-pen c) there)))]
    (ctx/update-mem! c #(-> %
                            (dissoc :release-tried :releasing)
                            (assoc :led [] :animal nil)
                            (cond-> (and in? (not (some #{k} (:brought %)))) (update :brought (fnil conj []) k))
                            (cond-> (and (not in?) (not (contains? (:given-up %) k))) (update :given-up assoc k :outside))))
    (set-phase! c next)))

(defn pinned!
  "The animal stays too far behind at position index idx: how the stepping goes on (step-in-step)."
  [c idx]
  (let [m (ctx/mem c)
        backs (:backs m 0)]
    (case (step-in-step idx backs (boolean (:retried m)))
      :back (set-phase! c :step {:pos-index (max out-1 (dec idx)) :back-step true :backs (inc backs) :deepest-tried false})
      :retry-from-out (set-phase! c :retry-back {:backs 0 :retried true :deepest-tried false})
      :give-up (set-phase! c :give-up-walk))))

(defn step-position!
  "One round of the step through the gate: walk to the position and settle; settled goes on to the next one (the last
  to the shut), pinned to pinned!, a position reached on a back-step to the one it backed from."
  [c]
  (let [{:keys [pos-index back-step]} (ctx/mem c)]
    (settle-at! c pos-index pinned-ms
                #(cond
                   back-step (set-phase! c :step {:pos-index (inc pos-index) :back-step false})
                   (= :pinned %) (pinned! c pos-index)
                   (= pos-index deepest) (set-phase! c :shut-behind)
                   :else (set-phase! c :step {:pos-index (inc pos-index)})))))

(defn ^:async step! [c phase]
  (let [m (ctx/mem c)
        axis-cell #(nth (:axis m) %)]
    (case phase
      :survey (await (survey! c))
      :regather (await (regather! c))
      :leash (await (leash-next! c))
      :approach (await (settle-at! c 0 settle-out-ms #(set-phase! c :clear-out)))
      :clear-out (await (wait-clear! c (crowded-near? c (axis-cell in-1) crowd-out-radius) crowd-ms :out
                                     #(set-phase! c :line-up {:pos-index 1})))
      :line-up (let [i (:pos-index m)]
                 (await (settle-at! c i settle-out-ms
                                    #(if (= i out-1) (set-phase! c :open) (set-phase! c :line-up {:pos-index (inc i)})))))
      :open (await (set-gate! c :open :step {:then {:pos-index gate-index :back-step false}}))
      :step (await (step-position! c))
      ;; the retry walks out and lines up again with the gate shut: out-1, the shut (an animal on the gate cell is
      ;; waited for, then the gate stays open), out-4
      :retry-back (await (go-then! c (axis-cell out-1) :retry-shut))
      :retry-shut (await (shut-when-clear! c 3 shut-look-ms :retry-out nil #(set-phase! c :retry-out)))
      :retry-out (await (settle-at! c 0 settle-out-ms #(set-phase! c :line-up {:pos-index 1})))
      ;; the animal is clear of the gate cell: step back one cell and shut; one that stays on it is waited for, then
      ;; looked at once more from the deepest cell, else it is as good as pinned
      :shut-behind (cond
                     (not (on-gate? c)) (set-phase! c :shut-back)
                     (< (waited-ms c) (if (:deepest-tried m) 0 shut-look-ms)) (await (look-wait! c))
                     (:deepest-tried m) (pinned! c deepest)
                     :else (set-phase! c :shut-deepest {:deepest-tried true}))
      :shut-deepest (await (go-then! c (axis-cell deepest) :shut-behind))
      :shut-back (await (go-then! c (axis-cell (dec deepest)) :shut-click))
      :shut-click (await (shut-when-clear! c 4 shut-look-ms :deep nil #(set-phase! c :shut-behind)))
      :deep (await (go-then! c (let-go-cell (:inside (read-pen c)) (gate-cell c) (cell (:inside-cell m))) :deep-release))
      :deep-release (await (release-step! c #(released! c :deep-unequip)))
      :deep-unequip (do (await (ctx/act c :unequip #js {})) (set-phase! c :exit))
      :exit (await (go-then! c (axis-cell in-1) :clear-in))
      :clear-in (await (wait-clear! c (or (crowded-near? c (axis-cell in-1) crowd-in-radius) (on-gate? c)) crowd-ms :in
                                    #(set-phase! c :exit-dash {:pen-before (count (in-pen-adults c (read-pen c))) :dash-tries 0 :reopens 0})))
      :exit-dash (await (exit-dash! c))
      :exit-open (await (set-gate! c :open :exit-out))
      :exit-out (await (go-then! c (axis-cell out-1) :shut-out))
      :shut-out (await (shut-when-clear! c 3 exit-shut-ms :shut-side nil
                                         #(if (:stepped-back m)
                                            (shut-failed! c)
                                            (set-phase! c :shut-back-in {:stepped-back true}))))
      :shut-side (shut-side! c)
      :census (census! c)
      :shut-back-in (await (go-then! c (axis-cell in-1) :shut-out))
      :give-up-walk (await (go-then! c (axis-cell 1) :give-up-unleash {:given-up (assoc (:given-up m) (:animal m) :jammed)}))
      :give-up-unleash (await (release-step! c #(released! c :exit-out)))
      :let-go (await (release-step! c #(set-phase! c :shut-gate)))
      ;; a run ending with the body on the pen side goes out first (the exit's own way, then the census ends it)
      :shut-gate (cond
                   (not (gate-open? c)) (after-shut! c)
                   (and (body-in-pen? c) (< (:leaves m 0) max-leaves))
                   (set-phase! c :exit {:leaves (inc (:leaves m 0)) :stepped-back false})
                   :else (await (shut-when-clear! c 3 shut-look-ms :shut-gate nil #(shut-failed! c)))))))

(defn ^:async step-once!
  "One step of the run: the timeout and the lead looked at, the :gate-held entry kept fresh, then the phase's step."
  [c]
  (let [now (ctx/now c)
        {:keys [phase animal-started gate]} (ctx/mem c)
        phase (or phase :survey)
        lead-phase? (contains? leading phase)]
    (when (and gate (gate-open? c)) (hold-gate! c))
    (cond
      (and lead-phase? animal-started (>= (- now animal-started) (* 1000 (:timeout-s (:args c))))) (end! c :timeout)
      (and lead-phase? (pos? (prune-led! c)) (empty? (:led (ctx/mem c)))) (regather-or-lose! c)
      :else (do (when lead-phase? (await (watch/watch! c {})))
                (await (step! c phase))))))

;; ------------------------------------------------------------------ the round: safe at its end

(defn led-by-me
  "The keys of the adults of :mob on this body's lead now, as the world shows them (not the memory)."
  [c]
  (into [] (comp (filter animals/led-by-me?) (map animals/key-of)) (adults c)))

(defn safe?
  "True when the body may be given away: before the survey has chosen the gate (nothing touched yet), else no animal
  on this body's lead, the gate shut and the body neither on a pen cell nor on the gate cell. Read from the world."
  [c]
  (or (nil? (:gate (ctx/mem c)))
      (and (empty? (led-by-me c)) (not (gate-open? c)) (not (body-in-pen? c)))))

(defn nearest-axis-index
  "The index of the axis cell between the gate cell and the deepest step position nearest (flat) to the body."
  [c]
  (let [axis (:axis (ctx/mem c))
        here (u/self-pos c)]
    (apply min-key #(flat-dist here (cell-middle (nth axis %))) (range gate-index (inc (min deepest (dec (count axis))))))))

(defn restart!
  "The round starts unsafe: the last one was cut there (or the world changed). Go on from what the world shows, the
  leads taken from it and the walk and wait state, children included, dropped: a run that was ending ends (end!); an
  animal led with the body in the pen steps on from the nearest axis cell; led with the body outside goes back to
  out-1, shuts the gate and lines up again (the retry's way, its one retry not used up); nobody led with the body in
  the pen goes out (:exit); else the open gate is shut (:shut-gate)."
  [c]
  (let [m (ctx/mem c)
        led (led-by-me c)
        inside? (body-in-pen? c)
        open? (gate-open? c)]
    (ctx/emit! c :herd.restarted :info {:led led :inside inside? :gate-open open? :phase (:phase m)
                                        :text (str "herd restarts from what it sees: " (count led) " led, body "
                                                   (if inside? "in the pen" "outside") ", gate " (if open? "open" "shut"))})
    (ctx/update-mem! c #(-> (apply dissoc % :releasing :release-tried transient-keys)
                            (assoc :children {} :led led :animal (first led))
                            (cond-> (and (seq led) (nil? (:animal-started %))) (assoc :animal-started (ctx/now c)))))
    (cond
      (:ending m) (end! c (:ending m))
      (and (seq led) inside?) (set-phase! c :step {:pos-index (nearest-axis-index c) :back-step false :backs 0
                                                   :deepest-tried false})
      (seq led) (set-phase! c :retry-back {:backs 0 :deepest-tried false})
      inside? (set-phase! c :exit {:stepped-back false})
      :else (set-phase! c :shut-gate))))

(def pace-ms "An unsafe step that changed nothing and took less than this is followed by a wait of settle-ms." 100)

(defn ^:async round
  "Steps until the run ends or the world is safe again (safe?), so the round never ends in the unsafe stretch. A round
  that starts unsafe restarts first (restart!). A step that changed no memory and took under pace-ms is followed by a
  wait, so a step that keeps answering at once never spins."
  [c]
  (ctx/update-mem! c update :started #(or % (ctx/now c)))
  (when-not (safe? c) (restart! c))
  (loop []
    (let [before (ctx/mem c)
          from (ctx/now c)
          r (await (step-once! c))]
      (cond
        (not= :continue r) r
        (safe? c) :continue
        :else (do (when (and (= before (ctx/mem c)) (< (- (ctx/now c) from) pace-ms))
                    (await (ctx/act c :wait #js {:ms settle-ms})))
                  (recur))))))
