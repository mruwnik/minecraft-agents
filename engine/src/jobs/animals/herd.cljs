(ns jobs.animals.herd
  (:require [engine.ctx :as ctx]
            [engine.jobs.animals :as animals]
            [engine.jobs.apiary :as apiary]
            [engine.jobs.pen :as pen]
            [engine.jobs.util :as u]))

(def doc
  "Bring grown animals of the mob type :mob into the pen :box until it holds
  :target of them: leads bring them to the gate, their food lures them through
  it. One convergent run that can be repeated. The check declines while the box
  already holds :target adults (a cheap count by the box's bounds), so a run
  with nothing to do never starts; a started run always passes, so a cut job
  resumes. A body further than 16 blocks from the pen first walks within 12 of
  it (a pen out of sight is never counted). The pen is read with
  engine.jobs.pen over :box: an unreadable one ends :no-pen, a leak other than
  the chosen gate standing open ends :leaky (with :leaks). The gate is :gate,
  else the pen's gate nearest the body, and must have pen floor on one side and
  free floor straight across (a corner gate has not): else :no-gate. Adults
  standing on the pen's cells count; when they already make :target it ends
  :full. With no breeding food of :mob carried (engine.jobs.animals/breeding-food)
  it ends :no-food before any lead is used. Then, one per round,
  jobs.animals.leash (radius :radius) puts a lead on the nearest adult outside
  the pen (babies, animals in the pen and those given up on are skipped) until
  the missing number is led or leashing ends (:no-lead, :none, :unreachable
  ...: with nobody led that is the reason, else the run goes on with fewer).
  The body walks to the cell outside the gate and lets every led animal go
  there (jobs.animals.unleash by uuid, leads picked up): a dragged animal does
  not path and jams in a 1-wide gate. A pen with no animals of :mob opens its
  gate first (jobs.access.toggle, one empty-hand click read back; :gate-stuck
  when it did not open) while they are still on the lead; one with animals
  keeps it shut, they would follow the food out. Every click empties the hand,
  so after the release the body goes back to the outside cell and takes the
  food in hand, and waits up to 15 s for every animal let go to stand within 4
  of it (they strolled off and circled the fence instead of using the gate).
  It opens the gate if that is not done, walks to the lure cell (the pen cell
  farthest from the gate within 5 of it, 2 or more off the line through the
  gate when the pen has one), keeps the food in hand there and the animals walk
  in by their own path and stop about 2.5 short of the body, inside. It waits
  up to :settle-s for every led animal to stand on a pen cell, the food away 5 s
  after every 10 s that did not do it (animals jammed side by side in the gate
  come apart), then goes round the far side of the animals (the mirror of the
  lure cell across the gate line: they trail the body, never between it and
  the gate, where it pushed them into the gate cell) to the cell inside the
  gate with the food still in hand, empties the hand there (unequip; with no
  free slot one warn herd.hand-full and it goes on), opens the gate again if it
  was shut meanwhile, walks out and shuts it from outside: the body ends
  outside, the gate shut, the hand empty. Animals of :mob within about 10 blocks that were not led may
  follow the food in too. Before each step until the gate, an animal seen off
  this body's lead is given up on (:lead-broke), one not seen at all too
  (:lost); when that leaves nobody led, the leads lying within 8 are picked up
  (jobs.forestry.collect-drops) and leashing starts again once, with the
  :lead-broke ones forgiven; a second time ends :lost. :timeout after
  :timeout-s from the first round, counted until the gate. A run that gives up
  with animals on the lead unleashes them where it stands first. Done (info
  herd.done, and one warn herd.gave-up unless the reason is :brought or :full;
  hands over {:reason :inside n :target :brought [keys] :given-up {key reason}
  :gate pos}, :inside counted on the pen's cells at the end) with :reason
  :brought (the pen holds :target), :short (fewer: some came, or none and no
  other reason), :full, :no-pen, :leaky, :no-gate, :no-food, :unreachable (a
  walk was blocked), :gate-stuck, :lost, :timeout, or the leash reason when
  nobody could be led.")

(def args
  {:mob {:doc "the animal's name, such as \"cow\"" :default nil}
   :box {:doc "the pen: {:min {:x :y :z} :max {:x :y :z}}, inclusive, the feet cells of its floor" :default nil}
   :target {:doc "grown animals of :mob the pen should hold" :default 2}
   :gate {:doc "the fence gate {:x :y :z} to bring them through; the pen's usable gate nearest the body when nil" :default nil}
   :radius {:doc "animals within this many blocks of the body are fetched" :default 24}
   :settle-s {:doc "seconds to wait at the lure cell, food in hand (put away for 5 s after every 10 s that did not do it), for the animals let go to walk in" :default 40}
   :timeout-s {:doc "seconds from the first round until the job gives up, counted until the gate" :default 300}})

(def near-pen 16)
(def approach-range 12)
(def watch-radius 64)
(def release-radius 8)
(def regather-radius 8)
(def settle-ms 500)
(def lure-reach 5)
(def orthogonal [[1 0] [-1 0] [0 1] [0 -1]])

(def leading
  "The phases in which animals are on the lead: watched before each step, bound by :timeout-s."
  #{:leash :to-gate :at-gate :open-first})

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

(defn food-in-hand?
  "True when the hand holds the food of :mob."
  [c]
  (let [p (:primitives c)
        food (animals/food-carried p (:mob (:args c)))]
    (boolean (and food (= food (.-held (.self p)))))))

(defn set-phase! [c phase]
  (ctx/update-mem! c assoc :phase phase)
  :continue)

;; ------------------------------------------------------------------ the end

(defn finish!
  "Count the pen, emit the outcome, hand it to the parent and end. A nil reason is judged by the count."
  [c reason & [extra]]
  (let [m (ctx/mem c)
        target (:target (:args c))
        inside (set (map animals/key-of (in-pen-adults c (read-pen c))))
        brought (filterv inside (:led m))
        reason (or reason
                   (cond (>= (count inside) target) :brought
                         (seq brought) :short
                         :else (or (:trouble m) :short)))
        result (merge {:reason reason :inside (count inside) :target target :brought brought
                       :given-up (:given-up m {}) :gate (:gate m)}
                      extra)]
    (ctx/emit! c :herd.done :info (assoc result :text (str "herd done: " (name reason) ", " (count inside) " of " target " in the pen")))
    (when-not (#{:brought :full} reason)
      (ctx/emit! c :herd.gave-up :warn {:reason reason :text (str "herding stopped: " (name reason))}))
    (ctx/result! c result)
    :done))

(defn end!
  "Give up with reason; animals still on the lead are let go first, and food in the hand is put away."
  [c reason]
  (if (or (seq (led-now c)) (food-in-hand? c))
    (do (ctx/update-mem! c assoc :phase :let-go :ending reason) :continue)
    (finish! c reason)))

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

(defn lure-cell
  "The pen cell farthest from gate g within lure-reach of it, off the line through the gate (the cell inside it is
  in) by 2 or more when the pen has one: lured animals stop about 2.5 short of the body, so they stand inside, the
  ones let go outside the gate stay within the food's pull of about 10, and on the way out the body does not walk
  through them (a cow pushed into the gate cell was shut out with the gate)."
  [inside g in]
  (let [[gx _ gz] (cell g)
        [ix _ iz] in
        [dx dz] [(- gx ix) (- gz iz)]
        far (fn [[x _ z]] (+ (* (- x gx) (- x gx)) (* (- z gz) (- z gz))))
        off-line (fn [[x _ z]] (js/Math.abs (- (* (- x gx) dz) (* (- z gz) dx))))]
    (->> inside
         (filter #(<= (far %) (* lure-reach lure-reach)))
         (sort-by (juxt #(if (>= (off-line %) 2) 0 1) (comp - far) identity))
         first)))

(defn via-cell
  "The lure cell mirrored across the line through the gate (the cell inside it is in), when that is a pen cell and
  not the lure cell itself: the animals let in stand between the lure cell and the gate, so the body goes round to
  the far side of them on its way out instead of pushing them into the gate cell."
  [inside g in lure]
  (let [[gx _ gz] (cell g)
        [ix _ iz] in
        [lx ly lz] lure
        mirrored (if (= gz iz) [lx ly (- (* 2 gz) lz)] [(- (* 2 gx) lx) ly lz])]
    (when (and (not= mirrored lure) (contains? inside mirrored)) mirrored)))

(defn other-leaks
  "The leaks of the pen but the chosen gate standing open."
  [answer g]
  (filterv #(not= (cell (:pos %)) (cell g)) (:leaks answer)))

(defn ^:async survey! [c]
  (let [{:keys [mob box target]} (:args c)
        centre (box-centre box)]
    (if (> (u/dist (u/self-pos c) centre) near-pen)
      (if (= :blocked (await (u/walk-near! c centre approach-range)))
        (finish! c :unreachable)
        :continue)
      (let [answer (read-pen c)
            {:keys [gate in out] :as chosen} (choose-gate c answer)
            leaks (when chosen (other-leaks answer gate))
            inside (count (in-pen-adults c answer))]
        (cond
          (#{:no-start :unloaded} (:reason answer)) (finish! c :no-pen)
          (nil? chosen) (finish! c :no-gate)
          (seq leaks) (finish! c :leaky {:leaks (vec (take 12 leaks))})
          (>= inside target) (finish! c :full)
          (nil? (animals/food-carried (:primitives c) mob)) (finish! c :no-food)
          :else (let [lure (lure-cell (:inside answer) gate in)
                      via (via-cell (:inside answer) gate in lure)]
                  (ctx/update-mem! c assoc :phase :leash :gate gate :inside-cell (cell-pos in) :outside-cell (cell-pos out)
                                   :pen-empty (zero? inside) :lure (cell-pos lure) :via (some-> via cell-pos) :wanted (- target inside))
                  :continue))))))

(defn ^:async set-gate!
  "Put the gate into state (:open or :closed) with jobs.access.toggle, then go to phase next; :gate-stuck when the
  toggle did not get it there."
  [c state next]
  (let [r (await (ctx/call-child c :gate 'jobs.access.toggle {:pos (:gate (ctx/mem c)) :state state}))]
    (cond
      (not= :done r) :continue
      (= :done (:status (ctx/child-result c :gate))) (set-phase! c next)
      :else (end! c :gate-stuck))))

(defn ^:async walk-to!
  "Walk within range of pos, then go to phase next; a blocked walk gives up :unreachable."
  [c pos range next]
  (case (await (u/walk-near! c pos range))
    :there (set-phase! c next)
    :partial :continue
    (end! c :unreachable)))

;; ------------------------------------------------------------------ the animals

(defn skip-keys
  "Keys never to leash: babies, animals in the pen, the ones given up on."
  [c]
  (let [{:keys [mob radius]} (:args c)
        answer (read-pen c)
        skip? #(or (true? (.-baby %)) (pen/in-pen? answer (u/pos-of (.-pos %))))]
    (into (vec (keys (:given-up (ctx/mem c))))
          (comp (filter skip?) (map animals/key-of))
          (animals/herd (:primitives c) mob (+ radius near-pen)))))

(defn ^:async leash-next!
  "Lead one more animal, or on to the gate once enough are led or none more can be."
  [c]
  (let [{:keys [mob radius]} (:args c)
        m (ctx/mem c)]
    (if (>= (count (:led m)) (:wanted m))
      (set-phase! c :to-gate)
      (let [r (await (ctx/call-child c :leash 'jobs.animals.leash {:mob mob :radius radius :skip (skip-keys c)}))
            res (when (= :done r) (ctx/child-result c :leash))]
        (cond
          (nil? res) :continue
          (= :leashed (:reason res)) (do (ctx/update-mem! c update :led (fnil conj []) (:animal res)) :continue)
          (empty? (:led m)) (finish! c (:reason res))
          :else (do (ctx/update-mem! c assoc :trouble (:reason res)) (set-phase! c :to-gate)))))))

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
  "Nobody is led any more: pick the leads up and start again once, else give up :lost."
  [c]
  (if (:regathered (ctx/mem c))
    (end! c :lost)
    (do (ctx/update-mem! c #(-> %
                                (assoc :regathered true)
                                (update :given-up (fn [g] (into {} (remove (fn [[_ r]] (= :lead-broke r))) g)))))
        (set-phase! c :regather))))

(defn ^:async regather! [c]
  (let [r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops {:radius regather-radius :filter ["lead"]}))]
    (if (= :done r) (set-phase! c :leash) :continue)))

(defn ^:async hold-food!
  "Take the mob's food in hand unless it is there already (a resumed run may have lost it)."
  [c]
  (let [p (:primitives c)
        food (animals/food-carried p (:mob (:args c)))]
    (when (and food (not= food (.-held (.self p))))
      (await (ctx/act c :equip #js {:item food})))))

(def gather-s 15)
(def gather-near 4)

(defn gather-step
  "Next step of the wait for the animals let go to come round the body at the gate: :go once all are near or
  gather-s seconds are used, else :wait."
  [{:keys [gather-from]} now gather-s all-near?]
  (if (or all-near? (>= (- now (or gather-from now)) (* 1000 gather-s))) :go :wait))

(defn ^:async gather!
  "Food in hand outside the gate: animals let go and then left alone strolled off and, from the far side of the
  fence, circled it instead of using the gate. Wait up to gather-s for all of them to stand within gather-near of
  the body, then on (:in for a pen whose gate is open, else :open-in; they follow the body in)."
  [c]
  (let [m (ctx/mem c)
        now (ctx/now c)
        me (u/self-pos c)
        near? #(some-> (animal-now c %) .-pos u/pos-of (u/dist me) (<= gather-near))]
    (ctx/update-mem! c update :gather-from (fnil identity now))
    (case (gather-step m now gather-s (every? near? (:led m)))
      :go (set-phase! c (if (:pen-empty m) :in :open-in))
      :wait (do (await (hold-food! c))
                (await (ctx/act c :wait #js {:ms settle-ms}))
                :continue))))

(def show-ms 10000)
(def hide-ms 5000)

(defn lure-step
  "What the lure does next: :leave (all in, or :settle-s used up), :hide (the food has been out show-ms with some
  animals still outside), :show (it has been away hide-ms) or :wait. Two animals pressing into a 1-wide gate at
  once jam each other for as long as the food stays out; with it away they stroll apart and come one at a time."
  [{:keys [settle-from cycle-from hidden-at]} now settle-s all-in?]
  (cond
    (or all-in? (>= (- now (or settle-from now)) (* 1000 settle-s))) :leave
    hidden-at (if (>= (- now hidden-at) hide-ms) :show :wait)
    (>= (- now (or cycle-from now)) show-ms) :hide
    :else :wait))

(defn ^:async lure!
  "Food in hand at the lure cell, wait until every led animal stands on a pen cell, at most :settle-s; the food is
  put away for hide-ms after every show-ms that did not bring them all in."
  [c]
  (let [m (ctx/mem c)
        now (ctx/now c)
        answer (read-pen c)
        in? #(some->> (animal-now c %) .-pos u/pos-of (pen/in-pen? answer))
        step (lure-step m now (:settle-s (:args c)) (every? in? (:led m)))]
    (ctx/update-mem! c #(-> % (update :settle-from (fnil identity now)) (update :cycle-from (fnil identity now))))
    (case step
      :leave (set-phase! c (if (:via m) :around :to-gate-in))
      :hide (do (await (ctx/act c :unequip #js {}))
                (ctx/update-mem! c assoc :hidden-at now)
                :continue)
      :show (do (await (hold-food! c))
                (ctx/update-mem! c #(-> % (dissoc :hidden-at) (assoc :cycle-from now)))
                :continue)
      :wait (do (when-not (:hidden-at m) (await (hold-food! c)))
                (await (ctx/act c :wait #js {:ms settle-ms}))
                :continue))))

(defn ^:async calm!
  "Put the food away so nothing follows the body out; a hand that cannot be emptied is warned about once."
  [c]
  (let [r (await (ctx/act c :unequip #js {}))]
    (when (= "full" (.-status r))
      (ctx/emit! c :herd.hand-full :warn {:text "no free slot: the food stays in hand on the way out"}))
    (set-phase! c :open-out)))

;; ------------------------------------------------------------------ the round

(defn ^:async step! [c phase]
  (let [m (ctx/mem c)]
    (case phase
      :survey (await (survey! c))
      :regather (await (regather! c))
      :leash (await (leash-next! c))
      :to-gate (await (walk-to! c (:outside-cell m) 0 :at-gate))
      ;; a round of its own, so leads that broke on the walk are seen before the release
      ;; a pen with no animals of its own opens first, while they are still on the lead: nothing can walk out after
      ;; the food, and the animals let go are drawn straight in. A pen with animals of its own stays shut until
      ;; the ones let go have gathered round the food (its own press to the fence inside), then opens
      :at-gate (set-phase! c (if (:pen-empty m) :open-first :release-out))
      :open-first (await (set-gate! c :open :release-out))
      :release-out (await (release-step! c #(set-phase! c :back-out)))
      ;; letting go walks to each animal wherever it stands: the food comes out at the gate, where they gather
      :back-out (await (walk-to! c (:outside-cell m) 0 :hold-out))
      ;; animals let go and left alone for ~15 s strolled off and, out of the food's range or on the far side of
      ;; the fence, circled it instead of using the gate: letting go emptied the hand, so the food comes out again
      :hold-out (do (await (hold-food! c)) (set-phase! c :gather))
      :gather (await (gather! c))
      :open-in (await (set-gate! c :open :in))
      :in (await (walk-to! c (:lure m) 0 :lure))
      :lure (await (lure! c))
      ;; the food stays in hand round the animals let in, to the far side of them: they trail the body, never
      ;; between it and the gate, where the body pushed them into the gate cell and shut them out. It goes away
      ;; at the cell inside the gate (live: putting it away on the far side instead brought fewer in, 2 of 8 runs
      ;; against 5 of 10)
      :around (await (walk-to! c (:via m) 0 :to-gate-in))
      :to-gate-in (await (walk-to! c (:inside-cell m) 0 :calm))
      :calm (await (calm! c))
      :open-out (await (set-gate! c :open :leave))
      :leave (await (walk-to! c (:outside-cell m) 0 :shut))
      :shut (await (set-gate! c :closed :census))
      :census (finish! c nil)
      :let-go (await (release-step! c #(set-phase! c :put-away)))
      :put-away (do (when (food-in-hand? c) (await (ctx/act c :unequip #js {})))
                    (finish! c (:ending (ctx/mem c)))))))

(defn ^:async round [c]
  (let [now (ctx/now c)]
    (ctx/update-mem! c update :started #(or % now))
    (let [{:keys [phase started]} (ctx/mem c)
          phase (or phase :survey)
          lead-phase? (contains? leading phase)]
      (cond
        (and lead-phase? (>= (- now started) (* 1000 (:timeout-s (:args c))))) (end! c :timeout)
        (and lead-phase? (pos? (prune-led! c)) (empty? (:led (ctx/mem c)))) (regather-or-lose! c)
        :else (await (step! c phase))))))
