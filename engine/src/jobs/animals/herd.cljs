(ns jobs.animals.herd
  (:require [engine.ctx :as ctx]
            [engine.jobs.animals :as animals]
            [engine.jobs.apiary :as apiary]
            [engine.jobs.pen :as pen]
            [engine.jobs.util :as u]))

(def doc
  "Bring grown animals of the mob type :mob into the pen :box on leads until it
  holds :target of them: one convergent run that can be repeated. The check
  declines while the box already holds :target adults (a cheap count by the
  box's bounds), so a run with nothing to do never starts; a started run always
  passes, so a cut job resumes. A body further than 16 blocks from the pen first
  walks within 12 of it (a pen out of sight is never counted). The pen is read
  with engine.jobs.pen over :box: an unreadable one ends :no-pen, a leak other
  than the chosen gate standing open ends :leaky (with :leaks). The gate is :gate,
  else the pen's gate nearest the body, and must have pen floor on one side and
  free floor straight across (a corner gate has not): else :no-gate. Adults
  standing on the pen's cells count; when they already make :target it ends
  :full. Then, one per round, jobs.animals.leash (radius :radius) puts a lead on
  the nearest adult outside the pen (babies, animals in the pen and those given
  up on are skipped) until the missing number is led or leashing ends (:no-lead,
  :none, :unreachable ...: with nobody led that is the reason, else the run goes
  on with fewer). The body walks to the cell outside the gate, opens it (jobs.access.toggle,
  one empty-hand click read back; :gate-stuck when it did not open), walks to the pen cell
  farthest from the gate, waits up to :settle-s for every led animal to stand on
  a pen cell, unleashes each (jobs.animals.unleash by uuid, leads picked up),
  walks back to the cell inside the gate, opens it again if it was shut
  meanwhile, walks out and shuts it from outside: the body ends outside, the gate
  shut. Before each step until the release, an animal seen off this body's lead
  is given up on (:lead-broke), one not seen at all too (:lost); when that leaves
  nobody led, the leads lying within 8 are picked up (jobs.forestry.collect-drops)
  and leashing starts again once, with the :lead-broke ones forgiven; a second
  time ends :lost. :timeout after :timeout-s from the first round, counted until
  the release. A run that gives up with animals on the lead unleashes them where
  it stands first. Done (info herd.done, and one warn herd.gave-up unless the
  reason is :brought or :full; hands over {:reason :inside n :target :brought
  [keys] :given-up {key reason} :gate pos}, :inside counted on the pen's cells at
  the end) with :reason :brought (the pen holds :target), :short (fewer: some
  came, or none and no other reason), :full, :no-pen, :leaky, :no-gate,
  :unreachable (a walk was blocked), :gate-stuck, :lost, :timeout, or the leash
  reason when nobody could be led.")

(def args
  {:mob {:doc "the animal's name, such as \"cow\"" :default nil}
   :box {:doc "the pen: {:min {:x :y :z} :max {:x :y :z}}, inclusive, the feet cells of its floor" :default nil}
   :target {:doc "grown animals of :mob the pen should hold" :default 2}
   :gate {:doc "the fence gate {:x :y :z} to bring them through; the pen's usable gate nearest the body when nil" :default nil}
   :radius {:doc "animals within this many blocks of the body are fetched" :default 24}
   :settle-s {:doc "seconds to wait at the far side of the pen for the led animals to come in" :default 8}
   :timeout-s {:doc "seconds from the first round until the job gives up, counted until the release" :default 300}})

(def near-pen 16)
(def approach-range 12)
(def watch-radius 64)
(def release-radius 8)
(def regather-radius 8)
(def settle-ms 500)
(def orthogonal [[1 0] [-1 0] [0 1] [0 -1]])

(def leading
  "The phases in which animals are on the lead: watched before each step, bound by :timeout-s."
  #{:leash :to-gate :open-in :in :settle})

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
  "Give up with reason; animals still on the lead are let go first."
  [c reason]
  (if (seq (led-now c))
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

(defn deepest
  "The pen cell farthest from gate g: led animals stop behind the body, so they end inside, not in the gateway."
  [inside g]
  (let [[gx _ gz] (cell g)
        far (fn [[x _ z]] (+ (* (- x gx) (- x gx)) (* (- z gz) (- z gz))))]
    (first (sort-by (juxt (comp - far) identity) inside))))

(defn other-leaks
  "The leaks of the pen but the chosen gate standing open."
  [answer g]
  (filterv #(not= (cell (:pos %)) (cell g)) (:leaks answer)))

(defn ^:async survey! [c]
  (let [{:keys [box target]} (:args c)
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
          :else (do (ctx/update-mem! c assoc :phase :leash :gate gate :inside-cell (cell-pos in) :outside-cell (cell-pos out)
                                     :deep (cell-pos (deepest (:inside answer) gate)) :wanted (- target inside))
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

(defn ^:async settle!
  "Wait at the far side until every led animal stands on a pen cell, at most :settle-s."
  [c]
  (let [m (ctx/mem c)
        now (ctx/now c)
        answer (read-pen c)
        in? #(some->> (animal-now c %) .-pos u/pos-of (pen/in-pen? answer))
        waited (- now (:settle-from m now))]
    (if (or (every? in? (:led m)) (>= waited (* 1000 (:settle-s (:args c)))))
      (set-phase! c :release)
      (do (ctx/update-mem! c update :settle-from #(or % now))
          (await (ctx/act c :wait #js {:ms settle-ms}))
          :continue))))

;; ------------------------------------------------------------------ the round

(defn ^:async step! [c phase]
  (let [m (ctx/mem c)]
    (case phase
      :survey (await (survey! c))
      :regather (await (regather! c))
      :leash (await (leash-next! c))
      :to-gate (await (walk-to! c (:outside-cell m) 0 :open-in))
      :open-in (await (set-gate! c :open :in))
      :in (await (walk-to! c (:deep m) 1 :settle))
      :settle (await (settle! c))
      :release (await (release-step! c #(set-phase! c :to-gate-in)))
      :to-gate-in (await (walk-to! c (:inside-cell m) 0 :open-out))
      :open-out (await (set-gate! c :open :leave))
      :leave (await (walk-to! c (:outside-cell m) 0 :shut))
      :shut (await (set-gate! c :closed :census))
      :census (finish! c nil)
      :let-go (await (release-step! c #(finish! c (:ending (ctx/mem c))))))))

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
