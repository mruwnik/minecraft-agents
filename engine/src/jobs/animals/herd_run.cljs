(ns jobs.animals.herd-run
  "The state of a herd run: its constants, the ending and what the animals on the lead did, shared by the pen, the trip and the job."
  (:require [jobs.lib.animals :as animals]
            [jobs.lib.apiary :as apiary]
            [engine.ctx :as ctx]
            [jobs.lib.pen :as pen]
            [triggers.animals.pen-gate :as pg]
            [jobs.lib.util :as u]))

(def near-pen 16)
(def approach-range 12)
(def watch-radius 64)
(def release-radius 8)
(def regather-radius 8)
(def settle-ms 500)
(def orthogonal [[1 0] [-1 0] [0 1] [0 -1]])

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

(defn view-radius
  "How far from the body animals are listed: the fetch radius, plus the way to the pen."
  [c]
  (let [{:keys [radius box]} (:args c)]
    (+ radius (u/dist (u/self-pos c) (box-centre box)) 8)))

(defn adults [c]
  (animals/adults (:primitives c) (:mob (:args c)) (view-radius c)))

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
                         :else :short))
        result (merge {:reason reason :inside (count inside) :target target :brought brought
                       :given-up (:given-up m {}) :gate (:gate m) :escaped (:escaped-total m 0)}
                      extra)]
    (when (:gate m) (drop-gate! c))
    (ctx/emit! c :herd.done :info (assoc result :text (str "herd done: " (name reason) ", " (count inside) " of " target " in the pen")))
    (when-not (#{:brought :full} reason)
      (ctx/emit! c :herd.gave-up :warn {:reason reason :text (str "herding stopped: " (name reason))}))
    (ctx/result! c result)
    (when (and (empty? brought) (not (#{:brought :full} reason)))
      (throw (ex-info (str "herd brought none of the " target " wanted: " (name reason) ", " (count inside) " in the pen"
                           (when (seq (:given-up m)) (str ", given up " (pr-str (:given-up m)))))
                      result)))
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

(def max-release-declines "How often the unleash child of one animal may decline before the animal is given up on." 3)

(defn ^:async release-step!
  "One round of unleashing (jobs.animals.unleash by key, kept on one animal until that child is done, which
  includes picking its lead up), the next led animal not yet tried after it; on-done once none is left. A child
  that declines max-release-declines times gives the animal up as :declined."
  [c on-done]
  (let [m (ctx/mem c)
        k (or (:releasing m) (first (remove (set (:release-tried m)) (led-now c))))]
    (if-not k
      (on-done)
      (do (ctx/update-mem! c assoc :releasing k)
          (let [r (await (ctx/call-child c :unleash 'jobs.animals.unleash
                                         (cond-> {:mob (:mob (:args c)) :animal k :radius release-radius}
                                           (:ignore-zones? (:args c)) (assoc :ignore-zones? true))))
                reason (when (= :done r) (:reason (ctx/child-result c :unleash)))
                declines (cond-> (:release-declines (ctx/mem c) 0) (= :declined r) inc)]
            (cond
              (= :done r)
              (ctx/update-mem! c #(-> %
                                      (update :release-tried (fnil conj []) k)
                                      (cond-> (not= :unleashed reason) (update :unleash-failed assoc k (or reason :refused)))
                                      (dissoc :releasing :release-declines)))
              (>= declines max-release-declines)
              (ctx/update-mem! c #(-> %
                                      (update :release-tried (fnil conj []) k)
                                      (update :unleash-failed assoc k :declined)
                                      (dissoc :releasing :release-declines)))
              :else (ctx/update-mem! c assoc :release-declines declines)))
          :continue))))
