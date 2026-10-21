(ns jobs.animals.herd-run
  "The state of a herd run: its constants, the ending and what the animals on the lead did, shared by the pen, the trip and the job."
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.settings :as settings]
            [jobs.lib.animals :as animals]
            [jobs.lib.bait :as bait]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.pen :as pen]
            [jobs.lib.pen-gate :as pg]
            [jobs.lib.pen-read :as pen-read]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::near-pen {:default 16 :doc "Blocks from the pen within which the body counts as near it." :spec (a/int-in 0 nil)}
   ::approach-range {:default 12 :doc "Blocks from the animal at which the body starts to approach." :spec (a/int-in 0 nil)}
   ::listed-range {:default 32 :doc "Blocks from the pen within which an animal known in it that is not listed is gone (dead or out); farther the server lists no animals there." :spec (a/int-in 0 nil)}
   ::watch-radius {:default 64 :doc "Blocks within which animals are watched." :spec (a/int-in 0 nil)}
   ::release-radius {:default 8 :doc "Blocks within which a released animal counts as near." :spec (a/int-in 0 nil)}
   ::follow-range {:default 10 :doc "Blocks within which an animal follows the food in the body's hand." :spec (a/int-in 0 nil)}
   ::regather-radius {:default 8 :doc "Blocks within which strays are gathered again." :spec (a/int-in 0 nil)}
   ::settle-ms {:default 500 :doc "How long the body waits for an animal to settle, in ms." :spec (a/int-in 0 nil)}
   ::settle-out-ms {:default 8000 :doc "How long the body waits for the animal to rest at a position outside the gate." :spec (a/int-in 0 nil)}
   ::crowd-ms {:default 10000 :doc "How long the cells inside the gate may stay crowded before the body goes on." :spec (a/int-in 0 nil)}
   ::shut-look-ms {:default 6000 :doc "How long a shut waits for the animals to clear the gate cell." :spec (a/int-in 0 nil)}
   ::exit-shut-ms {:default 2000 :doc "How long the shut from outside waits before stepping back in." :spec (a/int-in 0 nil)}
   ::crowd-out-radius {:default 2.5 :doc "Radius of crowding outside the gate, in blocks." :spec (a/num-in 0 nil)}
   ::crowd-in-radius {:default 2.0 :doc "Radius of crowding inside the gate, in blocks." :spec (a/num-in 0 nil)}
   ::max-reopens {:default 2 :doc "How often a gate shut with the body inside is opened again for the body to go out." :spec (a/int-in 0 nil)}})
(defn near-pen [] (settings/get settings ::near-pen))
(defn approach-range [] (settings/get settings ::approach-range))
(defn watch-radius [] (settings/get settings ::watch-radius))
(defn release-radius [] (settings/get settings ::release-radius))
(defn follow-range [] (settings/get settings ::follow-range))
(defn regather-radius [] (settings/get settings ::regather-radius))
(defn settle-ms [] (settings/get settings ::settle-ms))
(def orthogonal [[1 0] [-1 0] [0 1] [0 -1]])

(def out-1 "Index in the axis cells of the cell outside the gate." 3)
(def gate-index "Index in the axis cells of the gate cell." 4)
(def in-1 "Index in the axis cells of the cell inside the gate." 5)
(def deepest "Index in the axis cells of the last position the body steps to (in-5)." 9)

(defn stage
  "Index in the axis cells of the cell the animal is lured or led to before the gate: out-4 (0) until a walk to it fails,
  then each cell nearer the gate."
  [c]
  (:stage (ctx/mem c) 0))

(defn settle-out-ms [] (settings/get settings ::settle-out-ms))
(defn crowd-ms [] (settings/get settings ::crowd-ms))
(defn shut-look-ms [] (settings/get settings ::shut-look-ms))
(defn exit-shut-ms [] (settings/get settings ::exit-shut-ms))
(defn crowd-out-radius [] (settings/get settings ::crowd-out-radius))
(defn crowd-in-radius [] (settings/get settings ::crowd-in-radius))

(defn cell [{:keys [x y z]}] [x y z])

(defn pen-box
  "The :box arg as {:min {:x :y :z} :max {:x :y :z}}, whichever form its corners were given in."
  [c]
  (pen-read/arg-box c))

(defn box-centre [{:keys [min max]}]
  {:x (/ (+ (:x min) (:x max) 1) 2) :y (:y min) :z (/ (+ (:z min) (:z max) 1) 2)})

(defn view-radius
  "How far from the body animals are listed: the fetch radius, plus the way to the pen."
  [c]
  (+ (:radius (:args c)) (u/dist (u/self-pos c) (box-centre (pen-box c))) 8))

(defn adults [c]
  (animals/adults (:primitives c) (:mob (:args c)) (view-radius c)))

(defn pen-opts [c]
  {:box (pen-box c)
   :near (let [{:keys [x y z]} (u/self-pos c)] [(Math/floor x) (Math/floor y) (Math/floor z)])})

(defn read-pen [c]
  (pen-read/read (:primitives c) (pen-opts c)))

(defn in-pen-adults
  "The adults of :mob standing on the pen's cells."
  [c answer]
  (filterv #(pen/in-pen? answer (u/pos-of (.-pos %))) (adults c)))

(defn animal-now [c k]
  (animals/find-by-key (:primitives c) (:mob (:args c)) (watch-radius) k))

(defn lure? [c] (= :lure (:method (ctx/mem c))))

(defn held-by-me?
  "True when the animal is on this body's lead, or in a lure run follows the food held in hand (within follow-range)."
  [c e]
  (if (lure? c)
    (and (bait/holds-food? c)
         (<= (u/dist (u/self-pos c) (u/pos-of (.-pos e))) (follow-range)))
    (animals/led-by-me? e)))

(defn led-now
  "The keys of :led whose animal is on this body's lead (or follows the food in hand) now."
  [c]
  (filterv #(some->> (animal-now c %) (held-by-me? c)) (:led (ctx/mem c))))

(def transient-keys "Memory of one wait or walk, dropped when the phase or position moves on." [:walked :settle-from :last-seen :wait-from :holding])

(defn set-phase!
  "Go to phase (the walk and wait state dropped, a pause's hold too), merging more into the memory."
  ([c phase] (set-phase! c phase nil))
  ([c phase more]
   (when (:holding (ctx/mem c)) (ctx/hold-still! c nil))
   (ctx/update-mem! c #(-> (apply dissoc % transient-keys) (merge more) (assoc :phase phase)))
   :continue))

(defn ^:async pause!
  "Hold still settle-ms on purpose, for why (a jobs.time.noop child): the animal settles, a cell clears. :continue."
  [c why]
  (ctx/update-mem! c assoc :holding why)
  (await (child/pause! c (/ (settle-ms) 1000) why))
  :continue)

(defn gate-cell [c] (cell (:gate (ctx/mem c))))

(defn gate-open?
  "True unless the gate is seen shut or the last toggle of this run found it closed (:gate-closed, dropped when the run
  opens it): a gate out of sight is remembered as it was last seen, and an unknown one is never taken for shut."
  [c]
  (and (not (:gate-closed (ctx/mem c)))
       (not= :shut (pg/gate-state (:primitives c) (gate-cell c)))))

(defn hold-gate!
  "Tell the pen-gate trigger the open gate is open on purpose (pg/hold!)."
  [c]
  (ctx/update-mem! c dissoc :gate-closed)
  (pg/hold! c (gate-cell c)))

(defn drop-gate! [c] (pg/release! c (gate-cell c)))

;; ------------------------------------------------------------------ the end

(def condition-reasons
  "Reasons that say the pen, the food or the zones will not do: a run that ends on one short of its least is condition-failed."
  #{:no-pen :leaky :no-gate :refused :too-shallow :no-food :no-zones})

(defn ending-kind
  "The kind of a run that ended for reason with n animals brought: :done when the pen holds the target (:brought,
  :full) or n reached least, else condition-failed for condition-reasons and nothing-possible for the rest."
  [reason n least]
  (cond
    (or (#{:brought :full} reason) (>= n least)) :done
    (condition-reasons reason) :condition-failed
    :else :nothing-possible))

(defn pen-keys
  "The keys of the adults in the pen: those on its cells as sensed, plus those known in it (in at the start or brought)
  that the sensing does not show while the body is farther than ::listed-range from the pen (near it, an animal not listed is dead or out)."
  [c answer]
  (let [m (ctx/mem c)
        sensed (into #{} (map animals/key-of) (in-pen-adults c answer))
        far? (> (u/dist (u/self-pos c) (box-centre (pen-box c))) (settings/get settings ::listed-range))
        unseen? #(and far? (nil? (animals/find-by-key (:primitives c) (:mob (:args c)) (watch-radius) %)))]
    (into sensed (filter unseen?) (concat (:in-pen-start m) (:brought m)))))

(defn pen-count-text
  "\"2 of 3 in the pen\"; past the target, \"4 in the pen, 3 wanted\"."
  [n target]
  (if (> n target) (str n " in the pen, " target " wanted") (str n " of " target " in the pen")))

(defn end-text
  "The ending text of the herd run: the count and why the others did not come."
  [c reason n]
  (let [{:keys [mob target radius]} (:args c)
        given-up (:given-up (ctx/mem c))]
    (str "herd done: " (name reason) ", " (pen-count-text n target)
         (cond
           (seq given-up) (str " (given up: " (str/join ", " (map (fn [[k r]] (str k " " (name r))) given-up)) ")")
           (= :no-food reason) (str " (no " (str/join " or " (get animals/breeding-food mob)) " carried, none could be got)")
           (= :short reason) (str " (no other grown " mob " to fetch within " radius " blocks)")))))

(defn detail-text
  "\" (why at cell, depth)\" for a result that names the cause of a refusal, else nil."
  [result]
  (let [leak (first (:leaks result))
        cell (or (:cell result) (some-> leak :pos cell))]
    (when (or (:why result) cell)
      (str " (" (some-> (or (:why result) (:why leak)) name) (when cell (str " at " (pr-str cell)))
           (when (:need result) (str ", " (:depth result) " cells in a line, " (:need result) " needed")) ")"
           (when (:walk result) (str ": " (:walk result)))))))

(defn finish!
  "Count the pen, emit the outcome and end with its ending (ending-kind; the call's :least, default 1, of animals
  brought makes it done). A nil reason is judged by the count."
  [c reason & [extra]]
  (let [m (ctx/mem c)
        target (:target (:args c))
        least (let [l (:least (ctx/frozen-call c) 1)] (if (= :all l) target l))
        answer (read-pen c)
        inside (pen-keys c answer)
        rows (animals/in-pen-rows (:primitives c) answer (watch-radius))
        brought (filterv inside (:brought m))
        reason (or reason
                   (cond (>= (count (remove (set (keys (:given-up m))) inside)) target) :brought
                         (and (empty? (:brought m)) (seq (:given-up m)) (every? #{:no-exit} (vals (:given-up m)))) :no-exit
                         :else :short))
        kind (ending-kind reason (count brought) least)
        text (str (end-text c reason (count inside)) (detail-text extra))
        result (merge {:reason reason :inside (count inside) :target target :brought brought
                       :given-up (:given-up m {}) :gate (:gate m) :escaped (:escaped-total m 0) :animals rows}
                      extra)]
    (ctx/update-mem! c assoc :finished true)
    (when (:gate m) (drop-gate! c))
    (ctx/end c kind (when-not (= :done kind) (if (seq extra) (assoc extra :reason reason) reason)) (assoc result :text text))))

(defn end!
  "Give up with reason: the animal on the lead is let go where the body stands, then an open gate shut."
  [c reason]
  (ctx/update-mem! c assoc :ending reason)
  (cond
    (seq (led-now c)) (set-phase! c :let-go)
    (gate-open? c) (set-phase! c :shut-gate)
    :else (finish! c reason)))

(defn max-reopens [] (settings/get settings ::max-reopens))

(defn escaped-count
  "How many animals left the pen between two counts of the adults on its cells."
  [before after]
  (max 0 (- before after)))

(defn shut-failed!
  "The gate cannot be shut: an animal still led is let go first (phase :let-go, the ending :shut-failed unless one is
  set: :gate-stuck if the gate stays open, judged by the count once it shuts after all), then the shut is tried again
  from :shut-gate, the shared fruitless tries; the next failure leaves the gate to the pen-gate trigger with one warn
  herd.gate-open naming it, and the run ends."
  [c]
  (let [m (ctx/mem c)
        [x y z] (gate-cell c)]
    (cond
      (seq (led-now c)) (set-phase! c :let-go {:ending (or (:ending m) :shut-failed)})
      (not (child/exhausted? c :shut-gate)) (do (child/fruitless! c :shut-gate) (set-phase! c :shut-gate))
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
  "One round of unleashing (in a lure run: the food put away, so nothing follows; jobs.animals.unleash by key, kept on one animal until that child ends, which
  includes picking its lead up), the next led animal not yet tried after it; on-done once none is left. A child that
  ends without freeing it books the animal under :unleash-failed with the child's reason."
  [c on-done]
  (let [m (ctx/mem c)
        k (or (:releasing m) (first (remove (set (:release-tried m)) (led-now c))))]
    (cond
      (not k) (on-done)
      (lure? c) (do (await (bait/put-food-away! c)) (on-done))
      :else
      (do (ctx/update-mem! c assoc :releasing k)
          (let [e (await (ctx/call c :unleash 'jobs.animals.unleash
                                   (cond-> {:mob (:mob (:args c)) :animal k :radius (release-radius)}
                                     (:ignore-zones? (:args c)) (assoc :ignore-zones? true))))
                reason (if (= :done (:kind e)) (get-in e [:data :reason]) (contract/cause-key e))]
            (when-not (= :continue e)
              (ctx/update-mem! c #(-> %
                                      (update :release-tried (fnil conj []) k)
                                      (cond-> (not= :unleashed reason) (update :unleash-failed assoc k (or reason :refused)))
                                      (dissoc :releasing)))))
          :continue))))
