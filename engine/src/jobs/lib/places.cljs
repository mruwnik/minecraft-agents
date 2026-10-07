(ns jobs.lib.places
  "Named places in body memory (:bed, :chest, :home, ...). A place is a memory kind named after it, with one
  entry {:pos {:x :y :z}} under engine.memory/place-policy (cap 1, forever); engine.memory/place reads it. A place
  that stopped being true is retracted by an entry {:gone true :was pos}, which has no :pos and so reads as unknown.

  The pure half (names, positions, the block search, what set-place or forget-place would do) takes plain data; the
  rest takes a job ctx: offer! records a place a job found by itself without overwriting a live one, and
  retract-if-missing! drops a recorded place whose block a job found missing."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.util :as u]
            [engine.memory :as mem]))

(def reserved
  "Memory kinds the engine writes for its own use. A place may not take one of these names: the write would replace
  the kind's own cap and ttl. (A kind that exists in memory and is not a place is refused too, see foreign-kind?.)"
  #{:hurt :died :chat :whisper :woke :player-joined :player-left :spawned :respawned :online :offline :disconnected :error :reconnect-failed
    :dependency-patches-missing :world-not-loaded :physics-stalled :picked-up
    :restart :moved :slept :fed :hungry :hostile :hazard :looked :recovered :breathe :extinguish :log-out
    :needs-bed :shelter :dig-in-futile :gate-gave-up :no-bake :no-craft :notify :bed-unreachable :chest-unusable
    :scaffold :heal-ended :recover-trip :sleep-status :bed-placed
    :opened :watched :watch-turned :sleep-failed :bed-place-failed :shelter-trapped :weather-changed})

(def owned-kinds
  "Every unnamespaced memory kind the engine or a job writes for itself, and the place names: jobs.memory.remember
  may not write these. (Namespaced kinds, :job/*, :bred/*, :forestry/*, belong to the engine and its jobs by shape.)"
  (into reserved #{:bed :chest :home :food-source :scaffold-held}))

(def name-pattern #"[a-z][a-z0-9-]{0,31}")

(def min-y -64)
(def max-y 320)
(def max-xz 30000000)

(defn refusal [reason message & {:as more}]
  (merge {:reason reason :message message} more))

;; ------------------------------------------------------------------ names and positions

(defn parse-name
  "{:name keyword} for a keyword or string of 1 to 32 lowercase letters, digits and dashes starting with a letter
  that is not in reserved, else a refusal {:reason :bad-name or :reserved-name :message}."
  [x]
  (let [s (cond (keyword? x) (when (nil? (namespace x)) (name x))
                (string? x) x)]
    (cond
      (not (and s (re-matches name-pattern s)))
      (refusal :bad-name (str "a place name is 1 to 32 lowercase letters, digits and dashes, starting with a letter; got " (pr-str x)))

      (contains? reserved (keyword s))
      (refusal :reserved-name (str s " is a memory kind the engine uses for something else; choose another name"))

      :else {:name (keyword s)})))

(defn coord [v limit]
  (when (and (number? v) (js/isFinite v) (<= (- limit) v limit))
    (js/Math.floor v)))

(defn parse-pos
  "{:pos {:x :y :z}} (floored cell) for [x y z] or {:x :y :z} of finite numbers inside the world, else a refusal
  {:reason :bad-pos :message}."
  [x]
  (let [[px py pz] (cond (and (sequential? x) (= 3 (count x))) x
                         (map? x) [(:x x) (:y x) (:z x)])
        cell [(coord px max-xz) (coord py max-y) (coord pz max-xz)]]
    (if (or (some nil? cell) (< (cell 1) min-y))
      (refusal :bad-pos (str "a position is [x y z] or {:x :y :z}, whole or fractional numbers, y from " min-y " to " max-y
                             "; got " (pr-str x)))
      {:pos (zipmap [:x :y :z] cell)})))

;; ------------------------------------------------------------------ the block search

(defn block-matches?
  "Whether the block called name is the wanted one: the same name, or any *_bed for \"bed\"."
  [wanted name]
  (boolean (and name (or (= wanted name) (and (= wanted "bed") (str/ends-with? name "_bed"))))))

(defn around
  "The cells within 1 of pos (itself excluded), in x, y, z order."
  [{:keys [x y z]}]
  (for [dx [-1 0 1] dy [-1 0 1] dz [-1 0 1]
        :when (not (and (zero? dx) (zero? dy) (zero? dz)))]
    {:x (+ x dx) :y (+ y dy) :z (+ z dz)}))

(defn find-block
  "Where block (a name) stands at pos or within 1 of it. block-at maps a cell to a block name (nil: unloaded).
  Returns {:pos cell :block name} for pos itself when it matches, else for the only matching cell within 1.
  Otherwise a refusal:
  - :not-loaded, pos is unloaded
  - :ambiguous, several cells around pos match and pos does not (:candidates lists them)
  - :no-such-block, none match (:found is what pos holds)"
  [block-at pos block]
  (let [here (block-at pos)]
    (cond
      (nil? here) (refusal :not-loaded (str "the chunk at " (pr-str pos) " is not loaded"))
      (block-matches? block here) {:pos pos :block here}
      :else
      (let [found (->> (around pos)
                       (keep (fn [cell] (let [n (block-at cell)] (when (block-matches? block n) {:pos cell :block n}))))
                       vec)]
        (case (count found)
          0 (refusal :no-such-block (str "no " block " at " (pr-str pos) " or within 1 of it; that cell holds " here) :found here)
          1 (first found)
          (refusal :ambiguous (str (count found) " " block " blocks within 1 of " (pr-str pos) "; give the exact position of one")
                   :candidates (mapv :pos found)))))))

(def block-kinds
  "For the places whose block can be checked, which block names belong to them."
  {:bed #(str/ends-with? % "_bed")
   :chest #{"chest" "trapped_chest" "barrel"}})

(defn gone?
  "Whether a place of kind is gone: its block is loaded (block is a name) and not one of that kind's. A kind whose
  block is not known (:home) is never gone."
  [kind block]
  (boolean (when-let [belongs? (block-kinds kind)]
             (and block (not (belongs? block))))))

;; ------------------------------------------------------------------ what is in memory

(defn place-entry?
  "Whether the latest entry of kind is a place: written under place-policy, with a :pos or retracted (:gone)."
  [view kind]
  (let [data (:data (mem/latest view kind))]
    (boolean (and (= mem/place-policy (mem/policy view kind))
                  (or (:pos data) (:gone data))))))

(defn foreign-kind?
  "Whether kind already holds unexpired memory that is not a place."
  [view kind]
  (and (some? (mem/latest view kind)) (not (place-entry? view kind))))

(defn resolve-set
  "What set-place does for args {:name :pos :block} (nil :pos = standing, a cell) in a memory view: {:name kw :pos
  cell :block name-or-nil}, or a refusal {:reason :message ...}."
  [view block-at standing {:keys [pos block] :as args}]
  (let [n (parse-name (:name args))
        at (if (some? pos) (parse-pos pos) {:pos standing})]
    (cond
      (:reason n) n
      (foreign-kind? view (:name n)) (refusal :not-a-place (str (name (:name n)) " already holds memory that is not a place"))
      (and (some? block) (not (and (string? block) (seq block)))) (refusal :bad-block (str ":block is a block name such as \"white_bed\"; got " (pr-str block)))
      (:reason at) at
      (nil? block) {:name (:name n) :pos (:pos at) :block nil}
      :else (let [found (find-block block-at (:pos at) block)]
              (if (:reason found)
                found
                {:name (:name n) :pos (:pos found) :block (:block found)})))))

(defn resolve-forget
  "What forget-place does for a name in a memory view: {:name kw :was pos}, or a refusal."
  [view place-name]
  (let [n (parse-name place-name)
        data (:data (mem/latest view (:name n)))]
    (cond
      (:reason n) n
      (nil? data) (refusal :no-such-place (str (name (:name n)) " is not a recorded place"))
      (not (place-entry? view (:name n))) (refusal :not-a-place (str (name (:name n)) " is memory that is not a place"))
      :else {:name (:name n) :was (or (:pos data) (:was data))})))

;; ------------------------------------------------------------------ jobs recording and dropping places

(defn offer!
  "A job found a place of kind at pos (anything parse-pos reads) by itself. Record it when none is recorded or the
  recorded one is gone (its block is loaded and not of that kind); one place.set event. A different live recorded
  place is kept, and one place.kept event says which and why. The same place changes nothing."
  [c kind pos]
  (let [offered (:pos (parse-pos pos))
        recorded (mem/place (ctx/view c) kind)]
    (cond
      (nil? offered) nil
      (= offered recorded) nil
      (or (nil? recorded) (gone? kind (u/block-name (:primitives c) recorded)))
      (do (ctx/remember! c kind {:pos offered} mem/place-policy)
          (ctx/emit! c :place.set :info {:name kind :pos offered :was recorded :auto true
                                         :text (str (name kind) " recorded at " (pr-str offered))}))
      :else
      (ctx/emit! c :place.kept :info {:name kind :kept recorded :offered offered
                                      :text (str "kept the recorded " (name kind) " at " (pr-str recorded) ", not " (pr-str offered)
                                                 ": it is still there; move it with jobs.memory.set-place")}))))

(defn retract-if-missing!
  "A job found status (an act's result status) at pos. When it is \"missing\", pos is the recorded place of kind and
  its chunk is loaded, retract the place ({:gone true :was pos}) with one <kind>_missing warn."
  [c kind pos status]
  (let [cell (:pos (parse-pos pos))]
    (when (and (= "missing" status)
               (some? cell)
               (= cell (mem/place (ctx/view c) kind))
               (some? (u/block-name (:primitives c) cell)))
      (ctx/remember! c kind {:gone true :was cell} mem/place-policy)
      (ctx/emit! c (keyword (str (name kind) "_missing")) :warn
                 {:pos cell :text (str "no " (name kind) " at the remembered place " (pr-str cell) "; forgot it")}))))
