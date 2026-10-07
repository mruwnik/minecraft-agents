(ns jobs.lib.animals
  "Helpers for the animal jobs: what each mob breeds on, and the herd near the body."
  (:require [engine.ctx :as ctx]
            [jobs.lib.gate :as gate]
            [jobs.lib.near :as near]
            [jobs.lib.util :as u]))

(defn in-box?
  "True when pos stands on a feet cell of the box ({:min :max}, cells, inclusive), floored as jobs.lib.pen/in-pen? floors it."
  [{:keys [min max]} {:keys [x y z]}]
  (let [fx (js/Math.floor x) fy (js/Math.floor (+ y 0.01)) fz (js/Math.floor z)]
    (and (<= (:x min) fx (:x max)) (<= (:y min) fy (:y max)) (<= (:z min) fz (:z max)))))

(def breeding-food
  "Mob name -> the foods that put it in love, in the order the body prefers them."
  {"cow" ["wheat"]
   "mooshroom" ["wheat"]
   "sheep" ["wheat"]
   "goat" ["wheat"]
   "pig" ["carrot" "potato" "beetroot"]
   "chicken" ["wheat_seeds" "melon_seeds" "pumpkin_seeds" "beetroot_seeds" "torchflower_seeds"]
   "rabbit" ["carrot" "golden_carrot" "dandelion"]
   "bee" ["dandelion" "poppy" "blue_orchid" "allium" "azure_bluet" "red_tulip" "orange_tulip"
          "white_tulip" "pink_tulip" "oxeye_daisy" "cornflower" "lily_of_the_valley" "sunflower"
          "lilac" "rose_bush" "peony" "torchflower" "pink_petals" "wildflowers"]})

(defn food-carried
  "The first food of mob's list the body carries, else nil."
  [p mob]
  (let [have (into #{} (map :name) (u/inventory p))]
    (first (filter have (get breeding-food mob)))))

(defn herd
  "The JS entities named mob within radius of the body, nearest first, as a vector."
  [p mob radius]
  (vec (array-seq (.entities p #js {:radius radius :names #js [mob] :max 64}))))


(defn allowed
  "The animals of es (JS entities) the zone rules let the job act on with action (:harvest :take), judged at the cell
  each stands in. One decline warn of kind (for job-name) names what refuses, or :no-zones. :ignore-zones? lets all.
  The last verdict is kept in the job's memory (see refusal)."
  [c kind job-name action es]
  (let [cell-of (fn [e] (let [{:keys [x y z]} (u/pos-of (.-pos e))]
                          {:x (js/Math.floor x) :y (js/Math.floor (+ y 0.01)) :z (js/Math.floor z)}))
        {:keys [allowed refusal]} (gate/judge c kind job-name action (map cell-of es))
        ok (set allowed)]
    (ctx/update-mem! c #(if refusal (assoc % :refusal refusal) (dissoc % :refusal)))
    (filterv #(contains? ok (cell-of %)) es)))

(defn refusal
  "Why the job found nothing to act on, when the zone rules refused animals: :refused, :no-zones, else nil."
  [c]
  (:refusal (ctx/mem c)))

(defn adults
  "The herd's members that are not babies."
  [p mob radius]
  (filterv #(not (true? (.-baby %))) (herd p mob radius)))

(defn babies
  "The herd's babies."
  [p mob radius]
  (filterv #(true? (.-baby %)) (herd p mob radius)))

(defn key-of
  "The key an animal is tracked by: its uuid (entity ids change when chunks
  reload), else its id."
  [e]
  (or (.-uuid e) (.-id e)))

(defn leashed?
  "True when the sensing says the animal is on a lead (to anyone)."
  [e]
  (true? (.-leashed e)))

(defn led-by-me?
  "True when the animal is on this body's lead."
  [e]
  (true? (.-leashedToMe e)))

(defn find-by-key
  "The member of the herd tracked by key, or nil."
  [p mob radius k]
  (first (filter #(= k (key-of %)) (herd p mob radius))))

(defn give-up!
  "Book animal k as given up on for reason in the job's memory."
  [c k reason]
  (ctx/update-mem! c assoc-in [:given-up k] reason))

(defn bump-row!
  "Count one more fruitless attempt in a row."
  [c]
  (ctx/update-mem! c update :in-row (fnil inc 0)))

(defn reset-row!
  [c]
  (ctx/update-mem! c assoc :in-row 0))

(defn book-out-of-reach!
  "Book an out-of-reach click on k: given up (:unreachable) on the second."
  [c k]
  (let [n (inc (get-in (ctx/mem c) [:fails k] 0))]
    (ctx/update-mem! c assoc-in [:fails k] n)
    (when (>= n 2) (give-up! c k :unreachable))
    (bump-row! c)))

(defn ^:async walk!
  "Walk within reach of the entity target (animal k) when further than reach. Resolves to :there, :partial or
  :blocked; a blocked walk gives k up and bumps the row. opts: :reset-row? (a :there ends the row), :doors."
  [c k target reach {:keys [reset-row? doors]}]
  (let [tpos (u/pos-of (.-pos target))]
    (if (<= (u/dist (u/self-pos c) tpos) reach)
      :there
      (let [r (await (near/go-near! c tpos 2 (cond-> {:zone-tolls true :leg-s (:walk-timeout-s (:args c)) :escalate false :look-round false}
                                               doors (assoc :doors doors))))]
        (case r
          :there (do (when reset-row? (reset-row! c)) :there)
          :partial :partial
          (do (give-up! c k :unreachable)
              (bump-row! c)
              :blocked))))))
