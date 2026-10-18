(ns jobs.lib.animals
  "Helpers for the animal jobs: what each mob breeds on, and the herd near the body."
  (:require [engine.ctx :as ctx]
            [jobs.lib.bait :as bait]
            [jobs.lib.gate :as gate]
            [jobs.lib.look :as look]
            [jobs.lib.near :as near]
            [jobs.lib.pen :as pen]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.util :as u]))

(defn in-box?
  "True when pos stands on a feet cell of the box ({:min :max}, cells, inclusive), floored as jobs.lib.pen/in-pen? floors it."
  [{:keys [min max]} {:keys [x y z]}]
  (let [fx (js/Math.floor x) fy (js/Math.floor (+ y 0.01)) fz (js/Math.floor z)]
    (and (<= (:x min) fx (:x max)) (<= (:y min) fy (:y max)) (<= (:z min) fz (:z max)))))

(def breeding-food bait/breeding-food)

(def bait-ms (* 5 60 1000))

(defn food-carried
  "The first food of mob's list the body carries, else nil."
  [p mob]
  (let [have (into #{} (map :name) (u/inventory p))]
    (first (filter have (get breeding-food mob)))))

(defn reserve-bait!
  "Reserve the foods of mob as bait in the register (strength 0.8, 5 minutes, ended with this job): an unnamed meal leaves them alone until near starving. Call it each round."
  [c mob]
  (let [until (+ (:now (ctx/view c)) bait-ms)]
    (ctx/forget-where! c reserve/kind #(= (:id c) (:by %)))
    (doseq [food (get breeding-food mob)]
      (ctx/remember! c reserve/kind {:id (str (:id c) "-bait-" food) :item food :count :all :strength 0.8 :by (:id c)
                                     :why "bait for the animal job" :until until}
                     reserve/policy))))

(defn herd
  "The JS entities named mob within radius of the body, nearest first, as a vector."
  [p mob radius]
  (vec (look/seen-objs p {:radius radius :names [mob] :max 64})))


(def livestock (vec (remove #{"bee"} (keys breeding-food))))

(defn in-pen-rows
  "The livestock listed within radius of the body that stand on the pen's cells (answer of jobs.lib.pen/check), as
  {:kind :id :pos [x y z]} rows, so a log line dates a loss. also? (a fn of a pos {:x :y :z}) counts further animals as inside."
  ([p answer radius] (in-pen-rows p answer radius (constantly false)))
  ([p answer radius also?]
   (->> (look/seen-objs p {:radius radius :names livestock :max 64})
        (filter #(let [pos (u/pos-of (.-pos %))] (or (pen/in-pen? answer pos) (also? pos))))
        (mapv (fn [e] (let [{:keys [x y z]} (u/pos-of (.-pos e))]
                        {:kind (.-name e) :id (.-id e) :pos [(/ (js/Math.round (* 10 x)) 10) y (/ (js/Math.round (* 10 z)) 10)]}))))))

(defn cell-of [e]
  (let [{:keys [x y z]} (u/pos-of (.-pos e))]
    {:x (js/Math.floor x) :y (js/Math.floor (+ y 0.01)) :z (js/Math.floor z)}))

(defn refusal-of
  "Why the zone rules let the job act on none of es with action: :refused, :no-zones, else nil. Quiet: no warn, no memory."
  [c action es]
  (let [{:keys [allowed refusal]} (gate/look c action (map cell-of es) {:own-plans-ok? true})]
    (when (and refusal (empty? allowed)) refusal)))

(defn allowed
  "The animals of es (JS entities) the zone rules let the job act on with action (:harvest :take), judged at the cell
  each stands in. One decline warn of kind (for job-name) names what refuses, or :no-zones. :ignore-zones? lets all.
  The last verdict is kept in the job's memory (see refusal)."
  [c kind job-name action es]
  (let [{:keys [allowed refusal]} (gate/judge c kind job-name action (map cell-of es) {:own-plans-ok? true})
        ok (set allowed)]
    (when (:token c) ; done? asks this in a check, which holds no token and writes nothing
      (ctx/update-mem! c #(if refusal (assoc % :refusal refusal) (dissoc % :refusal))))
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
      (let [r (await (near/go-near! c tpos 2 (cond-> {:zone-tolls true :ways #{:walk :clear-seal} :look-round false}
                                               doors (assoc :doors doors))))]
        (case r
          :there (do (when reset-row? (reset-row! c)) :there)
          :partial :partial
          (do (give-up! c k :unreachable)
              (bump-row! c)
              :blocked))))))

(defn lead-carried? [c]
  (some #(= "lead" (:name %)) (u/inventory (:primitives c))))
