(ns jobs.explore.look
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a] [engine.settings :as settings] [engine.entity-observations :as obs] [jobs.lib.util :as u]
            ["minecraft-data" :as minecraft-data] [engine.contract :as contract] [engine.ctx :as ctx] [engine.game :as game] [jobs.lib.look :as look] [jobs.lib.places :as places]))
(def doc
  "Observe the nearby blocks and entities the body has seen, once, without moving or changing the world. :at inspects one exact
  block as last seen, with properties (a cell never seen is :unknown). Nearby samples are nearest first and bounded;
  :more-blocks?/:more-entities? report extra matches. Nothing is sensed through walls (players are always listed).
  This is sight evidence, not a terrain map; it only reads, so no hostile or dark makes it wait. A refused argument ends :impossible with cause {:reason :bad-args :text why} (from can-start?). On success ends :done with the observation as its data.")
(a/defargs args
  {:radius {:doc "nearby sample radius, 1..32 blocks" :spec (a/num-in 1 32) :default 16}
   :block-names {:doc "nil, one block name or a vector/set of up to 16 names" :spec (a/or-of a/name? (a/coll-of a/name?)) :default nil}
   :entity-names {:doc "nil, one entity name or a vector/set of up to 16 names" :spec (a/or-of a/name? (a/coll-of a/name?)) :default nil}
   :max-blocks {:doc "nearest blocks to return, 0..16; zero skips scan" :spec (a/int-in 0 16) :default 6}
   :max-entities {:doc "nearest entities to return, 0..8; zero skips scan" :spec (a/int-in 0 8) :default 2}
   :useful? {:doc "list the workstations, storage, beds and ores seen (each with its :distance); excludes :block-names" :spec boolean? :default false}
   :properties? {:doc "include nearby block state properties" :spec boolean? :default false}
   :at {:doc "optional exact block position [x y z] or {:x :y :z}" :spec ::a/pos :default nil}})
(a/defargs settings
  {::job-time-s {:default 10 :doc "Seconds the call may take (a guess until measured)." :spec (a/num-in 0 nil)}})

(def target [:at])

(def safe? ready/ok)
(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s}})
(defn done? "An act with no goal state: a look is never done before it is made." [_c]
  (contract/later :looking "the look is not made yet" :units :looks :done 0 :left 1))
(defn quote-of [_c] {:uses {:job-time [0 0 1]}})
(def useful-names #{"crafting_table" "furnace" "blast_furnace" "smoker" "chest" "trapped_chest" "barrel" "ender_chest"
                    "enchanting_table" "anvil" "brewing_stand" "ancient_debris"})
(defn useful-block?
  "Whether a block name is a workstation, storage, bed or ore."
  [n]
  (boolean (or (useful-names n) (re-find #"(?:_bed|_ore)$" n))))
(defn names [v]
  (cond (nil? v) nil (string? v) [v] (or (vector? v) (set? v)) (vec v) :else ::bad))
(defn valid-names? [v]
  (or (nil? v) (and (vector? v) (<= 1 (count v) 16)
                    (every? #(and (string? %) (re-matches #"[a-z0-9_]{1,80}" %)) v))))
(defn block-name-checker
  "A fn of a block name: does minecraft-data list it for the body's version?"
  [p]
  (let [by-name (.-blocksByName (minecraft-data (game/version-of p)))]
    (fn [n] (.hasOwnProperty by-name n))))
(defn options
  "The args with names listed and :at parsed, or {:error text} for what the arg specs do not say (name lists, :useful? with
  :block-names, a cell). known-block? (optional) refuses block names minecraft-data does not list."
  ([a] (options a nil))
  ([a known-block?]
  (let [a (merge (settings/job-defaults (quote jobs.explore.look) args) a)
        blocks (names (:block-names a)) entities (names (:entity-names a))
        at (when (:at a) (places/parse-pos (:at a)))]
    (cond
      (not (and (valid-names? blocks) (valid-names? entities))) {:error "names must be nil, a name, or up to 16 block/entity names"}
      (and known-block? (seq (remove known-block? blocks))) {:error (str "unknown block names: " (pr-str (vec (remove known-block? blocks))))}
      (and (:useful? a) blocks) {:error ":useful? and :block-names are exclusive"}
      (:reason at) {:error (:message at)}
      :else (assoc a :block-names blocks :entity-names entities :at (:pos at))))))
(defn pos [p]
  (mapv #(/ (js/Math.round (* 10 %)) 10) (if (map? p) [(:x p) (:y p) (:z p)] [(.-x p) (.-y p) (.-z p)])))
(defn block [b]
  (cond-> {:name (:name b) :pos (pos (:pos b))}
    (:properties b) (assoc :properties (:properties b))))
(defn entity [e]
  (cond-> {:name (:name e) :kind (:kind e) :pos (pos (:pos e))}
    (:uuid e) (assoc :uuid (:uuid e))
    (:username e) (assoc :username (:username e))
    (some? (:visible e)) (assoc :visible? (:visible e))))
(defn heard-only?
  "A hostile the body hears but does not see now: a sound, not a sighting, wherever it was seen before."
  [e]
  (and (= "hostile" (:kind e)) (contains? e :seen) (not (:visible e)) (boolean (:heard e))))
(defn heard-entity
  "What a sound tells of hostile e: its name, direction and band from the body (obs/rough-hearing), never its place."
  [from e]
  (-> (obs/rough-hearing from (:pos e))
      (assoc :name (:name e) :kind (:kind e) :heard? true)))
(defn listed?
  "Whether the look lists e: a hostile only if it was seen (with its place) or is heard now (roughly)."
  [e]
  (or (not= "hostile" (:kind e)) (not (contains? e :seen)) (:seen e) (heard-only? e)))
(defn listed [from e]
  (if (heard-only? e) (heard-entity from e) (entity e)))
(defn distance-row [from b]
  (assoc (block b) :distance (/ (js/Math.round (* 10 (u/dist from (:pos b)))) 10)))
(defn observe [p {:keys [radius block-names entity-names max-blocks max-entities properties? useful? at]}]
  (let [self (.self p)
        blocks (if (pos? max-blocks)
                 (look/seen-blocks p (cond-> {:radius radius :max (inc max-blocks) :properties? properties?}
                                       block-names (assoc :names block-names) useful? (assoc :match useful-block?)
                                       (and (not block-names) (not useful?)) (assoc :all? true)))
                 [])
        entities (if (pos? max-entities)
                   (filterv listed? (look/seen-entities p (cond-> {:radius radius :max (inc max-entities)} entity-names (assoc :names entity-names))))
                   [])
        exact (when at (look/seen-block p at))]
    (cond-> {:center (pos (.-pos self)) :radius radius :scope :seen
             :blocks (mapv (if useful? #(distance-row (u/pos-of (.-pos self)) %) block) (take max-blocks blocks)) :entities (mapv #(listed (u/pos-of (.-pos self)) %) (take max-entities entities))}
      (> (count blocks) max-blocks) (assoc :more-blocks? true)
      (> (count entities) max-entities) (assoc :more-entities? true)
      at (assoc :at (if (or (nil? exact) (:unknown exact))
                      {:pos (pos at) :unknown true}
                      (assoc (block exact) :seen? true))))))
(defn bad-args
  "Never :bad-args when the call's args are refused."
  [c]
  (if-let [error (:error (options (:args c) (block-name-checker (:primitives c))))]
    (contract/never :bad-args error :text error)
    (contract/ok :args "args fit")))
(def can-start? bad-args)
(defn round [c]
  (ctx/end c :done nil (observe (:primitives c) (options (:args c)))))
