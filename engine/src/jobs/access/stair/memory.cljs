(ns jobs.access.stair.memory
  "What a stair remembers on the body: the floors of the ways it walked and the cells it plugged against water."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.access.stair.cells :as cells]
            [jobs.lib.util :as u]
            [plan.shape :as shape]))

(a/defargs settings
  {::max-ways {:default 200 :doc "Stair ways the body remembers (memory :stair-way). Past it the one used least recently goes, unless it lies within ::home-keep of home." :spec (a/int-in 1 nil)}
   ::home-keep {:default 64 :doc "Blocks from its :home or :bed place within which a remembered stair way is never dropped for room." :spec (a/num-in 0 nil)}})
(defn max-ways [] (settings/get settings ::max-ways))
(defn home-keep [] (settings/get settings ::home-keep))

(def plug-policy {:cap 60 :ttl :forever})

(defn plugs-of
  "The set of cells this body plugged against water in its current dimension (memory :stair-plug), whatever job did."
  [c]
  (let [dim (.-dimension (.self (:primitives c)))]
    (set (keep #(when (= dim (:dim (:data %))) (:cell (:data %))) (ctx/entries c :stair-plug)))))

(defn plug!
  "Remember cell as filled against water, so a later stair keeps it shut."
  [c cell]
  (ctx/remember! c :stair-plug {:dim (.-dimension (.self (:primitives c))) :cell cell} plug-policy))

(defn near-home?
  "Whether a floor of the way (entry data) lies within ::home-keep of the body's :home or :bed place."
  [c {:keys [floors]}]
  (let [homes (keep #(:pos (:data (ctx/latest c %))) [:home :bed])]
    (boolean (some (fn [[x y z]] (some #(<= (u/dist % {:x (+ x 0.5) :y y :z (+ z 0.5)}) (home-keep)) homes)) floors))))

(defn forget-way!
  [c floors]
  (ctx/forget-where! c :stair-way #(= floors (:floors %))))

(defn make-room!
  "Drop the least recently used ways (the oldest entries: use-way! and remember-way! write last) not near home until fewer
  than ::max-ways remain; returns the number left."
  [c]
  (loop [es (ctx/entries c :stair-way)]
    (let [victim (when (>= (count es) (max-ways)) (first (remove #(near-home? c (:data %)) es)))]
      (if-not victim
        (count es)
        (do (forget-way! c (:floors (:data victim)))
            (recur (ctx/entries c :stair-way)))))))

(defn use-way!
  "The body walks the stand cell [x y z] of a way: its entry becomes the newest, so it goes last."
  [c [x y z]]
  (let [floor [x (dec y) z]]
    (when-let [e (first (filter #(some #{floor} (:floors (:data %))) (ctx/entries c :stair-way)))]
      (forget-way! c (:floors (:data e)))
      (ctx/remember! c :stair-way (:data e) {:cap (inc (count (ctx/entries c :stair-way))) :ttl :forever}))))

(defn remember-way!
  "Remember floors as one connected way (memory :stair-way) with every earlier way that holds ground or one of the floors:
  those are taken in, so a climb cut in many runs does not push the body's other stairs out of the memory. Past
  ::max-ways the least recently used way not near home goes (make-room!). Returns the joined floors."
  [c ground floors]
  (let [dim (.-dimension (.self (:primitives c)))
        olds (keep #(when (= dim (:dim (:data %))) (:floors (:data %))) (ctx/entries c :stair-way))
        {:keys [floors gone]} (cells/joined-way olds ground floors)]
    (when (seq gone) (ctx/forget-where! c :stair-way #(and (= dim (:dim %)) (contains? gone (:floors %)))))
    (let [left (make-room! c)]
      (ctx/remember! c :stair-way {:dim dim :floors floors} {:cap (inc left) :ttl :forever}))
    floors))

(defn record-way!
  "Remember the steps walked (memory :stair-way, one entry a connected way: remember-way!)."
  [c origin dir heading steps]
  (remember-way! c (shape/add origin [0 -1 0]) (cells/way-floors origin dir heading steps)))

(defn tool-failed?
  "Whether a fetch of a tool for block failed in this call (job memory), so it is dug bare-handed and not asked for again."
  [c block]
  (contains? (:tool-failed (ctx/mem c)) block))

(defn tool-failed!
  "Remember the failed fetch of a tool for block for the rest of the call."
  [c block]
  (ctx/update-mem! c update :tool-failed (fnil conj #{}) block))
