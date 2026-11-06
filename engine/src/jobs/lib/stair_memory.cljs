(ns jobs.lib.stair-memory
  "What a stair remembers on the body: the floors of the ways it walked and the cells it plugged against water."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.look :as look]
            [jobs.lib.stair-cells :as cells]
            [jobs.lib.util :as u]
            [plan.shape :as shape]))

(a/defargs settings
  {::max-ways {:default 200 :doc "Stair ways the body remembers (memory :stair-way). Past it the one used least recently goes, unless it lies within ::home-keep of home." :spec (a/int-in 1 nil)}
   ::home-keep {:default 64 :doc "Blocks from its :home or :bed place within which a remembered stair way is never dropped for room." :spec (a/num-in 0 nil)}
   ::dig-plugs? {:default false :doc "A cell this body plugged against water may be dug while the water is still behind it (kept-plugs)." :spec boolean?}})
(defn max-ways [] (settings/get settings ::max-ways))
(defn home-keep [] (settings/get settings ::home-keep))
(defn dig-plugs? [] (settings/get settings ::dig-plugs?))

(def plug-policy {:cap 60 :ttl :forever})

(defn plugs-of
  "The set of cells this body plugged against water in its current dimension (memory :stair-plug), whatever job did."
  [c]
  (let [dim (.-dimension (.self (:primitives c)))]
    (set (keep #(when (= dim (:dim (:data %))) (:cell (:data %))) (ctx/entries c :stair-plug)))))

(defn kept-plugs
  "The cells this body plugged against water (plugs-of) that its digs leave shut: those the body sees or remembers
  (within its memory age, jobs.lib.util/seen-name) water beside or above, which could run into the opened cell; none
  under dig-plugs?. A plug without such water is an ordinary block (a dig route prices an unseen neighbour as water)."
  [c]
  (if (dig-plugs?)
    #{}
    (let [p (:primitives c)
          seen (fn [[x y z]] (u/seen-name p {:x x :y y :z z}))]
      (into #{} (filter (fn [[_ y _ :as cell]] (some (fn [[_ [_ ny _]]] (>= ny y)) (rules/fluid-neighbours seen cell))))
            (plugs-of c)))))

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
  ::max-ways the least recently used way not near home goes (make-room!). A heading (name) is kept in the entry as the way
  its lowest step goes down. Returns the joined floors."
  ([c ground floors] (remember-way! c ground floors nil))
  ([c ground floors heading]
  (let [dim (.-dimension (.self (:primitives c)))
        olds (keep #(when (= dim (:dim (:data %))) (:floors (:data %))) (ctx/entries c :stair-way))
        {:keys [floors gone]} (cells/joined-way olds ground floors)]
    (when (seq gone) (ctx/forget-where! c :stair-way #(and (= dim (:dim %)) (contains? gone (:floors %)))))
    (let [left (make-room! c)]
      (ctx/remember! c :stair-way (cond-> {:dim dim :floors floors} heading (assoc :heading heading)) {:cap (inc left) :ttl :forever}))
    floors)))

(defn bottom-of
  "The lowest stand of a way (entry data) and the heading name its last step goes down: the recorded :heading, else read
  from the two lowest floors; {:cell [x y z] :heading name-or-nil }, nil for a way without floors."
  [{:keys [floors heading]}]
  (let [[[bx by bz] [nx _ nz]] (sort-by second floors)]
    (when bx
      {:cell [bx (inc by) bz]
       :heading (or heading
                    (when nx (some (fn [[h step]] (when (= step [(- bx nx) (- bz nz)]) h)) look/headings)))})))

(defn foot-heading
  "The heading name of the last step down of a stair this body cut (memory :stair-way) whose lowest stand is the feet cell
  [x y z]: the way that stair goes on from here; nil when none ends here."
  [c feet]
  (let [dim (.-dimension (.self (:primitives c)))]
    (some (fn [{d :data}]
            (let [b (bottom-of d)]
              (when (and (= dim (:dim d)) (= feet (:cell b))) (:heading b))))
          (reverse (ctx/entries c :stair-way)))))

(defn record-way!
  "Remember the steps walked (memory :stair-way, one entry a connected way: remember-way!)."
  [c origin dir heading steps]
  (remember-way! c (shape/add origin [0 -1 0]) (cells/way-floors origin dir heading steps) (when (= :down dir) (some-> heading name))))
