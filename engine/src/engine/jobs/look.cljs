(ns engine.jobs.look
  "Looking around like a player: a job that needs a block it has to have seen turns the body through the four headings
  (level, then down at the floor ahead) with a sight pass after each look, so what lies beside or behind it comes
  into perception's memory. Never sensing through walls: the looks only widen what the eye can reach."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [engine.perception :as perception]))

(def headings {"north" [0 -1] "south" [0 1] "east" [1 0] "west" [-1 0]})
(def heading-short {"n" "north" "s" "south" "e" "east" "w" "west"})

(defn cell-of [pos] {:x (js/Math.floor (:x pos)) :y (js/Math.floor (:y pos)) :z (js/Math.floor (:z pos))})

(defn heading-name
  "north, south, east or west for a heading arg (a name or n/s/e/w, string or keyword), else nil."
  [d]
  (when d
    (let [n (str/lower-case (name d))]
      (cond (headings n) n
            (heading-short n) (heading-short n)))))

(defn facing
  "The heading nearest the way the body looks (perception's eye; mineflayer yaw 0 looks north), south without a
  perception."
  [p]
  (if-let [eye (some-> (aget p "perception") :raw (.eye))]
    (let [yaw (.-yaw eye)
          dx (- (js/Math.sin yaw))
          dz (- (js/Math.cos yaw))]
      (if (> (js/Math.abs dx) (js/Math.abs dz))
        (if (pos? dx) "east" "west")
        (if (pos? dz) "south" "north")))
    "south"))

(defn glances
  "The points a look along [dx dz] takes in from eye: level, 4 blocks ahead, then down (about 50 degrees) at the
  floor ahead."
  [eye [dx dz]]
  [{:x (+ (:x eye) (* 4 dx)) :y (:y eye) :z (+ (:z eye) (* 4 dz))}
   {:x (+ (:x eye) (* 1.5 dx)) :y (- (:y eye) 1.8) :z (+ (:z eye) (* 1.5 dz))}])

(defn see!
  "A sight pass now, so what the last look faced is in memory before the next decision."
  [c]
  (when-let [per (aget (:primitives c) "perception")]
    (perception/pass! per)))

(defn ^:async glance!
  "Look along each of dirs ([dx dz]), level and down, a sight pass after each look."
  [c dirs]
  (let [{:keys [x y z]} (cell-of (u/self-pos c))
        eye {:x (+ x 0.5) :y (+ y 1.62) :z (+ z 0.5)}]
    (loop [points (mapcat #(glances eye %) dirs)]
      (when-let [pt (first points)]
        (await (ctx/act c :look (clj->js {:pos pt})))
        (see! c)
        (recur (rest points))))))

(defn ^:async look-around!
  "Look along every heading from here and note the cell in job memory under :looked."
  [c]
  (await (glance! c (vals headings)))
  (ctx/update-mem! c assoc :looked (cell-of (u/self-pos c)))
  :continue)

(defn looked-here?
  "Whether the job has already looked around from the cell the body stands in."
  [c]
  (= (:looked (ctx/mem c)) (cell-of (u/self-pos c))))
