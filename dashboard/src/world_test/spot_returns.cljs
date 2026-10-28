(ns world-test.spot-returns
  "tools/event-counts.mjs spot-returns: counts the measured prior jobs.lib.cost.threat ::spot-occupied from the trial bodies' event logs.
  A spot is a death by a mob, or a hurt by one that left the body too hurt for risky work (jobs.lib.death-spot's near-death). Its return is the first time the body is within
  return-blocks of it again after away-s or more with every known position 32 to 128 blocks off (vanilla: the killer has
  despawned, and mobs spawn there again); a later spot elsewhere, a position over 128 blocks off or memory-ms end the spot.
  k: returns where, within met-ms, a hostile was decided about within return-blocks of the spot, or hurt or killed the
  body there. Positions: :planned, :respawned, :hurt and :died (a hostile.decided's is the mob's).
  Pure part: returns."
  (:require ["fs" :as fs]
            ["path" :as path]
            ["zlib" :as zlib]
            [clojure.string :as str]
            [jobs.lib.cost.risk :as risk]
            [world-test.fresh-drop :as fresh-drop]))

(def hostiles #{"zombie" "husk" "drowned" "zombie_villager" "skeleton" "stray" "bogged" "spider" "cave_spider" "creeper" "witch"
                "enderman" "slime" "phantom" "pillager" "vindicator"})
(def return-blocks 16)
(def away-s 300)
(def met-ms 60000)
(def memory-ms "jobs.lib.cost.death ::memory-ms's default: how long a spot counts." 7200000)

(defn- num-of [re line] (some-> (re-find re line) second js/parseFloat))

(defn- hostile-cause [line]
  (or (some-> (re-find #":attacker \{[^}]*:name \"([^\"]+)\"" line) second hostiles)
      (some->> (re-find #":causes \[([^\]]*)\]" line) second (re-seq #"\"([^\"]+)\"") (map second) (some hostiles))))

(defn scan-line
  "One log line -> {:t :k :pos :cause}, :k one of :spot :pos :met, or nil."
  [line]
  (let [t (num-of #":time-ms (\d+)" line)
        [_ x y z] (re-find #":pos \{:x (-?[\d.]+(?:E-?\d+)?), :y (-?[\d.]+(?:E-?\d+)?), :z (-?[\d.]+(?:E-?\d+)?)\}" line)
        pos (when x {:x (js/parseFloat x) :y (js/parseFloat y) :z (js/parseFloat z)})
        body? (str/includes? line ":source :body")
        cause (hostile-cause line)
        r {:t t :pos pos :cause cause}]
    (when t
      (cond
        (and body? (str/includes? line ":kind :died")) (assoc r :k (if cause :spot :pos))
        (and body? (str/includes? line ":kind :hurt")) (assoc r :k (cond (and cause (risk/too-hurt? (or (num-of #":health (-?[\d.]+)" line) 99) (num-of #":food (-?[\d.]+)" line) :long)) :spot
                                                                         cause :met
                                                                         :else :pos))
        (str/includes? line ":kind :hostile.decided") (assoc r :k :met)
        (or (str/includes? line ":kind :planned") (str/includes? line ":kind :respawned")) (assoc r :k :pos)))))

(defn- dist [a b] (js/Math.hypot (- (:x a) (:x b)) (- (:y a) (:y b)) (- (:z a) (:z b))))

(defn- return-of
  "The return [t met?] after spot (the records after it, by time), or nil. A spot or a met record within return-blocks
  of the spot (a hostile decided about there, or one hurting the body there) within met-ms of the return is met."
  [{spot :pos t0 :t} later]
  (let [there? #(and (:pos %) (<= (dist (:pos %) spot) return-blocks))
        met? (fn [t rs] (boolean (some #(and (#{:met :spot} (:k %)) (there? %) (<= t (:t %) (+ t met-ms))) rs)))]
    (loop [[r & more :as rs] later near-t t0 away? false]
      (cond
        (or (nil? r) (> (- (:t r) t0) memory-ms)) nil
        (nil? (:pos r)) (recur more near-t away?)
        :else
        (let [d (dist (:pos r) spot)]
          (cond
            (> d 128) nil
            (and (<= d return-blocks) away? (>= (- (:t r) near-t) (* 1000 away-s))) [(:t r) (met? (:t r) rs)]
            (= :spot (:k r)) nil
            (<= d 32) (recur more (:t r) false)
            :else (recur more near-t true)))))))

(defn returns
  "{:spots :n :k} of the records of one body, sorted by :t."
  [records]
  (let [rs (vec records)
        spots (keep-indexed (fn [i r] (when (and (= :spot (:k r)) (:pos r)) i)) rs)
        backs (keep #(return-of (rs %) (subvec rs (inc %))) spots)]
    {:spots (count spots) :n (count backs) :k (count (filter second backs))}))

(defn- read-body [dir]
  (->> (fresh-drop/log-files (array-seq (fs/readdirSync dir)))
       (mapcat (fn [f] (let [p (path/join dir f)
                             text (if (str/ends-with? f ".gz") (.toString (zlib/gunzipSync (fs/readFileSync p)) "utf8") (fs/readFileSync p "utf8"))]
                         (keep scan-line (str/split-lines text)))))
       (sort-by :t)))

(defn count-in
  "{:spots :n :k} over the bodies under dir (each body's engine/ event logs)."
  [dir]
  (apply merge-with + {:spots 0 :n 0 :k 0}
         (for [b (array-seq (fs/readdirSync dir))
               :let [d (path/join dir b "engine")]
               :when (fs/existsSync d)]
           (returns (read-body d)))))
