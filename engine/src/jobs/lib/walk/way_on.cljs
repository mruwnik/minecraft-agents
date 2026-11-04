(ns jobs.lib.walk.way-on
  "How often a way out of a seen stand goes on, in a body's block memory: the counts jobs.lib.cost/way-on-chance reads
  for the chance that a way on through cells the body has not seen goes on."
  (:require [engine.args :as a]
            [engine.path.blocks :as blocks]
            [engine.perception.store :as store]
            [engine.settings :as settings]))

(a/defargs settings
  {::sample {:default 256 :doc "Seen stands, nearest the start first (breadth first over the seen ways between them), whose ways out way-counts counts: the work bound of one count." :spec (a/int-in 1 nil)}})

(def classes*
  (delay (let [^js t (blocks/default-state-table)
               top (.-top t)
               kind (.-kind t)
               a (js/Uint8Array. (.-length top))]
           (dotimes [id (.-length top)]
             (let [k (aget kind id)]
               (aset a id (cond
                            (== k blocks/WATER) 3
                            (== k blocks/LAVA) 0
                            (zero? (aget top id)) 1
                            :else 2))))
           a)))

(def ^:private exit-gap "Cells apart that two openings of one place are two ways: a wide frontier is several ways, a corridor's mouth one." 3)

(def ^:private way-cap "Most unseen ways scan lists." 6)

(defn cell-fn
  "(fn [x y z]) the class of a remembered cell: -1 not seen, 0 neither passed nor stood on (lava), 1 passed (no
  collision), 2 stood on (collision), 3 both (water); known: (fn [cx sy cz]) the memory's ids + 1 of a section."
  [known]
  (let [^js classes @classes*
        last-key (volatile! nil)
        last-ids (volatile! nil)]
    (fn [x y z]
      (let [k (store/section-key (bit-shift-right x 4) (bit-shift-right y 4) (bit-shift-right z 4))
            ^js ids (if (== k @last-key)
                      @last-ids
                      (let [ids (known (bit-shift-right x 4) (bit-shift-right y 4) (bit-shift-right z 4))]
                        (vreset! last-key k)
                        (vreset! last-ids ids)))
            v (if ids (aget ids (store/cell-index x y z)) 0)]
        (if (zero? v) -1 (aget classes (dec v)))))))

;; a cell's class read as passed / stood on: 1 yes, 0 no, -1 not seen
(defn- pass [c] (if (neg? c) -1 (bit-and c 1)))
(defn- holds [c] (if (neg? c) -1 (if (>= c 2) 1 0)))

(defn- all-of
  "1 when every one of the reads is 1, 0 when one is 0, else -1 (a cell not seen)."
  ([a b c] (all-of a b c 1))
  ([a b c d] (if (or (zero? a) (zero? b) (zero? c) (zero? d)) 0 (if (and (== a 1) (== b 1) (== c 1) (== d 1)) 1 -1))))

(def ^:private blind
  "Cells round the feet a body looking ahead has not seen (the ground under and beside it): the only unseen cells a count passes through."
  2)

(def ^:private dxs #js [1 -1 0 0])
(def ^:private dzs #js [0 0 1 -1])

(defn side-read
  "What lies one step from stand [x y z] toward d (0-3, dxs/dzs): {:ny the stand's y when the way goes on level, up or down, else nil; :level :up :down 1 passable, 0 not, -1 not seen}. at: cell-fn."
  [at x y z d]
  (let [nx (+ x (aget dxs d)) nz (+ z (aget dzs d))
        col #(at nx (+ y %) nz)
        level (all-of (holds (col -1)) (pass (col 0)) (pass (col 1)))
        up (all-of (holds (col 0)) (pass (col 1)) (pass (col 2)) (pass (at x (+ y 2) z)))
        down (all-of (holds (col -2)) (pass (col -1)) (pass (col 0)) (pass (col 1)))]
    {:nx nx :nz nz :level level :up up :down down
     :ny (cond (== level 1) y (== up 1) (inc y) (== down 1) (dec y))}))

(defn way-outcome
  "Whether the first step of a logged way [x y z d] (a stand and a side, ways in scan) goes on now that the body's memory (known) may hold its cells: true a passable step, false a wall (none of level, up, down passable), nil a cell still unseen."
  [known [x y z d]]
  (let [{:keys [ny level up down]} (side-read (cell-fn known) x y z d)]
    (cond ny true
          (and (zero? level) (zero? up) (zero? down)) false)))

(defn scan
  "{:k :n :ways} over the seen stands a body could walk to, nearest start ([x y z] feet cell) first (at most ::sample of them,
  breadth first over the ways between them, and at most 8 times that many expanded): n the stands whose every side is seen
  that lead out or not, joined into one trial where neighbouring stands share the fate (a closed corridor or hall counts
  once, a fork's shut branch apart from its open one; an open place counts once per opening exit-gap cells from the others,
  so open ground is many ways and a corridor's mouth one), k the trials from whose stands a way onward, not back through the stand
  they were entered by, leads out to cells not seen (or to stands past the sample); the rest are dead ends: a shut
  corridor, a closed room (a wall beside a way is no way). A stand with a side not seen is
  left uncounted, as is a level way with a cell not seen within 2 cells of start, gone through (the cells by the feet a
  body looking ahead has not seen); farther unseen cells are the worse case, not passed. known: (fn [cx sy cz]) the
  memory's ids + 1 of a section. :ways: the first way into cells not seen (a stand and a side [x y z d], not the cells by the feet), nearest the start first, at most way-cap of them."
  [known [sx sy sz]]
  (let [at (cell-fn known)
        cap (settings/get settings ::sample)
        qx #js [sx] qy #js [sy] qz #js [sz]
        qp #js [-1]
        ways #js []
        adj #js [#js []]
        open #js [false]
        index (js/Map. #js [#js [(str sx "," sy "," sz) 0]])
        visit! (fn [x y z from]
                 (let [key (str x "," y "," z)
                       j (or (.get index key)
                             (let [j (.-length qx)]
                               (.set index key j) (.push qx x) (.push qy y) (.push qz z) (.push qp from) (.push adj #js []) (.push open false)
                               j))]
                   (.push (aget adj from) j)))
        out? (fn [s]
               (let [blocked (aget qp s)
                     done (js/Set. #js [s blocked])]
                 (loop [stack #js [s]]
                   (if (zero? (.-length stack))
                     false
                     (let [v (.pop stack)]
                       (if (aget open v)
                         true
                         (do (doseq [w (aget adj v)]
                               (when-not (.has done w) (.add done w) (.push stack w)))
                             (recur stack))))))))
        counted (loop [i 0 stands 0 found #js []]
                  (if (or (>= i (.-length qx)) (>= i (* 8 cap)) (>= stands cap))
                    (do (loop [j i] (when (< j (.-length qx)) (aset open j true) (recur (inc j)))) found)
                    (let [x (aget qx i) y (aget qy i) z (aget qz i)
                          unseen?
                          (loop [d 0 unseen? false]
                            (if (== d 4)
                              unseen?
                              (let [{:keys [nx nz level up down ny]} (side-read at x y z d)]
                                (cond
                                  ny (do (visit! nx ny nz i) (recur (inc d) unseen?))
                                  (and (zero? level) (zero? up) (zero? down)) (recur (inc d) unseen?)
                                  (neg? level) (do (if (<= (max (js/Math.abs (- nx sx)) (js/Math.abs (- nz sz))) blind)
                                                     (visit! nx y nz i)
                                                     (when (< (.-length ways) way-cap) (.push ways #js [x y z d])))
                                                   (recur (inc d) true))
                                  :else (recur (inc d) (or unseen? (neg? up) (neg? down)))))))
                          counted? (and (pos? i) (not unseen?))]
                      (aset open i unseen?)
                      (when counted? (.push found i))
                      (recur (inc i) (if counted? (inc stands) stands) found))))]
    (let [apart (fn [opens]
                  (count (reduce (fn [kept j]
                                   (if (some #(<= (max (js/Math.abs (- (aget qx %) (aget qx j))) (js/Math.abs (- (aget qz %) (aget qz j)))) exit-gap) kept)
                                     kept
                                     (conj kept j)))
                                 [] opens)))
          status (js/Map.)
          group (js/Map.)
          root (fn root [i] (let [p (.get group i)] (if (== p i) i (root p))))]
      (doseq [i counted] (.set status i (boolean (out? i))) (.set group i i))
      (doseq [i counted j (aget adj i)]
        (when (and (.has status j) (= (.get status i) (.get status j)))
          (.set group (root i) (root j))))
      (let [roots (set (map root counted))
            exits (js/Map.)]
        (dotimes [j (.-length qx)]
          (when (aget open j)
            (when-let [r (loop [i (aget qp j)] (cond (neg? i) nil (.has status i) (root i) :else (recur (aget qp i))))]
              (when (.get status r) (.set exits r (conj (.get exits r []) j))))))
        (let [ways-of (fn [r] (if (.get status r) (max 1 (apart (.get exits r []))) 1))
              k (reduce + (map ways-of (filter #(.get status %) roots)))
              dead (count (remove #(.get status %) roots))]
          {:k k :n (+ k dead) :ways (mapv vec ways)})))))

(defn way-counts
  "{:k :n} of scan."
  [known start]
  (select-keys (scan known start) [:k :n]))
