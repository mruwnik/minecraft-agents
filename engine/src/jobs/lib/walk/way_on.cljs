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

(def ^:private dxs #js [1 -1 0 0])
(def ^:private dzs #js [0 0 1 -1])

(defn way-counts
  "{:k :n} over the seen stands nearest start ([x y z] feet cell; at most ::sample of them, breadth first over the
  seen ways between them): n the ways out of them (four headings, each level, one up or one down) whose cells are all
  seen, k those that lead to another stand. known: (fn [cx sy cz]) the memory's ids + 1 of a section."
  [known [sx sy sz]]
  (let [at (cell-fn known)
        cap (settings/get settings ::sample)
        qx #js [sx] qy #js [sy] qz #js [sz]
        seen (js/Set. #js [(str sx "," sy "," sz)])]
    (loop [i 0 k 0 n 0]
      (if (or (>= i (.-length qx)) (>= i cap))
        {:k k :n n}
        (let [x (aget qx i) y (aget qy i) z (aget qz i)
              [k n] (loop [d 0 k k n n]
                      (if (== d 4)
                        [k n]
                        (let [nx (+ x (aget dxs d)) nz (+ z (aget dzs d))
                              col #(at nx (+ y %) nz)
                              level (all-of (holds (col -1)) (pass (col 0)) (pass (col 1)))
                              up (all-of (holds (col 0)) (pass (col 1)) (pass (col 2)) (pass (at x (+ y 2) z)))
                              down (all-of (holds (col -2)) (pass (col -1)) (pass (col 0)) (pass (col 1)))
                              ny (cond (== level 1) y (== up 1) (inc y) (== down 1) (dec y))]
                          (cond
                            ny (let [key (str nx "," ny "," nz)]
                                 (when-not (.has seen key)
                                   (.add seen key) (.push qx nx) (.push qy ny) (.push qz nz))
                                 (recur (inc d) (inc k) (inc n)))
                            (and (zero? level) (zero? up) (zero? down)) (recur (inc d) k (inc n))
                            :else (recur (inc d) k n)))))]
          (recur (inc i) k n))))))
