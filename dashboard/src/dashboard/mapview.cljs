(ns dashboard.mapview)

(defn sightings [body]
  (->> (when (:up body) (get-in body [:state :players]))
       (filter (fn [[_ p]] (map? p)))
       (map (fn [[n p]] {:name (name n) :x (:x p) :y (:y p) :z (:z p) :seenBy (:name body) :at (or (:at body) 0)}))))

;; The humans in the world: every player some body can see whose name is not an agent's. One entry per human, at the
;; freshest sighting, sorted by name.
(defn human-sightings [bodies agent-names]
  (let [agents (set agent-names)]
    (->> (mapcat sightings bodies)
         (remove #(agents (:name %)))
         (sort-by :at >)
         (reduce (fn [freshest s] (if (contains? freshest (:name s)) freshest (assoc freshest (:name s) s))) {})
         vals
         (sort-by :name)
         vec)))

(defn finite-pos? [n] (and (number? n) (js/Number.isFinite n) (pos? n)))

(defn plan-rects [places]
  (vec (for [p places
             :let [{:keys [x z width depth]} (get-in p [:village :bounds])]
             :when (and (finite-pos? width) (finite-pos? depth))]
         {:name (:name p) :x (or x (:x p)) :z (or z (:z p)) :w width :h depth})))

(defn map-points [bodies places zones humans]
  (vec (concat
        (for [b bodies :when (and (:up b) (get-in b [:state :pos]))]
          {:x (get-in b [:state :pos :x]) :z (get-in b [:state :pos :z])})
        (mapcat (fn [p] (cons {:x (:x p) :z (:z p)}
                              (map (fn [r] {:x (+ (:x r) (:w r)) :z (+ (:z r) (:h r))}) (plan-rects [p]))))
                places)
        (mapcat (fn [z] [{:x (:x1 z) :z (:z1 z)} {:x (:x2 z) :z (:z2 z)}]) zones)
        (map (fn [h] {:x (:x h) :z (:z h)}) humans))))

(defn empty-world? [world]
  (and (not-any? :up (:bodies world))
       (empty? (:places world))
       (empty? (:zones world))))

(defn world-bounds
  ([points] (world-bounds points 16))
  ([points pad]
   (when (seq points)
     (let [pad (or pad 16)
           xs (map :x points)
           zs (map :z points)]
       {:min-x (- (apply min xs) pad) :max-x (+ (apply max xs) pad)
        :min-z (- (apply min zs) pad) :max-z (+ (apply max zs) pad)}))))

;; One scale for both axes (a squashed map lies about distances); the shorter side is centred in the canvas.
(defn fit-view [bounds width height]
  (when bounds
    (let [span-x (max (- (:max-x bounds) (:min-x bounds)) 1)
          span-z (max (- (:max-z bounds) (:min-z bounds)) 1)
          scale (min (/ width span-x) (/ height span-z))]
      {:scale scale
       :origin-x (- (:min-x bounds) (/ (- (/ width scale) span-x) 2))
       :origin-z (- (:min-z bounds) (/ (- (/ height scale) span-z) 2))})))

(defn project [view x z]
  {:px (* (- x (:origin-x view)) (:scale view))
   :py (* (- z (:origin-z view)) (:scale view))})

(defn zone-rect [view zone]
  (let [a (project view (min (:x1 zone) (:x2 zone)) (min (:z1 zone) (:z2 zone)))
        b (project view (max (:x1 zone) (:x2 zone)) (max (:z1 zone) (:z2 zone)))]
    {:px (:px a) :py (:py a) :w (- (:px b) (:px a)) :h (- (:py b) (:py a))}))

(defn overlaps? [a b]
  (and (< (:px a) (+ (:px b) (:w b))) (< (:px b) (+ (:px a) (:w a)))
       (< (:py a) (+ (:py b) (:h b))) (< (:py b) (+ (:py a) (:h a)))))

;; Greedy and in the order given, so whatever matters most (the bodies) is offered first and always wins its space.
(defn fit-labels [boxes]
  (reduce (fn [kept box] (if (some #(overlaps? % box) kept) kept (conj kept box))) [] boxes))
