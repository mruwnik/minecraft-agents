(ns engine.perception.mobs
  "Mob memory: the hostiles the body has seen or heard (see engine.perception)."
  (:require [engine.perception.light :refer [table-now]]
            [engine.perception.rays :refer [basis in-cone?]]
            [engine.perception.store :refer [eye-height]]))

(def silent-mobs "Hostiles that make no sound while they stalk: known only once seen, or while fusing (hissing)." #{"creeper"})

(def mob-speed
  "Blocks a second a hostile covers when it chases (rough game values); one not listed `default-mob-speed`."
  {"zombie" 2.3 "husk" 2.3 "drowned" 2.3 "zombie_villager" 2.3 "zombified_piglin" 2.3 "blaze" 2.3
   "skeleton" 2.5 "stray" 2.5 "bogged" 2.5 "wither_skeleton" 2.5 "creeper" 2.5 "witch" 2.5
   "silverfish" 2.5 "endermite" 2.5 "slime" 2 "magma_cube" 2
   "spider" 3 "cave_spider" 3 "enderman" 3 "hoglin" 3 "zoglin" 3 "ravager" 3 "piglin_brute" 3.5
   "pillager" 3.5 "vindicator" 3.5 "evoker" 3 "guardian" 5 "phantom" 6 "vex" 8})

(def default-mob-speed 3)

(defn mob-forget-ms
  "How long a mob of name not sensed again stays known: the time it takes to drift :mob-drift blocks."
  [{:keys [opts]} name]
  (* 1000 (/ (:mob-drift opts) (get mob-speed name default-mob-speed))))

(def mob-middle 0.9)

(defn mob-lit?
  "Whether the mob at pos stands in light bright enough to make out: the light rule of the sight table, read at the
  cells of its feet and head."
  [{:keys [raw opts st]} ^js pos]
  (let [^js visible (or (.-visible ^js st) (set! (.-visible ^js st) (table-now raw (:seeing-min opts))))
        x (js/Math.floor (.-x pos)) z (js/Math.floor (.-z pos)) y (js/Math.floor (.-y pos))
        lit? (fn [yy] (== 1 (aget visible (.lightAt ^js raw x yy z))))]
    (or (lit? y) (lit? (inc y)))))

(defn sense-thing
  "How the body senses a thing at pos (feet) from eye, given whether a line from the eye to it is clear (`visible?`)
  and whether it makes no sound (`silent?`): :seen, :heard or nil. One rule for mobs (mob-sense) and for what
  engine.entity-observations lists (the /entities tool)."
  [{:keys [opts grid] :as per} ^js eye ^js pos visible? silent?]
  (let [vx (- (.-x pos) (.-x eye)) vy (- (+ (.-y pos) mob-middle) (.-y eye)) vz (- (.-z pos) (.-z eye))
        d (js/Math.hypot vx vy vz)
        heard? (and (<= d (:hearing opts)) (not silent?))
        seen? (and visible?
                   (<= d (:radius opts))
                   (or (<= d (:dark-sight opts)) (mob-lit? per pos))
                   (or heard? (in-cone? (basis (.-yaw eye) (.-pitch eye)) (:hx grid) (:hy grid) vx vy vz)))]
    (cond seen? :seen heard? :heard :else nil)))

(defn mob-sense
  "How the body senses hostile e (an entities() entry) from eye: :seen, :heard or nil (see the ns doc)."
  [per ^js eye ^js e]
  (sense-thing per eye (.-pos e) (true? (.-visible e)) (and (silent-mobs (.-name e)) (not (true? (.-fusing e))))))

(defn sense-mobs!
  "One mob sample over primitives src: sensed mobs take their place and time, mobs the client no longer tracks or that
  could have drifted :mob-drift blocks since they were last sensed are forgotten. Returns the map of live entities
  sensed now, by id."
  [{:keys [raw opts st] :as per} ^js src]
  (let [^js st st
        ^js mobs (.-mobs st)
        ^js eye (.eye ^js raw)
        now ((:now opts))
        sensed (js/Map.)]
    (if-not (and eye src)
      sensed
      (let [listed (js/Set.)]
        (doseq [^js e (array-seq (.entities src #js {:radius (:mob-scan opts) :kind "hostile" :max 64}))
                :when (.-pos e)]
          (.add listed (.-id e))
          (when-let [how (mob-sense per eye e)]
            (let [^js old (.get mobs (.-id e))
                  ^js pos (.-pos e)]
              (.set sensed (.-id e) e)
              (.set mobs (.-id e)
                    #js {:id (.-id e) :name (.-name e) :kind (.-kind e)
                         :pos #js {:x (.-x pos) :y (.-y pos) :z (.-z pos)}
                         :at now
                         :seenAt (if (keyword-identical? how :seen) now (some-> old .-seenAt))
                         :heard (keyword-identical? how :heard)}))))
        (doseq [[id ^js m] (vec (es6-iterator-seq (.entries mobs)))]
          (when (or (not (.has listed id))
                    (> (- now (.-at m)) (mob-forget-ms per (.-name m))))
            (.delete mobs id)))
        sensed))))

(defn known-mobs
  "The hostiles the body knows of after a fresh sample over primitives src, nearest first, as entities() entries: a mob
  sensed now is its live entry; one remembered is {:id :name :kind :pos (where it was last sensed) :distance :visible
  false}. Each adds seen (seen now or since it was last out of mind), heard (heard now, unseen), remembered (not
  sensed now) and ageMs (since last sensed)."
  [{:keys [raw opts st] :as per} ^js src]
  (let [sensed (sense-mobs! per src)
        ^js eye (.eye ^js raw)
        now ((:now opts))]
    (if-not eye
      #js []
      (let [fx (.-x eye) fy (- (.-y eye) eye-height) fz (.-z eye)
            out (into-array
                 (for [^js m (es6-iterator-seq (.values (.-mobs ^js st)))]
                   (let [^js live (.get sensed (.-id m))
                         ^js pos (.-pos m)
                         base (if live
                                (js/Object.assign #js {} live)
                                #js {:id (.-id m) :name (.-name m) :kind (.-kind m) :pos pos
                                     :distance (js/Math.hypot (- (.-x pos) fx) (- (.-y pos) fy) (- (.-z pos) fz))
                                     :visible false})]
                     (when (= "creeper" (.-name m)) (aset base "creeper" true))
                     (js/Object.assign base #js {:seen (some? (.-seenAt m))
                                                 :heard (and (some? live) (.-heard m))
                                                 :remembered (nil? live)
                                                 :ageMs (- now (.-at m))}))))]
        (.sort out (fn [^js a ^js b] (- (.-distance a) (.-distance b))))
        out))))
