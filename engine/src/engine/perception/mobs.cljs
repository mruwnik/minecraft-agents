(ns engine.perception.mobs
  "Mob memory: the hostiles the body has seen or heard (see engine.perception)."
  (:require [engine.perception.light :refer [table-now]]
            [engine.perception.rays :refer [basis in-cone? near-of]]
            [engine.constants :as constants]
            [engine.game :as game]))

(def silent-mobs "Hostiles that make no sound while they stalk: known only once seen, or while fusing (hissing)." #{"creeper"})

(def default-mob-speed "Blocks a second of a hostile not in engine.constants/mob-speeds." 3)

(defn mob-forget-ms
  "How long a mob of name not sensed again stays known: the time it takes to drift :mob-drift blocks."
  [{:keys [opts]} name]
  (* 1000 (/ (:mob-drift opts) (get constants/mob-speeds name default-mob-speed))))

(def mob-middle 0.9)

(defn mob-lit?
  "Whether the mob at pos stands in light bright enough to make out: the light rule of the sight table, read at the
  cells of its feet and head."
  [{:keys [raw opts st]} ^js pos]
  (let [^js visible (or (.-visible ^js st) (set! (.-visible ^js st) (table-now raw (:seeing-min opts))))
        x (js/Math.floor (.-x pos)) z (js/Math.floor (.-z pos)) y (js/Math.floor (.-y pos))
        lit? (fn [yy] (== 1 (aget visible (.lightAt ^js raw x yy z))))]
    (or (lit? y) (lit? (inc y)))))

(defn in-view-cone?
  "Whether a thing at pos (feet) is inside the view cone of eye (yaw, pitch; no look is yaw 0, pitch 0) with tangent
  half-extents hx and hy."
  [hx hy ^js eye ^js pos]
  (in-cone? (basis (or (.-yaw eye) 0) (or (.-pitch eye) 0)) hx hy
            (- (.-x pos) (.-x eye)) (- (+ (.-y pos) mob-middle) (.-y eye)) (- (.-z pos) (.-z eye))))

(defn heard-at?
  "Whether a thing at pos (feet) within hearing blocks of eye makes itself heard (not silent)."
  [hearing ^js eye ^js pos silent?]
  (and (not silent?)
       (<= (js/Math.hypot (- (.-x pos) (.-x eye)) (- (+ (.-y pos) mob-middle) (.-y eye)) (- (.-z pos) (.-z eye))) hearing)))

(defn faces-or-hears?
  "The cone-or-sound rule of sensing one thing, shared by sense-thing and engine.senses: a thing in the view cone, or one
  that sounds within hearing, is noticed whichever way the eye faces."
  [hx hy hearing ^js eye ^js pos silent?]
  (or (heard-at? hearing eye pos silent?) (in-view-cone? hx hy eye pos)))

(defn sense-thing
  "How the body senses a thing at pos (feet) from eye, given whether a line from the eye to it is clear (`visible?`)
  and whether it makes no sound (`silent?`): :seen, :heard or nil. One rule for mobs (mob-sense) and for what
  engine.entity-observations lists (the /entities tool)."
  [{:keys [opts grid] :as per} ^js eye ^js pos visible? silent?]
  (let [vx (- (.-x pos) (.-x eye)) vy (- (+ (.-y pos) mob-middle) (.-y eye)) vz (- (.-z pos) (.-z eye))
        d (js/Math.hypot vx vy vz)
        heard? (heard-at? (:hearing opts) eye pos silent?)
        seen? (and visible?
                   (<= d (:radius opts))
                   (or (<= d (near-of per)) (mob-lit? per pos))
                   (faces-or-hears? (:hx grid) (:hy grid) (:hearing opts) eye pos silent?))]
    (cond seen? :seen heard? :heard :else nil)))

(defn mob-sense
  "How the body senses hostile e (an entities() entry) from eye: :seen, :heard or nil (see the ns doc)."
  [per ^js eye ^js e]
  (sense-thing per eye (.-pos e) (true? (.-visible e)) (and (silent-mobs (.-name e)) (not (true? (.-fusing e))))))

(defn- fade!
  "Keep a forgotten mob m that was seen as a sighting (its kind, where and when it was last seen), dropping sightings
  older than :sighting-ms and the oldest past :max-sightings."
  [{:keys [opts]} ^js st ^js m now]
  (let [^js faded (.-faded st)]
    (when (.-seenAt m)
      (.set faded (.-id m) #js {:id (.-id m) :uuid (.-uuid m) :name (.-name m) :kind (.-kind m)
                                :pos (.-seenPos m) :seenAt (.-seenAt m)}))
    (doseq [[id ^js f] (vec (es6-iterator-seq (.entries faded)))]
      (when (> (- now (.-seenAt f)) (:sighting-ms opts)) (.delete faded id)))
    (doseq [[id] (take (- (.-size faded) (:max-sightings opts)) (sort-by #(.-seenAt ^js (second %)) (es6-iterator-seq (.entries faded))))]
      (.delete faded id))))

(defn sense-mobs!
  "One mob sample over primitives src: sensed mobs take their place and time, mobs the client no longer tracks or that
  could have drifted :mob-drift blocks since they were last sensed are forgotten. A forgotten mob once seen fades into
  a sighting (fade!), unless the body sensed it at the last sample and the client dropped it since (it died or
  despawned before the body's eyes); a sensed mob is no sighting. Returns the map of live entities sensed now, by id."
  [{:keys [raw opts st] :as per} ^js src]
  (let [^js st st
        ^js mobs (.-mobs st)
        ^js eye (.eye ^js raw)
        now ((:now opts))
        prev (.-mobSampleAt st)
        sensed (js/Map.)]
    (set! (.-mobSampleAt st) now)
    (if-not (and eye src)
      sensed
      (let [listed (js/Set.)]
        (doseq [^js e (array-seq (.entities src #js {:radius (:mob-scan opts) :kind "hostile" :max 64}))
                :when (.-pos e)]
          (.add listed (.-id e))
          (when-let [how (mob-sense per eye e)]
            (let [^js old (.get mobs (.-id e))
                  ^js pos (.-pos e)
                  seen? (keyword-identical? how :seen)
                  at #js {:x (.-x pos) :y (.-y pos) :z (.-z pos)}]
              (.set sensed (.-id e) e)
              (.delete (.-faded st) (.-id e))
              (.set mobs (.-id e)
                    #js {:id (.-id e) :uuid (.-uuid e) :name (.-name e) :kind (.-kind e) :held (.-held e)
                         :pos at
                         :at now
                         :seenAt (if seen? now (some-> old .-seenAt))
                         :seenPos (if seen? at (some-> old .-seenPos))
                         :heard (keyword-identical? how :heard)}))))
        (doseq [[id ^js m] (vec (es6-iterator-seq (.entries mobs)))]
          (let [gone? (not (.has listed id))]
            (when (or gone? (> (- now (.-at m)) (mob-forget-ms per (.-name m))))
              (.delete mobs id)
              (when-not (and gone? (= prev (.-at m))) (fade! per st m now)))))
        sensed))))

(defn sightings
  "The hostiles the body saw and then forgot (sense-mobs!), oldest kept :sighting-ms: {:id :uuid :name :kind :pos (where
  last seen) :ageMs (since)}, JS objects."
  [{:keys [opts st]}]
  (let [now ((:now opts))]
    (into-array (for [^js f (es6-iterator-seq (.values ^js (.-faded ^js st)))
                      :when (<= (- now (.-seenAt f)) (:sighting-ms opts))]
                  (js/Object.assign #js {} f #js {:ageMs (- now (.-seenAt f))})))))

(defn enderman-at-yaw?
  "Whether a known enderman stands in the view cone of a head turned to yaw (level) from the body's eye: a glance there
  would meet its eyes."
  [{:keys [raw grid st]} yaw]
  (when-let [^js eye (some-> raw (.eye))]
    (let [probe #js {:x (.-x eye) :y (.-y eye) :z (.-z eye) :yaw yaw :pitch 0}]
      (boolean (some (fn [^js m] (and (= "enderman" (.-name m)) (in-view-cone? (:hx grid) (:hy grid) probe (.-pos m))))
                     (es6-iterator-seq (.values ^js (.-mobs ^js st))))))))

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
      (let [fx (.-x eye) fy (- (.-y eye) game/eye-height) fz (.-z eye)
            out (into-array
                 (for [^js m (es6-iterator-seq (.values (.-mobs ^js st)))]
                   (let [^js live (.get sensed (.-id m))
                         ^js pos (.-pos m)
                         base (if live
                                (js/Object.assign #js {} live)
                                #js {:id (.-id m) :name (.-name m) :kind (.-kind m) :held (.-held m) :pos pos
                                     :distance (js/Math.hypot (- (.-x pos) fx) (- (.-y pos) fy) (- (.-z pos) fz))
                                     :visible false})]
                     (when (= "creeper" (.-name m)) (aset base "creeper" true))
                     (js/Object.assign base #js {:seen (some? (.-seenAt m))
                                                 :heard (and (some? live) (.-heard m))
                                                 :remembered (nil? live)
                                                 :ageMs (- now (.-at m))}))))]
        (.sort out (fn [^js a ^js b] (- (.-distance a) (.-distance b))))
        out))))
