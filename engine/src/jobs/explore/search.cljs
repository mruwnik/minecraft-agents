(ns jobs.explore.search
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.util :as u]
            [engine.notes :as notes]))

(def doc
  "Look for :count things named :target (a block or entity name, or several) by walking legs from where the job
  starts (the origin).

  Each step looks around (blocks and entities with those names within :scan-radius), writes what it saw as
  :seen notes and the spot as a :searched note (engine.notes), and ends when :count are found. Otherwise it
  chooses the next leg and walks it with jobs.movement.go-to as a child (range 2), one child round per round.

  Legs are :spacing apart: :spiral rings round the origin, or :outward ahead along :heading, then the sides. A
  leg is never tried twice and never goes beyond :max-distance (XZ) of the origin. It is skipped (:skipped)
  when a live :searched note of any body, this one included, listing every target name lies within half its
  radius.

  A leg needs a standing cell in its column within 12 of the feet, read top down through leaves. An unloaded,
  wet or standless column is recorded failed (:not-loaded, :wet, :no-surface) and the next candidate is tried
  in the same round. An unloaded column is not tried but looked at again later. When only unloaded ones are
  left the round ends and the check declines for 2 s, up to :load-wait-s (a body just logged in has no chunks
  yet), then the search ends :not-loaded. A walk go-to gives up on is recorded :unreachable. Three of those in
  a row end the search (:stuck).

  With :use-notes, live :seen notes of a target within :max-distance count as found (:noted true, :by).

  Ends with {:reason :found|:not-found :why :found [{:what :pos :id? :noted? :by?}] :coverage {:legs :scans
  :failed [{:pos :reason}] :skipped :farthest}}, info search.done or warn search.not-found. :why is one of
  :distance (no leg left), :legs (:max-legs walked), :time (:timeout-s since the first round), :stuck,
  :not-loaded or :bad-args.

  Memory: :origin :started :legs :scans :found :tried :failed :skipped :farthest :failed-in-row, and :leg while
  a walk is under way, so a cut or restart resumes it.")

(def args
  {:target {:doc "a block or entity name, or several (a vector or set)" :default nil}
   :count {:doc "how many targets end the search" :default 1}
   :max-distance {:doc "how far (XZ) from the origin a leg may go" :default 96}
   :pattern {:doc ":spiral (rings round the origin) or :outward (ahead along :heading)" :default :spiral}
   :heading {:doc ":north, :east, :south or :west, for :outward" :default :north}
   :spacing {:doc "blocks between legs" :default 16}
   :scan-radius {:doc "how far round the body each look reaches" :default 24}
   :max-legs {:doc "legs walked before it gives up" :default 32}
   :timeout-s {:doc "seconds from the first round before it gives up" :default 600}
   :use-notes {:doc "targets noted (by any body) within :max-distance count as found" :default true}
   :seen-ttl-s {:doc "how long a note of a block seen lasts" :default 259200}
   :entity-ttl-s {:doc "how long a note of an entity seen lasts" :default 600}
   :searched-ttl-s {:doc "how long a note of searched ground lasts" :default 86400}
   :load-wait-s {:doc "how long to wait for unloaded leg columns to load when no loaded leg is left" :default 30}})

(def backoff
  "A failed leg is up to three fruitless go-to rounds and three failed legs in a row end the search, so it
  concludes within nine."
  {:after 9})

(def max-failed-in-row 3)
(def wait-ms 2000)
(def reach 12)
(def headings {:north [0 -1] :east [1 0] :south [0 1] :west [-1 0]})
(def patterns #{:spiral :outward})

;; ------------------------------------------------------------------ pure

(defn targets
  "The target names as a sorted vector."
  [c]
  (let [t (:target (:args c))]
    (if (string? t) [t] (vec (sort (set t))))))

(defn xz-dist [[ax az] [bx bz]]
  (js/Math.hypot (- ax bx) (- az bz)))

(defn spiral-points
  "[x z] points spacing apart in rings round the origin [x z], nearest ring first, within max-distance."
  [[ox oz] spacing max-distance]
  (let [ring (fn [r] (concat (for [x (range (- r) r)] [x (- r)])
                             (for [z (range (- r) r)] [r z])
                             (for [x (range r (- r) -1)] [x r])
                             (for [z (range r (- r) -1)] [(- r) z])))]
    (->> (range 1 (inc (js/Math.floor (/ max-distance spacing))))
         (mapcat ring)
         (filter (fn [[x z]] (<= (js/Math.hypot (* x spacing) (* z spacing)) max-distance)))
         (mapv (fn [[x z]] [(+ (js/Math.floor ox) (* x spacing)) (+ (js/Math.floor oz) (* z spacing))])))))

(def outward-offsets
  "[ahead across] fractions of the spacing: straight on, then ever more to the sides."
  [[1 0] [0.75 0.5] [0.75 -0.5] [0.25 0.75] [0.25 -0.75] [0 1] [0 -1]])

(defn outward-points
  "[x z] points spacing away from [x z] along heading, then to its sides."
  [[x z] heading spacing]
  (let [[dx dz] (headings heading)
        [sx sz] [(- dz) dx]]
    (mapv (fn [[f a]] [(js/Math.floor (+ x (* spacing (+ (* dx f) (* sx a)))))
                       (js/Math.floor (+ z (* spacing (+ (* dz f) (* sz a)))))])
          outward-offsets)))

(def open-names #{"air" "cave_air" "void_air" "short_grass" "grass" "tall_grass" "fern" "large_fern" "dead_bush"
                  "snow" "dandelion" "poppy" "blue_orchid" "allium" "azure_bluet" "oxeye_daisy" "cornflower"
                  "lily_of_the_valley" "torch" "leaf_litter" "short_dry_grass" "tall_dry_grass" "bush" "firefly_bush"})

(defn open? [n]
  (boolean (and n (or (open-names n) (str/ends-with? n "_sapling") (str/ends-with? n "_tulip")))))

(defn leaves? [n] (str/ends-with? n "_leaves"))
(def wet-names #{"water" "lava" "bubble_column"})

(defn stand-cell
  "Where a body could stand in column x z near feet height y, read top down from y+12 through open cells and
  leaves to the first other block: {:y feet} when the two cells above it are open, else {:fail :wet} (a fluid),
  {:fail :not-loaded} (an unloaded cell on the way) or {:fail :no-surface}. block-at: [x y z] -> name or nil."
  [block-at x z y]
  (let [y0 (js/Math.floor y)]
    (loop [h (+ y0 reach)]
      (if (< h (- y0 reach))
        {:fail :no-surface}
        (let [n (block-at [x h z])]
          (cond
            (nil? n) {:fail :not-loaded}
            (or (open? n) (leaves? n)) (recur (dec h))
            (wet-names n) {:fail :wet}
            (and (open? (block-at [x (inc h) z])) (open? (block-at [x (+ h 2) z]))) {:y (inc h)}
            :else {:fail :no-surface}))))))

(defn finding-key [{:keys [what id pos]}] [what (or id pos)])

(defn add-found
  "found with the new findings whose key it lacks, at most want of them."
  [found new want]
  (vec (take want (reduce (fn [v f] (if (some #(= (finding-key f) (finding-key %)) v) v (conj v f))) found new))))

(defn noted
  "Findings from live :seen notes of names within max-distance (XZ) of the origin [x y z]."
  [ns names [ox _ oz] max-distance]
  (->> ns
       (filter #(and (= :seen (:kind %)) (some #{(:what %)} names)
                     (<= (xz-dist [((:pos %) 0) ((:pos %) 2)] [ox oz]) max-distance)))
       (map (fn [n] (cond-> {:what (:what n) :pos (:pos n) :noted true :by (:by n)} (:id n) (assoc :id (:id n)))))))

;; ------------------------------------------------------------------ the body

(defn waiting?
  "Whether job memory m says the job waits at now for leg columns to load."
  [m now]
  (> (:wait-until m 0) now))

(defn check
  "A target is named, and the job is not waiting for leg columns to load."
  [c]
  (and (boolean (seq (targets c)))
       (not (waiting? (ctx/mem c) (ctx/now c)))))

(defn cell [p] (mapv js/Math.floor [(.-x p) (.-y p) (.-z p)]))
(defn here [c] (cell (.-pos (.self (:primitives c)))))

(defn sense
  "The targets in sight: blocks {:what :pos}, entities {:what :pos :id}."
  [p names radius]
  (concat (map (fn [b] {:what (.-name b) :pos (cell (.-pos b))})
               (array-seq (.blocks p #js {:names (clj->js names) :radius radius :max 64})))
          (map (fn [e] (cond-> {:what (.-name e) :pos (cell (.-pos e))} (.-uuid e) (assoc :id (.-uuid e))))
               (array-seq (.entities p #js {:names (clj->js names) :radius radius :max 32})))))

(defn finish!
  "Emit the outcome, hand it to the parent, end the job."
  [c reason why]
  (let [m (ctx/mem c)
        result (cond-> {:reason reason :found (:found m [])
                        :coverage {:legs (:legs m 0) :scans (:scans m 0) :failed (:failed m []) :skipped (:skipped m 0)
                                   :farthest (:farthest m 0)}}
                 why (assoc :why why))
        what (str/join "/" (targets c))]
    (if (= :found reason)
      (ctx/emit! c :search.done :info (assoc result :text (str "found " (count (:found result)) " " what)))
      (ctx/emit! c :search.not-found :warn
                 (assoc result :text (str "no " what " found (" (name why) ") after " (:legs m 0) " legs, "
                                          (:scans m 0) " looks, " (:farthest m 0) " blocks out"))))
    (ctx/result! c result)
    :done))

(defn scan!
  "Look round, note what was seen and the spot as searched, book the findings."
  [c names]
  (let [{:keys [scan-radius seen-ttl-s entity-ttl-s searched-ttl-s] want :count} (:args c)
        sighted (vec (sense (:primitives c) names scan-radius))
        [x y z] (here c)
        [ox _ oz] (:origin (ctx/mem c))]
    (notes/note! c (conj (mapv #(assoc % :kind :seen :ttl-ms (* 1000 (if (:id %) entity-ttl-s seen-ttl-s))) sighted)
                         {:kind :searched :what names :pos [x y z] :r scan-radius :ttl-ms (* 1000 searched-ttl-s)}))
    (ctx/update-mem! c (fn [m] (-> m
                                   (update :scans (fnil inc 0))
                                   (update :farthest (fnil max 0) (js/Math.round (xz-dist [x z] [ox oz])))
                                   (update :found #(add-found (or % []) sighted want)))))))

(defn candidates [c m]
  (let [{:keys [pattern heading spacing max-distance]} (:args c)
        [ox _ oz] (:origin m)
        [x _ z] (here c)]
    (if (= :outward (keyword pattern))
      (outward-points [x z] (keyword heading) spacing)
      (spiral-points [ox oz] spacing max-distance))))

(defn choose-leg
  "{:leg [x y z] (or nil when none is left) :tried [points looked at] :failed [{:pos :reason}] :skipped n
  :unloaded [{:pos :reason :not-loaded}]}; an unloaded column is not tried, so a later round looks again."
  [c m names ns]
  (let [{:keys [max-distance]} (:args c)
        [ox y oz] (here c)
        origin [((:origin m) 0) ((:origin m) 2)]
        tried (set (:tried m))
        p (:primitives c)
        block-at (fn [[bx by bz]] (u/block-name p {:x bx :y by :z bz}))]
    (loop [[pt & more] (candidates c m) acc {:tried [] :failed [] :skipped 0 :unloaded []}]
      (cond
        (nil? pt) acc
        (or (tried pt) (> (xz-dist pt origin) max-distance) (= pt [ox oz])) (recur more acc)
        (some #(notes/covers? % pt names) ns) (recur more (-> acc (update :tried conj pt) (update :skipped inc)))
        :else (let [{sy :y fail :fail} (stand-cell block-at (pt 0) (pt 1) y)
                    failed {:pos [(pt 0) y (pt 1)] :reason fail}]
                (cond
                  (= :not-loaded fail) (recur more (update acc :unloaded conj failed))
                  fail (recur more (-> acc (update :tried conj pt) (update :failed conj failed)))
                  :else (assoc (update acc :tried conj pt) :leg [(pt 0) sy (pt 1)])))))))

(defn ^:async walk!
  "One go-to round toward the leg; on its end book the leg arrived or failed."
  [c]
  (let [[x y z] (:leg (ctx/mem c))
        r (await (ctx/call-child c :leg 'jobs.movement.go-to {:pos {:x x :y y :z z} :range 2 :escalate false}))]
    (if-not (= :done r)
      :continue
      (let [arrived (:arrived (ctx/child-result c :leg))
            in-row (if arrived 0 (inc (:failed-in-row (ctx/mem c) 0)))]
        (ctx/update-mem! c (fn [m] (cond-> (assoc (dissoc m :leg) :failed-in-row in-row)
                                     (not arrived) (update :failed (fnil conj []) {:pos [x y z] :reason :unreachable}))))
        (if (>= in-row max-failed-in-row) (finish! c :not-found :stuck) :continue)))))

(defn ^:async step!
  "Look round; end when enough is found or the legs are used up, else start the next leg."
  [c]
  (let [{:keys [max-legs use-notes max-distance] want :count} (:args c)
        names (targets c)
        _ (scan! c names)
        ns (notes/notes c)
        m (ctx/mem c)
        found (cond-> (:found m) use-notes (add-found (noted ns names (:origin m) max-distance) want))]
    (ctx/update-mem! c assoc :found found)
    (cond
      (>= (count found) want) (finish! c :found nil)
      (>= (:legs m 0) max-legs) (finish! c :not-found :legs)
      :else (let [{:keys [leg tried failed skipped unloaded]} (choose-leg c m names ns)
                  now (ctx/now c)
                  waited (- now (:waiting-since m now))]
              (ctx/update-mem! c (fn [m] (-> m
                                             (update :tried (fnil into []) tried)
                                             (update :failed (fnil into []) failed)
                                             (update :skipped (fnil + 0) skipped))))
              (cond
                leg (do (ctx/update-mem! c (fn [m] (-> m (dissoc :waiting-since) (assoc :leg leg) (update :legs (fnil inc 0)))))
                        (await (walk! c)))
                (empty? unloaded) (finish! c :not-found :distance)
                (< waited (* 1000 (:load-wait-s (:args c))))
                (do (ctx/update-mem! c assoc :waiting-since (- now waited) :wait-until (+ now wait-ms))
                    :continue)
                :else (do (ctx/update-mem! c update :failed (fnil into []) unloaded)
                          (finish! c :not-found :not-loaded)))))))

(defn bad-args? [c]
  (let [{:keys [pattern heading]} (:args c)]
    (not (and (patterns (keyword pattern)) (headings (keyword heading))))))

(defn ^:async round [c]
  (ctx/update-mem! c (fn [m] (cond-> m (nil? (:origin m)) (assoc :origin (here c) :started (ctx/now c)))))
  (let [{:keys [leg started]} (ctx/mem c)]
    (cond
      (bad-args? c) (finish! c :not-found :bad-args)
      (>= (- (ctx/now c) started) (* 1000 (:timeout-s (:args c)))) (finish! c :not-found :time)
      leg (await (walk! c))
      :else (await (step! c)))))
