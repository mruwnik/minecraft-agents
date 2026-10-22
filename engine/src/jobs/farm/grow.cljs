(ns jobs.farm.grow
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.child :as child]
            [jobs.lib.crops :as crops]
            [jobs.lib.need :as need]
            [jobs.lib.next-call :as next-call]
            [jobs.lib.pace :as pace]
            [jobs.lib.patch :as patch]
            [jobs.lib.ready :as ready]
            [jobs.lib.util :as u]))

(def meta {:tags [:goal]})

(def doc
  "Grow a crop and harvest it, from a body that has no field: the way to wheat for animals when none stands anywhere. Every round
  reads the world first, so a cut or restarted run takes up the field it finds (a field already standing is never started
  again):
  1. A crop of :crop seen and ripe (a zone or claim of another owner leaves it): cut it with jobs.farm.harvest, which replants.
  2. A crop seen, none ripe: wait (a jobs.time.noop pause of ::wait-s, holding still, then look again). A field growing is never replaced.
  3. No crop seen: get a hoe (jobs.items.get-tool) and one seed per cell of a :w x :h field (jobs.items.obtain; wheat seeds are
     broken out of grass), pick the spot (jobs.farm.find-spot, nearest to the body) and work it with jobs.farm.tend (till,
     irrigate, plant, bone meal when carried), once per round; a spot that is dark is lit first (jobs.build.light-area to crop light, torches hung beside the field). Three tends that leave no crop standing end the run (a tend refused before it started, e.g. :away, is not one: the run yields). A planted field is judged from its remembered cells at any distance.
  Needs: a source of a hoe and seed (the fetch jobs say what lacks), a field spot in range, a zone list.
  Done once :count of the crop's item is carried. Ends nothing-possible with the failed child's cause: :no-spot (find-spot found none),
  :not-planted (three tends planted nothing), :no-light (the spot could not be lit), or a fetch's :need/:tool; limit-reached :timeout after :spend :job-time.
  Every ending but done has :next (jobs.lib.next-call): the failed child's calls, then the grow again.
  Quote: the count as yield; time is the wait for ripening, a guess.")

(a/defargs args
  {:crop {:doc "the crop block to grow (wheat, carrots, potatoes, beetroots); only wheat has a seed that can be got without a field" :spec a/name? :default "wheat"}
   :count {:doc "how many of the crop's item to carry at least (carried at the start count)" :spec (a/int-in 1 nil) :default 1}
   :w {:doc "field width (x), 1 to 16" :spec (a/int-in 1 16) :default 3}
   :h {:doc "field depth (z), 1 to 16" :spec (a/int-in 1 16) :default 3}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 3600 :doc "Seconds a grow may take: the field has to ripen." :spec (a/num-in 0 nil)}
   ::wait-s {:default 30 :doc "Seconds the body holds still before it looks at the growing crop again." :spec (a/num-in 1 nil)}
   ::max-tends {:default 3 :doc "Tend passes in a row that leave no crop standing before the run gives up." :spec (a/int-in 1 nil)}})

(def target [:crop])
(def defaults {:spend {:job-time ::job-time-s}})
(def safe?
  "It roams to its field's spot and the children it calls judge their own exposure."
  (with-meta ready/threatened
    {::ready/exposure (fn [_] (contract/ok :survey "the children judge their own exposure"))}))
(def fit? ready/ok)

(defn item-of
  "The item the crop's harvest gives."
  [crop]
  (first (get-in crops/field-items [crop :produce])))

(defn carried-count [c item]
  (reduce + 0 (for [s (u/inventory (:primitives c)) :when (= item (:name s))] (:count s))))

(defn done?
  "Ok once :count of the crop's item is carried."
  [c]
  (let [{:keys [crop count]} (:args c)
        item (item-of crop)
        have (carried-count c item)]
    (if (>= have count)
      (contract/ok :carried (str "carrying " have " " item) :units :items :done have :left 0 :result {:got have :item item})
      (contract/later :growing (str item ": " have " of " count " carried") :units :items :done have :left (- count have)))))

(defn quote-of [c]
  (let [n (:count (:args c))]
    {:uses {:job-time [0 (settings/get settings ::wait-s) (settings/get settings ::job-time-s)]}
     :yields {:items {(item-of (:crop (:args c))) [0 n n]}}}))

(defn remembered
  "The crops of :crop the body remembers in its field's area (any distance), as seen-crops gives them."
  [c]
  (let [{:keys [min max]} (:area (ctx/mem c))
        crop (:crop (:args c))
        p (:primitives c)]
    (when min
      (vec (for [x (range (min 0) (inc (max 0))) y (range (min 1) (inc (max 1))) z (range (min 2) (inc (max 2)))
                 :let [pos {:x x :y y :z z}
                       b (u/seen-block p pos)]
                 :when (and b (= crop (.-name b)))]
             {:name crop :pos pos :age (some-> (aget (.-properties b) "age") js/Number)})))))

(defn standing
  "The crops of :crop the body has seen standing (its field's remembered cells first, then those near the body), that its zones allow cutting and no harvest gave up on (the cells it left)."
  [c]
  (let [in (access/rules-input c)
        given-up (:given-up (ctx/mem c))
        near (crops/seen-crops (:primitives c) [(:crop (:args c))] 64 256)
        known (remembered c)]
    (filterv #(and (:ok (access/may? in :harvest (:pos %)))
                   (every? (fn [g] (> (u/dist (:pos %) g) 0.5)) given-up))
             (concat known (remove (fn [b] (some #(= (:pos %) (:pos b)) known)) near)))))

(defn ripe? [crop b] (some-> (:age b) (>= (crops/ripe-age crop))))

(defn hoe? [c] (boolean (some #(.endsWith (:name %) "_hoe") (u/inventory (:primitives c)))))

(defn box-of
  "The tend area of a find-spot spot: the ground layer at the spot's y, the crop layer above."
  [{:keys [pos w h]}]
  {:min [(:x pos) (:y pos) (:z pos)] :max [(+ (:x pos) (dec w)) (inc (:y pos)) (+ (:z pos) (dec h))]})

(defn failed
  "The ending of the fetch e that failed: its kind, cause and data."
  [c e]
  (ctx/end c (:kind e) (:cause e) (:data e)))

(defn ^:async fetch-step!
  "What the field needs carried, one fetch at a time: nil when all is carried, :continue while a fetch waits, else the failed ending."
  [c]
  (let [{:keys [crop w h]} (:args c)
        seed (get-in crops/field-items [crop :seed])
        cells (* w h)
        opts {:explore true :slot :need :minutes 10}
        e (cond
            (not (hoe? c)) (await (need/tool! c {:kind "hoe"} opts))
            (< (carried-count c seed) cells) (await (need/item! c seed (- cells (carried-count c seed)) opts)))]
    (cond
      (nil? e) nil
      (= :continue e) :continue
      (= :done (:kind e)) :again
      :else (failed c e))))

(defn ^:async spot-step!
  "Pick the field's place into memory: :continue while the search runs, :again once set, else the ending."
  [c]
  (let [{:keys [w h]} (:args c)
        e (await (ctx/call c :spot 'jobs.farm.find-spot {:w w :h h}))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) (do (let [spot (first (:spots (:data e)))]
                                        (ctx/update-mem! c assoc :area (box-of spot) :dark? (boolean (:dark? spot)))
                                        :again))
      :else (contract/with-because (ctx/end c :nothing-possible :no-spot (select-keys (:data e) [:why])) e))))

(defn ^:async light-step!
  "A dark spot: light the field to crop light with torches hung beside it, once. nil when lit or done, :continue while it waits, else the ending."
  [c]
  (let [{:keys [area dark? lit]} (ctx/mem c)]
    (when (and dark? (not lit))
      (let [e (await (ctx/call c :light 'jobs.build.light-area
                               (cond-> {:boxes [[(:min area) (:max area)]] :level patch/crop-light :beside? true}
                                 (:ignore-zones? (:args c)) (assoc :ignore-zones? true))))]
        (cond
          (= :continue e) :continue
          (= :done (:kind e)) (do (ctx/update-mem! c assoc :lit true) nil)
          :else (ctx/end c :nothing-possible :no-light (select-keys (:data e) [:uncovered])))))))

(defn declined?
  "Whether child ending e is a refusal before it ran (tend waits :away while another job is listed): no tend happened."
  [e]
  (and (= :condition-failed (:kind e)) (empty? (:data e))))

(defn ^:async tend-step!
  "One tend pass over the field: :again after it, :continue while it waits; three passes in a row that left no crop end the run."
  [c]
  (let [e (await (ctx/call c :tend 'jobs.farm.tend
                           (cond-> {:area (:area (ctx/mem c)) :fertilize true}
                             (:ignore-zones? (:args c)) (assoc :ignore-zones? true))))]
    (cond
      (= :continue e) :continue
      (declined? e) :continue
      :else
      (do (child/fruitless! c :tend e)
          (if (child/exhausted? c :tend (settings/get settings ::max-tends))
            (child/give-up c :tend :not-planted {:area (:area (ctx/mem c))})
            :again)))))

(defn ^:async found-field!
  "No crop seen: fetch the hoe and seeds, pick a spot, tend it."
  [c]
  (let [f (await (fetch-step! c))]
    (cond
      f f
      (nil? (:area (ctx/mem c))) (await (spot-step! c))
      :else (or (await (light-step! c)) (await (tend-step! c))))))

(defn ^:async step [c]
  (let [{:keys [crop ignore-zones?]} (:args c)
        seen (standing c)]
    (cond
      (some #(ripe? crop %) seen)
      (let [ripe (first (filter #(ripe? crop %) seen))
            e (await (ctx/call c :harvest 'jobs.farm.harvest
                               (cond-> {:crops [crop] :area {:pos (:pos ripe) :radius crops/cut-radius}} ignore-zones? (assoc :ignore-zones? true))))]
        (cond
          (= :continue e) :continue
          (not= :done (:kind e)) (do (ctx/update-mem! c update :given-up into (or (seq (:skipped (:data e))) [(:pos ripe)])) :again)
          :else :again))

      (seq seen)
      (do (ctx/update-mem! c dissoc :fruitless)
          (await (child/pause! c (settings/get settings ::wait-s) :growing))
          :continue)

      :else (await (found-field! c)))))

(defn ^:async round [c]
  (next-call/on-ending c (await (pace/steps! c #(step c))) 'jobs.farm.grow args))
