(ns jobs.farm.fertilize
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.job-common :as common]
            [jobs.lib.gate :as gate]
            [jobs.lib.need :as need]
            [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [jobs.lib.work-place :as work-place]
            [jobs.lib.util :as u]
            [jobs.lib.crops :as crops]
            [jobs.lib.field-area :as fa]
            [jobs.lib.farm :as farm]
            [jobs.lib.look :as look]
            [jobs.lib.pace :as pace]
            [jobs.lib.places :as places]
            [jobs.lib.toll-cells :as tc]
            [jobs.lib.trees :as trees]))

(def doc
  "Use bone meal on unripe crops: the unripe crops of the :area (a box, a circle, or {:cells [pos ...]} of crops or saplings; nil: a circle of radius 8 round the body), nearest first.
  With :grass true it uses bone meal once on each open grass block (nothing on top) it has seen, instead of on crops.
  Needs bone meal in the inventory; it is fetched (jobs.lib.need, from the call's :ways) when none is carried and none was used yet; a
  failed fetch ends the call with its ending as the :because. A crop that refuses bone meal or cannot be reached is skipped.
  Zones: a crop in another owner's zone or claim, or inside a plan's footprint, is skipped (it counts as a
  :harvest). :ignore-zones? true skips the check.
  Refused before it starts, as failed conditions: {:reason :no-zones} (no zone list read, unless :ignore-zones?) and
  {:reason :need :item \"bone_meal\"} (none carried, targets left, :ways #{:carried}).
  Ends when :max uses are spent, the bone meal runs out or no unripe crop is left, with data {:used n}: done when it used
  some (at least :least, uses) or when no unripe crop was there; condition-failed :refused when crops were there and under :least
  took bone meal (zones, unreachable, the crop refused), :need when the bone meal ran out under :least. done? counts the uses and the targets left.")

(a/defargs args
  {:area {:doc "the crops: a box {:min :max}, a circle {:pos :radius} or {:cells [pos ...]} (one or more crops or saplings); nil: a circle of radius 8 round the body" :spec fa/spec :default nil}
   :grass {:doc "fertilize open grass blocks instead of crops" :spec boolean? :default false}
   :max {:doc "bone meal uses, at most" :spec (a/int-in 1 nil) :default 16}
   :ignore-zones? :shared})

(def ripe-age crops/ripe-age)

(defn age-of
  "The age of a block result from the primitives, or nil."
  [b]
  (or (.-age b) (some-> b .-properties .-age)))

(defn unripe?
  "True when b (a block result) is a crop below its ripe age."
  [b]
  (let [ripe (some-> b .-name ripe-age)
        age (when b (age-of b))]
    (boolean (and ripe age (< age ripe)))))

(def default-radius 8)

(defn open-grass
  "The seen grass blocks within reach of the body with air above, as positions."
  [p reach]
  (->> (look/seen-blocks p {:names ["grass_block"] :radius reach :max 4096 :live? true})
       (map :pos)
       (filter #(= "air" (u/seen-name p (update % :y inc))))))

(def ways need/fetch-ways)

(a/defargs settings
  {::job-time-s {:default 300 :doc "Seconds the call may take (a guess until measured)." :spec (a/num-in 0 nil)}})

(def fit? (ready/fit-for :work))
(def target [:area :grass])

(def defaults {:spend {:job-time ::job-time-s}})
(def safe? (price/staying :work (ready/safe-at-by-night work-place/spot)))

(defn raw-targets
  "The positions to fertilize (unripe crops, or open grass with :grass), before the zone rules and the refused ones."
  [c]
  (let [p (:primitives c)
        me (u/self-pos c)
        area (or (fa/of c) {:pos me :radius default-radius})
        grass? (:grass (:args c))]
    (if (= :cells (fa/kind area))
      (filter (if grass?
                #(and (= "grass_block" (u/seen-name p %)) (= "air" (u/seen-name p (update % :y inc))))
                #(let [b (u/seen-block p %)] (or (unripe? b) (trees/sapling-name? (some-> b .-name)))))
              (map a/cell (:cells area)))
      (let [[mid radius] (fa/sphere area)
            reach (+ radius (u/dist me mid))]
        (->> (if grass?
               (open-grass p reach)
               (->> (crops/seen-crops p (keys ripe-age) reach 4096)
                    (filter #(some-> (:age %) (< (ripe-age (:name %)))))
                    (map :pos)))
             (filter #(fa/contains-pos? area %)))))))

(defn targets
  "The positions to fertilize, nearest first, minus the refused ones and what the zone rules keep from the body."
  [c]
  (let [refused (:refused (ctx/mem c) #{})
        me (u/self-pos c)]
    (->> (:allowed (gate/look c :harvest (remove refused (raw-targets c)) {:own-plans-ok? true}))
         (sort-by #(u/dist me %)))))

(defn lacks-meal?
  "Whether bone meal is to be got: none carried, none used yet and targets left."
  [c]
  (and (not (contains? (u/carried-names (:primitives c)) "bone_meal"))
       (not (pos? (:used (ctx/mem c) 0)))
       (seq (targets c))))

(defn fetch-opts
  "The limits {:how} for fetching bone meal, nil when the call's ways leave only :carried."
  [c]
  (when-let [how (seq (need/fetch-how c))]
    {:how (set how)}))

(defn no-meal-source
  "The cause :need bone_meal while bone meal is to be got and the ways forbid it."
  [c]
  (when (and (lacks-meal? c) (nil? (fetch-opts c))) {:reason :need :item "bone_meal"}))

(def bad-args (checks/args-fit "fertilize :area must be a box, circle or cells" #(contains? #{nil :box :circle :cells} (fa/kind (fa/of %)))))

(def can-start? (cond/and bad-args checks/zones-read
                          (checks/refusing :meal "bone meal carried or fetchable" no-meal-source)))

(defn done?
  "Ok once :max uses are made, or a look round showed nothing left to fertilize and the uses meet :least; else later
  with the uses made and the targets left within :max."
  [c]
  (let [used (:used (ctx/mem c) 0)]
    (if (or (>= used (:max (:args c)))
            (and (look/surveyed? c) (empty? (raw-targets c))
                 (or (zero? used) (common/enough? c used used))))
      (contract/ok :fertilized (str "fertilized with " used " bone meal") :units :cells :done used :left 0 :result {:used used})
      (contract/later :fertilizing (str "fertilize: " used " bone meal used") :units :cells :done used
                      :left (min (count (targets c)) (max 0 (- (:max (:args c)) used))) :result {:used used}))))

(defn quote-of [c]
  (let [n (min (:max (:args c)) (count (raw-targets c)))]
    {:uses {:job-time [n (* 3 n) (* 15 n)] :items {"bone_meal" [0 n (* 2 n)]}}
     :done [0 n n]}))

(defn conclude!
  "The ending of a run with nothing left to do, budget spent or no bone meal."
  [c]
  (let [used (:used (ctx/mem c) 0)
        left (count (raw-targets c))
        data {:used used}
        done (assoc data :text (str "fertilized with " used " bone meal"))]
    (cond
      (and (pos? used) (common/enough? c used (min (:max (:args c)) (+ used left)))) (ctx/end c :done nil done)
      (pos? used) (if (farm/has-meal? c)
                    (ctx/end c :condition-failed :refused data)
                    (ctx/end c :condition-failed {:reason :need :item "bone_meal" :count 1} data))
      (pos? left) (ctx/end c :condition-failed :refused data)
      :else (ctx/end c :done nil done))))

(defn ^:async fertilize-one!
  "Finish when nothing is left to fertilize, the budget is spent or the bone meal ran out; else walk to the nearest
  target of todo and use one bone meal."
  [c todo used]
  (if (or (empty? todo) (>= used (:max (:args c))) (not (farm/has-meal? c)))
    (conclude! c)
    (let [target (first todo)
          refuse! #(ctx/update-mem! c update :refused (fnil conj #{}) target)
          w (await (common/walk! c target 3 {:tolls (tc/walk-tolls c (:pos (places/parse-pos target)))}))]
      (case w
        :continue :continue
        :blocked (do (refuse!) :again)
        (if-not (gate/allowed? c :fertilize.declined "fertilize" :harvest target {:own-plans-ok? true})
          (do (refuse!) :again)
          (let [res (await (child/use-on! c target "bone_meal" "up"))]
            (if (= :continue res)
              :continue
              (do (if (= :used (:status res))
                    (do (ctx/update-mem! c update :used (fnil + 0) (max 1 (or (some-> (:raw res) .-consumed) 1)))
                        (when (:grass (:args c)) (refuse!)))
                    (refuse!))
                  :again))))))))

(defn ^:async step
  "One step: fetch missing bone meal when none is carried (a failed fetch ends the call with its ending), then
  fertilize-one!."
  [c]
  (let [_ (when (empty? (targets c))
            (await (look/survey! c :cell)))
        fetched (when (lacks-meal? c) (await (need/item! c "bone_meal" 1 (fetch-opts c))))]
    (cond
      (= :continue fetched) :continue
      (and fetched (not= :done (:kind fetched))) (ctx/end c (:kind fetched) (:cause fetched) {:used (:used (ctx/mem c) 0)})
      fetched :again
      :else (await (fertilize-one! c (targets c) (:used (ctx/mem c) 0))))))

(defn ^:async round
  "The whole attempt: loop the steps until the targets or the bone meal are done; :continue only while a walk or fetch
  waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async s [] (await (step c))))))
