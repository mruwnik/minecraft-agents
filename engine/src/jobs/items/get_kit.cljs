(ns jobs.items.get-kit
  (:require [jobs.lib.ready :as ready]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.settings :as settings]
            [jobs.items.recipes :as recipes]
            [jobs.items.smelt.plan :as smelt-plan]
            [jobs.lib.armour :as armour]
            [jobs.lib.checks :as checks]
            [jobs.lib.combat :as combat]
            [jobs.lib.foods :as foods]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.util :as u]))

(def doc
  "Get :items in order (default the iron kit: pickaxe, sword, axe, helmet, chestplate, leggings, boots) and, with
  :wear, wear each armour piece once it is carried. One call is the whole goal; it yields (:continue) only while a
  child waits on the world.
  Each round starts from the world: an item carried or worn (or a better armour piece worn in its slot) is skipped.
  When the next missing item needs furnace material, one jobs.items.obtain child gets what all missing pieces need together (iron_ingot: 32 for the
  whole kit, less what is carried), so the ore is mined and smelted in one go; if it cannot, the job stops at once
  naming the material, unless some of it (raw input, or the material itself) is carried: then raw input is smelted, the pieces it covers are made in
  order and the job stops at the first one it cannot make. Then one obtain child per missing piece, in order, with :explore and a :spend :job-time of the ::minutes setting
  (longer than the fetch limit). Each piece got is an info kit.progress.
  While furnace material is still short (mining or smelting ahead; pure crafting needs no food) and fewer than
  :min-food food points are carried (nil: ::min-food; harmful food does not count), a jobs.survival.get-food child
  (with :explore, and :stock :min-food to fill the pack) runs before the work and again after each piece (:food false: no child, stops :needs-food instead). A child that
  stops (:no-food, or :no-stock when fed with nothing to stock) ends the job :needs-food with its reason as :cause only
  when the body is starving (jobs.lib.foods/starving?); else a warn kit.no-food names the cause and the work goes on, with
  no new food search unless the body starves. Get a base first.
  A death since the job began (body memory :died) clears the children and what was made; the next round re-plans from
  what is carried. A death only keeps the job listed when it was submitted with :survive-death?.
  Needs (condition): :items, else impossible :bad-args. Ends done with {:have [..] :made [..]}. Every other ending has
  :have, :made, the next :item, a :why and a :text:
  - the obtain child's kind and cause (:no-source, a walk cause such as :zone-exit or :not-home, :time ..; :item is then the furnace material when that failed);
  - condition-failed {:reason :needs-food :cause c :food :min-food} (the starving body found no food, or :food is false);
  - nothing-possible {:reason :wear-failed}.
  Quote: a guess per missing item. done? is ok when every item is carried or worn.")

(def iron-kit
  ["iron_pickaxe" "iron_sword" "iron_axe" "iron_helmet" "iron_chestplate" "iron_leggings" "iron_boots"])

(a/defargs args
  {:items {:doc "the items to get, in order" :spec (a/coll-of a/item?) :default iron-kit}
   :wear {:doc "wear each armour piece once it is carried" :spec boolean? :default true}
   :explore {:doc "passed to the obtain children: walk and look for raw needs not seen" :spec (a/or-of boolean? map?) :default true}
   :food {:doc "run a get-food child when mining or smelting is ahead and fewer than :min-food points are carried; false: stop :needs-food" :spec boolean? :default true}
   :min-food {:doc "food points to carry before mining or smelting starts and between pieces; nil: ::min-food" :spec (a/int-in 0 nil) :default nil}})

(a/defargs settings
  {::minutes {:default 45 :doc "Time budget of each obtain child of the kit." :spec (a/num-in 0 nil)}
   ::job-time-s {:default 10800 :doc "Seconds a kit may last: every child of every piece." :spec (a/num-in 0 nil)}
   ::item-s {:default 120 :doc "Seconds a quote counts for each missing item of the kit." :spec (a/num-in 0 nil)}
   ::min-food {:default 20 :doc "Food points carried before the kit job starts work or a new piece." :spec (a/int-in 0 nil)}})

(def target [:items])

(def safe? ready/threatened)
(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s}})

(defn furnace-short
  "{material n}: the furnace products (iron_ingot ..) that the first recipes of items need together, beyond carried
  ({name count})."
  [version items carried]
  (let [rs (recipes/recipes-for version)
        need (apply merge-with + {} (for [item items
                                          [k v] (:needs (first (get rs item)))
                                          :when (contains? smelt-plan/made-from k)]
                                      {k v}))]
    (into {} (keep (fn [[k v]] (let [n (- v (get carried k 0))] (when (pos? n) [k n])))) need)))

(defn kit-entries
  "The reserve entries (strength 0.9, by job id) for the furnace material the missing items need together and the raw
  input it is smelted from: a deposit or craft leaves those carried units alone."
  [id version items]
  (vec (for [[mat n] (furnace-short version items {})
             item [mat (get smelt-plan/made-from mat)]]
         {:id (str id "-kit-" item) :item item :count n :strength 0.9 :by id :why "material for the kit"})))

(defn reserve-kit!
  "Replace this job's reserve entries with kit-entries for todo; none once nothing is missing."
  [c todo]
  (ctx/forget-where! c reserve/kind #(= (:id c) (:by %)))
  (doseq [e (kit-entries (:id c) (game/version-of (:primitives c)) todo)]
    (ctx/remember! c reserve/kind e reserve/policy)))

(defn restarted
  "The job memory after a death: the children and what was made went with the body; the job starts from now."
  [m now]
  (-> m (dissoc :children :materials :made :fed :partial :no-food :weapon-tried) (assoc :since now)))

(defn why-of
  "The text a child ending e gives for its stop: its data's :why, else its cause map's."
  [e]
  (or (get-in e [:data :why]) (when (map? (:cause e)) (:why (:cause e))) (get-in e [:data :text])))

(defn stop-of
  "A way the kit stops: the ending kind and cause, the cause's keyword as :reason and the text why."
  [kind cause why]
  {:kind kind :cause cause :reason (contract/cause-key {:kind kind :cause cause}) :why why})

(defn wear-outcome
  "What a wear child ending e means: :continue (yield), nil (it wore something), or a stop (stop-of) :wear-failed."
  [e]
  (cond (= :continue e) :continue
        (and (= :done (:kind e)) (seq (:worn (:data e)))) nil
        :else (stop-of :nothing-possible {:reason :wear-failed} (contract/cause-key e))))

(defn stale?
  "Whether the body died (latest :died entry) after the job booked :since."
  [m died]
  (boolean (and died (:since m) (> (:t died) (:since m)))))

(defn had?
  "Carried, worn, or an armour piece whose slot holds one at least as good."
  [carried worn item]
  (let [{:keys [slot rank]} (armour/parse item)]
    (boolean (or (contains? carried item)
                 (some #{item} (vals worn))
                 (and slot (>= (armour/rank-of (get worn slot)) rank))))))

(defn state [c]
  (let [p (:primitives c)
        inv (u/inventory p)]
    {:inv inv
     :carried (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} inv)
     :worn (armour/worn-of (.-equipment (.self p)))}))

(defn to-wear
  "The listed armour pieces carried that beat what their slot holds."
  [items {:keys [carried worn]}]
  (filter (fn [item] (let [{:keys [slot rank]} (armour/parse item)]
                       (and slot (contains? carried item) (> rank (armour/rank-of (get worn slot))))))
          items))

(defn bad-args [c]
  (when-not (seq (:items (:args c)))
    {:reason :bad-args :why "needs :items, a list of item names"}))

(defn can-start? [c] (checks/cause-status :never (bad-args c)))

(defn done?
  "Ok when every item is carried or worn (with :wear, no carried piece is left to put on) and no child is under way."
  [c]
  (let [st (state c)
        items (:items (:args c))
        had (filterv #(had? (:carried st) (:worn st) %) items)
        n (count had)
        data [:units :items :done n :left (- (count items) n) :result {:have had :made (vec (:made (ctx/mem c)))}]
        text (str "get-kit: " n " of " (count items) " item(s) had")]
    (cond
      (checks/child-under-way? c) (apply contract/later :wrapping-up text data)
      (and (= n (count items)) (not (and (:wear (:args c)) (seq (to-wear items st))))) (apply contract/ok :kitted text data)
      :else (apply contract/later :kitting text data))))

(defn quote-of
  "A guess of the seconds each missing item takes."
  [c]
  (let [st (state c)
        n (count (remove #(had? (:carried st) (:worn st) %) (:items (:args c))))
        t (* n (settings/get settings ::item-s))]
    {:uses {:job-time [0 t ##Inf]}}))

(defn obtain-args [c item total]
  (let [a (:args c)]
    (assoc (if (vector? item) {:any-of item} {:item item})
           :count total :explore (:explore a)
           :spend {:job-time (* 60 (settings/get settings ::minutes))})))

(defn armed?
  "Whether a weapon (jobs.lib.combat/default-weapons: a sword or an axe) is carried."
  [carried]
  (boolean (some #(combat/weapon? combat/default-weapons %) (keys carried))))

(def starter-weapons
  "The cheap weapons made before the mining starts, best first."
  ["stone_sword" "wooden_sword"])

(defn ^:async run-obtain!
  "One obtain child round in slot: :continue, or the stop (stop-of) of the child's ending when it ended without the item
  (nil when it got it)."
  [c slot item n]
  (let [e (await (need/call! c slot (quote jobs.items.obtain) (obtain-args c item (need/carry-count c slot (if (vector? item) item [item]) n))))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) nil
      :else (stop-of (:kind e) (:cause e) (why-of e)))))

(defn finish!
  "End the call: done when out is nil, else with the stop out; the data has what is had and made, and a text."
  [c items have out]
  (let [data {:have have :made (vec (:made (ctx/mem c)))}]
    (reserve-kit! c [])
    (if-not out
      (ctx/end c :done nil data)
      (ctx/end c (:kind out) (:cause out)
               (merge data (select-keys out [:item :why])
                      {:text (str "kit stopped at " (:item out) ": " (name (:reason out)) (when (:why out) (str " (" (:why out) ")")))})))))

(defn food-args
  "The get-food child's args: feed the body, then stock the pack up to :min-food points."
  [c]
  {:explore (:explore (:args c))
   :stock (or (:min-food (:args c)) (settings/get settings ::min-food))})

(defn ^:async run-food!
  "One get-food child round: :continue, nil when the body is fed or not hungry, or {:cause :why} when it could not be fed."
  [c]
  (let [e (await (ctx/call c :food 'jobs.survival.get-food (food-args c)))
        cause (contract/cause-key e)]
    (cond
      (= :continue e) :continue
      (or (= :done (:kind e)) (= :not-hungry cause)) nil
      :else {:cause cause :why (why-of e)})))

(defn ^:async round [c]
  (let [a (:args c)
        items (vec (:items a))
        min-food (or (:min-food a) (settings/get settings ::min-food))
        version (game/version-of (:primitives c))
        m (ctx/mem c)]
    (cond
      (nil? (:since m)) (let [st (state c)
                              todo (remove #(had? (:carried st) (:worn st) %) items)
                              short (furnace-short version todo (:carried st))]
                          (ctx/update-mem! c assoc :since (ctx/now c))
                          (ctx/emit! c :kit.plan :info {:items (vec todo) :short short
                                                        :text (str "kit: getting " (str/join ", " todo)
                                                                   (when (seq short) (str " (needs " (str/join ", " (map (fn [[k n]] (str n " " k)) short)) " from the furnace)")))}))
      (stale? m (ctx/latest c :died))
      (do (ctx/update-mem! c restarted (ctx/now c))
          (ctx/emit! c :kit.resumed :info {:text "died since the kit began: starting again from what is carried and worn"})))
    (pace/steps!
     c
     (fn ^:async step []
      (let [st (state c)
            wear (when (:wear a) (first (to-wear items st)))
            have (filterv #(had? (:carried st) (:worn st) %) items)
            todo (remove (set have) items)
            _ (reserve-kit! c todo)
            fp (foods/stock-points (:inv st))
            partial (:partial (ctx/mem c))
            need (when (and (seq todo) (seq (furnace-short version [(first todo)] {})))
                   (first (furnace-short version todo (:carried st))))
            short (when-not partial need)
            raw (when partial (get (:carried st) (get smelt-plan/made-from (first need)) 0))]
        (cond
          wear
          (let [out (wear-outcome (await (ctx/call c :wear 'jobs.items.wear {:item wear})))]
            (cond (= :continue out) :continue
                  out (finish! c items have (assoc out :item wear))
                  :else :again))

          (empty? todo) (finish! c items have nil)

          (and need (not (armed? (:carried st))) (not (:weapon-tried (ctx/mem c))))
          (let [r (await (run-obtain! c :weapon starter-weapons 1))]
            (cond (= :continue r) :continue
                  :else (do (ctx/update-mem! c assoc :weapon-tried true)
                            (when r (ctx/emit! c :kit.no-weapon :warn {:reason (:reason r) :why (:why r)
                                                                       :text "no sword or axe to fight with, going on unarmed"}))
                            :again)))

          (and short (< fp min-food) (not (:fed (ctx/mem c)))
               (not (and (:no-food (ctx/mem c)) (not (foods/starving? (.self (:primitives c)) {})))))
          (let [r (when (:food a) (await (run-food! c)))]
            (cond (= :continue r) :continue
                  (and (some? r) (not (foods/starving? (.self (:primitives c)) {})))
                  (do (ctx/update-mem! c assoc :no-food true)
                      (ctx/emit! c :kit.no-food :warn {:cause (:cause r) :why (:why r) :food fp
                                                       :text (str "no food found (" (name (:cause r)) "), going on with " fp " food points carried")})
                      :again)
                  (some? r) (finish! c items have (assoc (stop-of :condition-failed {:reason :needs-food :cause (:cause r) :food fp :min-food min-food} (:why r))
                                                         :item (first todo)))
                  (not (:food a)) (finish! c items have (assoc (stop-of :condition-failed {:reason :needs-food :food fp :min-food min-food}
                                                                        (str "carries " fp " food points, needs " min-food ": get a food supply first"))
                                                               :item (first todo)))
                  :else (do (ctx/update-mem! c assoc :fed true) :again)))

          short
          (let [[mat n] short
                r (await (run-obtain! c :materials mat n))
                carried-some (+ (get (:carried st) (get smelt-plan/made-from mat) 0) (get (:carried st) mat 0))]
            (cond (= :continue r) :continue
                  (and (some? r) (pos? carried-some)) (do (ctx/update-mem! c assoc :partial (assoc r :item mat))
                                                         :again)
                  (some? r) (finish! c items have (assoc r :item mat))
                  :else :again))

          (and partial (not (:smelted partial)) (or (:smelting partial) (pos? raw)))
          (let [[mat n] need
                r (await (run-obtain! c :smelt mat (min (or (:smelting partial) raw) n)))]
            (cond (= :continue r) (do (ctx/update-mem! c assoc-in [:partial :smelting] (or (:smelting partial) raw))
                                      :continue)
                  (some? r) (finish! c items have (assoc r :item mat))
                  :else (do (ctx/update-mem! c assoc-in [:partial :smelted] true)
                            :again)))

          :else
          (let [item (first todo)
                r (await (run-obtain! c (keyword item) item 1))]
            (cond
              (= :continue r) :continue
              (some? r) (finish! c items have (assoc (cond-> r partial (update :why #(str % " (the materials for the whole kit failed: "
                                                                                                           (:reason partial) ")")))
                                                     :item item))
              :else (do (ctx/update-mem! c #(-> % (update :made (fnil conj []) item) (dissoc :fed)))
                        (ctx/emit! c :kit.progress :info {:item item :have (inc (count have)) :of (count items)
                                                          :text (str "got " item " (" (inc (count have)) " of " (count items) ")")})
                        :again)))))))))

(def meta {:tags [:craft]})
