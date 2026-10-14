(ns jobs.items.get-kit
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.settings :as settings]
            [jobs.items.recipes :as recipes]
            [jobs.items.smelt.plan :as smelt-plan]
            [jobs.lib.armour :as armour]
            [jobs.lib.blocks :as b]
            [jobs.lib.combat :as combat]
            [jobs.lib.foods :as foods]
            [jobs.lib.util :as u]))

(def doc
  "Get :items in order (default the iron kit: pickaxe, sword, axe, helmet, chestplate, leggings, boots) and, with
  :wear, wear each armour piece once it is carried. One call is the whole goal; it yields (:continue) only while a
  child waits on the world.
  Each round starts from the world: an item carried or worn (or a better armour piece worn in its slot) is skipped.
  When the next missing item needs furnace material, one jobs.items.obtain child gets what all missing pieces need together (iron_ingot: 32 for the
  whole kit, less what is carried), so the ore is mined and smelted in one go; if it cannot, the job stops at once
  naming the material, unless raw input of it is carried: then that is smelted, the pieces it covers are made in
  order and the job stops at the first one it cannot make. Then one obtain child per missing piece, in order, with :explore and :minutes (nil: ::minutes,
  longer than the fetch limit). Each piece got is an info kit.progress.
  While furnace material is still short (mining or smelting ahead; pure crafting needs no food) and fewer than
  :min-food food points are carried (nil: ::min-food; harmful food does not count), a jobs.survival.get-food child
  (with :explore, and :stock :min-food to fill the pack) runs before the work and again after each piece (:food false: no child, stops :needs-food instead). A child that
  stops (:no-food, or :no-stock when fed with nothing to stock) ends the job :needs-food with its reason as :cause only
  when the body is starving (jobs.lib.foods/starving?); else a warn kit.no-food names the cause and the work goes on, with
  no new food search for ::no-food-wait-ms. Get a base first.
  A death since the job began (body memory :died) clears the children and what was made; the next round re-plans from
  what is carried. A death only keeps the job listed when it was submitted with :survive-death?.
  Ends {:status :done :have [..] :made [..]} or {:status :stopped :item next :reason r :why text :have :made}, warn
  kit.stopped; r is the obtain's (:no-source, :timeout ..; :item is then the furnace material when that failed),
  :needs-food (:food :min-food :cause) or :wear-failed.")

(def iron-kit
  ["iron_pickaxe" "iron_sword" "iron_axe" "iron_helmet" "iron_chestplate" "iron_leggings" "iron_boots"])

(a/defargs args
  {:items {:doc "the items to get, in order" :spec (a/coll-of a/item?) :default iron-kit}
   :wear {:doc "wear each armour piece once it is carried" :spec boolean? :default true}
   :explore {:doc "passed to the obtain children: walk and look for raw needs not seen" :spec (a/or-of boolean? map?) :default true}
   :minutes {:doc "time budget of each obtain child; nil: ::minutes" :spec (a/num-in 0 nil) :default nil}
   :food {:doc "run a get-food child when mining or smelting is ahead and fewer than :min-food points are carried; false: stop :needs-food" :spec boolean? :default true}
   :min-food {:doc "food points to carry before mining or smelting starts and between pieces; nil: ::min-food" :spec (a/int-in 0 nil) :default nil}})

(a/defargs settings
  {::minutes {:default 45 :doc "Time budget of each obtain child of the kit." :spec (a/num-in 0 nil)}
   ::no-food-wait-ms {:default 300000 :doc "After a food search fails while the body is not starving, how long the kit goes on without asking get-food again." :spec (a/num-in 0 nil)}
   ::min-food {:default 20 :doc "Food points carried before the kit job starts work or a new piece." :spec (a/int-in 0 nil)}})

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

(defn restarted
  "The job memory after a death: the children and what was made went with the body; the job starts from now."
  [m now]
  (-> m (dissoc :children :materials :made :fed :partial :no-food-until :weapon-tried) (assoc :since now)))

(defn wear-outcome
  "What a wear child round means: :continue (yield), nil (it wore something), or {:reason :wear-failed :why}.
  r is the call-child return, res the child's result, declined-why the reason its check waits with."
  [r res declined-why]
  (cond (= :continue r) :continue
        (= :declined r) {:reason :wear-failed :why declined-why}
        (seq (:worn res)) nil
        :else {:reason :wear-failed :why (:reason res)}))

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

(defn check [c]
  (or (seq (:items (:args c)))
      (ctx/wait c {:reason :bad-args :why "needs :items, a list of item names"})))

(defn obtain-args [c item n]
  (let [a (:args c)]
    (assoc (if (vector? item) {:any-of item} {:item item})
           :count (min 64 n) :explore (:explore a)
           :minutes (or (:minutes a) (settings/get settings ::minutes)))))

(defn armed?
  "Whether a weapon (jobs.lib.combat/default-weapons: a sword or an axe) is carried."
  [carried]
  (boolean (some #(combat/weapon? combat/default-weapons %) (keys carried))))

(def starter-weapons
  "The cheap weapons made before the mining starts, best first."
  ["stone_sword" "wooden_sword"])

(defn ^:async run-obtain!
  "One obtain child round in slot: :continue, or {:reason :why} when it ended without the item (nil when it got it)."
  [c slot item n]
  (let [oargs (obtain-args c item n)
        r (await (ctx/call-child c slot 'jobs.items.obtain oargs))]
    (case r
      :continue :continue
      :declined (let [w (b/child-wait c slot 'jobs.items.obtain oargs)]
                  {:reason (:reason w) :why (:why w)})
      (let [res (ctx/child-result c slot)]
        (when-not (= :done (:status res)) {:reason (:reason res :failed) :why (:why res)})))))

(defn finish! [c items have out]
  (let [res (merge {:have have :made (vec (:made (ctx/mem c)))} out)]
    (if (= :done (:status out))
      (ctx/emit! c :kit.done :info {:have have :text (str "kit complete: " (count have) " item(s)")})
      (ctx/emit! c :kit.stopped :warn (assoc (select-keys out [:item :reason :why]) :have have
                                             :text (str "kit stopped at " (:item out) ": " (name (:reason out))
                                                        (when (:why out) (str " (" (:why out) ")"))))))
    (ctx/result! c res)
    :done))

(defn food-args
  "The get-food child's args: feed the body, then stock the pack up to :min-food points."
  [c]
  {:explore (:explore (:args c))
   :stock (or (:min-food (:args c)) (settings/get settings ::min-food))})

(defn ^:async run-food!
  "One get-food child round: :continue, nil when the body is fed or not hungry, or {:reason :why} when it could not be fed."
  [c]
  (let [r (await (ctx/call-child c :food 'jobs.survival.get-food (food-args c)))]
    (case r
      :continue :continue
      :declined (let [w (b/child-wait c :food 'jobs.survival.get-food (food-args c))]
                  (when-not (= :not-hungry (:reason w)) {:cause (:reason w) :why (:why w)}))
      (let [res (ctx/child-result c :food)]
        (when (= :stopped (:status res)) {:cause (:reason res) :why (:text res)})))))

(defn ^:async round [c]
  (let [a (:args c)
        items (vec (:items a))
        min-food (or (:min-food a) (settings/get settings ::min-food))
        version (game/version-of (:primitives c))
        m (ctx/mem c)]
    (cond
      (nil? (:since m)) (ctx/update-mem! c assoc :since (ctx/now c))
      (stale? m (ctx/latest c :died))
      (do (ctx/update-mem! c restarted (ctx/now c))
          (ctx/emit! c :kit.resumed :info {:text "died since the kit began: starting again from what is carried and worn"})))
    (loop []
      (let [st (state c)
            wear (when (:wear a) (first (to-wear items st)))
            have (filterv #(had? (:carried st) (:worn st) %) items)
            todo (remove (set have) items)
            fp (foods/stock-points (:inv st))
            partial (:partial (ctx/mem c))
            need (when (and (seq todo) (seq (furnace-short version [(first todo)] {})))
                   (first (furnace-short version todo (:carried st))))
            short (when-not partial need)
            raw (when partial (get (:carried st) (get smelt-plan/made-from (first need)) 0))]
        (cond
          wear
          (let [r (await (ctx/call-child c :wear 'jobs.items.wear {:item wear}))
                out (wear-outcome r (ctx/child-result c :wear)
                                  (when (= :declined r) (:reason (b/child-wait c :wear 'jobs.items.wear {:item wear}))))]
            (cond (= :continue out) :continue
                  out (finish! c items have (assoc out :status :stopped :item wear))
                  :else (recur)))

          (empty? todo) (finish! c items have {:status :done})

          (and need (not (armed? (:carried st))) (not (:weapon-tried (ctx/mem c))))
          (let [r (await (run-obtain! c :weapon starter-weapons 1))]
            (cond (= :continue r) :continue
                  :else (do (ctx/update-mem! c assoc :weapon-tried true)
                            (when r (ctx/emit! c :kit.no-weapon :warn {:reason (:reason r) :why (:why r)
                                                                       :text "no sword or axe to fight with, going on unarmed"}))
                            (recur))))

          (and short (< fp min-food) (not (:fed (ctx/mem c))) (not (< (ctx/now c) (or (:no-food-until (ctx/mem c)) 0))))
          (let [r (when (:food a) (await (run-food! c)))]
            (cond (= :continue r) :continue
                  (and (some? r) (not (foods/starving? (.self (:primitives c)) {})))
                  (do (ctx/update-mem! c assoc :no-food-until (+ (ctx/now c) (settings/get settings ::no-food-wait-ms)))
                      (ctx/emit! c :kit.no-food :warn {:cause (:cause r) :why (:why r) :food fp
                                                       :text (str "no food found (" (name (:cause r)) "), going on with " fp " food points carried")})
                      (recur))
                  (some? r) (finish! c items have (assoc r :status :stopped :item (first todo) :reason :needs-food
                                                         :food fp :min-food min-food))
                  (not (:food a)) (finish! c items have {:status :stopped :item (first todo) :reason :needs-food :food fp :min-food min-food
                                                         :why (str "carries " fp " food points, needs " min-food ": get a food supply first")})
                  :else (do (ctx/update-mem! c assoc :fed true) (recur))))

          short
          (let [[mat n] short
                r (await (run-obtain! c :materials mat n))
                carried-raw (get (:carried st) (get smelt-plan/made-from mat) 0)]
            (cond (= :continue r) :continue
                  (and (some? r) (pos? carried-raw)) (do (ctx/update-mem! c assoc :partial (assoc r :item mat))
                                                         (recur))
                  (some? r) (finish! c items have (merge {:status :stopped :item mat} r))
                  :else (recur)))

          (and partial (not (:smelted partial)) (or (:smelting partial) (pos? raw)))
          (let [[mat n] need
                r (await (run-obtain! c :smelt mat (min (or (:smelting partial) raw) n)))]
            (cond (= :continue r) (do (ctx/update-mem! c assoc-in [:partial :smelting] (or (:smelting partial) raw))
                                      :continue)
                  (some? r) (finish! c items have (merge {:status :stopped :item mat} r))
                  :else (do (ctx/update-mem! c assoc-in [:partial :smelted] true)
                            (recur))))

          :else
          (let [item (first todo)
                r (await (run-obtain! c (keyword item) item 1))]
            (cond
              (= :continue r) :continue
              (some? r) (finish! c items have (merge {:status :stopped :item item}
                                                     (cond-> r partial (update :why #(str % " (the materials for the whole kit failed: "
                                                                                          (:reason partial) ")")))))
              :else (do (ctx/update-mem! c #(-> % (update :made (fnil conj []) item) (dissoc :fed)))
                        (ctx/emit! c :kit.progress :info {:item item :have (inc (count have)) :of (count items)
                                                          :text (str "got " item " (" (inc (count have)) " of " (count items) ")")})
                        (recur)))))))))
