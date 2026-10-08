(ns jobs.items.get-kit
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.settings :as settings]
            [jobs.items.recipes :as recipes]
            [jobs.items.smelt :as smelt]
            [jobs.lib.armour :as armour]
            [jobs.lib.blocks :as b]
            [jobs.lib.foods :as foods]
            [jobs.lib.util :as u]))

(def doc
  "Get :items in order (default the iron kit: pickaxe, sword, axe, helmet, chestplate, leggings, boots) and, with
  :wear, wear each armour piece once it is carried. One call is the whole goal; it yields (:continue) only while a
  child waits on the world.
  Each round starts from the world: an item carried or worn (or a better armour piece worn in its slot) is skipped.
  Before any work and between pieces it needs :min-food food points carried (nil: ::min-food; harmful food does not
  count), else it stops :needs-food: get a base and food first. Then one jobs.items.obtain child gets the furnace
  material all missing pieces need together (iron_ingot: 32 for the whole kit, less what is carried), so the ore is
  mined and smelted in one go; a stop there does not end the job. Then one obtain child per missing piece, in order,
  with :explore and :minutes (nil: ::minutes, longer than the fetch limit). Each piece got is an info kit.progress.
  A death since the job began (body memory :died) drops the children's booked targets; the next round re-plans.
  Writes :goals {:job :items :have :next :status :reason} at each piece and at the end.
  Ends {:status :done :have [..] :made [..]} or {:status :stopped :item next :reason r :why text :have :made}, warn
  kit.stopped; r is the obtain's (:no-source, :timeout ..), :needs-food (:food :min-food) or :wear-failed.")

(def iron-kit
  ["iron_pickaxe" "iron_sword" "iron_axe" "iron_helmet" "iron_chestplate" "iron_leggings" "iron_boots"])

(a/defargs args
  {:items {:doc "the items to get, in order" :spec (a/coll-of a/item?) :default iron-kit}
   :wear {:doc "wear each armour piece once it is carried" :spec boolean? :default true}
   :explore {:doc "passed to the obtain children: walk and look for raw needs not seen" :spec (a/or-of boolean? map?) :default true}
   :minutes {:doc "time budget of each obtain child; nil: ::minutes" :spec (a/num-in 0 nil) :default nil}
   :min-food {:doc "food points to carry before work starts and between pieces; nil: ::min-food" :spec (a/int-in 0 nil) :default nil}})

(a/defargs settings
  {::minutes {:default 45 :doc "Time budget of each obtain child of the kit." :spec (a/num-in 0 nil)}
   ::min-food {:default 20 :doc "Food points carried before the kit job starts work or a new piece." :spec (a/int-in 0 nil)}})

(def goals-policy {:cap 10 :ttl :forever})

(defn furnace-short
  "{material n}: the furnace products (iron_ingot ..) that the first recipes of items need together, beyond carried
  ({name count})."
  [version items carried]
  (let [rs (recipes/recipes-for version)
        need (apply merge-with + {} (for [item items
                                          [k v] (:needs (first (get rs item)))
                                          :when (contains? smelt/made-from k)]
                                      {k v}))]
    (into {} (keep (fn [[k v]] (let [n (- v (get carried k 0))] (when (pos? n) [k n])))) need)))

(defn stale?
  "Whether the body died (latest :died entry) after the job booked :since."
  [m died]
  (boolean (and died (:since m) (> (:t died) (:since m)))))

(defn food-points [inv]
  (reduce + 0 (for [{:keys [name count]} inv
                    :when (and (foods/food? name) (not (foods/harmful name)) (not (foods/named-only name)))]
                (* count (foods/points name)))))

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
    {:item item :count (min 64 n) :explore (:explore a)
     :minutes (or (:minutes a) (settings/get settings ::minutes))}))

(defn ^:async run-obtain!
  "One obtain child round in slot: :continue, or {:reason :why} when it ended without the item (nil when it got it)."
  [c slot item n]
  (let [oargs (obtain-args c item n)
        r (await (ctx/call-child c slot 'jobs.items.obtain oargs))]
    (case r
      :continue :continue
      :declined (let [w (b/child-wait c slot 'jobs.items.obtain oargs)] {:reason (:reason w) :why (:why w)})
      (let [res (ctx/child-result c slot)]
        (when-not (= :done (:status res)) {:reason (:reason res :failed) :why (:why res)})))))

(defn finish! [c items have out]
  (let [m (ctx/mem c)
        made (vec (:made m))
        res (merge {:have have :made made} out)]
    (ctx/remember! c :goals (merge {:job "jobs.items.get-kit" :items items :have have :next (:item out)}
                                   (select-keys out [:status :reason]))
                   goals-policy)
    (if (= :done (:status out))
      (ctx/emit! c :kit.done :info {:have have :text (str "kit complete: " (count have) " item(s)")})
      (ctx/emit! c :kit.stopped :warn (assoc (select-keys out [:item :reason :why]) :have have
                                             :text (str "kit stopped at " (:item out) ": " (name (:reason out))
                                                        (when (:why out) (str " (" (:why out) ")"))))))
    (ctx/result! c res)
    :done))

(defn ^:async round [c]
  (let [a (:args c)
        items (vec (:items a))
        min-food (or (:min-food a) (settings/get settings ::min-food))
        version (game/version-of (:primitives c))
        m (ctx/mem c)]
    (cond
      (nil? (:since m)) (ctx/update-mem! c assoc :since (ctx/now c))
      (stale? m (ctx/latest c :died))
      (do (ctx/update-mem! c #(-> % (dissoc :children :materials) (assoc :since (ctx/now c))))
          (ctx/emit! c :kit.resumed :info {:text "died since the kit began: starting again from what is carried and worn"})))
    (loop []
      (let [st (state c)
            wear (when (:wear a) (first (to-wear items st)))
            have (filterv #(had? (:carried st) (:worn st) %) items)
            todo (remove (set have) items)
            fp (food-points (:inv st))
            short (when (seq todo) (first (remove (comp (set (:materials (ctx/mem c))) key) (furnace-short version todo (:carried st)))))]
        (cond
          wear
          (do (await (ctx/call-child c :wear 'jobs.items.wear {:item wear}))
              (let [res (ctx/child-result c :wear)]
                (if (seq (:worn res))
                  (recur)
                  (finish! c items have {:status :stopped :item wear :reason :wear-failed :why (:reason res)}))))

          (empty? todo) (finish! c items have {:status :done})

          (< fp min-food)
          (finish! c items have {:status :stopped :item (first todo) :reason :needs-food :food fp :min-food min-food
                                 :why (str "carries " fp " food points, needs " min-food ": get a food supply first")})

          short
          (let [[mat n] short
                r (await (run-obtain! c :materials mat n))]
            (if (= :continue r)
              :continue
              (do (ctx/update-mem! c update :materials (fnil conj #{}) mat)
                  (recur))))

          :else
          (let [item (first todo)
                r (await (run-obtain! c (keyword item) item 1))]
            (cond
              (= :continue r) :continue
              (some? r) (finish! c items have (merge {:status :stopped :item item} r))
              :else (do (ctx/update-mem! c update :made (fnil conj []) item)
                        (ctx/emit! c :kit.progress :info {:item item :have (inc (count have)) :of (count items)
                                                          :text (str "got " item " (" (inc (count have)) " of " (count items) ")")})
                        (ctx/remember! c :goals {:job "jobs.items.get-kit" :items items :have (conj have item)
                                                 :next (second todo) :status :running}
                                       goals-policy)
                        (recur)))))))))
