(ns jobs.items.get-kit
  (:require [jobs.lib.ready :as ready]
            [jobs.lib.storage :as storage]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.settings :as settings]
            [jobs.items.obtain.bill :as bill]
            [jobs.lib.armour :as armour]
            [jobs.lib.checks :as checks]
            [jobs.lib.cost.food :as food-cost]
            [jobs.lib.foods :as foods]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.util :as u]))

(def doc
  "Get :items in order (default the iron kit: pickaxe, sword, axe, helmet, chestplate, leggings, boots) and, with
  :wear, wear each armour piece once it is carried. One call is the whole goal; it yields (:continue) only while a
  child waits on the world.
  The items not carried or worn (a better armour piece worn in its slot counts) are one bill for a jobs.items.obtain child
  (jobs.items.obtain.bill: the furnace material all pieces need together first, so the ore is mined and smelted in one go, then
  one piece after the other), with a :spend :job-time of ::job-time-s; the pieces are worn when it is done.
  While furnace material is still short (mining or smelting ahead; pure crafting needs no food) and fewer than
  the 3-day food reserve (60 points) are carried (harmful food does not count), one jobs.survival.get-food child (with :explore;
  it fills the pack to its :carry default, the reserve) runs before the bill (:food false: none). A child that stops warns kit.no-food and the work goes on.
  Get a base first.
  A death since the job began (body memory :died) clears the children and the bill; the next round re-plans from
  what is carried. A death only keeps the job listed when it was submitted with :survive-death?.
  Needs (condition): :items, else impossible :bad-args. Ends done with {:have [..] :made [..]}. Every other ending has
  :have, :made, the next :item, a :why and a :text:
  - the obtain child's kind and cause (:no-source, a walk cause such as :zone-exit, :time ..; :item is then the one it stopped at);
  - nothing-possible {:reason :wear-failed}.
  Quote: a guess per missing item. done? is ok when every item is carried or worn.")

(def iron-kit
  ["iron_pickaxe" "iron_sword" "iron_axe" "iron_helmet" "iron_chestplate" "iron_leggings" "iron_boots"])

(a/defargs args
  {:items {:doc "the items to get, in order" :spec (a/coll-of a/item?) :default iron-kit}
   :wear {:doc "wear each armour piece once it is carried" :spec boolean? :default true}
   :explore {:doc "passed to the obtain children: walk and look for raw needs not seen" :spec boolean? :default true}
   :food {:doc "run a get-food child when mining or smelting is ahead and fewer than the food reserve is carried" :spec boolean? :default true}})

(a/defargs settings
  {::job-time-s {:default 10800 :doc "Seconds a kit may last: every child of every piece (the bill's obtain child too)." :spec (a/num-in 0 nil)}
   ::item-s {:default 120 :doc "Seconds a quote counts for each missing item of the kit." :spec (a/num-in 0 nil)}})

(def target [:items])

(def safe? ready/threatened)
(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s}})

(defn restarted
  "The job memory after a death: the children and the bill went with the body; the job starts from now."
  [m now]
  (-> m (dissoc :children :bill :bill-done :fed) (assoc :since now)))

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
     :carried (storage/carried-totals inv)
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

(defn made-of
  "The bill's items among those had."
  [c have]
  (filterv (set have) (keys (:bill (ctx/mem c)))))

(defn done?
  "Ok when every item is carried or worn (with :wear, no carried piece is left to put on) and no child is under way."
  [c]
  (let [st (state c)
        items (:items (:args c))
        had (filterv #(had? (:carried st) (:worn st) %) items)
        n (count had)
        data [:units :items :done n :left (- (count items) n) :result {:have had :made (made-of c had)}]
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

(defn bill-of
  "The bill {item 1} of the items not had, booked once in memory so the obtain child's args hold still."
  [c]
  (or (:bill (ctx/mem c))
      (let [st (state c)
            bill (into {} (map (fn [item] [item 1])) (remove #(had? (:carried st) (:worn st) %) (:items (:args c))))]
        (ctx/update-mem! c assoc :bill bill)
        bill)))

(defn ^:async run-bill!
  "The obtain child of the bill: :continue, nil when it got everything, else the stop (stop-of) of its ending."
  [c bill]
  (let [a (:args c)
        e (await (need/call! c :bill (quote jobs.items.obtain)
                             {:bill bill :explore (:explore a) :spend {:job-time (settings/get settings ::job-time-s)}}))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) nil
      :else (assoc (stop-of (:kind e) (:cause e) (why-of e)) :item (get-in e [:data :item])))))

(defn finish!
  "End the call: done when out is nil, else with the stop out; the data has what is had and made, and a text."
  [c items have out]
  (let [data {:have have :made (made-of c have)}]
    (if-not out
      (ctx/end c :done nil data)
      (ctx/end c (:kind out) (:cause out)
               (merge data (select-keys out [:item :why])
                      {:text (str "kit stopped at " (:item out) ": " (name (:reason out)) (when (:why out) (str " (" (:why out) ")")))})))))

(defn food-args
  "The get-food child's args: feed the body, then fill the pack to get-food's :carry default."
  [c]
  {:explore (:explore (:args c))})

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
        version (game/version-of (:primitives c))
        m (ctx/mem c)]
    (cond
      (nil? (:since m)) (let [st (state c)
                              todo (remove #(had? (:carried st) (:worn st) %) items)
                              short (bill/furnace-short version todo (:carried st))]
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
            fp (foods/stock-points (:inv st))]
        (cond
          wear
          (let [out (wear-outcome (await (ctx/call c :wear 'jobs.items.equip {:item wear})))]
            (cond (= :continue out) :continue
                  out (finish! c items have (assoc out :item wear))
                  :else :again))

          (empty? todo) (finish! c items have nil)

          (and (:food a) (< fp food-cost/reserve-points) (not (:fed (ctx/mem c)))
               (seq (bill/furnace-short version todo (:carried st))))
          (let [r (await (run-food! c))]
            (if (= :continue r)
              :continue
              (do (ctx/update-mem! c assoc :fed true)
                  (when r
                    (ctx/emit! c :kit.no-food :warn {:cause (:cause r) :why (:why r) :food fp
                                                     :text (str "no food found (" (name (:cause r)) "), going on with " fp " food points carried")}))
                  :again)))

          (:bill-done (ctx/mem c))
          (finish! c items have (stop-of :nothing-possible {:reason :kit-incomplete} "the bill is carried but the kit is not complete") )

          :else
          (let [r (await (run-bill! c (bill-of c)))]
            (cond (= :continue r) :continue
                  (some? r) (finish! c items have (update r :item #(or % (first todo))))
                  :else (do (ctx/update-mem! c assoc :bill-done true)
                            (ctx/emit! c :kit.progress :info {:text (str "got " (str/join ", " todo))})
                            :again)))))))))

(def meta {:tags [:craft]})
