(ns jobs.items.get-kit
  (:require [jobs.lib.death-spot :as death-spot]
            [jobs.lib.ready :as ready]
            [jobs.lib.storage :as storage]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.settings :as settings]
            [jobs.items.obtain.bill :as bill]
            [jobs.items.smelt.plan :as smelt-plan]
            [jobs.lib.armour :as armour]
            [jobs.lib.checks :as checks]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.food :as food-cost]
            [jobs.lib.foods :as foods]
            [jobs.lib.makers :as makers]
            [jobs.lib.need :as need]
            [jobs.lib.next-call :as next-call]
            [jobs.lib.pace :as pace]
            [jobs.lib.util :as u]))

(def doc
  "Get :items in order (default the iron kit: pickaxe, sword, axe, shovel, helmet, chestplate, leggings, boots) and, with
  :wear, wear each armour piece once it is carried. One call is the whole goal; it yields (:continue) only while a
  child waits on the world.
  The items not carried or worn (a better armour piece worn in its slot counts) whose furnace material the carried ingots (or the raw ore that smelts to them) already cover are one bill
  first and worn at once; the rest are one bill for a jobs.items.obtain child
  (jobs.items.obtain.bill: the furnace material all pieces need together first, so the ore is mined and smelted in one go, then
  one piece after the other), with a :spend :job-time of ::job-time-s; the pieces are worn when it is done.
  While furnace material is still short (mining or smelting ahead; pure crafting needs no food) and fewer than
  the 3-day food reserve (60 points) are carried (harmful food does not count), one jobs.survival.get-food child (with :explore;
  it fills the pack to its :carry default, the reserve) runs before the bill (:food false: none). A child that stops warns kit.no-food; a hungry body (foods/hungry?, the hungry trigger's line) then stops {:reason :no-food}, a fed one goes on (the hungry trigger feeds it later) and tries again after the next bill round that ends in a retry. A body that cannot heal (food 0, or ready/hurt-cannot-heal? of a mine's :long work: too hurt for it with no food carried and the bar under 18) asks get-food before more work (once more if it is still so after a fed answer), and stops {:reason :no-food} when it finds none.
  With furnace material short and no weapon carried, one jobs.items.obtain child first makes a sword (iron, stone or wooden); if it cannot, kit.unarmed warns and the work goes on.
  Items that need no furnace material, and those the carried material covers, are made first (:partial-first, the default) so the job stops naming only the shortfall; with :partial-first false the list order rules and the job stops at the first item that cannot be made.
  With :explore, a bill that stops because a gathering ended (a mine that stopped, no way up) runs again (kit.retry, the pieces newly covered made first), up to 3 rounds without new furnace material.
  Get a base first.
  A death since the job began (body memory :died) clears the children and the bill; the next round re-plans from
  what is carried. A death keeps the job listed unless it was submitted with --drop-on-death.
  Needs (condition): :items, else impossible :bad-args. Ends done with {:have [..] :made [..]}. Every other ending has
  :have, :made, the next :item, a :why, a :text and :next (jobs.lib.next-call: the obtain child's, a get-food for :no-food, then the kit again):
  - the obtain child's kind and cause (:no-source, a walk cause such as :zone-exit, :time ..; :item is then the one it stopped at);
  - nothing-possible {:reason :wear-failed}.
  Quote: a guess per missing item. done? is ok when every item is carried or worn.")

(def iron-kit makers/iron-kit)

(a/defargs args
  {:items {:doc "the items to get, in order" :spec (a/coll-of a/item?) :default iron-kit}
   :wear {:doc "wear each armour piece once it is carried" :spec boolean? :default true}
   :explore {:doc "passed to the obtain children: walk and look for raw needs not seen" :spec boolean? :default true}
   :partial-first {:doc "make the items that need no furnace material (and those the carried material covers) first, then stop naming the shortfall; false: stop at the first item that cannot be made" :spec boolean? :default true}
   :food {:doc "run a get-food child when mining or smelting is ahead and fewer than the food reserve is carried" :spec boolean? :default true}})

(a/defargs settings
  {::job-time-s {:default 10800 :doc "Seconds a kit may last: every child of every piece (the bill's obtain child too)." :spec (a/num-in 0 nil)}
   ::item-s {:default 120 :doc "Seconds a quote counts for each missing item of the kit." :spec (a/num-in 0 nil)}})

(def target [:items])

(defn sword-material?
  "Whether wood, cobblestone or iron ingots are carried: a sword can be made."
  [c]
  (boolean (some #(re-find #"_(log|planks)$|^(cobblestone|cobbled_deepslate|iron_ingot)$" (:name %)) (u/inventory (:primitives c)))))

(declare state with-raw had?)

(defn arm-due?
  "Whether the sword step runs: no weapon carried, not made yet, and furnace material still short."
  [c]
  (let [st (state c)
        version (game/version-of (:primitives c))
        todo (remove #(had? (:carried st) (:worn st) %) (:items (:args c)))]
    (boolean (and (not (:armed (ctx/mem c))) (nil? (combat/best-weapon (:primitives c) combat/default-weapons))
                  (seq (bill/furnace-short version todo (with-raw (:carried st))))))))

(defn arm-next?
  "Whether the sword step will run and can make a sword from what is carried: it ends the unarmed state, so the unarmed
  hold does not stop it (go-to prices the walk to a bench)."
  [c]
  (and (arm-due? c) (sword-material? c)))

(defn safe? [c] (contract/all [(ready/threatened c) (if (arm-next? c) (contract/ok :arming "the sword is made first") (ready/unarmed-threatened c))]))
(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s}})

(defn restarted
  "The job memory after a death: the children and the bill went with the body; the job starts from now."
  [m now]
  (-> m (dissoc :children :bill :bill-done :covered :covered-done :fed :stop :bill-tries :bill-round :bill-material) (assoc :since now)))

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
     :carried (storage/usable-totals inv)
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
  (filterv (set have) (concat (keys (:covered (ctx/mem c))) (keys (:bill (ctx/mem c))))))

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

(defn with-raw
  "The carried totals with each raw input of a furnace product counted as the product it will smelt to (raw_iron as ingots)."
  [carried]
  (reduce (fn [m [out in]] (cond-> m (pos? (get carried in 0)) (update out (fnil + 0) (get carried in))))
          carried smelt-plan/made-from))

(defn covered-of
  "The bill {item 1} of the items, in order, that the carried furnace material (ingots, or the raw input to smelt) already covers, each one's share
  taken off what is left for the next; {} when none or all (then the one bill does it). An item needing no furnace material is covered
  only with partial?; else it is left to the bill and keeps its place in the list."
  [version partial? items carried*]
  (let [carried (with-raw carried*)
        covered (first (reduce (fn [[bill left] item]
                                 (let [need (bill/furnace-short version [item] {})]
                                   (if (and (or partial? (seq need)) (every? (fn [[k n]] (<= n (get left k 0))) need))
                                     [(assoc bill item 1) (merge-with - left need)]
                                     [bill left])))
                               [{} (select-keys carried (keys (bill/furnace-short version items {})))]
                               items))]
    (if (= (count covered) (count items)) {} covered)))

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
  [c slot bill]
  (let [a (:args c)
        e (await (need/call! c slot (quote jobs.items.obtain)
                             {:bill bill :explore (:explore a) :spend {:job-time (settings/get settings ::job-time-s)}}))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) nil
      :else (assoc (stop-of (:kind e) (:cause e) (why-of e)) :item (get-in e [:data :item]) :gathered (get-in e [:data :tried :gather])
                   :next (get-in e [:data :next])))))

(defn name-blocker
  "A no-source stop whose gathering ended :cannot-heal (hurt, only food heals) named for that: the real blocker leads the why."
  [c out]
  (if-not (= :cannot-heal (:gathered out))
    out
    (let [self (.self (:primitives c))]
      (assoc out :reason :cannot-heal
             :why (str "hurt (health " (.-health self) ", food " (.-food self) ") and only food heals the body, so mining on would risk it; " (:why out))))))

(defn finish!
  "End the call: done when out is nil, else with the stop out; the data has what is had and made, and a text."
  [c items have out*]
  (let [out (some->> out* (name-blocker c))
        data {:have have :made (made-of c have)}]
    (if-not out
      (ctx/end c :done nil data)
      (ctx/end c (:kind out) (:cause out)
               (next-call/with-next
                 (merge data (select-keys out [:item :why :next])
                        {:text (str "kit stopped at " (:item out) ": " (name (:reason out)) (when (:why out) (str " (" (:why out) ")")))})
                 c 'jobs.items.get-kit args nil
                 (when (#{:no-food :cannot-heal} (:reason out)) (next-call/call 'jobs.survival.get-food nil)))))))

(defn stop-after-wear!
  "Book the stop; the next step puts on the carried pieces first, then ends with it."
  [c out]
  (ctx/update-mem! c assoc :stop out)
  :again)

(defn food-args
  "The get-food child's args: feed the body, then fill the pack to get-food's :carry default."
  [c]
  {:explore (:explore (:args c))})

(defn food-first?
  "Food 0, or hurt with only food able to heal it: more mining would not end well."
  [c]
  (or (ready/hurt-cannot-heal? c :long) (zero? (.-food (.self (:primitives c))))))

(defn hungry?
  "Below the hungry trigger's line: a food bar that would not last the mining ahead."
  [c]
  (let [self (.self (:primitives c))]
    (foods/hungry? (.-food self) (.-health self) nil)))

(defn ^:async run-food!
  "One get-food child round: :continue, nil when the body is fed or not hungry, or {:cause :why} when it could not be fed."
  [c]
  (let [e (await (ctx/call c :food 'jobs.survival.get-food (food-args c)))
        cause (contract/cause-key e)]
    (cond
      (= :continue e) :continue
      (or (= :done (:kind e)) (= :not-hungry cause)) nil
      :else {:cause cause :why (why-of e)})))

(defn ^:async run-arm!
  "One obtain child round for a sword (the best it can make), its search worth the kit's: :continue, nil when armed, or {:cause :why} when it could not."
  [c]
  (let [e (await (ctx/call c :arm 'jobs.items.obtain {:any-of ["iron_sword" "stone_sword" "wooden_sword"] :count 1 :explore (:explore (:args c))
                                                       :search-worth (cost/items-find-worth (bill-of c))}))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) nil
      :else {:cause (contract/cause-key e) :why (why-of e)})))

(def max-bill-tries
  "Bill rounds a kit runs before a gathering that keeps ending without material is final."
  3)

(defn retry?
  "Whether the bill's stop r is a gathering try that ended (r's :gathered, obtain's data :tried :gather: a mine that stopped, no way up ..) and the kit may run the bill again: it was searching (:explore), and fewer than max-bill-tries fruitless rounds passed."
  [r tries explore]
  (boolean (and r explore (< (inc tries) max-bill-tries)
                (= :no-source (:reason r))
                (some? (:gathered r))
                (not= :none-seen (:gathered r)))))

(defn material-of
  "The furnace material (ingots, counting raw ore) carried, to tell whether a bill round gained any."
  [st]
  (reduce + 0 (vals (select-keys (with-raw (:carried st)) (keys smelt-plan/made-from)))))

(defn retry!
  "Book another bill round: a fresh child slot, the pieces the new material covers made first. A round that gained material
  starts the count again."
  [c r tries]
  (let [gained? (> (material-of (state c)) (:bill-material (ctx/mem c) 0))
        tries' (if gained? 1 (inc tries))]
    (ctx/emit! c :kit.retry :warn {:cause (:reason r) :why (:why r) :tries tries'
                                   :text (str "gathering ended without enough (" (:why r) "): trying again, round " (inc tries'))})
    (ctx/update-mem! c (fn [m] (-> m (dissoc :covered :covered-done :bill :fed)
                                   (assoc :bill-tries tries' :bill-material (material-of (state c)))
                                   (update :bill-round (fnil inc 0)))))
    :continue))

(defn ^:async round [c]
  (let [a (:args c)
        items (vec (:items a))
        version (game/version-of (:primitives c))
        m (ctx/mem c)]
    (cond
      (nil? (:since m)) (let [st (state c)
                              todo (remove #(had? (:carried st) (:worn st) %) items)
                              short (bill/furnace-short version todo (with-raw (:carried st)))]
                          (ctx/update-mem! c assoc :since (ctx/now c))
                          (ctx/emit! c :kit.plan :info {:items (vec todo) :short short
                                                        :text (str "kit: getting " (str/join ", " todo)
                                                                   (when (seq short) (str " (needs " (str/join ", " (map (fn [[k n]] (str n " " k)) short)) " from the furnace)")))}))
      (death-spot/died-after? (ctx/latest c :died) (:since m))
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

          (:stop (ctx/mem c)) (finish! c items have (:stop (ctx/mem c)))

          (empty? todo) (finish! c items have nil)

          (and (:food a) (not (:hurt-fed (ctx/mem c))) (food-first? c))
          (let [r (await (run-food! c))]
            (cond (= :continue r) :continue
                  (nil? r) (do (when (food-first? c) (ctx/update-mem! c assoc :hurt-fed true)) :again)
                  :else (do (ctx/emit! c :kit.no-food :warn {:cause (:cause r) :why (:why r) :food fp
                                             :text (str "no food found (" (name (:cause r)) ")")})
                            (finish! c items have (assoc (stop-of :nothing-possible {:reason :no-food} (str "hurt with no food to heal on, and none found (" (or (:why r) (name (:cause r))) "); mining on would risk death"))
                                                         :item (first todo))))))

          (and (not (:covered-done (ctx/mem c))) (seq (or (:covered (ctx/mem c)) (covered-of version (:partial-first a) todo (:carried st)))))
          (let [covered (or (:covered (ctx/mem c)) (covered-of version (:partial-first a) todo (:carried st)))
                _ (ctx/update-mem! c assoc :covered covered)
                r (await (run-bill! c :covered covered))]
            (cond (= :continue r) :continue
                  (some? r) (stop-after-wear! c (update r :item #(or % (first (keys covered)))))
                  :else (do (ctx/update-mem! c assoc :covered-done true)
                            :again)))

          (arm-due? c)
          (let [r (await (run-arm! c))]
            (if (= :continue r)
              :continue
              (do (ctx/update-mem! c assoc :armed true)
                  (when r
                    (ctx/emit! c :kit.unarmed :warn {:cause (:cause r) :why (:why r)
                                                     :text (str "no sword could be made (" (name (:cause r)) "), going on unarmed")}))
                  :again)))

          (and (:food a) (< fp food-cost/reserve-points) (not (:fed (ctx/mem c)))
               (seq (bill/furnace-short version todo (with-raw (:carried st)))))
          (let [r (await (run-food! c))]
            (if (= :continue r)
              :continue
              (do (ctx/update-mem! c assoc :fed true)
                  (when r
                    (ctx/emit! c :kit.no-food :warn {:cause (:cause r) :why (:why r) :food fp
                                                     :text (str "no food found (" (name (:cause r)) ")")}))
                  (if (and r (hungry? c))
                    (finish! c items have (assoc (stop-of :nothing-possible {:reason :no-food} (str "hungry with no food carried and none found (" (or (:why r) (name (:cause r))) "); mining ahead needs some"))
                                                 :item (first todo)))
                    :again))))

          (:bill-done (ctx/mem c))
          (finish! c items have (stop-of :nothing-possible {:reason :kit-incomplete} "the bill is carried but the kit is not complete") )

          :else
          (let [tries (:bill-tries (ctx/mem c) 0)
                round (:bill-round (ctx/mem c) 0)
                _ (when-not (:bill-material (ctx/mem c)) (ctx/update-mem! c assoc :bill-material (material-of st)))
                r (await (run-bill! c (if (zero? round) :bill (keyword (str "bill" round))) (bill-of c)))]
            (cond (= :continue r) :continue
                  (retry? r tries (:explore a)) (retry! c r tries)
                  (and r (not (:covered-done (ctx/mem c))) (seq (covered-of version (:partial-first a) todo (:carried (state c)))))
                  (do (ctx/update-mem! c (fn [m] (-> m (dissoc :covered :bill) (update :bill-round (fnil inc 0)))))
                      :again)
                  (some? r) (stop-after-wear! c (update r :item #(or % (first todo))))
                  :else (do (ctx/update-mem! c assoc :bill-done true)
                            (ctx/emit! c :kit.progress :info {:text (str "got " (str/join ", " todo))})
                            :again)))))))))

(def meta {:tags [:goal :craft]})
