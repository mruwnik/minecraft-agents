(ns jobs.lib.trip
  "Trips start with room: a gathering job that leaves home calls ready! first. See ready!."
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [clojure.string :as str]
            [jobs.lib.blocks :as b]
            [jobs.lib.choose :as choose]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.need :as need]
            [jobs.lib.look :as look]
            [jobs.lib.foods :as foods]
            [jobs.lib.near :as near]
            [jobs.lib.near :as near]
            [jobs.lib.tool-kind :as kind]
            [jobs.lib.tools :as tools]
            [jobs.lib.torch :as torch]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::want-free {:default 12 :doc "Free slots a trip wants at its start: with fewer free and a known chest near, the body deposits first." :spec (a/int-in 0 36)}
   ::max-cost {:default 30 :doc "Walk cost (jobs.lib.cost/walk-cost, about 100 blocks) up to which a known chest is near enough to deposit at before a trip." :spec (a/num-in 0 nil)}})

(a/defargs food-settings
  {::gave-up-s {:default 600 :doc "Seconds after a fruitless get-food (a :hungry entry) in which a long job stops rather than dig on with no food." :spec (a/int-in 0 nil)}})

(a/defargs spare-settings
  {::spent-uses {:default 3 :doc "A job digging with only tools of its kind that harvest the block, every one with this many uses left or fewer, stops before the last breaks deep down with no way to make another." :spec (a/int-in 0 nil)}
   ::spare-fraction {:default 0.25 :doc "A pickaxe, axe or shovel at or under this fraction of its durability is worn: a job about to dig a lot with the last one gets a spare first." :spec (a/num-in 0 1)}})

(defn food-short?
  "Whether a long job should stop for food: the body is hungry (foods/hungry?), carries nothing a meal would eat, and
  get-food gave up less than ::gave-up-s ago (the hungry trigger had its try; digging on only starves it)."
  [c]
  (let [self (.self (:primitives c))
        view (ctx/view c)
        gave-up (:t (mem/latest view :hungry))]
    (boolean (and gave-up
                  (< (- (:now view) gave-up) (* 1000 (settings/get food-settings ::gave-up-s)))
                  (foods/hungry? (.-food self) (.-health self) {})
                  (not (foods/carries-food? self (foods/reserved view)))))))

(defn spare-fraction [] (settings/get spare-settings ::spare-fraction))

(defn want-free [] (settings/get settings ::want-free))
(defn max-cost [] (settings/get settings ::max-cost))

(a/defargs torch-settings
  {::torch-low {:default 8 :doc "A job that digs down or tunnels gets torches first when fewer than this are carried." :spec (a/int-in 0 nil)}
   ::torch-stock {:default 16 :doc "How many torches that fetch brings the bag up to." :spec (a/int-in 1 nil)}})

(defn torch-low [] (settings/get torch-settings ::torch-low))
(defn torch-stock [] (settings/get torch-settings ::torch-stock))

(def child :trip-deposit)

(defn done! [c outcome fields]
  (ctx/update-mem! c assoc :trip outcome)
  (ctx/emit! c (keyword (str "trip." (name outcome))) :info fields))

(def goods
  "Raw materials and metal a body gathers for crafts (kit, tools): a trip's deposit leaves them carried."
  (into (set (vals b/ores)) ["iron_ingot" "gold_ingot" "copper_ingot"]))

(defn keep-goods
  "{name count} of the goods carried, as a deposit's :keep."
  [p]
  (reduce (fn [m {:keys [name count]}] (cond-> m (goods name) (update name (fnil + 0) count))) {} (u/inventory p)))

(defn plan
  "What a trip start does: :roomy, :no-chest, :far or :deposit, with the chest and its walk cost."
  [c]
  (let [chest (mem/place (ctx/view c) :chest)
        free (u/free-slots (:primitives c))
        cost (when chest (:cost (cost/walk-cost {:distance (u/dist (u/self-pos c) chest)})))]
    (cond
      (>= free (want-free)) {:do :roomy :free free}
      (nil? chest) {:do :no-chest :free free}
      (> cost (max-cost)) {:do :far :free free :cost cost}
      :else {:do :deposit :chest chest :free free :cost cost})))

(defn ^:async ready!
  "Before a gathering job sets out: when fewer than ::want-free slots are free and the known :chest place is within
  ::max-cost of walking, run jobs.storage.deposit as a child (:free ::want-free; it keeps tools, armour, the food
  reserve and raw ore and metal (goods)). Resolves nil to go on, :continue while the child waits on the world. Done once per job (:trip in the job's
  memory), so a restart does not repeat it. No chest known or a far one: info trip.no-chest or trip.far and the trip
  goes on. A deposit that stops (its own warn says why) does not stop the trip."
  [c]
  (if (:trip (ctx/mem c))
    nil
    (let [p (plan c)]
      (case (:do p)
        :roomy (do (ctx/update-mem! c assoc :trip :roomy) nil)
        (:no-chest :far) (do (done! c (case (:do p) :no-chest :no-chest :far) (select-keys p [:free :cost])) nil)
        :deposit
        (let [e (await (ctx/call c child 'jobs.storage.deposit {:free (want-free) :keep (keep-goods (:primitives c))}))]
          (if (= :continue e)
            :continue
            (do (done! c :deposited (assoc (select-keys p [:free :cost]) :result (:data e))) nil)))))))

(defn spare-need
  "The carried tool ({:item :uses-left :max}) that tool-ready! wants a spare for when digging block-name, or nil. With
  an amount (digs the job will make) the chooser (jobs.lib.choose/tools-for) must be short of it: the listed tools'
  uses together fall under amount. Without one, they must fall under ::spare-fraction of the first tool's durability.
  A tool with no known durability counts as whole."
  ([c block-name] (spare-need c block-name nil))
  ([c block-name amount]
   (let [{uses :list short :short} (choose/tools-for c [:dig block-name] (or amount 1))
         best (first uses)
         full (some #(when (= (:item best) (:name %)) (:max %)) (u/inventory (:primitives c)))
         worn? (if amount
                 (pos? short)
                 (< (transduce (map :uses-left) + 0 uses) (inc (js/Math.floor (* (spare-fraction) (or full 0))))))]
     (when (and best full worn?)
       (assoc best :max full)))))

(defn spent-tool?
  "Whether every carried tool that digs block-name is down to ::spent-uses uses or fewer (a tool with no known durability counts as whole) and some is carried:
  the spare was not had, so one more dig may leave the body without any. Never for a block that needs no tool (dirt): a worn shovel only costs speed."
  [p block-name]
  (let [harvest (some-> (.harvestTools p block-name) js->clj)
        suffix (str "_" (kind/tool-kind block-name))
        ok (set harvest)
        usable (filter #(and (str/ends-with? (:name %) suffix) (ok (:name %))) (u/inventory p))]
    (boolean (and (seq usable) (every? #(and (:durability %) (<= (:durability %) (settings/get spare-settings ::spent-uses))) usable)))))

(defn spare-name
  "The tool to obtain as the spare for the worn need: the cheapest tool of its kind that harvests block-name and
  lasts (not wooden or golden: a stone pickaxe will do for stone), else the cheapest at all, else the worn tool's own name."
  [p block-name need]
  (let [kind (kind/tool-kind block-name)
        harvest (filter #(kind/of-kind? % kind) (some-> (.harvestTools p block-name) js->clj))
        lasting (remove #(#{"wooden" "golden"} (weapon/material-of %)) harvest)]
    (or (tools/cheapest-tool (seq lasting)) (tools/cheapest-tool harvest) (:item need))))

(defn in-spare?
  "Whether this job runs under a spare fetch (a /spare slot in its chain): the digging that fetch needs makes no spare of its own."
  [c]
  (boolean (some #(str/ends-with? % "/spare") (:chain c))))

(defn ^:async tool-ready!
  "Before a job that digs a lot sets out: when the tool for block-name is the last of its kind that harvests it and is
  worn (spare-need), run jobs.items.obtain for the cheapest tool of its kind that harvests block-name (spare-name; child :spare; chest, craft, gather). Resolves nil
  to go on, :continue while the child waits on the world. Told once per tool name in the job's memory (:spare-tried):
  info tool.spare when one came, warn tool.no-spare with the child's reason when not (the job goes on with the worn
  tool). amount (the digs the job will make, optional) is spare-need's. The job's :fetch off, or a set of kinds without :tool, skips it; so does a job under a spare fetch (no spare of a spare)."
  ([c block-name] (tool-ready! c block-name nil))
  ([c block-name amount]
  (let [p (:primitives c)
        need (when (and block-name (not (in-spare? c)) (let [f (need/parse-arg (:fetch (:args c)))] (and (map? f) (not (:error f)) (or (nil? (:what f)) (contains? (:what f) :tool))))) (spare-need c block-name amount))
        tool-name (when need (spare-name p block-name need))]
    (when (and need (not (contains? (:spare-tried (ctx/mem c)) tool-name)))
      (let [e (await (ctx/call c :spare 'jobs.items.obtain {:item tool-name :count 1 :explore false}))]
        (if (= :continue e)
          :continue
          (let [got? (= :done (:kind e))
                reason (contract/cause-key e)
                tried (:tried (:data e))]
            (ctx/update-mem! c update :spare-tried (fnil conj #{}) tool-name)
            (if got?
              (ctx/emit! c :tool.spare :info {:tool tool-name :text (str "carrying a spare " tool-name)})
              (ctx/emit! c :tool.no-spare :warn {:tool tool-name :durability (:uses-left need) :reason reason :status :stopped :tried tried
                                                 :text (str "the last " tool-name " is worn (" (:uses-left need) " left) and no spare could be had"
                                                            ": " (name reason))}))
            nil)))))))

(defn ^:async back-from-stocking!
  "After the torch trip, walk back to the cell it began on (:torches-from): :continue while the walk waits on the world, else nil."
  [c]
  (let [{:keys [torches-from torches-tried]} (ctx/mem c)]
    (when (and torches-from torches-tried)
      (if (= :partial (await (near/go-near! c torches-from 2)))
        :continue
        (do (ctx/update-mem! c dissoc :torches-from) nil)))))

(def heading-steps {:north [0 -1] :east [1 0] :south [0 1] :west [-1 0]})

(defn line-ahead
  "The cells a dig on along its heading (the :heading or :direction arg, else the way the body faces) cuts next: one and two
  steps ahead, at the feet cell and one above and below it (a stair or a tunnel goes there): [[x y z] ..]. A crafting table
  put down for the trip must not stand in them."
  [c]
  (let [heading (keyword (or (some-> (or (:heading (:args c)) (:direction (:args c))) (#(when (or (string? %) (keyword? %)) (name %))))
                             (look/facing (:primitives c))))
        [dx dz] (heading-steps heading [0 0])
        {:keys [x y z]} (u/self-pos c)
        [x y z] (mapv #(js/Math.floor %) [x y z])]
    (vec (for [n [1 2] dy [-1 0 1]] [(+ x (* n dx)) (+ y dy) (+ z (* n dz))]))))

(defn ^:async torches-ready!
  "Before a job that digs down or tunnels sets out: when fewer than ::torch-low torches are carried, run jobs.items.obtain
  for torches up to ::torch-stock (child :torches; chest, craft from coal or charcoal, charcoal from logs by smelt, gather).
  With coal or charcoal carried the count is what that fuel makes (4 torches each), so the obtain plans no charcoal smelt
  and no table for the rest. Resolves nil to go on, :continue while the child waits on the world. Once per job (:torches-tried in its memory): info
  torch.stocked when it came back with some, warn torch.no-stock with the child's reason when not (the job goes on, dark
  when none are carried;), then walks back to the cell the trip began on (:torches-from), so a chest elsewhere does not move the dig. The job's :fetch off, a set of kinds without :item, or a body short of food (food-short?) skips it."
  [c]
  (or (await (back-from-stocking! c))
      (let [held (torch/torches-held (:primitives c))
          limits (need/parse-arg (:fetch (:args c)))
          fetch? (and (map? limits) (not (:error limits)) (or (nil? (:what limits)) (contains? (:what limits) :item)))
          fuel (torch/fuel-held (:primitives c))
          want (- (torch-stock) held)
          args {:item "torch" :count (if (pos? fuel) (min want (* 4 fuel)) want) :avoid (line-ahead c) :explore false}]
      (when (and fetch? (< held (torch-low)) (not (:torches-tried (ctx/mem c))) (not (food-short? c)))
        (when-not (:torches-from (ctx/mem c)) (ctx/update-mem! c assoc :torches-from (u/self-pos c)))
        (let [e (await (ctx/call c :torches 'jobs.items.obtain args))]
          (if (= :continue e)
            :continue
            (let [reason (contract/cause-key e)
                  tried (:tried (:data e))
                  why (:why (:data e))
                  now (torch/torches-held (:primitives c))]
              (ctx/update-mem! c assoc :torches-tried true)
              (when (and (pos? fuel) (<= now held)) (ctx/update-mem! c assoc :craft-failed true))
              (if (> now held)
                (ctx/emit! c :torch.stocked :info {:held now :text (str "carrying " now " torches for the dig")})
                (ctx/emit! c :torch.no-stock :warn {:held now :reason reason :tried tried
                                                    :text (str "carries " (if (zero? now) "no torches" (str "only " now " torches"))
                                                               (if (zero? now)
                                                                 (str ", none made" (when (seq tried) (str " (" (str/join ", " (map (fn [[k v]] (str (name k) " " (if (keyword? v) (name v) v))) tried)) ")")))
                                                                 " and could not get more")
                                                               ": the dig goes on " (if (zero? now) "dark" "with them")
                                                               (when why (str " (" why ")")))}))
              (await (back-from-stocking! c)))))))))
