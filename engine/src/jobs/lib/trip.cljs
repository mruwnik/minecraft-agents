(ns jobs.lib.trip
  "Trips start with room: a gathering job that leaves home calls ready! first. See ready!."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [clojure.string :as str]
            [jobs.lib.blocks :as b]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.foods :as foods]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::want-free {:default 12 :doc "Free slots a trip wants at its start: with fewer free and a known chest near, the body deposits first." :spec (a/int-in 0 36)}
   ::max-cost {:default 30 :doc "Walk cost (jobs.lib.cost/walk-cost, about 100 blocks) up to which a known chest is near enough to deposit at before a trip." :spec (a/num-in 0 nil)}})

(a/defargs food-settings
  {::gave-up-s {:default 600 :doc "Seconds after a fruitless get-food (a :hungry entry) in which a long job stops rather than dig on with no food." :spec (a/int-in 0 nil)}})

(a/defargs spare-settings
  {::spare-fraction {:default 0.25 :doc "A pickaxe, axe or shovel at or under this fraction of its durability is worn: a job about to dig a lot with the last one gets a spare first." :spec (a/num-in 0 1)}})

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

(def child :trip-deposit)

(defn done! [c outcome fields]
  (ctx/update-mem! c assoc :trip outcome)
  (ctx/emit! c (keyword (str "trip." (name outcome))) :info fields))

(defn plan
  "What a trip start does: :off, :roomy, :no-chest, :far or :deposit, with the chest and its walk cost."
  [c]
  (let [chest (mem/place (ctx/view c) :chest)
        free (u/free-slots (:primitives c))
        cost (when chest (:cost (cost/walk-cost {:distance (u/dist (u/self-pos c) chest)})))]
    (cond
      (false? (:deposit-first? (:args c))) {:do :off}
      (>= free (want-free)) {:do :roomy :free free}
      (nil? chest) {:do :no-chest :free free}
      (> cost (max-cost)) {:do :far :free free :cost cost}
      :else {:do :deposit :chest chest :free free :cost cost})))

(defn ^:async ready!
  "Before a gathering job sets out: when fewer than ::want-free slots are free and the known :chest place is within
  ::max-cost of walking, run jobs.storage.deposit as a child (:free ::want-free; it keeps tools, armour and the food
  reserve). Resolves nil to go on, :continue while the child waits on the world. Done once per job (:trip in the job's
  memory), so a restart does not repeat it. No chest known or a far one: info trip.no-chest or trip.far and the trip
  goes on. The job's :deposit-first? false skips it. A deposit that stops (its own warn says why) does not stop the trip."
  [c]
  (if (:trip (ctx/mem c))
    nil
    (let [p (plan c)]
      (case (:do p)
        :off (do (ctx/update-mem! c assoc :trip :off) nil)
        :roomy (do (ctx/update-mem! c assoc :trip :roomy) nil)
        (:no-chest :far) (do (done! c (case (:do p) :no-chest :no-chest :far) (select-keys p [:free :cost])) nil)
        :deposit
        (let [r (await (ctx/call-child c child 'jobs.storage.deposit {:free (want-free)}))]
          (if (= :continue r)
            :continue
            (do (done! c :deposited (assoc (select-keys p [:free :cost]) :result (ctx/child-result c child))) nil)))))))

(defn spare-need
  "The carried tool (map) that tool-ready! wants a spare for when digging block-name, or nil: the one digging it
  (jobs.lib.tools/suited-item) is at or under ::spare-fraction of its durability and no other carried tool of that kind
  that harvests it is above it. A tool with no known durability counts as whole."
  [p block-name]
  (let [harvest (some-> (.harvestTools p block-name) js->clj)
        suffix (str "_" (tools/tool-kind block-name))
        ok (set harvest)
        worn? (fn [t] (and (:durability t) (:max t) (<= (:durability t) (* (spare-fraction) (:max t)))))
        usable (filter #(and (str/ends-with? (:name %) suffix) (or (empty? ok) (ok (:name %)))) (u/inventory p))
        best (tools/suited-item usable block-name harvest)]
    (when (and best (every? worn? usable))
      best)))

(defn spare-name
  "The tool to obtain as the spare for the worn need: the cheapest tool of its kind that harvests block-name and
  lasts (not wooden or golden: a stone pickaxe will do for stone), else the cheapest at all, else the worn tool's own name."
  [p block-name need]
  (let [suffix (str "_" (tools/tool-kind block-name))
        harvest (filter #(str/ends-with? % suffix) (some-> (.harvestTools p block-name) js->clj))
        lasting (remove #(#{"wooden" "golden"} (weapon/material-of %)) harvest)]
    (or (tools/cheapest-tool (seq lasting)) (tools/cheapest-tool harvest) (:name need))))

(defn ^:async tool-ready!
  "Before a job that digs a lot sets out: when the tool for block-name is the last of its kind that harvests it and is
  worn (spare-need), run jobs.items.obtain for the cheapest tool of its kind that harvests block-name (spare-name; child :spare; chest, craft, gather). Resolves nil
  to go on, :continue while the child waits on the world. Told once per tool name in the job's memory (:spare-tried):
  info tool.spare when one came, warn tool.no-spare with the child's reason when not (the job goes on with the worn
  tool). The job's :fetch off, or a set of kinds without :tool, skips it."
  [c block-name]
  (let [p (:primitives c)
        need (when (and block-name (let [f (fetch/parse-arg (:fetch (:args c)))] (and (map? f) (not (:error f)) (or (nil? (:what f)) (contains? (:what f) :tool))))) (spare-need p block-name))
        tool-name (when need (spare-name p block-name need))]
    (when (and need (not (contains? (:spare-tried (ctx/mem c)) tool-name)))
      (let [r (await (ctx/call-child c :spare 'jobs.items.obtain {:item tool-name :count 1}))]
        (if (= :continue r)
          :continue
          (let [res (if (= :declined r)
                      (b/child-wait c :spare 'jobs.items.obtain {:item tool-name :count 1})
                      (ctx/child-result c :spare))
                got? (and (not= :declined r) (= :done (:status res)) (pos? (:got res 0)))]
            (ctx/update-mem! c update :spare-tried (fnil conj #{}) tool-name)
            (if got?
              (ctx/emit! c :tool.spare :info {:tool tool-name :text (str "carrying a spare " tool-name)})
              (ctx/emit! c :tool.no-spare :warn {:tool tool-name :durability (:durability need) :reason (:reason res) :status (:status res) :tried (:tried res)
                                                 :text (str "the last " tool-name " is worn (" (:durability need) " left) and no spare could be had"
                                                            (when-let [r (:reason res)] (str ": " (name r))))}))
            nil))))))
