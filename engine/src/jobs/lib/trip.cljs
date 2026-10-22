(ns jobs.lib.trip
  "Trips start with room: a gathering job that leaves home calls ready! first. See ready!."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [clojure.string :as str]
            [jobs.lib.choose :as choose]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.need :as need]
            [jobs.lib.look :as look]
            [jobs.lib.foods :as foods]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.tool-kind :as kind]
            [jobs.lib.tools :as tools]
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
                  (not (foods/carries-food? self (reserve/spared view self)))))))

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
  ::max-cost of walking, run jobs.storage.make-room as a child (:free ::want-free; it keeps tools, armour and what the
  reserve register holds). Resolves nil to go on, :continue while the child waits on the world. Done once per job (:trip in the job's
  memory), so a restart does not repeat it. No chest known or a far one: info trip.no-chest or trip.far and the trip
  goes on. A make-room that stops (its own warn says why) does not stop the trip."
  [c]
  (if (:trip (ctx/mem c))
    nil
    (let [p (plan c)]
      (case (:do p)
        :roomy (do (ctx/update-mem! c assoc :trip :roomy) nil)
        (:no-chest :far) (do (done! c (case (:do p) :no-chest :no-chest :far) (select-keys p [:free :cost])) nil)
        :deposit
        (let [e (await (ctx/call c child 'jobs.storage.make-room {:free (want-free)}))]
          (if (= :continue e)
            :continue
            (do (done! c :deposited (assoc (select-keys p [:free :cost]) :result (:data e))) nil)))))))

(declare spare-name)

(defn dear-tool?
  "Whether the tool (carried item name) to dig block-name with is dearer than the stone pickaxe a player would make from the
  cobblestone carried: a finished iron pickaxe is not worn out on stone."
  [p block-name item]
  (let [cheap (spare-name p block-name nil)]
    (boolean (and cheap (= "stone" (weapon/material-of cheap)) (> (weapon/cheap-rank item) (weapon/cheap-rank cheap))
                  (<= 3 (transduce (comp (filter #(= "cobblestone" (:name %))) (map :count)) + 0 (u/inventory p)))))))

(defn spare-need
  "The carried tool ({:item :uses-left :max}) that tool-ready! wants a spare for when digging block-name, or nil. With
  an amount (digs the job will make) the chooser (jobs.lib.choose/tools-for) must be short of it: the listed tools'
  uses together fall under amount. Without one, they must fall under ::spare-fraction of the first tool's durability.
  Tools all down to ::spent-uses always want a spare (the dig would stop :spent).
  A tool with no known durability counts as whole. A first tool dearer than a stone pickaxe, with cobblestone to make one
  (dear-tool?), is wanted a spare too, marked :dear."
  ([c block-name] (spare-need c block-name nil))
  ([c block-name amount]
   (let [{uses :list short :short} (choose/tools-for c [:dig block-name] (or amount 1))
         best (first uses)
         full (some #(when (= (:item best) (:name %)) (:max %)) (u/inventory (:primitives c)))
         spent? (every? #(<= (:uses-left %) (settings/get spare-settings ::spent-uses)) uses)
         worn? (or spent?
                   (if amount
                     (pos? short)
                     (< (transduce (map :uses-left) + 0 uses) (inc (js/Math.floor (* (spare-fraction) (or full 0)))))))]
     (when (and best full)
       (cond
         worn? (assoc best :max full)
         (dear-tool? (:primitives c) block-name (:item best)) (assoc best :max full :dear true))))))

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
  lasts (not wooden or golden: a stone pickaxe will do for stone), else the cheapest at all; a block that needs no tool (a log)
  takes a stone tool of the kind, else the worn tool's own name."
  [p block-name need]
  (let [kind (kind/tool-kind block-name)
        harvest (filter #(kind/of-kind? % kind) (some-> (.harvestTools p block-name) js->clj))
        lasting (remove #(#{"wooden" "golden"} (weapon/material-of %)) harvest)]
    (or (tools/cheapest-tool (seq lasting)) (tools/cheapest-tool harvest) (when (and kind (not= kind "shears")) (str "stone_" kind)) (:item need))))

(defn in-spare?
  "Whether this job runs under a spare fetch (a /spare slot in its chain): the digging that fetch needs makes no spare of its own."
  [c]
  (boolean (some #(str/ends-with? % "/spare") (:chain c))))

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

(defn ^:async tool-ready!
  "Before a job that digs a lot sets out: when the tool for block-name is the last of its kind that harvests it and is
  worn (spare-need), get the cheapest lasting tool of its kind that harvests block-name (spare-name) through need/fetch!
  (slot :spare; chest, craft, gather, and a walk to look for what is not seen, as a player goes up for wood; a table is never put in the dig line). Resolves nil to go on, :continue while the
  get waits on the world. Told per tool name in the job's memory: info tool.spare when one came (the next worn one gets another), warn
  tool.no-spare with the get's reason when not, told once per tool in body memory (:tool.no-spare-told) and tried once per job (:spare-tried; the job goes on with the worn tool). amount (the digs the job will make,
  optional) is spare-need's. The job's fetch off (ways #{:carried}) only tells tool.no-spare (reason :fetch-off); a job under a spare
  fetch skips it (no spare of a spare)."
  ([c block-name] (tool-ready! c block-name nil))
  ([c block-name amount]
   (let [p (:primitives c)
         worn (when (and block-name (not (in-spare? c)))
                (spare-need c block-name amount))
         tool-name (when worn (spare-name p block-name worn))
         told? #(some (fn [x] (= tool-name (:name (:data x)))) (ctx/entries c tools/no-spare-told-kind))
         tell! (fn [why tried]
                 (ctx/remember! c tools/no-spare-told-kind {:name tool-name} {:cap 10 :ttl :forever})
                 (ctx/emit! c :tool.no-spare :warn {:tool tool-name :durability (:uses-left worn) :reason why :tried tried
                                                    :text (str "the last " tool-name " is worn (" (:uses-left worn) " left) and no spare could be had"
                                                               (when why (str ": " (name why))))}))]
     (cond
       (nil? worn) nil
       (nil? (need/fetch-opts c)) (do (when-not (or (:dear worn) (told?)) (tell! :fetch-off nil)) nil)
       (contains? (:spare-tried (ctx/mem c)) tool-name) nil
       :else
       (let [e (await (need/fetch! c 'jobs.items.obtain {:item tool-name :count 1 :avoid (line-ahead c)} :need {:item tool-name :count 1} {:slot :spare :explore true}))]
         (if (= :continue e)
           :continue
           (do (if (= :done (:kind e))
                 (do (ctx/forget-where! c tools/no-spare-told-kind #(= tool-name (:name %)))
                     (ctx/emit! c :tool.spare :info {:tool tool-name :text (str "carrying a spare " tool-name)}))
                 (do (when-not (or (:dear worn) (told?)) (tell! (need/cause-why e) (get-in e [:data :tried])))
                     (ctx/update-mem! c update :spare-tried (fnil conj #{}) tool-name)))
               nil)))))))
