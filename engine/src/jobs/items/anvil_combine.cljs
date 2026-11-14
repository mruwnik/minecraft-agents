(ns jobs.items.anvil-combine
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.checks :as checks]
            [jobs.lib.cost :as cost]
            [jobs.lib.look :as look]
            [jobs.lib.near :as near]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.ready :as ready]
            [jobs.lib.spent :as spent]
            [jobs.lib.spot :as free-spot]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]
            [jobs.lib.waits :as waits]))

(def doc
  "Put the enchantment of a carried enchanted book on a carried :item at an anvil (a Mending book on an iron pickaxe).
  The goal: a carried :item holds :enchant. The anvil is :anvil, else the nearest seen, else a carried one put down
  beside the body, else one fetched (jobs.lib.need: obtain crafts it from iron) and put down; an anvil this call put
  down is dug up again when it ends. A missing :item is fetched from the ways the call allows. The book is never
  fetched (no job gets a book with a given enchantment: buy it with jobs.village.trade). The level cost is the
  server's, read at the anvil.
  Ends :done with {:combined true :item :enchant :enchants [{:name :level}] :level-cost :levels-spent :xp-level :text}.
  Impossible: :bad-args (no item or enchantment name). Conditions (a listed root job waits): :no-book (no carried
  enchanted book holds :enchant), :no-item (not carried, the call's :ways #{:carried}), :too-few-levels (no level at all),
  :no-anvil (none given, seen or carried, after one look round, and the ways allow no fetch). A failed fetch ends as
  {:reason :need} with its ending as the :because. Condition-failed in the round: :no-book, :no-item, :no-anvil (the
  anvil is gone and none other is known), :not-an-anvil, :too-few-levels (data :need :have), :not-combinable,
  :too-expensive. Nothing-possible: :unreachable, :anvil-failed (the visit failed: :why), :timeout.
  done? is ok once a carried :item holds :enchant.")

(a/defargs args
  {:item {:doc "name of the carried item to put the book on" :spec a/item? :default nil}
   :enchant {:doc "the enchantment the book holds, e.g. \"mending\"" :spec a/name? :default nil}
   :anvil {:doc "the anvil position {:x :y :z}; nil: the nearest seen, else one carried or fetched and put down" :spec ::a/pos :default nil}})

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds the call may take (a guess until measured)." :spec (a/num-in 0 nil)}
   ::reach {:default 3 :doc "How near the body stands to use the anvil, in blocks." :spec (a/int-in 0 nil)}
   ::visit-s {:default 5 :doc "Seconds the visit at the anvil takes (the quote)." :spec (a/num-in 0 nil)}})
(defn reach [] (settings/get settings ::reach))

(def target [:item :enchant :anvil])
(def ways need/fetch-ways)

(def safe? ready/safe-here-by-night)
(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s}})

(def book "enchanted_book")
(def anvil-names ["anvil" "chipped_anvil" "damaged_anvil"])
(def anvil-radius "How far an anvil counts as seen: all that perception remembers." look/memory-radius)

(defn blank? [s] (not (and (string? s) (not (str/blank? s)))))

(defn problem
  "The cause {:reason :bad-args} when the item or the enchantment name is missing."
  [c]
  (let [{:keys [item enchant]} (:args c)]
    (cond (blank? item) {:reason :bad-args :why "no item name"}
          (blank? enchant) {:reason :bad-args :why "no enchantment name"})))

(defn holds?
  "Whether a carried stack (the body's JS inventory entry) holds the enchantment."
  [stack enchant]
  (boolean (some #(= enchant (.-name %)) (.-enchants stack))))

(defn stacks [c name] (filter #(= name (.-name %)) (.-inventory (.self (:primitives c)))))

(defn goal? [c] (let [{:keys [item enchant]} (:args c)] (boolean (some #(holds? % enchant) (stacks c item)))))
(defn plain? [c] (let [{:keys [item enchant]} (:args c)] (boolean (some #(not (holds? % enchant)) (stacks c item)))))
(defn book? [c] (boolean (some #(holds? % (:enchant (:args c))) (stacks c book))))
(defn carried-anvil? [c] (pos? (storage/carried (u/inventory (:primitives c)) "anvil")))
(defn xp-level [c] (or (some-> (.self (:primitives c)) .-experience .-level) 0))

(defn find-anvil
  "The nearest anvil the body has seen within the radius and not found gone in this call, or nil."
  [c]
  (let [gone (:gone (ctx/mem c) #{})]
    (some #(when-not (gone (spent/cell-vec (:pos %))) (:pos %))
          (look/seen-blocks (:primitives c) {:names anvil-names :radius anvil-radius :max 8 :live? true}))))

(defn anvil-at [c] (or (:anvil (:args c)) (:anvil (ctx/mem c)) (find-anvil c)))

(defn lacking
  "The cause the round cannot cure: no book, no item while nothing fetches it, no level at all."
  [c]
  (let [{:keys [item enchant]} (:args c)]
    (cond
      (goal? c) nil
      (not (book? c)) {:reason :no-book :why (str "no carried enchanted book holds " enchant)}
      (and (not (plain? c)) (empty? (need/fetch-how c))) {:reason :no-item :why (str "no " item " without " enchant " carried")}
      (zero? (xp-level c)) {:reason :too-few-levels :need 1 :why "no experience level"})))

(defn no-anvil
  "Look :no-anvil while none is given, seen or carried and no look round has run; then ok when the ways allow a fetch, else later."
  [c]
  (checks/seen-or-look c (boolean (or (goal? c) (anvil-at c) (carried-anvil? c))) :no-anvil
                       {:known "an anvil is known" :missing "no anvil in sight" :fetches? (boolean (seq (need/fetch-how c)))}))

(defn can-start? [c]
  (contract/all [(checks/cause-status :never (problem c)) (checks/cause-status :later (lacking c)) (no-anvil c)]))

(defn done?
  "Ok once a carried :item holds :enchant."
  [c]
  (if (goal? c)
    (contract/ok :combined "the item holds the enchantment" :units :items :done 1 :left 0 :result {:combined true})
    (contract/later :combining "anvil: the round puts the book on" :units :items :done 0 :left 1 :result {:combined false})))

(defn quote-of [c]
  (let [anvil (anvil-at c)
        walk (/ (if anvil (u/dist (u/self-pos c) anvil) anvil-radius) (cost/walk-blocks-per-s))
        visit (settings/get settings ::visit-s)]
    {:uses {:job-time [visit (+ walk visit) (+ walk (* 3 visit))]
            :items {book [0 1 1]}}}))

;; ------------------------------------------------------------------ the round

(def condition-reasons
  "Give-ups that are a fact about the call; any other give-up is a failed visit."
  #{:no-book :no-item :no-anvil :not-an-anvil :too-few-levels :not-combinable :too-expensive})

(defn finish!
  "The ending of kind with cause; the data says nothing was combined unless extra does."
  [c kind cause extra]
  (let [placed (spent/log c)]
    (ctx/end c kind cause (cond-> (merge {:combined false :item (:item (:args c)) :enchant (:enchant (:args c))} extra)
                            placed (merge (spent/summary placed))))))

(defn give-up!
  [c reason extra]
  (finish! c (if (condition-reasons reason) :condition-failed :nothing-possible) reason
           (assoc extra :text (str "anvil gave up: " (name reason) (some->> (:why extra) (str ": "))))))

(defn fail-up!
  "One more failed try of kind (u/failed!): :again while another is worth its price (cost/try-again?; a visit is priced as
  the furnace window's, the measured block-window visit), else give up with the reason."
  [c kind reason extra]
  (u/failed! c)
  (if (cost/try-again? c kind) :again (give-up! c reason extra)))

(defn ^:async fetched
  "nil once the fetch of n of item ended done, :continue while it waits, else its ending as the job's."
  [c item opts]
  (let [e (await (need/item! c item 1 opts))]
    (cond (= :continue e) :continue
          (= :done (:kind e)) nil
          :else (finish! c (:kind e) (:cause e) {}))))

(defn ^:async supply!
  "Fetch the item when none without the enchantment is carried: nil when there is one, :continue while the fetch waits,
  :again once it came, else the ending."
  [c]
  (cond
    (plain? c) nil
    (empty? (need/fetch-how c)) (give-up! c :no-item {})
    :else (or (await (fetched c (:item (:args c)) {:how (need/fetch-how c) :slot :need-item})) :again)))

(defn ^:async put-down!
  "No anvil known: put a carried one down beside the body (fetching one first when the ways allow). :again once it
  stands, :continue while a child waits, else the ending."
  [c]
  (if-not (carried-anvil? c)
    (if (empty? (need/fetch-how c))
      (give-up! c :no-anvil {:why "none known or carried"})
      (or (await (fetched c "anvil" {:how (need/fetch-how c) :slot :need-anvil})) :again))
    (let [r (await (free-spot/place-beside! c "anvil" [:anvil-spot] {}))]
      (cond
        (= :continue r) :continue
        (:placed r) (do (ctx/update-mem! c assoc :anvil (:placed r) :anvil-placed (:placed r))
                        (spent/note! c "anvil" (:placed r))
                        :again)
        :else (give-up! c :no-anvil {:why (str "the carried anvil could not be put down: " (:why r))})))))

(defn gone!
  "The anvil at pos is no more (it broke, or was taken): forget it, or give up when it was the one asked for."
  [c pos]
  (if (:anvil (:args c))
    (give-up! c :no-anvil {:why "the anvil given is gone"})
    (do (ctx/update-mem! c #(-> % (update :gone (fnil conj #{}) (spent/cell-vec pos)) (dissoc :anvil)))
        :again)))

(defn ^:async visit!
  "Use the anvil at pos and act on the answer."
  [c pos]
  (let [{:keys [item enchant]} (:args c)
        r (js->clj (await (ctx/act c :anvil (clj->js {:pos pos :item item :enchant enchant}))) :keywordize-keys true)]
    (case (:status r)
      "combined" (finish! c :done nil {:combined true :enchants (:enchants r) :level-cost (:levelCost r)
                                       :levels-spent (:levelsSpent r) :xp-level (:xpLevel r)
                                       :text (str "put " enchant " on " item)})
      "no-levels" (give-up! c :too-few-levels {:need (:need r) :have (:have r) :why (str "the anvil asks " (:need r) " levels, the body has " (:have r))})
      "no-book" (give-up! c :no-book {})
      "no-item" :again
      "missing" (gone! c pos)
      "cannot" (case (:reason r)
                 "already-has" :again
                 "not-an-anvil" (if (:anvil (:args c)) (give-up! c :not-an-anvil {}) (gone! c pos))
                 "too-expensive" (give-up! c :too-expensive {:need (:need r)})
                 "not-combinable" (give-up! c :not-combinable {:why (str enchant " cannot go on this " item)})
                 (fail-up! c :furnace :anvil-failed {:why (:reason r)}))
      "unreachable" (fail-up! c :walk :unreachable {})
      "timeout" (fail-up! c :furnace :timeout {})
      (fail-up! c :furnace :anvil-failed {:why (or (:reason r) (:status r))}))))

(defn ^:async step!
  "One piece: the item, then an anvil (seen, else put down), then walk there (a go-to child) and use it."
  [c]
  (or (await (supply! c))
      (if-let [pos (anvil-at c)]
        (let [r (await (near/go-near! c pos (reach) {:ways #{:walk :clear-seal}}))]
          (case r
            :partial :continue
            :blocked (or (await (near/risky-wait! c r)) (fail-up! c :walk :unreachable {}))
            (do (ctx/update-mem! c assoc :anvil pos)
                (await (visit! c pos)))))
        (await (put-down! c)))))

(defn ^:async round
  "The whole combine in one call: step! again until it is done or stopped."
  [c]
  (await (pace/steps! c #(step! c))))

(defn pickup-spot
  "The anvil cell this call put down, still an anvil, outside the body's own areas and open to digging; else nil."
  [c]
  (let [spot (:anvil-placed (ctx/mem c))]
    (when (and spot (not (:picked-up (ctx/mem c)))
               (some #{(u/seen-name (:primitives c) spot)} anvil-names)
               (not (access/in-own-area? c spot))
               (:ok (access/may? (access/rules-input c {}) :dig spot)))
      spot)))

(defn ^:async cleanup
  "However the call ended: dig up the anvil this call put down (jobs.blocks.dig child, :continue while it waits).
  Nothing-possible :anvil-left (data :anvil) when the dig did not end done."
  [c _]
  (let [spot (pickup-spot c)]
    (if-not spot
      (ctx/end c :done nil {})
      (let [r (waits/waiting-out c (await (ctx/call c :pickup 'jobs.blocks.dig {:pos spot :collect true :need-drop false :ways #{:carried} :on-fluid :fail})))]
        (if (= :continue r)
          :continue
          (do (ctx/update-mem! c assoc :picked-up true)
              (if (= :done (:kind r))
                (ctx/end c :done nil {})
                (ctx/end c :nothing-possible :anvil-left {:anvil spot}))))))))

(def meta {:tags [:craft]})
