(ns jobs.lib.trade-window
  "What the village jobs share about a villager's trade window: offers (choosing, text, the info event), and finding,
  walking to and reading a villager. The calling job passes finish!, (c reason extra), to end the call."
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.child :as child]
            [jobs.lib.pace :as pace]
            [jobs.lib.util :as u]
            [jobs.lib.villager :as villager]))

(a/defargs settings
  {::chase-timeout-s {:default 15 :doc "Seconds one walk toward the villager may take: it wanders, so the walk aims again at where it is now this often." :spec (a/num-in 0 nil)}
   ::reach {:default 2 :doc "How near the body stands to act on its target, in blocks." :spec (a/int-in 0 nil)}})

(defn reach [] (settings/get settings ::reach))

(defn chase-timeout-s [] (settings/get settings ::chase-timeout-s))

(defn retry
  "A step that failed but may be tried again: counted per call, the third in a row gives up."
  [reason]
  {:retry reason})

(defn enchant-wants
  "An :enchant arg as {name min-level}: a name (any level), or a map of names to minimum levels; nil for none."
  [enchant]
  (cond (string? enchant) {enchant 1} (map? enchant) enchant))

(defn carries
  "The enchantment of the gives that satisfies wants ({name min-level}), as {:name :level}; nil when none does."
  [wants gives]
  (some (fn [e] (when (>= (:level e 1) (get wants (:name e) ##Inf)) e)) (:enchants gives)))

(defn choose
  "Pick an offer from the read offers: {:offer o} with :price, or {:reason
  r} (and :price, the cheapest open one, for \"price\"). :enchant is a name or a map name -> minimum level (any one of them)."
  [offers {:keys [buy max-price payment enchant]}]
  (let [mine (filter #(and (= buy (get-in % [:gives :item]))
                           (or (nil? payment) (contains? payment (get-in % [:cost 0 :item])))
                           (or (nil? enchant) (carries (enchant-wants enchant) (:gives %))))
                     offers)
        open (map #(assoc % :price (get-in % [:cost 0 :count])) (remove :disabled mine))
        limit (fn [o] (cond (nil? max-price) ##Inf (fn? max-price) (max-price (get-in o [:cost 0 :item])) :else max-price))
        fit (filter #(<= (:price %) (limit %)) open)]
    (cond
      (empty? mine) {:reason :no-offer}
      (empty? open) {:reason :sold-out}
      (empty? fit) {:reason :price :price (apply min (map :price open))}
      :else {:offer (first (sort-by (juxt :price :index) fit))})))

(defn offer-text
  "One offer as a line: \"2 emerald -> 1 enchanted_book (mending 1)\"."
  [{:keys [cost gives disabled]}]
  (str (str/join " + " (map #(str (:count %) " " (:item %)) cost))
       " -> " (:count gives) " " (:item gives)
       (when-let [es (seq (:enchants gives))]
         (str " (" (str/join ", " (map #(str (:name %) " " (:level %)) es)) ")"))
       (when disabled " [sold out]")))

(defn emit-offers!
  "Say what the villager offers (profession, level, every offer with its enchantments), as a player reads the window."
  [c kind villager r offers]
  (ctx/emit! c kind :info
             {:villager villager :profession (.-profession r) :level (.-level r) :offers offers
              :enchants (vec (for [o offers e (get-in o [:gives :enchants])] (str (:name e) " " (:level e))))
              :text (str "villager " villager " (" (.-profession r) " " (.-level r) ") offers: "
                         (str/join "; " (map offer-text offers)))}))

(defn ^:async read!
  "Read the chosen villager's offers (event kind `kind`) and hand them to use! (c villager r offers); other windows
  end (by finish!, (c reason extra)), retry or skip the villager."
  [c finish! kind use!]
  (let [villager (villager/chosen c)
        r (await (ctx/act c :trade (clj->js {:villager villager :op "offers"})))
        status (.-status r)]
    (case status
      "ok" (let [offers (js->clj (.-offers r) :keywordize-keys true)]
             (emit-offers! c kind villager r offers)
             (await (use! c villager r offers)))
      "gone" (finish! c (villager/gone-cause c) nil)
      "cannot" (cond
                 (= "not-villager" (.-reason r)) (finish! c :not-villager nil)
                 (:villager (:args c)) (finish! c :no-offers nil)
                 :else (do (ctx/update-mem! c #(-> % (update :skipped (fnil conj #{}) villager) (dissoc :chosen)))
                           :again))
      "out-of-reach" (retry :unreachable)
      (retry :window))))

(defn cell-of
  "The cell [x y z] of the entity's feet."
  [e]
  (let [{:keys [x y z]} (u/pos-of (.-pos e))]
    (mapv #(js/Math.floor %) [x y z])))

(defn in-reach?
  "Whether the body is within reach of villager e and a line from its eye to the villager's feet, middle or head clears
  the collision boxes between (`hittable`: a player aims through the gap over a lectern or under a top slab; a glass wall blocks)."
  [c e]
  (and (u/within? (u/self-pos c) (u/pos-of (.-pos e)) (reach))
       (not (false? (.-hittable e)))))

(defn ^:async step!
  "Find the villager, walk within reach with one go-to leg (it wanders), then act! (c) in reach. :again,
  :continue (go-to yields), an ending (by finish!, (c reason extra)) or a retry."
  [c finish! act!]
  (let [skipped (:skipped (ctx/mem c) #{})
        ok? #(not (or (= "nitwit" (.-profession %)) (skipped (villager/ident %))))
        uuid (villager/pick! c ok?)
        e (when uuid (villager/find-by-uuid (:primitives c) uuid))]
    (cond
      (and (nil? e) (await (villager/find-in-view! c ok? nil))) :again ; villagers count only while in view
      (and (nil? uuid) (seq skipped)) (finish! c :no-offers nil)
      (nil? e) (finish! c (villager/gone-cause c) nil)
      (in-reach? c e) (await (act! c))
      :else
      (let [r (await (child/call-chase* c :walk
                                       {:pos (zipmap [:x :y :z] (cell-of e)) :entity (.-id e) :range (reach) :leg-s (chase-timeout-s)
                                        :walk {:see true :ways #{:walk :clear-seal}}}))]
        (cond
          (= :continue r) :continue
          (let [res (:data r)] (or (:arrived res) (:nearer res))) :again
          :else (finish! c :unreachable nil))))))

(defn ^:async run!
  "steps (with a pace between) of step! with act! until one ends; a retry counts failures in a row, reset by
  progress, and the third gives up. Yields only while go-to waits on the world. before! runs ahead of each step."
  [c finish! act! before!]
  (let [fails (atom 0)]
    (await (pace/steps! c (fn ^:async trade-step []
                            (before! c)
                            (let [r (await (step! c finish! act!))]
                              (cond
                                (:retry r) (if (< (swap! fails inc) (u/max-failures))
                                           :again
                                           (finish! c (:retry r) nil))
                                (= :again r) (do (reset! fails 0) :again)
                                :else r)))))))
