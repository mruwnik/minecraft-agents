(ns jobs.survival.respond.refuge
  "A cornered body's refuges (jobs.survival.respond-flight): a pillar (respond.pillar) or a plug (jobs.survival.shut-in), and the hiding in them."
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.settings :as settings]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.hide-spot :as hide-spot]
            [jobs.lib.combat :as combat]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.ready :as ready]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.look :as look]
            [jobs.lib.pillar :as pl]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.spent :as spent]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.respond.state :as flight]
            [jobs.survival.respond.pillar :as pillar]))

(a/defargs settings
  {::on-lava {:default :seal :doc ":seal: lava a plug's dig lays open is filled with a carried block; :stop: the pocket or pit is dropped and the lava left." :spec #{:seal :stop}}
   ::creeper-range {:default 16 :doc "Blocks within which a creeper past its swell range is too near for a plug to be begun (walls, pit or pocket): the walls take longer than it needs to arrive. The flight runs, fights or pillars instead." :spec (a/num-in 0 nil)}})
(defn held?
  "Whether a retreat child (its memory, as the parent sees it) holds a refuge: sealed in, on a pillar or in a pit."
  [child-mem]
  (some? (:refuge child-mem)))

(def hide-texts
  {:plug "cornered: shut in with blocks until the hostile leaves"
   :pillar "cornered: pillared up out of reach until the hostile leaves"})

(declare hide-now! hide-hold! hostiles-known)

(defn ^:async abandon-refuge!
  "The refuge failed: forget it, never try that option again this flight."
  [c]
  (let [kind (:kind (:refuge (ctx/mem c)))]
    (ctx/update-mem! c dissoc :refuge)
    (flight/tried! c kind)
    :again))

(defn ^:async refuge-round!
  "A step in a refuge: building the pillar, or hidden in it (one hold step, :again, until it ends the flight). A pillar
  that ends short for a mob (a refused block or a knockback with a hostile near) is a seen failure of its class
  (flight/seen-fail!; a hostile in the cell of the next block struck out of it first): the refuge is dropped and the
  next step decides again, that pillar dearer. Short for anything else (no floor, no headroom) the option is tried."
  [c]
  (let [{:keys [kind hidden class] :as refuge} (:refuge (ctx/mem c))]
    (if hidden
      (await (hide-hold! c))
      (case (await (pillar/step! c refuge))
        :mob-failed (do (await (pillar/knock-off! c))
                        (flight/seen-fail! c :pillar class)
                        (ctx/update-mem! c dissoc :refuge)
                        :again)
        :failed (await (abandon-refuge! c))
        :hidden (hide-now! c refuge (hide-texts kind))
        :again))))

(defn ^:async start-refuge!
  "Note the pillar refuge (a map with :kind, :anchor the feet cell it starts from, :cells [x y z] the cells it fills) in
  job memory and run its first step."
  [c refuge]
  (ctx/update-mem! c assoc :refuge refuge)
  (await (refuge-round! c)))

(defn plug-cells
  "The cells of the blocks this flight's plugs hold (the open ledger entries of the flight's plug purpose), as [x y z]."
  [c]
  (let [purpose (flight/plug-purpose c)]
    (vec (keep #(when (= purpose (:purpose %)) (:cell %)) (ledger/open-entries (ctx/view c))))))

(def all-ways #{:walls :pit :pocket})

(defn ^:async plug-in!
  [c option ways]
  (let [e (await (ctx/call c option 'jobs.survival.shut-in
                           (cond-> {:ways ways :on-lava (settings/get settings ::on-lava) :trespass? (contains? #{:plug-zone :plug-dig-zone} option)}
                             (ctx/use c :blocks) (assoc :use {:blocks (ctx/use c :blocks)}))))
        feet (sh-sense/feet (:primitives c))]
    (cond
      (= :continue e) (do (ctx/update-mem! c assoc :plugging {:at feet :option option :ways ways}) :again)
      (= :done (:kind e)) (do (ctx/update-mem! c dissoc :plugging)
                              (spent/absorb! c (:data e))
                              (hide-now! c {:kind (case option :plug-zone :plug :plug-dig-zone :plug-dig option) :anchor feet :cells (plug-cells c)} (hide-texts :plug)))
      :else (do (ctx/update-mem! c dissoc :plugging)
                (when (= :trespass (get-in e [:data :stops :walls :reason])) (ctx/update-mem! c assoc :zone-walls true))
                (when (some (comp #{:trespass :no-site} :reason) (vals (select-keys (get-in e [:data :stops]) [:pit :pocket])))
                  (ctx/update-mem! c assoc :zone-dig true))
                nil))))

(defn hurt-or-unarmed?
  "Whether the body is at or under flight/::hide-health or carries no weapon: it cannot fight its way out, so the seal stays."
  [c]
  (let [p (:primitives c)]
    (or (<= (.-health (.self p)) (flight/hide-health))
        (nil? (combat/best-weapon p combat/default-weapons)))))

(defn seen-hostiles
  "hostiles-known, the ones the body has seen (not merely heard)."
  [c]
  (filterv :seen (hostiles-known c)))

(defn ^:async plug!
  "Shut the body in with jobs.survival.shut-in by ways and hide there (a :plug refuge): :again while it works or once shut
  in, nil when no way shuts it, or when the body already hid from this kind of mob here lately (jobs.lib.hide-spot/repeated?:
  hiding does not end a stream of them; the flight fights or leaves). :plugging marks the option, ways and cell a plug is under way at (plugging-here?)."
  [c option ways]
  (when-not (and (not (:plugging (ctx/mem c)))
                 (not (hurt-or-unarmed? c))
                 (hide-spot/repeated? c (u/self-pos c) (map :name (seen-hostiles c))))
    (await (plug-in! c option ways))))

(defn creeper-coming?
  "Whether a known creeper is past swell-range and within ::creeper-range of the body: the walls would come too late
  and the body has time to do better. One already swelling beside the body is walled off."
  [c]
  (let [p (:primitives c) range (settings/get settings ::creeper-range)]
    (boolean (some #(and (combat/creeper? %) (< danger-q/swell-range (danger-q/mob-distance p %) (inc range)))
                   (flight/near-known p (set (flight/dead-ids c)) range range)))))

(defn ^:async plug-option!
  "A plug option of a cornered body (:plug: the walls, :plug-dig: pit or pocket), once a flight: plug!'s result, nil when
  it failed or was tried or a creeper is near (creeper-coming?: not begun, not tried either); a pit or pocket is not dug in a body already shut in (its walls did not keep the hostile off)."
  [c option ways]
  (when-not (or (flight/tried? c option)
                (and (not (:plugging (ctx/mem c))) (creeper-coming? c))
                (and (#{:plug-dig :plug-dig-zone} option) (dig-cells/sealed-in? (:primitives c) :walls (sh-sense/roof-height))))
    (or (await (plug! c option ways)) (do (flight/tried! c option) nil))))

(defn plugging-here?
  "Whether a plug was begun at the body's feet cell this flight and has not failed: it is finished before the walk goes on."
  [c]
  (let [{:keys [at option]} (:plugging (ctx/mem c))]
    (and (= at (sh-sense/feet (:primitives c))) (not (flight/tried? c option)))))

(defn ^:async plug-on!
  "The plug begun here, one more step."
  [c]
  (let [{:keys [option ways]} (:plugging (ctx/mem c))]
    (await (plug! c option ways))))

(defn ^:async pillar!
  "Start a pillar (respond.pillar/plan!, through another's area only with trespass?) when it fits (info retreat.pillar
  {:class :at}: its class and feet cell, for event-counts wall-places): a step's result, else nil."
  [c trespass?]
  (when-let [refuge (await (pillar/plan! c trespass?))]
    (ctx/emit! c :retreat.pillar :info {:class (:class refuge) :at (mapv (:anchor refuge) [:x :y :z])})
    (await (start-refuge! c refuge))))

(defn ranged-known?
  "Whether a shooter is known near: a pillar raised higher only lifts the body further into its line of fire."
  [c]
  (let [p (:primitives c)]
    (boolean (some combat/ranged? (flight/near-known p (set (flight/dead-ids c)) threats/max-follow-range (danger-q/ranged-radius))))))

(defn ^:async hide-hold!
  "Plugged in or up the pillar: one round of jobs.survival.hide-out (a hold, why hiding; :again) while the refuge keeps a danger off, then
  the flight ends :hidden with the job's hold. A body hurt while hidden (a mob reaching it round a corner of the seal) leaves the refuge as failed
  (abandon-refuge!), a pillar raised once first (respond.pillar/raise!) unless a shooter is known near (ranged-known?); so does one a creeper or climber would breach."
  [c]
  (let [{:keys [kind anchor cells]} (:refuge (ctx/mem c))
        e (await (ctx/call c :hide 'jobs.survival.hide-out {:kind kind :anchor anchor :cells (vec cells) :skip (flight/dead-ids c)}))]
    (cond
      (= :continue e) :again
      (= :done (:kind e)) (flight/end-flight! c :hidden (select-keys (:data e) [:hold :dug-out]))
      (= :hurt (:cause e)) (do (ctx/update-mem! c update :reached (fnil into #{}) (:ids (:data e)))
                               (ctx/emit! c :retreat.hide-hurt :warn {:text (:text (:data e)) :pos (sh-sense/feet (:primitives c)) :hostiles (mapv #(dissoc % :seen) (hostiles-known c))})
                               (if (and (not (ranged-known? c)) (= :again (await (pillar/raise! c)))) :again (await (abandon-refuge! c))))
      :else (do (ctx/update-mem! c update :reached (fnil conj #{}) (:id (:data e)))
                (ctx/emit! c :retreat.hide-breached :warn {:text (:text (:data e)) :mob (:name (:data e)) :pos (sh-sense/feet (:primitives c))})
                (await (abandon-refuge! c))))))

(defn hostiles-known
  "The hostiles the body knows within follow range, as [{:name :pos [x y z]}]; empty when it only heard one."
  [c]
  (let [p (:primitives c)]
    (->> (flight/near-known p (set (flight/dead-ids c)) threats/max-follow-range (danger-q/ranged-radius))
         (reduce (fn [[seen out] e] (if (contains? seen (.-id e)) [seen out] [(conj seen (.-id e)) (conj out e)])) [#{} []])
         second
         (mapv (fn [e] {:name (.-name e) :seen (danger-q/seen-only? e) :pos (mapv #(js/Math.floor (% (danger-q/mob-pos p e))) [:x :y :z])})))))

(defn hide-now! [c refuge text]
  (ctx/update-mem! c assoc :refuge (assoc refuge :hidden true))
  (let [placed (spent/log c)
        all (hostiles-known c)
        hostiles (mapv #(dissoc % :seen) all)
        from (if (seq hostiles)
               (str/join ", " (map (fn [[{:keys [name pos]} n]] (str name (when (< 1 n) (str " x" n)) " at " (pr-str pos))) (frequencies hostiles)))
               "a hostile heard, not seen")]
    (hide-spot/remember! c (u/self-pos c) (map :name (filter :seen all)))
    (ctx/emit! c :retreat_sealed :warn (merge {:text (str text "; hiding from " from (some->> (spent/text placed "refuge") (str "; "))) :pos (sh-sense/feet (:primitives c)) :hostiles hostiles}
                                              (when (seq placed) (spent/summary placed)))))
  :again)
