(ns jobs.survival.respond-choose
  "What a response to a hostile does next, from facts it sensed: the step of a flight (choose) and the cornered options in
  order (escape-order, by-price, with-zone-pillar). Pure: no reads, no calls.")

(defn choose
  "The option of a flight step, {:option kw :why str} (why: said to the cornered options; :cornered: where home is judged by the room the body stands in or near, :cornered-inside: by the room it is inside only), in the order the
  retreat doc lists them. facts: :threat :no-gain? :angry-enderman? :hit? :flyer?
  :door (to shut) :inside? :home-door :gap-way (inside a home whose doorway gap is open: the run in fills it) :way (in) :outruns? (the body outruns the chasers) :shot-open? (no threat, but shot at from an unseen spot: :dodge). :walk when none applies."
  [{:keys [threat no-gain? angry-enderman? hit? flyer? door inside? home-door gap-way way outruns? shot-open?]}]
  (cond
    (nil? threat) {:option (if shot-open? :dodge :end)}
    no-gain? {:option :stuck :why "the chaser keeps up" :reset-gain? true}
    angry-enderman? {:option :cornered :why "an enderman cannot be outrun"}
    hit? {:option :cornered :why "a hostile keeps hitting the body as it flees"}
    flyer? {:option :cornered :why "a phantom cannot be outrun"}
    door {:option :shut-door}
    (and inside? home-door) {:option :shut-home-door}
    (and inside? gap-way) {:option :run-home}
    inside? {:option :stuck :why "at home, the hostile still has a way in"}
    way {:option :run-home}
    (not outruns?) {:option :cornered-inside :why "the hostile outruns the body"}
    :else {:option :walk}))

(defn escape-order
  "Pure: the cornered options to try, safest first. {:win? the fight is the cheapest cornered answer (never true
  for a creeper), :flyer? a phantom is near (it outflies a walker: shelter, else fight), :creeper? one is near (it comes first), :enderman? an angry enderman is near (angry-enderman?) (it teleports, so no walk or back-off, and a
  pillar is within its reach: the body enters water within water-reach, which it avoids, else walks under a 2-high roof within roof-reach, which it cannot enter, else shuts itself in), :ranged? the threat shoots, or the body is under fire (under-fire?), :climber? it is a spider (it climbs the pillar), :faster? a mob
  the body cannot outrun is near (no step back: it gains nothing), :home? the body is in its home
  room: it digs no pit or pocket in its own floor or walls, :lean? the pillar would leave the body too few blocks to roof in on its top (respond.pillar/lean?): the dig-down comes before it (a pit regains its blocks), :arriving? an armed body, a melee mob reaches the body within ::arrival-s: no pit or pocket is dug beside it (it takes longer than the mob needs), :pillar-first? a mob in a diagonal cell the walls must fill, no weapon, and a pillar fits: the pillar comes first (a fist fight there only trades hits), :pillar-cheaper? an armed body loses the fight and a pillar is priced under it (respond.options/pillar-cover): pressed, it tries a pillar before the fight, :pressed? pressed? holds and the body is not under fire (a seal or pillar cannot be built beside the mob: the fight comes first, except against a creeper or an enderman), :swim? the body is in water with a drowned near: swimming out to dry land comes first unless a fight wins}."
  [{:keys [win? creeper? enderman? flyer? ranged? climber? faster? swim? home? pressed? lean? pillar-first? pillar-cheaper? arriving?]}]
  (cond->> (cond
             creeper? [:back-off :plug :pillar :plug-dig :fight]
             enderman? [:water :roof :plug :plug-dig :fight]
             win? [:fight]
             flyer? [:plug :plug-dig :fight]
             :else (let [pressed? (and pressed? (not pillar-first?))]
                     (distinct (concat (cond pillar-first? [:pillar] pressed? (if pillar-cheaper? [:pillar :fight] [:fight]) :else [])
                                       (if (or ranged? climber?) [:plug] [:plug :pillar])
                                       (when-not faster? [:back-off])
                                       [:plug-dig]
                                       (when-not pressed? [:fight])))))
    (and swim? (not creeper?) (not enderman?) (not win?)) (into [:swim-out])
    (and lean? (not home?)) (#(if (some #{:pillar} %)
                                (vec (mapcat (fn [o] (case o :pillar [:plug-dig :pillar] :plug-dig [] [o])) %))
                                %))
    (or home? (and arriving? (not creeper?) (not enderman?))) (filterv #(not= :plug-dig %))))

(def priced-options "The cornered options escape-order places by rule and by-price orders among themselves: the covers and the back-off." #{:plug :pillar :plug-dig :back-off})

(defn by-price
  "order with its priced-options, in the places they hold in it, cheapest first by prices ({option seconds}; one not
  priced last, ties in order)."
  [order prices]
  (let [sorted (sort-by #(get prices % ##Inf) (filter priced-options order))]
    (first (reduce (fn [[out [s & more :as ss]] o] (if (priced-options o) [(conj out s) more] [(conj out o) ss])) [[] sorted] order))))

(defn with-zone-pillar
  "order with :plug-zone, :plug-dig-zone and :pillar-zone (walls, a pit or pocket, a pillar through another's area) after every way that keeps off, before a last :fight."
  [order]
  (let [order (vec order)
        fight-last? (= :fight (peek order))
        extra (cond-> [] (some #{:plug} order) (conj :plug-zone) (some #{:plug-dig} order) (conj :plug-dig-zone) (some #{:pillar} order) (conj :pillar-zone))]
    (-> (if fight-last? (pop order) order)
        (into extra)
        (cond-> fight-last? (conj :fight)))))
