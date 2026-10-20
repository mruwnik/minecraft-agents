(ns jobs.survival.retreat-choose
  "Which option a flight step takes, from facts the step sensed. Pure: no reads, no calls.")

(defn choose
  "The option of a flight step, {:option kw :why str} (why: said to the cornered options; :cornered: where home is judged by the room the body stands in or near, :cornered-inside: by the room it is inside only), in the order the
  retreat doc lists them. facts: :threat :early (an early refuge's result) :no-gain? :angry-enderman? :hit? :flyer?
  :door (to shut) :inside? :home-door :way (in) :outruns? (the body outruns the chasers) :shot-open? (no threat, but shot at from an unseen spot: :dodge). :walk when none applies."
  [{:keys [threat early no-gain? angry-enderman? hit? flyer? door inside? home-door way outruns? shot-open?]}]
  (cond
    (nil? threat) {:option (if shot-open? :dodge :end)}
    (some? early) {:option :early}
    no-gain? {:option :stuck :why "the chaser keeps up" :reset-gain? true}
    angry-enderman? {:option :cornered :why "an enderman cannot be outrun"}
    hit? {:option :cornered :why "a hostile keeps hitting the body as it flees"}
    flyer? {:option :cornered :why "a phantom cannot be outrun"}
    door {:option :shut-door}
    (and inside? home-door) {:option :shut-home-door}
    inside? {:option :stuck :why "at home, the hostile still has a way in"}
    way {:option :run-home}
    (not outruns?) {:option :cornered-inside :why "the hostile outruns the body"}
    :else {:option :walk}))
