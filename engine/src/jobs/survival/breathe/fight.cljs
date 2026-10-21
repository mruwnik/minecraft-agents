(ns jobs.survival.breathe.fight
  "Breathe's answer to a mob that hits the body in the water: a player under attack fights back when swimming up
  gets nowhere, rather than swim on in place."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.combat :as combat]))

(a/defargs settings
  {::radius {:default 4 :doc "A hostile this close, with a hit taken within the window, is hitting the body." :spec (a/num-in 0 nil)}
   ::window-s {:default 5 :doc "How recent a hit counts as the body being attacked." :spec (a/num-in 0 nil)}
   ::stall-s {:default 4 :doc "Seconds a drowning body may rise no higher while attacked before it fights back." :spec (a/num-in 0 nil)}
   ::rise {:default 0.5 :doc "Blocks higher than its best so far that count as the swim up moving." :spec (a/num-in 0 nil)}})

(defn attacked?
  "A hostile within ::radius and a hit taken within ::window-s."
  [c]
  (boolean (and (seq (ctx/since c :hurt (- (ctx/now c) (* 1000 (settings/get settings ::window-s)))))
                (seq (combat/hostiles (:primitives c) (settings/get settings ::radius))))))

(defn step-rise
  "[rise stalled?] after the body is at height y at time now: rise is {:y best :t when it rose} (nil at the start); it moves
  on when y is ::rise above the best, else the body is stalled once ::stall-s have passed since."
  [rise y now]
  (if (or (nil? rise) (> y (+ (:y rise) (settings/get settings ::rise))))
    [{:y y :t now} false]
    [rise (> (- now (:t rise)) (* 1000 (settings/get settings ::stall-s)))]))

(defn stalled?
  "Notes the body's height in the job's memory and tells whether it has risen no higher than its best for ::stall-s."
  [c]
  (let [[rise stalled] (step-rise (:rise (ctx/mem c)) (.. (.self (:primitives c)) -pos -y) (ctx/now c))]
    (ctx/update-mem! c assoc :rise rise)
    stalled))

(defn ^:async fight!
  "One call of the attack child against the hostiles about, holding where it floats; the swim-up's stall clock starts afresh."
  [c]
  (ctx/update-mem! c dissoc :rise)
  (await (ctx/call c :fight 'jobs.combat.attack
                   {:targets :hostiles :radius (settings/get settings ::radius) :hold true
                    :use {:tools (combat/weapons c)} :checks {:safe? '(always) :fit? '(always)}}))
  :again)
