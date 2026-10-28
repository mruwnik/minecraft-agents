(ns engine.constants
  "The engine's physical constants, in blocks and ticks. The numbers are in constants.json beside this file, the one copy:
  the JS side (engine/js) imports the same file."
  (:require [shadow.resource :as rc]))

(def ^:private values (js/JSON.parse (rc/inline "engine/constants.json")))

(def body-half-width
  "Half the width of the body's box in the client physics, and so in the planner and the executor: 5079/16384. Wider than
  the server's (hitbox-half-width), which throws back every move of a box that touches a block face exactly; under 0.3105,
  where a body no longer threads a bamboo gap the planner fits it through."
  (unchecked-get values "bodyHalfWidth"))
(def hitbox-half-width "Half the width of the body's box as the server has it." (unchecked-get values "hitboxHalfWidth"))
(def body-height "The height of a standing body's box." (unchecked-get values "bodyHeight"))
(def eye-height "From a standing body's feet to its eyes." (unchecked-get values "eyeHeight"))
(def reach "How far from its eyes a body can use a block." (unchecked-get values "reach"))
(def step-height "The highest rise a body walks up without a jump." (unchecked-get values "stepHeight"))
(def gravity "The speed a falling body gains each tick, before air-drag." (unchecked-get values "gravity"))
(def air-drag "What is left of a body's vertical speed each tick." (unchecked-get values "airDrag"))
(def airborne-inertia "What is left of a body's horizontal speed each tick off the ground." (unchecked-get values "airborneInertia"))
(def ladder-max-speed "The most horizontal speed a body on a vine or ladder keeps, blocks a tick." (unchecked-get values "ladderMaxSpeed"))
(def regen-seconds-per-hp "Seconds a fed body (food 18 or more) takes to regenerate one hp." (unchecked-get values "regenSecondsPerHp"))
(defn ground-speed
  "Blocks a second a walker on open ground (slipperiness 0.6) settles at with move input `input` and movement_speed attribute
  `speed`: vanilla adds input x speed to its velocity each tick and keeps slipperiness x 0.91 of it, so 20 x input x speed
  / (1 - 0.546) a second. A player's input is 0.98 (walk 0.1: 4.32, sprint 0.13: 5.61); a mob's is 0.98 x its own speed
  (Mob.setSpeed sets its forward input to the speed), so a mob goes as its speed squared."
  [input speed]
  (/ (* 20 input speed) (- 1 (* (unchecked-get values "groundSlipperiness") (unchecked-get values "groundDrag")))))

(defn mob-ground-speed
  "ground-speed of a mob whose move control runs at `speed`: its chase goal's speed modifier x its movement_speed attribute."
  [speed]
  (ground-speed (* (unchecked-get values "moveInputScale") speed) speed))

(def baby-speed
  "Blocks a second a baby zombie covers chasing: the zombie's attribute raised by the baby speed boost (+50% of base)."
  (mob-ground-speed (* (get (js->clj (unchecked-get values "mobMovementSpeed")) "zombie") (+ 1 (unchecked-get values "babySpeedBoost")))))

(def mob-speeds
  "Blocks a second each hostile covers chasing a body on open ground: mob-ground-speed of its vanilla movement_speed
  attribute plus the modifier it adds while it attacks (mobAttackSpeedAdd: an enderman with a target, an adult angry
  zombified piglin), times its chase goal's speed modifier (mobChaseModifier, else 1: a wither skeleton's melee goal is
  1.2, every other listed mob's attack goal or walk target 1); mobOtherSpeeds for the mobs that hop, fly or swim (not
  from game data, not measured). Spiders also leap 2 to 4 blocks.
  Read for the time a mob stays remembered (engine.perception.mobs) and for outrunning and racing it (jobs.lib.cost.danger)."
  (let [add (js->clj (unchecked-get values "mobAttackSpeedAdd"))
        modifier (js->clj (unchecked-get values "mobChaseModifier"))]
    (merge (into {} (map (fn [[m attr]] [m (mob-ground-speed (* (get modifier m 1) (+ attr (get add m 0))))]))
                 (js->clj (unchecked-get values "mobMovementSpeed")))
           (js->clj (unchecked-get values "mobOtherSpeeds")))))

(def crop-ripe-age
  "The growth stage at which each crop block can be harvested (a sweet berry bush from 2)."
  {"wheat" 7 "carrots" 7 "potatoes" 7 "beetroots" 3 "nether_wart" 3 "sweet_berry_bush" 2 "cocoa" 2})
