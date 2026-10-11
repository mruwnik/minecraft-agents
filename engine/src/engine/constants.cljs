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
(def step-height "The highest rise a body walks up without a jump." (unchecked-get values "stepHeight"))
(def gravity "The speed a falling body gains each tick, before air-drag." (unchecked-get values "gravity"))
(def air-drag "What is left of a body's vertical speed each tick." (unchecked-get values "airDrag"))
(def clip-tolerance
  "Boxes this close count as touching in the client physics' collision clip, on every axis (vanilla's own 1e-7): a box a
  rounding step into a block face is held at the face, and one resting a rounding step into a floor is not stopped by the
  next floor block."
  (unchecked-get values "clipTolerance"))
