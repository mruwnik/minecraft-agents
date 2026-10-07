(ns engine.path.planner.doors
  "Search methods: doors, gates and trapdoors: what opens them, and the opening pass of an expansion."
  (:require [engine.path.planner.base :refer [ACT-BUTTON ACT-LEVER ACT-PLATE ATTACH BODY OPEN-REDSTONE in-list? not-in]]
            [engine.path.planner.search :refer [Search]]))

(set! *warn-on-infer* true)

(extend-type Search
  Object

  ;; ---- doors, gates and trapdoors ----
  (nearOpenable [s x y z]
    (loop [cz (dec z)
           cx (dec x)
           cy (dec y)]
      (cond
        (> cz (inc z)) false
        (> cx (inc x)) (recur (inc cz) (dec x) (dec y))
        (> cy (+ y 2)) (recur cz (inc cx) (dec y))
        (pos? (aget (.-tbl-openable s) (.stateAt ^js (.-snapshot s) cx cy cz))) true
        :else (recur cz cx (inc cy)))))

  ;; the closed openable blocks in the body column of a node at (x, y, z) standing h/16 up, a door by its lower half:
  ;; [{ x, y, z, id }]
  (closedIn [s x y z h]
    (let [out #js []
          last-k (bit-shift-right (dec (+ (* y 16) h BODY)) 4)]
      (loop [k y]
        (when (<= k last-k)
          (let [id (.stateAt ^js (.-snapshot s) x k z)]
            (when (pos? (aget (.-tbl-openable s) id))
              (let [lower (== (aget (.-tbl-door-half s) id) 2)
                    by (if lower (dec k) k)]
                (when-not (true? (.some out (fn [^js b] (== (.-y b) by))))
                  (.push out #js {:x x :y by :z z :id (if lower (.stateAt ^js (.-snapshot s) x by z) id)})))))
          (recur (inc k))))
      out))

  ;; an activator for an iron door at `door`, for a body in the cell (sx, sy, sz) in front of it: a plate in that cell, or a
  ;; button or lever on the body's side of the door within 4 blocks, on a block next to the door's frame; a button beats a
  ;; nearer lever (levers stay walls for now). nil when none.
  (findActivator [s ^js door sx sy sz side]
    (if (and (== (aget (.-tbl-activator s) (.stateAt ^js (.-snapshot s) sx sy sz)) ACT-PLATE)
             (== (+ (js/Math.abs (- (.-x door) sx)) (js/Math.abs (- (.-z door) sz))) 1))
      #js {:via "plate" :at #js {:x sx :y sy :z sz}}
      (let [along-x (<= (aget (.-tbl-facing s) (.-id door)) 2)]
        (loop [dz -4
               dy -2
               dx -4
               best nil
               best-dist js/Infinity]
          (cond
            (> dz 4) best
            (> dy 4) (recur (inc dz) -2 -4 best best-dist)
            (> dx 4) (recur dz (inc dy) -4 best best-dist)
            :else
            (let [d0 (+ (* dx dx) (* dy dy) (* dz dz))
                  d (if (== (aget (.-tbl-activator s) (.stateAt ^js (.-snapshot s) (+ sx dx) (+ sy dy) (+ sz dz))) ACT-LEVER) (+ d0 100) d0)]
              (if (or (> d0 16) (>= d best-dist))
                (recur dz dy (inc dx) best best-dist)
                (let [x (+ sx dx)
                      y (+ sy dy)
                      z (+ sz dz)
                      id (.stateAt ^js (.-snapshot s) x y z)
                      act (aget (.-tbl-activator s) id)]
                  (if (and (or (== act ACT-BUTTON) (== act ACT-LEVER))
                           (== (js/Math.sign (if along-x (- x (.-x door)) (- z (.-z door)))) side))
                    (let [^js att (aget ATTACH (aget (.-tbl-attach s) id))]
                      (if (> (js/Math.max (js/Math.abs (- (+ x (aget att 0)) (.-x door)))
                                          (js/Math.abs (- (+ y (aget att 1)) (.-y door)))
                                          (js/Math.abs (- (+ z (aget att 2)) (.-z door)))) 2)
                        (recur dz dy (inc dx) best best-dist)
                        (recur dz dy (inc dx)
                               #js {:via (if (== act ACT-BUTTON) "button" "lever") :at #js {:x x :y y :z z}}
                               d)))
                    (recur dz dy (inc dx) best best-dist))))))))))

  (activatorFor [s ^js door sx sy sz side]
    (let [key (str (.-x door) "," (.-y door) "," (.-z door) "|" sx "," sy "," sz "|" side)
          hit (.get ^js (.-activators s) key)]
      (if (undefined? hit)
        (let [found (.findActivator s door sx sy sz side)]
          (.set ^js (.-activators s) key found)
          found)
        hit)))

  ;; what a move from the cell (sx, sy, sz) into a node column holding the closed blocks `blocks` opens: { list, seconds },
  ;; or nil when an iron door among them has nothing to open it
  (opening [s sx sy sz dx dz ^js blocks]
    (let [list #js []]
      (loop [k 0
             seconds 0]
        (if (>= k (.-length blocks))
          #js {:list list :seconds seconds}
          (let [^js b (aget blocks k)
                bid (.-id b)
                iron (== (aget (.-tbl-openable s) bid) OPEN-REDSTONE)
                along-x (<= (aget (.-tbl-facing s) bid) 2)
                ;; the body's side of the door: where it is, or, standing in the door's cell, where it came from
                near-side (js/Math.sign (if along-x (- sx (.-x b)) (- sz (.-z b))))
                side (if (zero? near-side) (- (js/Math.sign (if along-x (- dx (.-x b)) (- dz (.-z b))))) near-side)
                found (if (or iron (== (aget (.-tbl-activator s) (.stateAt ^js (.-snapshot s) sx sy sz)) ACT-PLATE))
                        (.activatorFor s b sx sy sz side)
                        nil)
                ^js act (if (or iron (and (some? found) (identical? (.-via ^js found) "plate"))) found nil)]
            (cond
              (and iron (nil? act)) nil
              (nil? act)
              (do (.push list #js {:x (.-x b) :y (.-y b) :z (.-z b)})
                  (recur (inc k) (+ seconds (.-c-open s))))
              :else
              (do (.push list #js {:x (.-x b) :y (.-y b) :z (.-z b) :via (.-via act) :at (.-at act)})
                  (recur (inc k) (+ seconds (if (identical? (.-via act) "plate") (.-c-open-plate s) (.-c-open-redstone s)))))))))))

  ;; the opening pass's edge: only what the first pass did not find, and that needs something opened
  (openingEdge [s x y z h move parent-node dsec drisk slow-to corner shape]
    (when-not (true? (.has ^js (.-seen-edges s) (.keyOf s x y z (bit-and shape 15))))
      (let [there (not-in (.closedIn s x y z h) ^js (.-door-here s))
            blocks (not-in (.concat ^js (.-door-here s) there) ^js (.-door-arrival s))]
        (when-not (and (zero? (.-length blocks)) (not ^boolean (.-door-through s)))
          (let [^js opened (.opening s (.-open-x s) (.-open-y s) (.-open-z s) x z blocks)]
            (when (some? opened)
              (set! (.-move-open s) (if (pos? (.-length (.-list opened))) (.push ^js (.-open-lists s) (.-list opened)) 0))
              (.sink s x y z h move parent-node (+ dsec (.-seconds opened)) drisk slow-to corner shape)
              (set! (.-move-open s) 0)))))))

  ;; Every expansion near a closed door, gate or trapdoor runs twice: once with the world as it stands, then in the opening
  ;; pass with those blocks open. An edge only the second pass finds is a move that needs something opened: it costs
  ;; costs.open per block opened by hand and records step.opens.
  (expandOpening [s x y z h slow-from i region]
    (set! (.-open-x s) x)
    (set! (.-open-y s) y)
    (set! (.-open-z s) z)
    (set! (.-seen-edges s) (js/Set.))
    (set! (.-edge-mode s) 2)
    (.expandMoves s x y z h slow-from i region)
    ;; what the move into the node here opened is open still; a body standing in a gate's cell without having opened it is not
    (set! (.-door-arrival s) (if (and (>= i 0) (pos? (aget (.-opens s) i))) (aget ^js (.-open-lists s) (dec (aget (.-opens s) i))) #js []))
    (set! (.-door-here s) (.closedIn s x y z h))
    (set! (.-door-through s) (true? (.some ^js (.-door-here s) (fn [b] (in-list? b ^js (.-door-arrival s))))))
    (set! (.-edge-mode s) 3)
    (set! (.-open-mode s) true)
    (.expandMoves s x y z h slow-from i region)
    (set! (.-open-mode s) false)
    (set! (.-edge-mode s) 0))

  (expandAt [s x y z h slow-from i region]
    ;; when no section near the cell holds a partial block or a climbable, no cell this expansion looks at is tight
    (set! (.-quiet s) (.sectionsClear s x y z 2 2 3))
    (if (or ^boolean (.-quiet s) (not ^boolean (.nearOpenable s x y z)))
      (.expandMoves s x y z h slow-from i region)
      (.expandOpening s x y z h slow-from i region))))
