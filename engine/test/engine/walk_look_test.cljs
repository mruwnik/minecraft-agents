(ns engine.walk-look-test
  "jobs.movement.walk-look against the fake world: one leg down a dark tunnel to look at a side a look from here cannot
  make out, and back."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.lib.look :as look]
            [jobs.lib.look-edge :as look-edge]
            [jobs.lib.reach.room :as reach-room]
            [engine.shelter-support :refer [night]]))

(def tunnel
  "A 1x2 tunnel x 0..12 in stone, sealed at both ends, the body at its west end."
  {:self {:pos {:x 0.5 :y 64 :z 0.5}}
   :blocks (merge (into {} (remove (fn [[k _]] (let [[x y z] (map js/Number (.split k ","))] (and (<= 0 x 12) (<= 64 y 65) (= z 0)))))
                        (tu/box -1 63 -1 13 66 1 "stone")))})

(defn ^:async walk-look
  "Runs walk-look for the side [8 64 0] from {:x 6}; the side shows once the body has stood within `sight` blocks of it
  (nil: never), over `jobs` (default registry/jobs). [farthest x the body reached, the cell it ended in, the :walk-looked cells, its ending kinds, the :walk-looked declined sides, its stands]."
  ([sight] (walk-look sight ##Inf))
  ([sight worth] (walk-look sight worth registry/jobs))
  ([sight worth jobs]
  (let [reached (atom 0)
        [seen sink] (tu/capture-sink)
        clock (atom 1000000)
        p (tu/seeing-all (tu/fake-on-floor tunnel))
        eng (core/create {:primitives p :dir (tu/tmp-dir) :now #(deref clock) :jobs jobs
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (with-redefs [ctx/worth (constantly worth)
                  look/unseen? (fn [p _]
                                 (swap! reached max (.-x (.-pos (.self p))))
                                 (not (and sight (<= (- 8 @reached) sight))))]
      (core/submit! eng '(jobs.movement.walk-look {:side [8 64 0] :to [6 64 0]}) {})
      (await (tu/tick-until-idle! eng 40)))
    [(js/Math.floor @reached)
     (mapv js/Math.floor ((juxt #(.-x %) #(.-y %) #(.-z %)) (.-pos (.self p))))
     (:cells (:data (mem/latest (mem/view (:store eng)) :walk-looked)))
     (into [] (comp (filter #(and (= :job (:source %)) (#{:completed :stopped} (:kind %)) (= "jobs.movement.walk-look" (:name (:data %))))) (map :kind)) @seen)
     (:declined (:data (mem/latest (mem/view (:store eng)) :walk-looked)))
     (:stands (:data (mem/latest (mem/view (:store eng)) :walk-looked)))])))

(deftest a-walk-to-look-walks-the-leg-it-was-priced-for-then-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= [6 [0 64 0] nil [:completed] nil [{:x 6 :y 64 :z 0}]] (await (walk-look 2)))
            "it walks to its :to, no farther, sees the side there and comes back; a side that shows is not remembered, the stand it was seen from is (no later leg walks back there)")
        (is (= [6 [0 64 0] [[8 64 0]] [:completed] nil [{:x 6 :y 64 :z 0}]] (await (walk-look nil)))
            "a side still unseen from close by is remembered, so no walk is offered for it again")
        (is (= [0 [0 64 0] nil [:completed] [[8 64 0]] nil] (await (walk-look nil 0)))
            "a walk the worth does not buy is not walked, and the side is not remembered as looked at from close by")))))

(deftest a-leg-whose-walk-ends-short-of-its-stand-looked-from-nowhere-and-is-declined-not-looked-at
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [short {:ways [:walk] :round (fn [c] (ctx/end c :nothing-possible {:reason :unreachable}))}]
          (is (= [0 [0 64 0] nil [:completed] [[8 64 0]] nil] (await (walk-look nil ##Inf (assoc registry/jobs 'jobs.movement.go-to short))))
              "the walk was bought but never left the start: no stand was reached, so the side is not looked at from close by (its neighbours keep their legs), only declined for this decision"))))))

(deftest the-walk-looked-entry-is-one-and-belongs-to-the-latest-root-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              p (tu/seeing-all (tu/fake-on-floor tunnel))
              eng (core/create {:primitives p :dir (tu/tmp-dir) :now #(deref clock) :jobs registry/jobs
                                :events (events/make {:body "Fake" :sinks [(second (tu/capture-sink))] :now #(deref clock)})})
              sides (mapv (fn [z] [8 64 z]) (range 21))]
          (with-redefs [ctx/worth (constantly ##Inf)
                        look/unseen? (constantly true)]
            (doseq [side sides]
              (core/submit! eng (list 'jobs.movement.walk-look {:side side :to [6 64 0]}) {})
              (await (tu/tick-until-idle! eng 40))))
          (is (= [(last sides)] (:cells (:data (mem/latest (mem/view (:store eng)) :walk-looked))))
              "each call is a decision of its own: the sides of the calls before it are not kept")
          (is (= 1 (count (mem/entries (mem/view (:store eng)) :walk-looked))) "the entry is rewritten, not copied per leg"))))))

(deftest a-leg-whose-walk-is-not-bought-records-its-side-as-declined-so-it-is-not-offered-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              p (tu/seeing-all (tu/fake-on-floor tunnel))
              eng (core/create {:primitives p :dir (tu/tmp-dir) :now #(deref clock) :jobs registry/jobs
                                :events (events/make {:body "Fake" :sinks [(second (tu/capture-sink))] :now #(deref clock)})})]
          (with-redefs [ctx/worth (constantly 0)
                        look/unseen? (constantly true)]
            (core/submit! eng '(jobs.movement.walk-look {:side [8 64 0] :to [6 64 0]}) {})
            (await (tu/tick-until-idle! eng 40)))
          (let [d (:data (mem/latest (mem/view (:store eng)) :walk-looked))]
            (is (= [[8 64 0]] (:declined d)))
            (is (empty? (:cells d)))))))))

(defn seen-within
  "p over the fake world of `blocks` (body at feet) that has seen the open cells of air? and the faces beside them within 4
  blocks of an eye in @eyes, but the cells in `missed`, and nothing else (a dark cell farther off is not recorded)."
  ([blocks feet air? eyes] (seen-within blocks feet air? eyes #{}))
  ([blocks feet air? eyes missed]
  (let [raw-p (tu/fake {:time night :self {:pos feet} :blocks blocks})
        face-air? (fn [x y z] (some (fn [[dx dy dz]] (air? (+ x dx) (+ y dy) (+ z dz))) [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]))
        seen? (fn [x y z] (and (or (air? x y z) (face-air? x y z)) (not (missed [x y z]))
                               (some (fn [[ex ey ez]] (<= (js/Math.hypot (- (+ x 0.5) ex) (- (+ y 0.5) ey) (- (+ z 0.5) ez)) 4)) @eyes)))]
    (aset raw-p "seenBlockAt" (fn [pos] (if (seen? (.-x pos) (.-y pos) (.-z pos))
                                          (let [b (.blockAt (tu/oracle raw-p) pos)] #js {:name (.-name b) :properties (.-properties b) :pos pos :age-ms 0})
                                          #js {:unknown true :pos pos})))
    (tu/set-sensed! raw-p (fn [pos] (if (seen? (.-x pos) (.-y pos) (.-z pos)) (.blockAt (tu/oracle raw-p) pos) #js {:unknown true})))
    raw-p)))

(deftest a-stand-already-looked-from-gets-no-second-leg
  ;; mine-wide-trench/dark-ore-3 (sweep cc48912d6): a cut x 16..19 ends at a 2-wide 6-deep trench, ore in its far wall. The
  ;; cut's end cell is a doorway onto the trench, so the stand nearest the far wall is x 18, beyond sight of it; live, two more
  ;; legs walked back to x 18 for the wall's cells and each stayed unseen.
  (let [air? (fn [x y z] (and (= z 16) (or (and (<= 16 x 19) (<= 64 y 65)) (and (<= 20 x 21) (<= 60 y 65)))))
        blocks (assoc (into {} (for [x (range 2 31) y (range 58 71) z (range 8 25) :when (not (air? x y z))] [(str x "," y "," z) "stone"])) "22,64,16" "iron_ore")
        eyes (atom [[16.5 65.6 16.5]])
        p (seen-within blocks {:x 16.5 :y 64 :z 16.5} air? eyes)
        cut {:x 16 :y 64 :z 16}]
    (with-redefs [look/near-of (constantly 4)]
      (is (= {:x 18 :y 64 :z 16} (:to (look-edge/far-side p cut cut #{}))) "premise: the first leg goes to x 18")
      (swap! eyes conj [18.5 65.6 16.5])
      (is (nil? (look-edge/far-side p cut cut #{} (constantly true) #{{:x 18 :y 64 :z 16}}))
          "looked from x 18: the trench's far wall is left to the prior, not walked back to"))))

(deftest a-side-inside-the-rock-gets-no-leg
  ;; The 1-wide tunnel's shell is seen but for one ceiling cell and the head-high wall beside it: the flood steps up into
  ;; that wall cell and on over the unseen rock (standing on the seen shell), whose sides no walk shows.
  (let [air? (fn [x y z] (and (zero? z) (<= 0 x 19) (#{64 65} y)))
        blocks (into {} (for [x (range -3 23) y (range 61 71) z (range -3 4) :when (not (air? x y z))] [(str x "," y "," z) "stone"]))
        missed #{[14 66 0] [14 65 1]}
        p (seen-within blocks {:x 10.5 :y 64 :z 0.5} air? (atom (for [x (range 0 20)] [(+ x 0.5) 65.6 0.5])) missed)
        at {:x 10 :y 64 :z 0}]
    (with-redefs [look/near-of (constantly 4)]
      (is (seq (remove missed (reach-room/unseen-sides p at))) "premise: the flood's sides reach into the rock")
      (is (nil? (look-edge/far-side p at at missed)) "the two missed cells looked at: no side left a walk shows"))))
