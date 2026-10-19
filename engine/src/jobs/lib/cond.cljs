(ns jobs.lib.cond
  "The condition library: checks of current state that give a status [kind text data] (engine.contract). An entry
  called with its args gives a checker, a fn of the ctx: a status entry ((mounted)) one returning a status, a value
  entry ((health)) one returning a value, which a comparison turns into a status. A value entry's args are literals
  or value checkers. and/or/not/because/known? take checkers and give one. engine.condition compiles an agent's
  :when onto these fns (library); job code calls them directly. A value the body cannot know now (offline, no such
  place) is unknown, and a status built on it is [:later \"... not known\" {:unknown true}]."
  (:refer-clojure :exclude [and or not < > <= >= =])
  (:require [clojure.string :as str]
            [engine.contract :as contract]
            [engine.memory :as mem]
            [jobs.lib.body :as body]
            [jobs.lib.breath :as breath]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]))

(def unknown ::unknown)

(defn unknown? [v] (cljs.core/= unknown v))

(defn world [c] (:primitives c))

(defn view [c] ((:view c)))

(defn self "The sensed self, or nil while the body is offline." [c]
  (let [s (.self (world c))]
    (when-not (cljs.core/= "offline" (.-status s)) s)))

(defn online
  "A read that is unknown while the body is offline and otherwise (f c self & args)."
  [f]
  (fn [c & args]
    (if-let [s (self c)] (apply f c s args) unknown)))

(defn number-or-unknown [x] (if (number? x) x unknown))

(defn boolean-or-unknown [x] (if (boolean? x) x unknown))

(defn position? [x] (every? #(number? (get x %)) [:x :y :z]))

;; ------------------------------------------------------------------ value entries

(defn arg-label [a]
  (cond (fn? a) (str "(" (:label (meta a)) ")") (string? a) a (keyword? a) (name a) :else (pr-str a)))

(defn arg-value [c a] (if (fn? a) (a c) a))

(defn value-entry
  "The value checker of entry fact over args (literals or value checkers): (read c & arg-values), unknown when an
  arg is. Its meta names the fact and labels it for texts (\"inventory torch\")."
  [fact read & args]
  (with-meta (fn [c]
               (let [vs (map #(arg-value c %) args)]
                 (if (some unknown? vs) unknown (apply read c vs))))
    {:fact fact :label (str/join " " (cons (name fact) (map arg-label args)))}))

(defn item-count [s item]
  (transduce (comp (filter #(cljs.core/= item (.-name %))) (map #(.-count %))) + 0 (array-seq (.-inventory s))))

(defn health [] (value-entry :health (online (fn [_ s] (number-or-unknown (.-health s))))))

(defn food [] (value-entry :food (online (fn [_ s] (number-or-unknown (.-food s))))))

(defn inventory [item] (value-entry :inventory (online (fn [_ s item] (item-count s item))) item))

(defn free-slots [] (value-entry :free-slots (online (fn [c _] (u/free-slots (world c))))))

(defn time-of-day [] (value-entry :time-of-day (online (fn [_ s] (number-or-unknown (.-timeOfDay s))))))

(defn place [kind]
  (value-entry :place (fn [c kind] (let [pos (mem/place (view c) kind)] (if (position? pos) pos unknown))) kind))

(defn since [kind]
  (value-entry :since (fn [c kind]
                        (let [v (view c)]
                          (if-let [t (:t (mem/latest v kind))] (/ (- (:now v) t) 1000) unknown)))
               kind))

(defn distance-to [pos]
  (value-entry :distance-to
               (online (fn [_ s pos]
                         (if (position? pos)
                           (let [here (.-pos s)]
                             (js/Math.hypot (- (.-x here) (:x pos)) (- (.-y here) (:y pos)) (- (.-z here) (:z pos))))
                           unknown)))
               pos))

(def max-scan-radius 32)

(def max-scan-count 256)

(def scans "World -> {[item radius] {:now :count}}: a scan is read once a tick." (js/WeakMap.))

(defn seen-count [c p item radius]
  (let [now (:now (view c))
        k [item radius]
        hit (get (.get scans p) k)]
    (if (cljs.core/= now (:now hit))
      (:count hit)
      (let [seen (aget p "seenBlocks")
            n (if seen (count (.call seen p #js {:radius (min radius max-scan-radius) :names #js [item] :max max-scan-count})) 0)]
        (.set scans p (assoc (.get scans p) k {:now now :count n}))
        n))))

(defn blocks-near [item radius]
  (value-entry :blocks-near (online (fn [c _ item radius] (seen-count c (world c) item radius))) item radius))

;; ------------------------------------------------------------------ comparisons

(def negated {"<" ">=" ">" "<=" "<=" ">" ">=" "<" "=" "!="})

(defn value-text [v]
  (cond (unknown? v) "unknown"
        (cljs.core/and (number? v) (cljs.core/not (integer? v))) (.toFixed v 1)
        :else (arg-label v)))

(defn side-text [a v] (if (fn? a) (str (:label (meta a)) " " (value-text v)) (value-text v)))

(defn comparison
  "The checker of (op a b): :ok with the relation as text (\"inventory torch 2 >= 1\"), :later with the relation that
  holds instead (\"inventory torch 0 < 1\"); its reason is the fact compared, :have and :want its two values."
  [op f a b]
  (let [reason (cljs.core/or (some #(when (fn? %) (:fact (meta %))) [a b]) :compare)]
    (fn [c]
      (let [x (arg-value c a)
            y (arg-value c b)
            unknown-side (some (fn [[side v]] (when (unknown? v) side)) [[a x] [b y]])
            text #(str (side-text a x) " " % " " (side-text b y))]
        (cond
          unknown-side (contract/later reason (str (:label (meta unknown-side)) " not known") :unknown true)
          (f x y) (contract/ok reason (text op) :have x :want y)
          :else (contract/later reason (text (negated op)) :have x :want y))))))

(defn < [a b] (comparison "<" cljs.core/< a b))
(defn > [a b] (comparison ">" cljs.core/> a b))
(defn <= [a b] (comparison "<=" cljs.core/<= a b))
(defn >= [a b] (comparison ">=" cljs.core/>= a b))
(defn = [a b] (comparison "=" cljs.core/= a b))

;; ------------------------------------------------------------------ status entries

(defn flag
  "The status checker of entry fact over holds? (c -> true, false or unknown): [:ok yes] or [:later no], each with its
  reason; unknown is an unknown :later with the fail reason."
  [fact [ok-reason yes] [fail-reason no] holds?]
  (fn [c]
    (let [x (holds? c)]
      (cond
        (unknown? x) (contract/later fail-reason (str (name fact) " not known") :unknown true)
        x (contract/ok ok-reason yes)
        :else (contract/later fail-reason no)))))

(defn daytime []
  (flag :daytime [:daytime "day"] [:night "night"] (online (fn [_ s] (boolean-or-unknown (.-isDay s))))))

(defn mounted []
  (flag :mounted [:mounted "aboard"] [:not-aboard "on foot"] (online (fn [_ s] (some? (.-vehicle s))))))

(defn in-water []
  (flag :in-water [:in-water "in water"] [:not-in-water "not in water"]
        (online (fn [_ s] (boolean-or-unknown (.-inWater s))))))

(defn worn?
  "Whether an armour slot or the off-hand holds item (the main hand is the held item, not worn)."
  [s item]
  (let [gear (.-equipment s)]
    (boolean (some #(cljs.core/= item (some-> (aget gear %) .-name)) ["head" "torso" "legs" "feet" "offHand"]))))

(defn wearing [item]
  (flag :wearing [:wearing (str "wearing " item)] [:not-wearing (str "not wearing " item)]
        (online (fn [_ s] (worn? s item)))))

(defn hostile-near [radius]
  (flag :hostile-near [:hostile-near (str "a hostile within " radius)] [:no-hostile-near (str "no hostile within " radius)]
        (online (fn [c _] (danger-q/danger-near? (world c) radius radius)))))

(defn burning []
  (flag :burning [:burning "burning"] [:not-burning "not burning"] (online (fn [c _] (body/burning? (world c))))))

(defn suffocating []
  (flag :suffocating [:suffocating "suffocating"] [:not-suffocating "breathing"]
        (online (fn [c _] (some? (breath/situation (world c) nil))))))

(defn night-unsafe []
  (flag :night-unsafe [:night-unsafe "night in the open"] [:night-safe "safe for the night"]
        (online (fn [c _] (sh-sense/unsafe-night? (world c) sh-sense/default-roof-height)))))

;; ------------------------------------------------------------------ operators

(defn with-parts [s parts] (assoc-in s [2 :parts] parts))

(defn ask-all [c checks] (mapv #(contract/asked c %) checks))

(defn and
  "Every check asked: any :never, else the first :later, else the first :look, else :ok (texts joined); :parts
  holds each."
  [& checks]
  (fn [c] (let [ss (ask-all c checks)] (with-parts (contract/all ss) ss))))

(defn or-later [ss]
  (let [laters (filter #(cljs.core/= :later (first %)) ss)]
    (cljs.core/or (first (filter #(:unknown (nth % 2)) laters)) (first laters))))

(defn or
  "Every check asked: the first :ok, else the first :look (a look may still make it hold), else the first :later (an
  unknown one before a definite one, so not of it stays :later), else :never; :parts holds each."
  [& checks]
  (fn [c]
    (let [ss (ask-all c checks)
          later (or-later ss)]
      (with-parts (some (fn [k] (if (cljs.core/= k :later) later (some #(when (cljs.core/= k (first %)) %) ss))) [:ok :look :later :never]) ss))))

(defn not
  "An :ok becomes :later and a definite :later or :never :ok, each with the check's text and data; an unknown :later
  and a :look stay."
  [check]
  (fn [c]
    (let [[k text data :as s] (contract/asked c check)]
      (cond
        (cljs.core/= :ok k) [:later text data]
        (cljs.core/and (#{:later :never} k) (cljs.core/not (:unknown data))) [:ok text data]
        :else s))))

(defn because
  "check's status with a failing one's reason (and text, when given) replaced; the kind stays."
  ([reason check] (because reason nil check))
  ([reason text check]
   (fn [c]
     (let [[k t data :as s] (contract/asked c check)]
       (if (cljs.core/= :ok k) s [k (cljs.core/or text t) (assoc data :reason reason)])))))

(defn known?
  "A definite status of whether x (a value checker or a status checker) is known now."
  [x]
  (fn [c]
    (let [v (x c)]
      (cond
        (contract/status? v) (if (:unknown (nth v 2)) (contract/later :not-known (second v)) (contract/ok :known (second v)))
        (unknown? v) (contract/later :not-known (str (:label (meta x)) " not known"))
        :else (contract/ok :known (str (:label (meta x)) " known"))))))

;; ------------------------------------------------------------------ the library

(def library
  "Every name an agent's condition may use (engine.condition): entries {:args [type ...] :type :number|:position|:status
  :doc :fn}, operators {:fn} (the compiler holds their grammar)."
  {'health {:args [] :type :number :doc "health, 0 to 20" :fn health}
   'food {:args [] :type :number :doc "food, 0 to 20" :fn food}
   'inventory {:args [:string] :type :number :doc "how many of the named item are carried (main and hotbar slots)"
               :fn inventory}
   'free-slots {:args [] :type :number :doc "empty main and hotbar slots, of 36" :fn free-slots}
   'time-of-day {:args [] :type :number :doc "the day's tick, 0 to 23999 (dusk at 12000)" :fn time-of-day}
   'place {:args [:keyword] :type :position
           :doc "the latest remembered place of that kind (:home :bed :chest ...); unknown when none" :fn place}
   'since {:args [:keyword] :type :number
           :doc "seconds since memory last recorded an entry of that kind (:slept :fed :looked ...); unknown when none"
           :fn since}
   'distance-to {:args [:position] :type :number :doc "blocks from the body to the position" :fn distance-to}
   'blocks-near {:args [:string :number] :type :number
                 :doc "how many blocks of that name the body has seen within the radius (at most 32), counted up to 256; read once a tick"
                 :fn blocks-near}
   'daytime {:args [] :type :status :doc "the sun is up" :fn daytime}
   'mounted {:args [] :type :status :doc "the body rides a vehicle (boat, minecart, horse)" :fn mounted}
   'in-water {:args [] :type :status :doc "the body is in water" :fn in-water}
   'wearing {:args [:string] :type :status
             :doc "the named item is worn (head, torso, legs, feet) or held in the off-hand" :fn wearing}
   'hostile-near {:args [:number] :type :status
                  :doc "a real danger (as the hostile-near trigger: a mob that can reach the body, or a ranged one with a line of fire) is within that many blocks"
                  :fn hostile-near}
   'burning {:args [] :type :status :doc "on fire or in lava without fire resistance (the burning trigger)" :fn burning}
   'suffocating {:args [] :type :status :doc "drowning or enclosed (the suffocating trigger, default oxygen)"
                 :fn suffocating}
   'night-unsafe {:args [] :type :status :doc "night, awake, nothing overhead and not buried" :fn night-unsafe}
   'and {:fn and}
   'or {:fn or}
   'not {:fn not}
   'because {:fn because}
   'known? {:fn known?}
   '< {:fn <}
   '> {:fn >}
   '<= {:fn <=}
   '>= {:fn >=}
   '= {:fn =}})
