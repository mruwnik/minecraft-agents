(ns engine.recover-drops-danger-test
  "recover-drops against the mobs that killed the body: the killer is priced at the pile, and a trip cut by hostiles
  over and over is given up."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.death-test :as dt :refer [die! job recovered recovered-trip pile-drops]]
            [engine.library-test :refer [setup]]
            [jobs.lib.child :as child]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.look :as look]
            [engine.memory :as mem]
            [engine.shelter-support :as st]
            [engine.sky-perception :as sky]
            [jobs.survival.recover-drops.config :as cfg]
            [jobs.survival.recover-drops.estimate :as estimate]
            [jobs.survival.recover-drops.fetch :as fetch]
            [engine.test-util :as tu :refer [run-until-empty]]
            [jobs.lib.walk-pick :as pick]
            [triggers.survival.died :as died]))

(defn die-noted!
  "die!, the death on the :deaths list too (where walk-risk remembers a killer), as the engine records it."
  [eng data ago-ms]
  (die! eng data ago-ms)
  (core/record-death! eng (mem/latest (mem/view (:store eng)) :died)))

(def cheap-pile
  "recover-drops with the field-report pile worth about 600 s: less than the walk past its skeleton killer costs an unarmed body."
  '(jobs.survival.recover-drops {:value-overrides {"raw_iron" 20 "cobblestone" 0.5 "oak_planks" 0.5}}))

(defn fetch-run
  "Die with the field-report pile and cause, run the job (job by default) to its decision."
  ([cause] (fetch-run cause {}))
  ([cause body] (fetch-run cause body job))
  ([cause body job]
  (let [pile dt/death-pile-of-the-field-report]
    (fn ^:async t []
      (let [{:keys [eng seen p]} (setup (merge {:floor tu/walk-floor :entities (vec (pile-drops pile))} body))]
        (die-noted! eng (cond-> {:pos dt/death-pos :inventory pile} cause (assoc :cause cause)) 5000)
        (core/submit! eng job {})
        (await (run-until-empty eng 20))
        {:eng eng :seen seen :p p})))))

(defn decided-text
  "The priced decision of the run: what the remembered danger says, else the decision of the :recovered entry."
  [eng]
  (or (:text (dt/danger-remembered eng))
      (let [r (recovered eng)]
        (estimate/decision-text (if (= :skip (:decision r)) :skip :fetch) r (cfg/margin)))))

(deftest a-skeleton-killer-is-priced-at-the-pile
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [with-cause (await ((fetch-run "skeleton" {} cheap-pile)))
              without (await ((fetch-run nil)))]
          (is (re-find #"danger [1-9][0-9.]*,.*: skeleton" (decided-text (:eng with-cause))))
          (is (re-find #"danger 0," (decided-text (:eng without)))))))))

(defn looked-at-pile?
  "Whether the body turned to look at the death pos's cell."
  [p]
  (boolean (some #(and (= "look" (.-name %))
                       (let [at (.-pos (.-args %))] (= [(:x dt/death-pos) (:z dt/death-pos)] (mapv js/Math.floor [(.-x at) (.-z at)]))))
                 (.-calls (.-world p)))))

(deftest the-body-looks-at-the-pile-of-a-killer-it-has-not-seen-gone-before-weighing-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (:p (await ((fetch-run "skeleton" {} cheap-pile))))]
          (is (looked-at-pile? p) "a lit spot seen empty would clear it")
          (is (< (:age-ms (look/seen-block p dt/death-pos)) 5000) "the look is in the block memory newer than the death, which is what seen-clear? reads"))
        (is (not (looked-at-pile? (:p (await ((fetch-run "fall")))))) "a death by terrain: the place itself killed")))))

(deftest a-damage-type-cause-adds-no-danger
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await ((fetch-run "fall")))]
          (is (re-find #"danger 0," (decided-text (:eng r)))))))))

(def iron-kit
  {:inventory [{:name "iron_sword" :count 1}]
   :equipment {:head {:name "iron_helmet"} :torso {:name "iron_chestplate"} :legs {:name "iron_leggings"} :feet {:name "iron_boots"}}})

(deftest an-unarmed-body-skips-a-pile-its-killer-guards
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (await ((fetch-run "skeleton" {} cheap-pile)))]
          (is (nil? (recovered eng)) "a danger skip stays open")
          (is (re-find #"skip: .*danger [1-9][0-9.]*,.*: skeleton" (decided-text eng))))))))

(deftest an-unarmed-body-whose-bed-is-beside-the-pile-fetches-it-past-its-killer
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pile dt/death-pile-of-the-field-report
              {:keys [eng]} (setup {:floor tu/walk-floor :entities (vec (pile-drops pile)) :blocks {"20,64,3" "red_bed"}})]
          (mem/write! (:store eng) :spawn-bed {:pos {:x 20 :y 64 :z 3}} {:cap 1 :ttl :forever})
          (die! eng {:pos dt/death-pos :inventory pile :cause "skeleton"})
          (core/submit! eng job {})
          (await (run-until-empty eng 20))
          (is (= :collected (:decision (recovered eng))) "a death on the way costs a short walk back, not the pile"))))))

(deftest a-whole-iron-kit-is-left-to-a-pack-of-skeletons-that-would-kill-an-unarmed-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pile (into dt/death-pile-of-the-field-report
                         (map #(hash-map :name (str "iron_" %) :count 1)
                              ["pickaxe" "sword" "axe" "shovel" "helmet" "chestplate" "leggings" "boots"]))
              pack (for [i (range 6)] {:id (+ 20 i) :name "skeleton" :kind "hostile" :pos {:x (+ 18.5 i) :y 64 :z 3.5}})
              {:keys [eng]} (setup {:floor tu/walk-floor :entities (vec (concat (pile-drops pile) pack))})]
          (die! eng {:pos dt/death-pos :inventory pile :cause "skeleton"})
          (core/submit! eng job {})
          (await (run-until-empty eng 20))
          (is (nil? (recovered eng)) "a death on the way loses the kit too")
          (is (re-find #"^skip: .*danger [1-9][0-9.]*,.*: skeleton, skeleton, skeleton" (decided-text eng)))
          (is (< 3600 (js/parseFloat (second (re-find #"value ([0-9.]+)" (decided-text eng))))) "the kit is priced at hours"))))))

(deftest a-pile-inside-a-skeleton-packs-range-is-left-though-the-walk-skirts-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pile (remove #(= "raw_iron" (:name %)) dt/death-pile-of-the-field-report)
              at {:x 22 :y 64 :z 22}
              pack (for [i (range 6)] {:id (+ 20 i) :name "skeleton" :kind "hostile" :pos {:x (+ 10.5 i) :y 64 :z 12.5}})
              {:keys [eng]} (setup {:floor [-10 -10 40 40] :entities (vec (concat (pile-drops pile) pack))})]
          (die! eng {:pos at :inventory pile :cause "skeleton"})
          (core/submit! eng job {})
          (await (run-until-empty eng 20))
          (is (nil? (recovered eng)) "the collect stands where all six shoot, and their arrows land together")
          (is (re-find #"^skip: .*danger [1-9][0-9.]*,.*: skeleton" (decided-text eng)) (decided-text eng)))))))

(deftest a-body-with-an-iron-sword-and-armour-goes-for-the-guarded-pile
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await ((fetch-run "skeleton" iron-kit)))]
          (is (re-find #"^fetch: " (decided-text eng))))))))

;; ------------------------------------------------------------ cut trips

(defn death-t [eng] (:t (mem/latest (mem/view (:store eng)) :died)))

(defn resume-run
  "A trip decided and cut before: a saved :recover-trip with `trip` merged in, `before` run against the engine (a
  :hostile entry written after the save, say), then the job's real resume through key-to-death!."
  ([trip] (resume-run trip (fn [_])))
  ([trip before]
   (fn ^:async t []
     (let [{:keys [eng seen]} (setup {:floor tu/walk-floor :entities (vec (pile-drops dt/diamonds))})]
       (die! eng {:pos dt/death-pos :inventory dt/diamonds})
       (mem/write! (:store eng)  :recover-trip
                   (merge {:death-t (death-t eng) :decided {:value 1400 :cost 50} :baseline {} :phase :go} trip)
                   {:cap 1 :ttl 1000000})
       (before eng)
       (core/submit! eng job {})
       (await (run-until-empty eng 20))
       {:eng eng :seen seen}))))

(defn hostile-cut! [eng] (mem/write! (:store eng) :hostile {:name "zombie"} {:cap 5 :ttl 1000000}))

(deftest a-trip-cut-by-hostiles-twice-is-abandoned-as-danger
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await ((resume-run {:cuts 1} hostile-cut!)))]
          (is (= {:decision :abandoned :reason :danger} (select-keys (recovered eng) [:decision :reason])))
          (is (false? (died/died nil (mem/view (:store eng)) {})) "the died trigger stops firing"))))))

(deftest hostile-cuts-count-while-the-trip-is-blocked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await ((resume-run {:cuts 1 :blocked true} hostile-cut!)))]
          (is (= {:decision :abandoned :reason :danger} (select-keys (recovered eng) [:decision :reason]))))))))

(deftest benign-cuts-are-not-hostile-cuts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await ((resume-run {:cuts 1})))]
          (is (= :collected (:decision (recovered eng)))))))))

(deftest a-trip-that-did-not-arrive-too-often-is-abandoned-as-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await ((resume-run {:blocked true :fruitless {:go 3}})))]
          (is (= {:decision :abandoned :reason :unreachable} (select-keys (recovered eng) [:decision :reason]))))))))

(deftest a-trip-cut-once-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await ((resume-run {:cuts 0} hostile-cut!)))]
          (is (= :collected (:decision (recovered eng)))))))))

(deftest a-pile-lost-to-drowning-is-priced-as-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await ((fetch-run "drowning")))]
          (is (re-find #"danger [1-9][0-9.]*,.*: drowning" (decided-text (:eng r)))))))))

(deftest a-pile-under-water-is-priced-as-water-whatever-the-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [wet (into {} (for [y [64 65]] [(str (:x dt/death-pos) "," y "," (:z dt/death-pos)) "water"]))
              r (await ((fetch-run "fall" {:blocks wet})))]
          (is (re-find #"danger [1-9][0-9.]*,.*: water" (decided-text (:eng r)))))))))

(deftest an-agent-retry-weighs-a-remembered-danger-anew
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pile dt/death-pile-of-the-field-report
              {:keys [eng p]} (setup {:floor tu/walk-floor :entities (conj (vec (pile-drops pile)) dt/zombie-by-the-way)})]
          (die! eng {:pos dt/death-pos :inventory pile})
          (core/submit! eng '(jobs.survival.recover-drops {:value-overrides {"raw_iron" 0.5 "cobblestone" 0.05 "oak_planks" 0.05 "stone_pickaxe" 1 "diamond_pickaxe" 12 "diamond_sword" 10}}) {})
          (await (run-until-empty eng 5))
          (swap! (fake/state p) update :entities #(filterv (fn [e] (not= "hostile" (:kind e))) %))
          (core/submit! eng '(jobs.survival.recover-drops {:retry true}) {})
          (await (run-until-empty eng 20))
          (is (= :collected (:decision (recovered eng)))))))))

(deftest recover-drops-prices-the-pile-by-the-route-it-will-walk
  (let [{:keys [eng p]} (setup {:floor tu/walk-floor :entities [dt/zombie-by-the-way]})
        death {:pos dt/death-pos :inventory dt/death-pile-of-the-field-report}
        c {:primitives p :args {} :view #(mem/view (:store eng))}
        quote-of (fn [risk] {:uses {:job-time [5 5 15]} :risk risk})
        priced (fn [risk] (estimate/estimate c death 5000 (quote-of risk)))
        clear (priced {:hp 0 :threats []})
        risk {:hp 3 :lethal-hp 1 :threats [{:mob "zombie" :pos {:x 15.5 :y 64 :z 3.5}}]}
        risky (priced risk)]
    (is (zero? (get-in clear [:parts :danger])) "a zombie in sight that the planned way leaves behind is no danger")
    (is (nil? (:top-mobs clear)))
    (is (= (- (pick/quoted-s c (quote-of risk)) 5) (get-in risky [:parts :danger]))
        "the quote's walk risk is the danger, priced as every walk choice prices it")
    (is (= [{:name "zombie"}] (:top-mobs risky)))
    (is (= [{:name "zombie" :pos {:x 15.5 :y 64 :z 3.5}}] (:seen-mobs risky)))))

(deftest a-route-that-drowns-and-takes-long-is-priced-for-the-air-and-the-time
  (let [{:keys [eng p]} (setup {:floor tu/walk-floor :entities [] :inventory [{:name "cobblestone" :count 8}]})
        death {:pos {:x 20 :y 64 :z 0} :inventory dt/death-pile-of-the-field-report}
        c {:primitives p :args {} :view #(mem/view (:store eng))}
        dry (estimate/estimate c death 5000)
        wet (estimate/estimate c death 5000 {:uses {:job-time [60 60 120]} :route {:drown 9}})]
    (is (= ["water"] (mapv :name (:top-mobs wet))))
    (is (< (get-in dry [:parts :walk] 0) (get-in wet [:parts :walk])) "the quoted seconds, not the straight line, price the walk")
    (is (< (:cost dry) (:cost wet)))))

(deftest a-pile-in-cells-the-body-has-not-seen-may-be-under-water
  (let [death {:pos dt/death-pos :inventory dt/death-pile-of-the-field-report}
        est (fn [hide?]
              (let [{:keys [eng p]} (setup {:floor tu/walk-floor :entities []})
                    sensed (.-sensedAt p)]
                (tu/set-sensed! p (fn [q] (if (and hide? (= (:x dt/death-pos) (.-x q)) (= (:z dt/death-pos) (.-z q)))
                                            #js {:unknown true :pos q}
                                            (sensed q))))
                (estimate/estimate {:primitives p :args {} :view #(mem/view (:store eng))} death 5000)))]
    (is (nil? (some #{"water"} (map :name (:top-mobs (est false))))) "seen dry ground is no water")
    (is (some #{"water"} (map :name (:top-mobs (est true)))) "an unseen pile cell is priced as possibly wet")))

(deftest a-pile-the-body-cannot-dive-to-on-one-breath-is-skipped-unless-it-carries-a-block
  (let [wet (into {} (for [y (range 40 65)] [(str (:x dt/death-pos) "," y "," (:z dt/death-pos)) "water"]))
        death {:pos {:x (:x dt/death-pos) :y 40 :z (:z dt/death-pos)} :inventory dt/death-pile-of-the-field-report}
        est (fn [inventory quoted]
              (let [{:keys [eng p]} (setup {:floor tu/walk-floor :entities [] :blocks wet :inventory inventory})]
                (estimate/estimate {:primitives p :args {} :view #(mem/view (:store eng))} death 5000 quoted)))
        bare (est [] nil)]
    (is (= [:infinite :no-air-way] ((juxt :cost :reason) (update bare :cost estimate/finite))) "25 cells down and up is more than the air, and no block")
    (is (not= js/Infinity (:cost (est [{:name "cobblestone" :count 8}] nil))) "a carried block can make air")
    (is (= :no-air-way (:reason (est [] {:uses {:job-time [5 5 5]} :route {:drown 4}}))) "a quoted route that drowns")))

(deftest a-later-death-by-a-mob-near-an-older-pile-guards-that-pile-too
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pile dt/death-pile-of-the-field-report
              {:keys [eng p seen]} (setup {:floor tu/walk-floor :entities (vec (pile-drops pile))})]
          (die-noted! eng {:pos dt/death-pos :inventory pile :cause "skeleton"} 120000)
          (die-noted! eng {:pos (update dt/death-pos :x - 6) :inventory [] :cause "skeleton"} 5000)
          (core/submit! eng cheap-pile {})
          (await (run-until-empty eng 5))
          (is (re-find #"skip: value [1-9].*: skeleton" (decided-text eng)) (decided-text eng))
          (is (= [] (tu/walk-calls p))))))))

(deftest a-death-with-nothing-carried-starts-no-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:floor tu/walk-floor :entities []})]
          (die! eng {:pos dt/death-pos :inventory [] :cause "skeleton"})
          (core/submit! eng job {})
          (await (run-until-empty eng 5))
          (is (= :skip (:decision (recovered eng))))
          (is (false? (died/died nil (mem/view (:store eng)) {})))
          (is (= [] (tu/walk-calls p))))))))

(deftest an-older-open-pile-is-fetched-before-a-newer-empty-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:floor tu/walk-floor :entities dt/drops})]
          (die! eng {:pos dt/death-pos :inventory dt/diamonds} 60000)
          (die! eng {:pos {:x 5 :y 64 :z 0} :inventory []} 5000)
          (core/submit! eng job {})
          (await (run-until-empty eng 20))
          (is (= :collected (:decision (recovered eng))) "the pile with the diamonds first")
          (is (true? (died/died nil (mem/view (:store eng)) {})) "the newer death is still open")
          (core/submit! eng job {})
          (await (run-until-empty eng 20))
          (is (= [:collected :skip] (mapv (comp :decision :data) (mem/entries (mem/view (:store eng)) :recovered))))
          (is (false? (died/died nil (mem/view (:store eng)) {}))))))))

(deftest a-second-run-after-a-skip-says-the-death-was-decided
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:floor tu/walk-floor :entities dt/drops})
              _ (die! eng {:pos dt/death-pos :inventory dt/junk})
              _ (core/submit! eng job {})
              _ (await (run-until-empty eng 5))
              id (core/submit! eng job {})]
          (await (core/tick! eng))
          (let [w (core/waiting eng id)]
            (is (= [:no-drops :skip] [(:reason w) (:decision w)]) (pr-str w))
            (is (re-find #"already decided: skip: value .*cost" (:text w)) (pr-str w))))))))

(def plug-cells
  "The two dirt cells the body placed in the mouth of a stone cell round the death point."
  [[19 64 0] [19 65 0]])

(def stone-cell
  "Stone round the death point's two interior cells, but for the mouth towards the body (west)."
  (for [x (range 19 22) z [-1 0 1] y [64 65 66]
        :when (not (and (= x 20) (= z 0) (#{64 65} y)))
        :when (not (some #{[x y z]} plug-cells))]
    [x y z]))

(def cell-floor
  "Stone under the cell's columns: fake-on-floor leaves a column holding a block floorless, a drop the pile's collect refuses."
  (into {} (for [x (range 19 22) z [-1 0 1]] [(str x ",63," z) "stone"])))

(deftest a-pile-inside-the-bodys-own-seal-is-dug-out-and-recovered
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pile dt/death-pile-of-the-field-report
              blocks (merge cell-floor
                            (into {} (map (fn [[x y z]] [(str x "," y "," z) "stone"])) stone-cell)
                            (into {} (map (fn [[x y z]] [(str x "," y "," z) "dirt"])) plug-cells))
              {:keys [eng]} (setup {:floor tu/walk-floor :blocks blocks :entities (vec (pile-drops pile))})]
          (ledger/write! (:store eng) (mapv (fn [cell] {:cell cell :item "dirt" :before "air" :job "j9" :purpose :retreat-plug :state :placed}) plug-cells))
          (die! eng {:pos dt/death-pos :inventory pile})
          (core/submit! eng job {})
          (await (run-until-empty eng 40))
          (is (= :collected (:decision (recovered eng))) (pr-str (recovered eng))))))))

(defn sealed-run
  "The field-report pile inside the stone cell closed by the plug cells; extra ledger entries and world spec merged in."
  [extra-entries spec]
  (let [pile dt/death-pile-of-the-field-report
        blocks (merge cell-floor
                      (into {} (map (fn [[x y z]] [(str x "," y "," z) "stone"])) stone-cell)
                      (into {} (map (fn [[x y z]] [(str x "," y "," z) "dirt"])) plug-cells)
                      (:blocks spec))
        {:keys [eng seen p]} (setup (merge {:floor tu/walk-floor :entities (vec (pile-drops pile))} spec {:blocks blocks}))]
    (ledger/write! (:store eng) (into (mapv (fn [cell] {:cell cell :item "dirt" :before "air" :job "j9" :purpose :retreat-plug :state :placed}) plug-cells)
                                      extra-entries))
    (die! eng {:pos dt/death-pos :inventory pile})
    (core/submit! eng job {})
    {:eng eng :seen seen :p p}))

(deftest the-seal-cleanup-takes-back-only-blocks-near-the-pile
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [far [40 64 0]
              {:keys [eng]} (sealed-run [{:cell far :item "dirt" :before "air" :job "j8" :purpose :bridge :state :placed}]
                                        {:blocks {"40,64,0" "dirt"}})]
          (await (run-until-empty eng 40))
          (is (= :collected (:decision (recovered eng))) (pr-str (recovered eng)))
          (is (= [far] (mapv :cell (ledger/open-entries (mem/view (:store eng))))) "the far scaffold block is still there"))))))

(deftest a-seal-block-four-cells-west-of-a-pile-is-near-it
  (let [{:keys [eng]} (setup {:floor tu/walk-floor})]
    (ledger/write! (:store eng) [{:cell [22 64 26] :item "dirt" :before "air" :job "j9" :purpose :retreat :state :placed}])
    (is (= 1 (count (fetch/own-seal-near {:view #(mem/view (:store eng))} {:x 26.5 :y 64 :z 26.5}))))))

(deftest an-unseen-seal-is-walked-up-to-before-it-is-taken-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (sealed-run [] {:self {:pos {:x -25 :y 64 :z 0}}})
              walk-leg! fetch/walk-leg!]
          (tu/seeing-within p 6)
          (set! fetch/walk-leg! (fn [_ _]
                                          (set! fetch/walk-leg! walk-leg!)
                                          [{:kind :done :data {:arrived false :why :exhausted}}]))
          (await (run-until-empty eng 60))
          (set! fetch/walk-leg! walk-leg!)
          (is (empty? (ledger/open-entries (mem/view (:store eng))))
              "the first cleanup saw the plug and took it back")
          (is (= :collected (:decision (recovered eng))) (pr-str (recovered eng))))))))

(deftest a-seal-cleanup-that-takes-nothing-back-is-not-counted-as-cleared
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (sealed-run [] {:self {:pos {:x -25 :y 64 :z 0}}})
              walk-leg! fetch/walk-leg!
              run!* child/run!*
              seals (atom 0)]
          (set! fetch/walk-leg! (fn [_ _] [{:kind :done :data {:arrived false :why :goal-enclosed}}]))
          (let [stub (fn ^:async stub [c slot & more]
                      (if (= :seal slot)
                        (do (swap! seals inc) {:kind :done :data {:removed []}})
                        (await (apply run!* c slot more))))]
            (set! (.-cljs$core$IFn$_invoke$arity$4 stub) stub)
            (set! (.-cljs$core$IFn$_invoke$arity$5 stub) stub)
            (set! child/run!* stub))
          (await (run-until-empty eng 60))
          (set! fetch/walk-leg! walk-leg!)
          (set! child/run!* run!*)
          (is (= 2 @seals) "the seal is tried again, up to twice a trip"))))))

;; ------------------------------------------------------------ night: safe? judges the pile

(def late-night 22500)

(def roofed-at-night {:time late-night :self {:pos {:x 0 :y 64 :z 0}} :blocks {"0,67,0" "stone"}})

(defn night-run
  "Die with the field-report pile far-x blocks off on the same row, run the job (call `job-form`) in the world, then
  tick a few rounds."
  ([world far-x] (night-run world far-x job))
  ([world far-x job-form]
   (fn ^:async t []
     (let [pile dt/death-pile-of-the-field-report
           at {:x far-x :y 64 :z 0}
           drops (map #(assoc % :pos at) (pile-drops pile))
           {:keys [eng seen p]} (setup (merge {:floor [-30 -10 (+ far-x 20) 10] :entities (vec drops)} world))
           _ (sky/with-sky p)]
       (die! eng {:pos at :inventory pile})
       (let [id (core/submit! eng job-form {})]
         (await (run-until-empty eng 20))
         {:eng eng :seen seen :p p :id id})))))

(deftest a-pile-is-fetched-from-a-shelter-in-the-dark-however-long-the-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[world far-x] [[roofed-at-night 20] [roofed-at-night 200] [(assoc roofed-at-night :time st/night) 20]]]
          (let [{:keys [eng]} (await ((night-run world far-x)))]
            (is (= :collected (:decision (recovered eng))) (pr-str [world far-x]))))))))

(deftest a-declined-mob-stays-priced-until-its-place-is-seen-empty
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pile dt/death-pile-of-the-field-report
              {:keys [eng p clock]} (setup {:floor tu/walk-floor :entities (conj (vec (pile-drops pile)) dt/zombie-by-the-way)})
              dear '(jobs.survival.recover-drops {:value-overrides {"raw_iron" 0.5 "cobblestone" 0.05 "oak_planks" 0.05 "stone_pickaxe" 1 "diamond_pickaxe" 12 "diamond_sword" 10}})]
          (die! eng {:pos dt/death-pos :inventory pile})
          (core/submit! eng dear {})
          (await (run-until-empty eng 5))
          (swap! clock + 151000)
          (swap! (fake/state p) #(-> (update % :entities (fn [es] (filterv (fn [e] (not= "hostile" (:kind e))) es)))
                                      (assoc-in [:self :pos] {:x -40 :y 64 :z 0})))
          (core/submit! eng dear {})
          (await (run-until-empty eng 20))
          (is (not= :collected (:decision (recovered eng))) "the mob forgotten out of sight is still priced")
          (is (= [] (tu/walk-calls p)) "no walk back toward it"))))))

(deftest a-declined-mob-is-forgotten-when-its-place-is-seen-empty-from-afar
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pile dt/death-pile-of-the-field-report
              zombie (assoc dt/zombie-by-the-way :pos {:x 22.5 :y 64 :z 3.5})
              {:keys [eng p clock]} (setup {:floor tu/walk-floor :entities (conj (vec (pile-drops pile)) zombie)})
              dear '(jobs.survival.recover-drops {:value-overrides {"raw_iron" 0.5 "cobblestone" 0.05 "oak_planks" 0.05 "stone_pickaxe" 1 "diamond_pickaxe" 12 "diamond_sword" 10}})]
          (die! eng {:pos dt/death-pos :inventory pile})
          (core/submit! eng dear {})
          (await (run-until-empty eng 5))
          (core/submit! eng dear {})
          (await (core/tick! eng))
          (swap! clock + 151000)
          (swap! (fake/state p) update :entities #(filterv (fn [e] (not= "hostile" (:kind e))) %))
          (await (run-until-empty eng 20))
          (is (= :collected (:decision (recovered eng))) "the empty place is in clear sight 23 blocks off: the mob is no longer priced"))))))

(deftest a-skip-for-danger-is-remembered-then-tried-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pile dt/death-pile-of-the-field-report
              {:keys [eng p seen clock]} (setup {:floor tu/walk-floor :entities (conj (vec (pile-drops pile)) dt/zombie-by-the-way)})
              dear '(jobs.survival.recover-drops {:value-overrides {"raw_iron" 0.5 "cobblestone" 0.05 "oak_planks" 0.05 "stone_pickaxe" 1 "diamond_pickaxe" 12 "diamond_sword" 10}})]
          (die! eng {:pos dt/death-pos :inventory pile})
          (core/submit! eng dear {})
          (await (run-until-empty eng 5))
          (is (nil? (recovered eng)) "a danger skip is not final")
          (is (false? (died/died nil (mem/view (:store eng)) {})) "the died trigger rests while the danger is remembered")
          (let [id (core/submit! eng dear {})]
            (await (core/tick! eng))
            (is (= :danger (:reason (core/waiting eng id))) (pr-str (core/waiting eng id)))
            (is (re-find #"zombie" (str (:text (core/waiting eng id)))) "the wait names what made it decline")
            (is (= 1 (count (mem/entries (mem/view (:store eng)) :recover-danger))) "the danger is not weighed again")
                        (is (= [] (tu/walk-calls p)))
            (swap! clock + 151000)
            (is (true? (died/died nil (mem/view (:store eng)) {})) "the died trigger fires again once the memory has run out")
            (swap! (fake/state p) update :entities #(filterv (fn [e] (not= "hostile" (:kind e))) %))
            (await (run-until-empty eng 20))
            (is (= :collected (:decision (recovered eng))))))))))

(deftest a-mob-still-standing-where-a-finished-hold-saw-it-is-no-reason-to-hold-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pile dt/death-pile-of-the-field-report
              {:keys [eng clock]} (setup {:floor tu/walk-floor :entities (conj (vec (pile-drops pile)) dt/zombie-by-the-way)})
              dear '(jobs.survival.recover-drops {:value-overrides {"raw_iron" 0.5 "cobblestone" 0.05 "oak_planks" 0.05 "stone_pickaxe" 1 "diamond_pickaxe" 12 "diamond_sword" 10}})]
          (die! eng {:pos dt/death-pos :inventory pile})
          (core/submit! eng dear {})
          (await (run-until-empty eng 5))
          (is (= 1 (count (mem/entries (mem/view (:store eng)) :recover-danger))) "the first sight of the danger holds")
          (swap! clock + 151000)
          (core/submit! eng dear {})
          (await (run-until-empty eng 20))
          (is (= 1 (count (mem/entries (mem/view (:store eng)) :recover-danger))) "no second hold for the same mob")
          (is (= :skip (:decision (recovered eng))) "the danger is still in the price: the pile is left"))))))

(deftest a-pile-worth-less-than-the-trip-with-its-danger-is-left-when-the-window-cannot-hold-a-wait
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pile dt/death-pile-of-the-field-report
              {:keys [eng clock p]} (setup {:floor tu/walk-floor :entities (conj (vec (pile-drops pile)) dt/zombie-by-the-way)})]
          (die! eng {:pos dt/death-pos :inventory pile})
          (swap! clock + 270000)
          (mem/write! (:store eng) :recover-trip {:death-t (:t (mem/latest (mem/view (:store eng)) :died)) :cuts 2 :hostile-cut true :phase :go}
                      {:cap 1 :ttl 1000000})
          (core/submit! eng '(jobs.survival.recover-drops {:value-overrides {"raw_iron" 0.5 "cobblestone" 0.05 "oak_planks" 0.05 "stone_pickaxe" 1 "diamond_pickaxe" 12 "diamond_sword" 10}}) {})
          (await (run-until-empty eng 20))
          (is (empty? (mem/entries (mem/view (:store eng)) :recover-danger)) "no wait for the danger to pass was set")
          (is (= :skip (:decision (recovered eng))))
          (is (= [] (tu/walk-calls p))))))))

(deftest a-climb-toward-a-pile-far-above-is-progress-though-the-distance-hardly-shrinks
  (let [pile {:x 220 :y 79 :z 174}]
    (is (fetch/nearer? {:x 53 :y 50 :z -100} {:x 53 :y 51 :z -100} pile))
    (is (fetch/nearer? {:x 0 :y 70 :z 0} {:x 30 :y 70 :z 0} pile))
    (is (not (fetch/nearer? {:x 53 :y 50 :z -100} {:x 53 :y 50 :z -100} pile)))
    (is (not (fetch/nearer? {:x 53 :y 51 :z -100} {:x 53 :y 50 :z -100} pile)))))

(deftest the-decision-text-names-the-worth-of-the-items-it-does-not-list
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pile (into dt/death-pile-of-the-field-report
                         [{:name "iron_pickaxe" :count 1} {:name "iron_sword" :count 1} {:name "iron_axe" :count 1} {:name "iron_shovel" :count 1}])
              {:keys [eng]} (setup {:floor tu/walk-floor :entities (vec (pile-drops pile))})]
          (die! eng {:pos dt/death-pos :inventory pile :cause "skeleton"})
          (core/submit! eng job {})
          (await (run-until-empty eng 20))
          (let [text (decided-text eng)]
            (is (re-find #"value [0-9.]+ \(.*, [0-9]+ other kinds [0-9.]+\)" text) text)))))))
