(ns world-test.fixture-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.fixture :as f]))

(def grid f/default-grid)

(def text
  "{:defaults {:tags [:hostile] :register [{:trigger :hostile-near}]
              :blocks [[:fill [10 -1 10] [22 -1 22] \"stone\"]]
              :body {:at [16.5 0 16.5] :effects [[\"resistance\" 3600 4]]}
              :expect [{:no-event {:kind :died} :for-s 10}]}
   :cases [{:name \"open\" :act [[:summon \"skeleton\" [22.5 0 16.5] \"{NoAI:1b}\"]]
            :expect [{:event {:kind :fired :data {:pos [:near #at [16.5 0 16.5] 2]}} :within-s 9}]}
           {:name \"glass\" :blocks [[:fill [19 0 12] [19 6 20] \"glass\"]] :body {:inventory [[\"bread\" 3]]}
            :act [[:job (jobs.blocks.dig {:pos #at [1 0 2] :box {:min #xyz [0 0 0]}})]]}
           {:name 7}]}")

(deftest plots-tile-the-grid-row-by-row
  (is (= [20000 150 20000] (f/plot-origin grid 0)))
  (is (= [20032 150 20000] (f/plot-origin grid 1)))
  (is (= [20000 150 20032] (f/plot-origin grid 20)))
  (is (thrown? js/Error (f/plot-origin grid 416))))

(defn rect [grid i]
  (let [[x _ z] (f/plot-origin grid i)] [x z (+ x (:size-x grid) -1) (+ z (:size-z grid) -1)]))

(defn overlap? [[ax az ax2 az2] [bx bz bx2 bz2]]
  (and (<= ax bx2) (<= bx ax2) (<= az bz2) (<= bz az2)))

(deftest large-plots-lie-beside-the-grid-and-never-overlap
  (let [big (f/case-grid {:plot {:length 1024 :width 64}})
        small (f/case-grid {:plot {}})
        rects (map #(rect big %) (range 400 416))
        grid-rect [20000 20000 20639 20639]]
    (is (= 1024 (:size-x big)))
    (is (= 32 (:size-x small) (:size-z small)))
    (is (= [20000 150 20704] (f/plot-origin big 400)))
    (is (not-any? #(overlap? grid-rect %) rects))
    (is (not-any? (fn [[a b]] (overlap? a b)) (for [a rects b rects :when (not= a b)] [a b])))
    (is (= [400 416] (f/plot-range big)))
    (is (= [0 400] (f/plot-range small)))))

(deftest large-plot-setup-covers-every-block-in-small-commands
  (let [g (f/case-grid {:plot {:length 400 :width 32}})
        origin (f/plot-origin g 400)
        cmds (f/setup-commands g origin {:plot {:height 16 :floor "stone"}})
        nums (fn [prefix] (for [c cmds :when (re-find (re-pattern (str "^" prefix)) c)] (mapv js/Number (re-seq #"-?\d+" c))))
        fills (nums "fill")
        loads (nums "forceload add")]
    (is (every? (fn [[x y z x2 y2 z2]] (<= (* (inc (- x2 x)) (inc (- y2 y)) (inc (- z2 z))) 32768)) fills))
    (is (every? (fn [[x z x2 z2]] (<= (* (quot (inc (- x2 x)) 16) (quot (inc (- z2 z)) 16)) 256)) loads))
    (is (= 20000 (apply min (map first fills)) (apply min (map first loads))))
    (is (= 20399 (apply max (map #(nth % 3) fills)) (apply max (map #(nth % 2) loads))))
    (is (= [150 166] [(apply min (map second (remove #(= 149 (second %)) fills))) (apply max (map #(nth % 4) fills))]))
    (is (re-find #"dx=399,dy=17,dz=31" (some #(when (re-find #"^kill" %) %) cmds)))
    (is (= (count loads) (count (filter #(re-find #"^forceload remove" %) (f/cleanup-commands g origin "B" {:plot {:height 16 :floor "stone"}})))))))

(deftest large-plot-setup-removes-a-wider-left-over-floor
  (let [g (f/case-grid {:plot {:length 400 :width 32}})
        origin (f/plot-origin g 400)
        cmds (f/setup-commands g origin {:plot {:height 16 :floor "stone"}})
        air-floor (filter #(re-find #"^fill \d+ 149 20704 \d+ 149 20767 air$" %) cmds)
        floor-idx (fn [pred] (first (keep-indexed #(when (pred %2) %1) cmds)))]
    (is (seq air-floor))
    (is (< (floor-idx #(re-find #"149 20704 \d+ 149 20767 air$" %)) (floor-idx #(re-find #"149 20735 stone$" %))))
    (is (= (count (filter #(re-find #"^forceload add" %) cmds))
           (count (filter #(re-find #"^forceload remove" %) (f/cleanup-commands g origin "B" {:plot {:height 16 :floor "stone"}})))))
    (is (every? #(re-find #"20767$" %) (filter #(re-find #"^forceload add" %) cmds)))))

(deftest a-plot-must-fit-its-lane
  (let [ps #(f/problems (merge {:name "a" :time :day :body {:at [1 0 1]} :act [] :after [] :expect [{:event {:kind :x} :within-s 1}]} %))]
    (is (empty? (ps {:plot {:height 16 :length 1024 :width 64}})))
    (is (seq (ps {:plot {:height 16 :length 1025}})))
    (is (seq (ps {:plot {:height 16 :width 65}})))
    (is (seq (ps {:plot {:height 16 :length 8}})))))

(deftest a-file-of-cases-merges-each-case-over-its-defaults
  (let [[open glass bad] (f/file-cases text "hostile")]
    (is (= "hostile/open" (:id open)))
    (is (= [:hostile] (:tags glass)))
    (is (= 2 (count (:blocks glass))) "blocks append to the defaults' blocks")
    (is (= 2 (count (:expect open))) "expectations append too")
    (is (= [["resistance" 3600 4]] (get-in glass [:body :effects])) "body merges key by key")
    (is (= [["bread" 3]] (get-in glass [:body :inventory])))
    (is (= 1 (get-in glass [:body :settle-s])) "the global defaults fill the rest")
    (is (= :day (:time open)))
    (is (nil? (:problems open)))
    (is (some #(re-find #":name" %) (:problems bad)))))

(deftest a-single-case-file-is-one-case
  (is (= ["one/x"] (map :id (f/file-cases "{:name \"x\"}" "one")))))

(deftest problems-name-what-cannot-run
  (let [ps (fn [c] (f/problems (f/merge-case {} c)))]
    (is (empty? (ps {:name "a" :after [[:block [1 0 1] "stone"]]})))
    (is (seq (ps {:name "a" :time :dusk})))
    (is (seq (ps {:name "a" :act [[:teleport]]})))
    (is (seq (ps {:name "a" :expect [{:event {:kind :x}}]})) "an :event needs :within-s")
    (is (seq (ps {:name "a" :expect [{:no-event {:kind :x}}]})) "a :no-event needs :for-s")
    (is (seq (ps {:name "a" :plot {:height 40}})))))

(deftest a-case-of-only-unanchored-no-events-checks-nothing
  (let [ps (fn [c] (f/problems (f/merge-case {} (merge {:name "a"} c))))]
    (is (seq (ps {:expect [{:no-event {:kind :x} :for-s 5}]})) "silence on an empty log proves nothing")
    (is (empty? (ps {:expect [{:no-event {:kind :x} :for-s 5 :from-event {:kind :start}}]})))
    (is (empty? (ps {:expect [{:no-event {:kind :x} :for-s 5} {:event {:kind :start} :within-s 3}]})))
    (is (empty? (ps {:expect [{:no-event {:kind :x} :for-s 5}] :after [[:block [1 0 1] "stone"]]})))))

(deftest tags-resolve-against-the-plot-origin
  (let [[_ glass] (f/file-cases text "hostile")
        [[_ spec]] (:act (f/resolve-tags glass [100 150 200]))]
    (is (= '(jobs.blocks.dig {:pos [101 150 202] :box {:min {:x 100 :y 150 :z 200}}}) spec))
    (is (= "(jobs.blocks.dig {:pos [101 150 202], :box {:min {:x 100, :y 150, :z 200}}})" (pr-str spec)))))

(deftest selection-by-tag-and-name
  (let [cases (f/file-cases text "hostile")]
    (is (= 3 (count (f/select-cases cases {:tag "hostile"}))))
    (is (= 0 (count (f/select-cases cases {:tag "herd"}))))
    (is (= ["hostile/glass"] (map :id (f/select-cases cases {:match "glass"}))))))

(deftest commands-build-the-plot-and-place-the-body
  (let [[open glass] (f/file-cases text "hostile")
        origin [20000 150 20000]
        setup (f/setup-commands grid origin open)]
    (is (= "forceload add 20000 20000 20031 20031" (first setup)))
    (is (= "kill @e[type=!player,x=20000,y=149,z=20000,dx=31,dy=17,dz=31]" (second setup)))
    (is (= ["fill 20000 150 20000 20031 166 20031 air" "fill 20000 149 20000 20031 149 20031 stone"] (drop 2 setup))
        "16 high fits one fill of 32x32x17 = 17408 blocks")
    (is (= 1 (count (filter #(re-find #"air$" %) (f/clear-commands grid origin 31 "stone")))) "32x32x32 is one fill at the limit")
    (is (= ["fill 20000 150 20000 20063 157 20063 air" "fill 20000 158 20000 20063 165 20063 air"
            "fill 20000 166 20000 20063 166 20063 air"]
           (butlast (f/clear-commands (assoc grid :size 64) origin 16 "stone")))
        "a 64-wide plot clears 8 layers a fill")
    (is (= ["fill 20010 149 20010 20022 149 20022 stone" "fill 20019 150 20012 20019 156 20020 glass"]
           (f/block-commands origin glass)))
    (is (= "fill 20001 150 20001 20003 152 20003 glass hollow" (f/block-command origin [:fill [1 0 1] [3 2 3] "glass" :hollow])))
    (is (= "setblock 20001 150 20002 oak_fence_gate[facing=east]" (f/block-command origin [:set [1 0 2] "oak_fence_gate[facing=east]"])))
    (let [body (f/body-commands origin "ProbeFixture" glass)]
      (is (some #{"tp ProbeFixture 20016.5 150 20016.5 0 0"} body))
      (is (some #{"give ProbeFixture bread 3"} body))
      (is (some #{"effect give ProbeFixture resistance 3600 4 true"} body))
      (is (= "clear ProbeFixture" (second body))))
    (is (some #{"tp ProbeFixture 20016.5 150 20016.5 180 0"}
              (f/body-commands origin "ProbeFixture" (assoc-in glass [:body :yaw] 180)))
        "a case may turn the body (yaw, degrees: 0 south, 180 north) so it can see what lies behind the default view")
    (is (some #{"tp ProbeFixture 20016.5 150 20016.5 0 -30"}
              (f/body-commands origin "ProbeFixture" (assoc-in glass [:body :pitch] -30)))
        "a case may tilt the body's view (pitch, degrees: 0 level, positive down)")
    (is (= "summon skeleton 20022.5 150 20016.5 {Tags:[\"wt\"],PersistenceRequired:1b,NoAI:1b}"
           (f/summon-command origin (first (:act open)))))
    (is (= "summon cow 20001.5 150 20001.5 {Tags:[\"wt\"],PersistenceRequired:1b}" (f/summon-command origin [:summon "cow" [1.5 0 1.5]])))
    (is (= "kill $BODY at 20000 150 20000" (f/substitute "kill $BODY at $X $Y $Z" "$BODY" origin)))
    (is (= "forceload remove 20000 20000 20031 20031" (last (f/cleanup-commands grid origin "ProbeFixture" open))))))

(deftest cleanup-leaves-the-body-on-a-clean-plot
  (let [[open glass] (f/file-cases text "hostile")
        origin [20000 150 20000]
        cmds (f/cleanup-commands grid origin "ProbeFixture" open)]
    (is (every? (set cmds) (f/clear-commands grid origin 16 "stone")) "the plot is cleared (huts, beds) so a restarted body finds nothing")
    (is (< (.indexOf cmds "fill 20000 150 20000 20031 166 20031 air") (.indexOf cmds "tp ProbeFixture 20001.5 150 20001.5 0 0")))
    (is (= "tp ProbeFixture 20001.5 150 20001.5 0 0" (some #(when (re-find #"^tp " %) %) cmds)))
    (is (< (.indexOf cmds "tp ProbeFixture 20001.5 150 20001.5 0 0") (.indexOf cmds "forceload remove 20000 20000 20031 20031")))))

(deftest after-checks-read-rcon-replies
  (let [origin [20000 150 20000]]
    (is (= "execute if block 20003 150 20004 air" (f/after-command origin "B" grid {} [:block [3 0 4] "air"])))
    (is (= "execute if entity @e[type=cow,x=20001,y=150,z=20001,dx=4,dy=1,dz=4]"
           (f/after-command origin "B" grid {} [:entities "type=cow" [[1 0 1] [5 1 5]] [:>= 1]])))
    (is (:pass? (f/judge-after origin [:block [3 0 4] "air"] "Test passed")))
    (is (not (:pass? (f/judge-after origin [:block [3 0 4] "air"] "Test failed"))))
    (is (:pass? (f/judge-after origin [:not-block [3 0 4] "air"] "Test failed")))
    (is (:pass? (f/judge-after origin [:item "cobblestone" [:>= 2]] "Test passed. Count: 3")))
    (is (not (:pass? (f/judge-after origin [:item "cobblestone" [:>= 2]] "Test failed"))))
    (is (:pass? (f/judge-after origin [:entities "type=cow" [[1 0 1] [5 1 5]] 1] "Test passed. Count: 1")))
    (is (:pass? (f/judge-after origin [:body-near [16.5 0 16.5] 1] "ProbeFixture has the following entity data: [20016.9d, 150.0d, 20016.5d]")))
    (is (not (:pass? (f/judge-after origin [:body-near [16.5 0 16.5] 1] "ProbeFixture has the following entity data: [20018.9d, 150.0d, 20016.5d]"))))
    (is (:pass? (f/judge-after origin [:body-far [16.5 0 16.5] 25] "ProbeFixture has the following entity data: [20046.9d, 150.0d, 20016.5d]")))
    (is (not (:pass? (f/judge-after origin [:body-far [16.5 0 16.5] 25] "ProbeFixture has the following entity data: [20018.9d, 150.0d, 20016.5d]"))))
    (is (= [-1.5 64 2.25] (f/reply-pos "X has the following entity data: [-1.5d, 64.0d, 2.25d]")))))

(deftest failed-entities-check-names-the-stray-entity
  (let [origin [20000 150 20000]
        check [:entities "tag=!wt,type=!player" [[0 -1 0] [32 6 32]] 0]
        reply "Zombie has the following entity data: [20010.5d, 151.0d, 20007.25d]"]
    (is (= "data get entity @e[tag=!wt,type=!player,x=20000,y=149,z=20000,dx=32,dy=7,dz=32,limit=1] Pos"
           (f/entity-probe-command origin check)))
    (is (nil? (f/entity-probe-command origin [:block [3 0 4] "air"])))
    (is (= "stray entity zombie at plot [10.5 1 7.25]" (f/describe-entity-reply origin reply)))
    (is (= "stray entity: no data in reply: No entity was found" (f/describe-entity-reply origin "No entity was found")))))

(deftest failed-entities-evidence-is-worded-by-check-kind
  (let [origin [20000 150 20000]
        box [[0 -1 0] [32 6 32]]
        judge (fn [want n] (f/judge-after origin [:entities "type=cow" box want] (str "Count: " n)))
        few (judge 2 1)
        over (judge 0 1)]
    (is (= "expected 2 cow, found 1" (:evidence few)))
    (is (not (:stray? few)) "too few: no stray to probe")
    (is (= "expected at least 3 cow, found 1" (:evidence (judge [:>= 3] 1))))
    (is (= "count 1" (:evidence over)))
    (is (:stray? over) "too many: probe names the stray")
    (is (:stray? (judge [:<= 0] 1)))
    (is (not (:stray? (judge 1 1))) "passing")))

(deftest plan-files-carry-the-runner-prefix
  (is (= "{:id \"test-probefixture-pen\", :parts []}" (f/plan-file-text {:id "pen" :parts []} "test-probefixture-"))))

(deftest plan-refs-resolve-to-the-runner-prefix
  (is (= '(jobs.forestry.prepare {:plan "test-probex-wood"})
         (f/resolve-plan-refs '(jobs.forestry.prepare {:plan "$plan:wood"}) "test-probex-")))
  (is (= {:plan "other"} (f/resolve-plan-refs {:plan "other"} "test-probex-"))))

(deftest body-refs-resolve-to-the-running-body
  (is (= {:id "w" :metadata {:by "ProbeX"}} (f/resolve-body-refs {:id "w" :metadata {:by "$body"}} "ProbeX")))
  (is (= {:metadata {:by "Other"}} (f/resolve-body-refs {:metadata {:by "Other"}} "ProbeX")))
  (is (= {:place "camp-wt-probex" :by "x ProbeX"} (f/resolve-body-refs {:place "camp-$tag" :by "x $body"} "ProbeX"))
      "$tag (the lowercase shared tag) and $body inside a string"))

(deftest after-block-checks-floor-fractional-coordinates
  (is (= "execute if block 20001 150 20002 air" (f/after-command [20000 150 20000] "B" {} {} [:block [1.5 0 2.5] "air"])))
  (is (= "execute if block 20011 150 20011 dirt" (f/after-command [20000 150 20000] "B" {} {} [:not-block [11.5 0 11.5] "dirt"]))))

(deftest body-start-plan-gives-each-case-clean-memory
  (is (= :restart-clean (f/body-start-plan {} true)))
  (is (= :restart-clean (f/body-start-plan {} false)))
  (is (= :restart-keep (f/body-start-plan {:keep-memory true} true)))
  (is (= :keep (f/body-start-plan {:keep-memory true} false)))
  (is (= :restart-clean (f/body-start-plan {:keep-memory false} false))))

(deftest reset-plot-clears-a-left-over-plot-before-the-body-starts
  (let [origin [20000 150 20000]
        cmds (f/reset-plot-commands grid origin)]
    (is (= "forceload add 20000 20000 20031 20031" (first cmds)))
    (is (every? (set cmds) (f/clear-commands grid origin 31 "stone")) "the plot is cleared to its full height")
    (is (some #(re-find #"^kill " %) cmds))
    (is (= "forceload remove 20000 20000 20031 20031" (last cmds)))
    (is (not-any? #(re-find #"^tp " %) cmds))))

(deftest clean-start-forgets-memory-and-seen-blocks
  (is (= ["memory.edn" "seen.bin"] f/clean-start-files)))

(deftest register-entries-become-trigger-put-commands
  (is (= [["Probe" "--world" "claude" "put" "night" "--trigger" "night" "--by" "world-test"]]
         (f/register-put-argvs "Probe" "claude" [{:trigger :night}] [0 0 0])))
  (is (= [["Probe" "--world" "claude" "put" "wedged" "--trigger" "wedged" "--persistence" "cooldown"
           "--cooldown-s" "0" "--job" "(jobs.survival.unwedge)" "--args" "{:radius 8}" "--by" "world-test"]]
         (f/register-put-argvs "Probe" "claude"
                               [{:trigger :wedged :persistence :cooldown :cooldown-s 0
                                 :job '(jobs.survival.unwedge) :args {:radius 8}}] [0 0 0])))
  (is (= [] (f/register-put-argvs "Probe" "claude" [] [0 0 0]))))

(deftest register-put-resolves-plot-relative-markers-in-job-args
  (is (= [["Probe" "--world" "claude" "put" "hungry" "--trigger" "hungry" "--job" "(jobs.storage.deposit {:chest [20016 150 20011]})" "--by" "world-test"]]
         (f/register-put-argvs "Probe" "claude" [{:trigger :hungry :job (list 'jobs.storage.deposit {:chest (f/rel [16 0 11])})}] [20000 150 20000]))))

(deftest block-commands-floor-fractional-coordinates
  (is (= "setblock 20001 150 20002 stone" (f/block-command [20000 150 20000] [:set [1.5 0 2.5] "stone"])))
  (is (= "fill 20001 150 20001 20003 152 20003 glass" (f/block-command [20000 150 20000] [:fill [1.5 0 1.5] [3.9 2 3.2] "glass"]))))

(deftest memory-seed-makes-memory-data-from-case-entries
  (let [c (f/resolve-tags (f/read-edn "{:memory [{:kind :food-source :data {:pos #at [3 0 4] :kind :chest}}
                                                {:kind :food-source :data {:pos #at [5 0 4] :kind :chest}}
                                                {:kind :bed :data {:pos #at [1 0 1]} :policy {:cap 1 :ttl :forever}}]}")
                          [100 150 200])
        data (f/memory-seed (:memory c) 5000)]
    (is (= [[103 150 204] [105 150 204]] (mapv #(get-in % [:data :pos]) (get-in data [:entries :food-source]))))
    (is (= [5000 5000] (mapv :t (get-in data [:entries :food-source]))))
    (is (= {:cap 1 :ttl :forever} (get-in data [:policies :bed])))
    (is (= {:cap 50 :ttl 3600000} (get-in data [:policies :food-source])))
    (is (= {:entries {} :policies {}} (f/memory-seed nil 5000)))))

(deftest memory-seed-age-s-backdates-an-entry
  (let [data (f/memory-seed [{:kind :spawn-set :data {} :age-s 7200} {:kind :spawn-set :data {}}] 10000000)]
    (is (= [2800000 10000000] (mapv :t (get-in data [:entries :spawn-set]))))))

(deftest memory-seed-problems
  (let [ps #(f/problems (merge {:name "a" :time :day :plot {:height 6} :body {:at [1 0 1]} :act [] :after [] :expect [{:event {:kind :x} :within-s 1}]} %))]
    (is (empty? (ps {:memory [{:kind :food-source :data {:pos [1 0 1]}}]})))
    (is (empty? (ps {:memory [{:kind :bed :data {} :age-s 7200}]})))
    (is (some #(re-find #":age-s" %) (ps {:memory [{:kind :bed :data {} :age-s "old"}]})))
    (is (some #(re-find #":memory" %) (ps {:memory [{:data {}}]})))
    (is (some #(re-find #":memory" %) (ps {:memory [{:kind :bed :data {}}] :keep-memory true})))))

(deftest clear-hostiles-kills-only-hostile-types-in-the-plot-box
  (let [cmds (f/clear-hostiles-commands grid [100 149 200] {:plot {:height 6}})]
    (is (seq cmds))
    (is (every? #(re-find #"^kill @e\[type=minecraft:[a-z_]+,x=100,y=148,z=200,dx=\d+,dy=7,dz=\d+\]$" %) cmds))
    (is (some #(re-find #"type=minecraft:enderman," %) cmds))
    (is (some #(re-find #"type=minecraft:spider," %) cmds))
    (is (some #(re-find #"type=minecraft:zombie," %) cmds))
    (is (not-any? #(re-find #"distance" %) cmds))
    (is (not-any? #(re-find #"player|type=minecraft:villager|cow|iron_golem|wolf" %) cmds))))

(deftest mobs-keep-is-the-opt-out
  (is (seq (f/clear-hostiles-commands grid [0 0 0] {})))
  (is (empty? (f/clear-hostiles-commands grid [0 0 0] {:mobs :keep}))))

(deftest night-cases-clear-natural-hostiles-around-the-plot-not-summoned-ones
  (let [cmds (f/natural-hostiles-commands grid [100 149 200] {:plot {:height 6}} true)]
    (is (seq cmds))
    (is (every? #(re-find #"^kill @e\[type=minecraft:[a-z_]+,tag=!wt,x=76,y=124,z=176,dx=79,dy=55,dz=79\]$" %) cmds)
        "the plot box widened by 24 each way, sparing what a runner summoned")
    (is (some #(re-find #"type=minecraft:skeleton," %) cmds))
    (is (not-any? #(re-find #"player|type=minecraft:villager|cow|iron_golem|wolf" %) cmds)))
  (is (empty? (f/natural-hostiles-commands grid [0 0 0] {} false)) "by day")
  (is (empty? (f/natural-hostiles-commands grid [0 0 0] {:mobs :keep} true)) ":mobs :keep"))

(deftest mobs-problems
  (let [ps #(f/problems (merge {:name "a" :time :day :plot {:height 6} :body {:at [1 0 1]} :act [] :after [] :expect [{:event {:kind :x} :within-s 1}]} %))]
    (is (empty? (ps {:mobs :keep})))
    (is (some #(re-find #":mobs" %) (ps {:mobs :bogus})))))

(deftest the-start-check-reads-the-body-position-and-judges-it-against-the-start
  (let [origin [20000 150 20000]
        c {:body {:at [16.5 0 16.5]}}]
    (is (= "data get entity ProbeFixture Pos" (f/start-check-command "ProbeFixture")))
    (is (:pass? (f/judge-start origin c "ProbeFixture has the following entity data: [20016.5d, 150.0d, 20016.5d]")))
    (is (:pass? (f/judge-start origin c "ProbeFixture has the following entity data: [20017.2d, 148.0d, 20015.9d]"))
        "a few blocks off is still at the start")
    (let [r (f/judge-start origin c "ProbeFixture has the following entity data: [9.5d, 68.0d, 0.5d]")]
      (is (not (:pass? r)) "world spawn is not the start")
      (is (re-find #"9\.5 68 0\.5" (:why r)) "the message names where the body is")
      (is (re-find #"20016\.5 150 20016\.5" (:why r)) "and where it should be"))
    (is (not (:pass? (f/judge-start origin c "No entity was found")))
        "no position at all is a failure")
    (is (re-find #"No entity" (:why (f/judge-start origin c "No entity was found"))))))

(deftest a-case-with-nothing-to-check-cannot-run
  (let [ps (fn [c] (f/problems (f/merge-case {} c)))]
    (is (some #(re-find #":expect or :after" %) (ps {:name "a"})))
    (is (some #(re-find #":expect or :after" %) (ps {:name "a" :act [[:wait-s 1]] :expect [] :after []})))
    (is (empty? (ps {:name "a" :expect [{:event {:kind :x} :within-s 1}]})))
    (is (empty? (ps {:name "a" :after [[:block [1 0 1] "stone"]]})))))

(deftest an-unrecognised-reply-is-not-a-count
  (is (nil? (f/reply-count "That position is not loaded")))
  (is (nil? (f/reply-count "")))
  (is (= 0 (f/reply-count "Test failed")))
  (is (= 1 (f/reply-count "Test passed")))
  (is (= 3 (f/reply-count "Test passed. Count: 3"))))

(deftest an-unrecognised-reply-fails-every-after-check
  (let [origin [0 0 0]
        bad "Unknown or incomplete command"
        judge (fn [check] (f/judge-after origin check bad))]
    (is (= [false false false false]
           (mapv (comp :pass? judge)
                 [[:block [1 0 1] "stone"] [:not-block [1 0 1] "stone"] [:item "dirt" 0] [:entities "type=cow" [[0 0 0] [1 1 1]] 0]])))
    (is (every? #(re-find #"unrecognised reply" (:evidence %))
                (map judge [[:block [1 0 1] "stone"] [:not-block [1 0 1] "stone"] [:item "dirt" 0]])))))

(deftest rcon-kills-must-be-plot-bounded
  (let [ps (fn [act] (f/problems (f/merge-case {} {:name "a" :act act :after [[:block [1 0 1] "stone"]]})))]
    (is (seq (ps [[:rcon "kill @e[type=zombie,tag=wt,x=$X,y=$Y,z=$Z,distance=..40]"]])) "a radius reaches the next plot")
    (is (seq (ps [[:rcon "execute at $BODY run kill @e[type=zombie,distance=..40]"]])))
    (is (seq (ps [[:rcon-until "kill @e[type=zombie]" {:until {:kind :x} :every-s 1 :limit-s 5}]])))
    (is (empty? (ps [[:rcon "kill @e[type=zombie,$BOX]"]])))
    (is (empty? (ps [[:rcon "kill @e[type=zombie,x=$X,y=$Y,z=$Z,dx=5,dy=3,dz=5]"]])))
    (is (seq (ps [[:rcon "kill @e"]])) "a bare kill")
    (is (seq (ps [[:rcon "tp @e[type=cow,tag=wt,distance=..5] ~ ~ ~3"]])) "tp with a radius")
    (is (seq (ps [[:rcon "execute as @e[type=cow,distance=..9] run effect give @s speed"]])))
    (is (empty? (ps [[:rcon "tp @e[type=cow,tag=wt,$BOX] ~ ~ ~3"]])))
    (is (empty? (ps [[:rcon "execute as $BODY at @s run tp @s ~ ~ ~ 270 0"]])) "@s is the body")
    (is (empty? (ps [[:rcon "execute if entity @e[type=cow,distance=..5] run say hi"]])))))

(deftest substitute-fills-the-plot-box
  (is (= "kill @e[type=zombie,x=20000,y=149,z=20000,dx=31,dy=7,dz=31]"
         (f/substitute "kill @e[type=zombie,$BOX]" "B" [20000 150 20000] (f/box-selector f/default-grid [20000 150 20000] 6)))))

;; ------------------------------------------------------------------ :zones and :places

(def zones-text "[{:name \"hut\" :min [1 2 3] :max [4 5 6] :owner \"Ann\"}\n {:name \"pen\" :min [7 8 9] :max [9 9 9] :owner \"Bob\" :allow #{:dig}}]\n")

(deftest shared-entries-are-tagged-with-the-body
  (is (= "wt-probefixture" (f/shared-tag "ProbeFixture")))
  (is (= [{:name "wt-probefixture-box" :min [20001 150 20002] :max [20003 152 20004] :owner "Other" :allow #{:dig}}
          {:name "wt-probefixture-b2" :min [1 2 3] :max [4 5 6] :owner "Zed"}]
         (f/zone-entries "ProbeFixture"
                         [{:name "box" :min [20001 150 20002] :max [20003 152 20004] :allow #{:dig}}
                          {:name "b2" :min [1 2 3] :max [4 5 6] :owner "Zed"}])))
  (is (= [{:name "home" :kind "base" :x 20001 :y 150 :z 20002 :by "wt-probefixture" :note "n"}
          {:name "mine" :kind "place" :x 1 :y 2 :z 3 :by "wt-probefixture"}]
         (f/marker-entries "ProbeFixture"
                           [{:name "home" :kind "base" :pos [20001 150 20002] :note "n"}
                            {:name "mine" :pos [1 2 3]}]))))

(deftest zones-text-gains-and-loses-the-case-zones-and-keeps-the-rest
  (let [mine (f/zone-entries "B" [{:name "box" :min [1 1 1] :max [2 2 2]}])
        with (f/zones-with zones-text mine)]
    (is (= ["hut" "pen" "wt-b-box"] (map :name (cljs.reader/read-string with))))
    (is (= zones-text (f/zones-without with "wt-b")))
    (is (= zones-text (f/zones-without zones-text "wt-b")))
    (is (= ["wt-b-box"] (map :name (cljs.reader/read-string (f/zones-with "[]" mine)))))
    (is (= ["wt-b-box"] (map :name (cljs.reader/read-string (f/zones-with "" mine)))))
    (is (= ["hut"] (map :name (cljs.reader/read-string (f/zones-without (f/zones-with "[{:name \"hut\" :min [1 2 3] :max [4 5 6] :owner \"Ann\"}]" mine) "wt-b")))))
    (is (thrown? js/Error (f/zones-with "[{:name" mine)))))

(deftest markers-text-gains-and-loses-the-case-markers-and-keeps-the-rest
  (let [text "[\n {\n  \"name\": \"hut\",\n  \"kind\": \"base\",\n  \"x\": 1,\n  \"y\": 2,\n  \"z\": 3,\n  \"by\": \"Ann\"\n }\n]\n"
        mine (f/marker-entries "B" [{:name "home" :pos [5 6 7]}])
        with (f/markers-with text mine)]
    (is (= ["hut" "home"] (map #(aget % "name") (js/JSON.parse with))))
    (is (= text (f/markers-without with "wt-b")))
    (is (= ["home"] (map #(aget % "name") (js/JSON.parse (f/markers-with "" mine)))))
    (is (= "" (f/markers-without "" "wt-b")))))

(deftest zones-and-places-problems
  (let [ps (fn [c] (f/problems (merge {:name "x" :time :day :plot {:height 16} :body {:at [0 0 0]} :expect [{:event {} :within-s 1}]} c)))]
    (is (empty? (ps {:zones [{:name "z" :min [0 0 0] :max [1 1 1]}] :places [{:name "p" :pos [0 0 0]}]})))
    (is (some #(re-find #":zones" %) (ps {:zones [{:name "z" :min [0 0] :max [1 1 1]}]})))
    (is (some #(re-find #":places" %) (ps {:places [{:pos [0 0 0]}]})))
    (is (some #(re-find #":places" %) (ps {:places [{:name "p" :pos [0 0]}]})))))

(deftest cli-and-http-steps-become-tool-argv
  (let [argv #(f/step-argv "B" "claude" % "j7")]
    (is (= ["engine/tools/plans.mjs" "check" "--body" "B" "--world" "claude"]
           (argv [:cli "plans" ["check" "--body" "$body" "--world" "$world"]])))
    (is (= ["engine/tools/drive.mjs" "B" "take" "--world" "claude" "--who" "wt" "--why" "t"]
           (argv [:http :take {:who "wt" :why "t"}])))
    (is (= ["engine/tools/drive.mjs" "B" "release" "--world" "claude" "--who" "wt"] (argv [:http :release {:who "wt"}])))
    (is (= ["engine/tools/jobs.mjs" "B" "--world" "claude" "cancel" "j7"] (argv [:http :cancel "$job"])))
    (is (= ["engine/tools/jobs.mjs" "B" "--world" "claude" "cancel" "j3"]
           (f/step-argv "B" "claude" [:http :cancel "$event-job"] "j7" "j3"))
        "$event-job is the job of the last awaited event (a reflex's job has no submit)")
    (is (= ["engine/tools/plans.mjs" "check" "test-bob-w" "--body" "Bob"]
           (f/step-argv "Bob" "claude" [:cli "plans" ["check" "$plan:w" "--body" "$body"]] nil nil))
        "$plan:<id> is the id the runner gives a case's plan (test-<lowercase body>-<id>)")
    (is (= ["engine/tools/map.mjs" "add" "marker" "hut-wt-bob" "--data" "{:kind \"base\", :x 3, :y 0, :z 4}"]
           (f/step-argv "Bob" "claude" [:cli "map" ["add" "marker" "hut-$tag" "--data" {:kind "base" :at [3 0 4]}]] nil nil))
        "$tag is the body's shared tag; a map argument is printed as EDN, its :at (resolved) becoming :x :y :z")
    (is (= ["engine/tools/jobs.mjs" "B" "--world" "claude" "submit" "(jobs.x {:a 1})" "--next"]
           (argv [:http :submit '(jobs.x {:a 1}) [:next]])))))

(deftest cli-steps-take-string-map-and-position-arguments
  (let [step-problem? f/step-problem?]
    (is (not (step-problem? [:cli "map" ["add" {:at [1 0 2]}]])))
    (is (not (step-problem? [:cli "world" ["b" "submit" "dig" [1 0 2]]])))
    (is (step-problem? [:cli "map" ["add" 5]]) "only strings, maps and positions")
    (is (= ["engine/tools/world.mjs" "B" "submit" "dig" "7" "0" "9"]
           (f/step-argv "B" "claude" [:cli "world" ["$body" "submit" "dig" [7 0 9]]] nil nil))
        "a position argument is spread into x y z")))

(deftest a-tool-answer-is-judged-by-its-edn-and-exit-code
  (is (:pass? (f/judge-reply {:ok true} 0 "{:ok true :id \"j1\"}")))
  (is (:pass? (f/judge-reply {:ok false :reason :job-not-found} 1 "{:ok false :reason :job-not-found}")) "a refusal is matched, not an error")
  (is (not (:pass? (f/judge-reply {:ok true} 1 "{:ok false}"))))
  (is (not (:pass? (f/judge-reply {:ok true} 0 "not edn {"))))
  (is (:pass? (f/judge-reply nil 0 "anything")) "without a pattern the exit code decides")
  (is (not (:pass? (f/judge-reply nil 1 "boom")))))

(def real-memory-text
  "A memory.edn as engine.memory/write! stores two deaths ({:t :wt :data} entries under :entries, kinds under :policies)."
  (pr-str {:entries {:deaths [{:t 1 :wt 100 :data {:pos [1 64 2] :dimension "overworld" :cause :lava}}
                              {:t 2 :wt 200 :data {:pos [3 64 4] :dimension "overworld" :cause :fall}}]}
           :policies {:deaths {:cap 20 :ttl :forever}}}))

(deftest file-and-memory-after-checks-read-the-body-files
  (is (= "engine/memory.edn" (f/after-file [:memory {:deaths [:any]}])))
  (is (= "engine/x.edn" (f/after-file [:file "engine/x.edn" {}])))
  (is (nil? (f/after-file [:block [0 0 0] "air"])))
  (is (:pass? (f/judge-file-after [:memory {:entries {:deaths [{:data {:dimension "overworld"}} {:data {:pos [:any]}}]}}]
                                  real-memory-text)))
  (is (not (:pass? (f/judge-file-after [:memory {:entries {:deaths [{} {} {}]}}] real-memory-text))))
  (is (not (:pass? (f/judge-file-after [:memory {:deaths [{:pos [:any]}]}] real-memory-text))) "the old top-level :deaths shape never matches")
  (is (not (:pass? (f/judge-file-after [:memory {:deaths [:any]}] nil))) "a missing file fails with a reason")
  (is (not (:pass? (f/judge-file-after [:file "a" {}] "{:bad")))))

(deftest cli-http-restart-steps-and-file-checks-validate
  (let [ps #(f/problems (merge {:name "x" :time :day :plot {:height 16} :body {:at [0 0 0]} :expect [{:event {} :within-s 1}]} %))]
    (is (empty? (ps {:act [[:restart-body] [:cli "plans" ["check"] {:ok true}] [:http :take {:who "w"}] [:http :cancel "j1" {:ok true}]]
                     :after [[:memory {:deaths [:any]}] [:file "engine/a.edn" {}]]})))
    (is (some #(re-find #":act steps" %) (ps {:act [[:http :bogus]]})))
    (is (some #(re-find #":act steps" %) (ps {:act [[:cli "no/such" []]]})))
    (is (some #(re-find #":act steps" %) (ps {:act [[:cli "plans" "check"]]})))
    (is (some #(re-find #":after" %) (ps {:after [[:file 7 {}]]})))))

(deftest submitted-job-id-is-read-from-jobs-submit-output
  (is (= "j12" (f/submitted-id "{:ok true :job {:id \"j12\" :status :queued}}")))
  (is (nil? (f/submitted-id "{:ok false :error \"refused\"}"))))

(deftest tool-step-argv-gap-names-an-unfilled-placeholder
  (is (nil? (f/argv-gap ["engine/tools/jobs.mjs" "B" "cancel" "j1"])))
  (is (some? (f/argv-gap (f/step-argv "B" "claude" [:http :cancel "$job"] nil)))))

(deftest argv-gap-names-which-placeholder-is-unfilled
  (let [step [:cli "jobs" ["cancel" "$event-job"]]
        gap (fn [step last-job event-job] (f/argv-gap (f/step-argv "B" "w" step last-job event-job) step last-job event-job))]
    (is (re-find #"\$event-job" (gap step "j1" nil)))
    (is (re-find #"\$job" (gap [:cli "jobs" ["cancel" "$job"]] nil "j2")))
    (is (nil? (gap step "j1" "j3")))))

(deftest shared-files-are-cleaned-after-a-case-that-writes-them
  (is (f/writes-shared? {:zones [{:name "z"}]}))
  (is (f/writes-shared? {:places [{:name "p"}]}))
  (is (f/writes-shared? {:act [[:cli "map" ["add" "marker" "m"]]]}))
  (is (not (f/writes-shared? {:act [[:cli "jobs" ["list"]] [:job {}]]})))
  (is (not (f/writes-shared? {}))))

(deftest a-pattern-names-the-submitted-job-by-its-placeholder
  (is (= {:items [:has {:id "j7" :status :queued}]}
         (f/fill-pattern {:items [:has {:id "$job" :status :queued}]} "j7" "j9")))
  (is (= {:job "j9"} (f/fill-pattern {:job "$event-job"} "j7" "j9")))
  (is (= {:total 0} (f/fill-pattern {:total 0} nil nil))))

(deftest until-step-polls-a-file-or-tool-check
  (let [ps #(f/step-problem? %)]
    (is (not (ps [:until [:memory {:entries {}}] 20])))
    (is (not (ps [:until [:cli "jobs" [] {:items [{:status :queued}]}] 20])))
    (is (ps [:until [:block [0 0 0] "air"] 20]) "only checks the runner can re-run")
    (is (ps [:until [:memory {}]]) "a bound is required")))
