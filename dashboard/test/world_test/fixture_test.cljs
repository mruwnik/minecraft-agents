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

(deftest a-plot-must-fit-its-lane
  (let [ps #(f/problems (merge {:name "a" :time :day :body {:at [1 0 1]} :act [] :after [] :expect []} %))]
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
    (is (= 3 (get-in glass [:body :settle-s])) "the global defaults fill the rest")
    (is (= :day (:time open)))
    (is (nil? (:problems open)))
    (is (some #(re-find #":name" %) (:problems bad)))))

(deftest a-single-case-file-is-one-case
  (is (= ["one/x"] (map :id (f/file-cases "{:name \"x\"}" "one")))))

(deftest problems-name-what-cannot-run
  (let [ps (fn [c] (f/problems (f/merge-case {} c)))]
    (is (empty? (ps {:name "a"})))
    (is (seq (ps {:name "a" :time :dusk})))
    (is (seq (ps {:name "a" :act [[:teleport]]})))
    (is (seq (ps {:name "a" :expect [{:event {:kind :x}}]})) "an :event needs :within-s")
    (is (seq (ps {:name "a" :expect [{:no-event {:kind :x}}]})) "a :no-event needs :for-s")
    (is (seq (ps {:name "a" :plot {:height 40}})))))

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
    (is (= [-1.5 64 2.25] (f/reply-pos "X has the following entity data: [-1.5d, 64.0d, 2.25d]")))))

(deftest plan-files-carry-the-runner-prefix
  (is (= "{:id \"test-probefixture-pen\", :parts []}" (f/plan-file-text {:id "pen" :parts []} "test-probefixture-"))))

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
         (f/register-put-argvs "Probe" "claude" [{:trigger :night}])))
  (is (= [["Probe" "--world" "claude" "put" "wedged" "--trigger" "wedged" "--persistence" "cooldown"
           "--cooldown-s" "0" "--job" "(jobs.survival.unwedge)" "--args" "{:radius 8}" "--by" "world-test"]]
         (f/register-put-argvs "Probe" "claude"
                               [{:trigger :wedged :persistence :cooldown :cooldown-s 0
                                 :job '(jobs.survival.unwedge) :args {:radius 8}}])))
  (is (= [] (f/register-put-argvs "Probe" "claude" []))))
