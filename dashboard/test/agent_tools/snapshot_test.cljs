(ns agent-tools.snapshot-test
  (:require [cljs.test :refer [deftest is are async]]
            [clojure.string :as str]
            [agent-tools.snapshot :as snap]
            ["node:fs" :as fs]
            ["node:os" :as os]
            ["node:path" :as path]))

(def pi js/Math.PI)
(defn close? [a b] (< (js/Math.abs (- a b)) 1e-9))
(defn same-angle? [a b] (and (close? (js/Math.sin a) (js/Math.sin b)) (close? (js/Math.cos a) (js/Math.cos b))))

(defn world-fixture []
  (let [dir (.mkdtempSync fs (.join path (.tmpdir os) "snapshot-"))
        worlds (.join path dir "worlds")
        view (.join path worlds "w" "agents" "B" "view")]
    (.mkdirSync fs view #js {:recursive true})
    (.writeFileSync fs (.join path worlds "w" "world.json") "{}")
    {:dir dir :worlds worlds :view view :workspace (.join path dir "ws")}))

(defn pose [overrides]
  (merge {:world "w" :status "online" :t 1000 :mcVersion "26.1" :dimension "overworld"
          :eye {:x 0.5 :y 65.62 :z 0.5} :yaw 0 :pitch 0 :entities [] :timeOfDay 6000}
         overrides))

(defn write-pose! [{:keys [view]} p]
  (.writeFileSync fs (.join path view "pose.json") (js/JSON.stringify (clj->js p))))

(def base ["B" "--world" "w"])

(deftest options-defaults-and-bounds
  (let [{:keys [worlds workspace]} (world-fixture)
        argv (into base ["--worlds" worlds])
        o (snap/options (into argv ["--workspace" workspace]))]
    (is (= ["B" 640 360 64 workspace] [(:body o) (:width o) (:height o) (:max-dist o) (:workspace o)]))
    (is (nil? (:yaw o)))
    (is (= (.join path worlds "w" "agents" "B") (:workspace (snap/options argv))) "standalone: the body folder")
    (is (= [800 450 -30 90] ((juxt :width :height :pitch :yaw)
                             (snap/options (into argv ["--width" "800" "--height" "450" "--pitch" "-30" "--yaw" "90"])))))
    (is (= [1 2 -3] (:look-at (snap/options (into argv ["--look-at" "1,2,-3"])))))
    (is (= [-1 2 3] (:look-at (snap/options (into argv ["--look-at" "-1,2,3"])))))
    (is (= [-1526 54 -1508] (:look-at (snap/options (into argv ["--look-at" "-1526" "54" "-1508"])))) "three separate numbers")
    (are [extra] (= "invalid-option" (.-reason (try (snap/options (into argv extra)) nil (catch :default e e))))
      ["--width" "0"] ["--width" "1921"] ["--height" "abc"] ["--width" "12.5"]
      ["--pitch" "91"] ["--yaw" "x"] ["--max-dist" "200"]
      ["--look-at" "1,2"] ["--look-at" "1,2,x"] ["--look-at" "1,2,3" "--yaw" "10"]
      ["extra-positional"])
    (is (= "invalid-option" (.-reason (try (snap/options ["--world" "w" "--worlds" worlds]) nil (catch :default e e))))
        "a body is required")))

(deftest look-angles-follow-the-mineflayer-convention
  (let [eye {:x 0 :y 10 :z 0}]
    (are [target yaw pitch] (let [a (snap/look-angles eye target)]
                              (and (same-angle? yaw (:yaw a)) (close? pitch (:pitch a))))
      [0 10 -5] 0 0                  ; north is -z
      [-5 10 0] (/ pi 2) 0           ; west is -x
      [0 10 5] pi 0                  ; south
      [0 15 -5] 0 (/ pi 4))))        ; north, 45 degrees up

(deftest compass-names-eight-directions
  (are [deg name] (= name (snap/compass (* deg (/ pi 180))))
    0 "north" 45 "north-west" 90 "west" 180 "south" 270 "east" -90 "east" 359 "north" 405 "north-west"))

(deftest camera-overrides-only-what-is-given
  (let [p (pose {:yaw 1.0 :pitch 0.2})]
    (is (= {:yaw 1.0 :pitch 0.2} (snap/camera p {})))
    (is (close? (/ pi 2) (:yaw (snap/camera p {:yaw 90}))))
    (is (= 0.2 (:pitch (snap/camera p {:yaw 90}))))
    (is (close? (/ pi -6) (:pitch (snap/camera p {:pitch -30}))))
    (is (same-angle? 0 (:yaw (snap/camera (pose {}) {:look-at [0 65 -5]}))))))

(deftest pose-problem-requires-a-fresh-online-pose-of-this-world
  (are [p reason] (= reason (:reason (snap/pose-problem p "w" 5000)))
    (pose {}) nil
    (pose {:t (- 5000 snap/stale-ms)}) nil
    (pose {:t (- 4999 snap/stale-ms)}) :body-offline
    (pose {:status "offline"}) :body-offline
    nil :no-pose
    (pose {:world "other"}) :no-pose
    (pose {:eye nil}) :no-pose
    (pose {:yaw "x"}) :no-pose))

(deftest file-names-sort-by-time-and-prune-keeps-the-newest
  (is (= "snap-2026-10-05T00-42-07-123Z.png" (snap/file-name (js/Date.parse "2026-10-05T00:42:07.123Z"))))
  (is (neg? (compare (snap/file-name 1000) (snap/file-name 2000))))
  (let [names (mapv snap/file-name (range 1000 26000 1000))]
    (is (= (take 5 names) (snap/prune (shuffle (conj names "notes.txt" "snap-other.png")) 20)))
    (is (empty? (snap/prune (take 3 names) 20)))))

(deftest summary-names-the-crosshair-and-what-is-in-view
  (let [p (pose {:eye {:x 63.5 :y 19.62 :z 24.5}})
        s (snap/summary {:pose p :camera {:yaw (/ pi 2) :pitch 0} :width 300 :height 100
                         :center {:name "stone" :x 60 :y 19 :z 24 :t 3.04 :face "east"}
                         :seen [{:name "zombie" :kind "hostile" :px 20 :py 50 :dist 9}
                                {:name "cow" :kind "passive" :px 150 :py 50 :dist 4}
                                {:name "Steve" :kind "player" :px 290 :py 50 :dist 12}]})]
    (is (= {:block "stone" :at [60 19 24] :face "east" :distance 3.0} (:crosshair s)))
    (is (= {:yaw 90 :pitch 0 :compass "west"} (:facing s)))
    (is (= [["cow" "centre" 4] ["zombie" "left" 9] ["Steve" "right" 12]]
           (map (juxt :name :side :distance) (:entities s))))
    (is (str/includes? (:text s) "Facing west"))
    (is (str/includes? (:text s) "Crosshair: stone at 60 19 24, 3.0 blocks"))
    (is (str/includes? (:text s) "cow 4 (centre)")))
  (let [s (snap/summary {:pose (pose {}) :camera {:yaw 0 :pitch 0} :width 10 :height 10 :center nil :seen []})]
    (is (nil? (:crosshair s)))
    (is (str/includes? (:text s) "Crosshair: nothing within reach of the loaded world"))
    (is (str/includes? (:text s) "No mobs or players in view")))
  (is (= 8 (count (:entities (snap/summary {:pose (pose {}) :camera {:yaw 0 :pitch 0} :width 10 :height 10
                                            :seen (map (fn [i] {:name "bat" :px 5 :py 5 :dist i}) (range 20))}))))))

(deftest summary-reports-the-light-of-the-crosshair-and-of-each-mob
  (let [light {:sky 4 :block 0 :seeing 0.2}
        s (snap/summary {:pose (pose {}) :camera {:yaw 0 :pitch 0} :width 10 :height 10
                         :center {:name "stone" :x 0 :y 64 :z -3 :t 3 :face "south" :light light}
                         :seen [{:name "zombie" :px 5 :py 5 :dist 3 :light light}]})]
    (is (= light (get-in s [:crosshair :light])))
    (is (= light (:light (first (:entities s)))))))

(deftest perceived-splits-the-entity-cache-into-seen-and-heard
  (let [rows [{:type "zombie" :id 7 :sense :seen :pos {:x 1 :y 64 :z 2}}
              {:type "player" :id 8 :sense :seen :username "Ann" :pos {:x 3 :y 64 :z 4}}
              {:type "creeper" :id 9 :sense :remembered :pos {:x 5 :y 64 :z 6} :age-ms 5000}
              {:type "skeleton" :sense :heard :direction :north-east :band :near}
              {:type "wolf" :sense :self :pos {:x 0 :y 64 :z 0} :self? true}]
        {:keys [entities heard]} (snap/perceived rows)]
    (is (= [["zombie" 1 2] ["player" 3 4]] (map (fn [e] [(:name e) (get-in e [:pos :x]) (get-in e [:pos :z])]) entities)))
    (is (= "Ann" (:username (second entities))))
    (is (= [{:name "skeleton" :direction :north-east :band :near}] heard))))

(deftest perceived-marks-hostile-mobs-and-drops-the-body
  (let [rows [{:type "zombie" :id 1 :sense :seen :pos {:x 1 :y 64 :z 2}}
              {:type "cow" :id 2 :sense :seen :pos {:x 3 :y 64 :z 4}}
              {:type "player" :id 3 :sense :seen :username "Ann" :pos {:x 5 :y 64 :z 6}}
              {:type "wolf" :id 4 :sense :seen :self? true :pos {:x 0 :y 64 :z 0}}]
        {:keys [entities]} (snap/perceived rows "26.1")]
    (is (= [["zombie" "hostile"] ["cow" nil] ["player" "player"]] (map (juxt :name :type) entities)))))

(deftest perceived-hostility-follows-the-bodys-version
  (let [rows [{:type "warden" :id 1 :sense :seen :pos {:x 1 :y 64 :z 2}}]]
    (is (= ["hostile"] (map :type (:entities (snap/perceived rows "26.1")))))
    (is (= [nil] (map :type (:entities (snap/perceived rows "1.16.5")))))))

(defn capture-stdout [f]
  (let [out (atom "") write (.-write (.-stdout js/process))]
    (set! (.-write (.-stdout js/process)) (fn [s] (swap! out str s) true))
    (-> (f)
        (.finally (fn [] (set! (.-write (.-stdout js/process)) write)))
        (.then (fn [code] [code @out])))))

(defn fake-render [calls]
  (fn [opts]
    (swap! calls conj (js->clj opts :keywordize-keys true))
    #js {:png (js/Buffer.from "PNGDATA") :ms 12 :columns 9
         :center #js {:name "dirt" :x 0 :y 64 :z -3 :t 3.5 :face "south"} :seen #js []}))

(deftest execute-draws-the-seen-set-not-the-raw-pose-entities
  (async done
    (let [{:keys [worlds workspace] :as f} (world-fixture) calls (atom [])
          raw {:id 1 :name "zombie" :type "hostile" :pos {:x 0 :y 64 :z -5}}
          rows [{:type "cow" :id 2 :sense :seen :pos {:x 1 :y 64 :z -4}}
                {:type "zombie" :sense :heard :direction :south :band :far}]]
      (write-pose! f (pose {:t (js/Date.now) :entities [raw]}))
      (-> (snap/execute! (snap/options (into base ["--worlds" worlds "--workspace" workspace]))
                         (fake-render calls) (js/Date.now)
                         (fn [] (js/Promise.resolve {:ok true :entities rows})))
          (.then (fn [out]
                   (is (= ["cow"] (map :name (get-in (first @calls) [:pose :entities]))))
                   (is (= [{:name "zombie" :direction "south" :band "far"}] (map #(update % :direction name) (map #(update % :band name) (:heard out)))))
                   (is (str/includes? (:text out) "Heard: zombie"))))
          (.finally done)))))

(deftest execute-draws-no-entities-when-the-body-cannot-be-asked
  (async done
    (let [{:keys [worlds workspace] :as f} (world-fixture) calls (atom [])]
      (write-pose! f (pose {:t (js/Date.now) :entities [{:id 1 :name "zombie" :pos {:x 0 :y 64 :z -5}}]}))
      (-> (snap/execute! (snap/options (into base ["--worlds" worlds "--workspace" workspace]))
                         (fake-render calls) (js/Date.now)
                         (fn [] (js/Promise.reject (js/Error. "no socket"))))
          (.then (fn [out]
                   (is (empty? (get-in (first @calls) [:pose :entities])))
                   (is (= "no socket" (:entities-error out)))))
          (.finally done)))))

(deftest main-writes-the-png-into-the-workspace-and-prints-a-relative-path
  (async done
    (let [{:keys [worlds workspace] :as f} (world-fixture) calls (atom [])]
      (write-pose! f (pose {:t (js/Date.now)}))
      (-> (capture-stdout #(snap/main! (into base ["--worlds" worlds "--workspace" workspace "--yaw" "90" "--width" "320" "--height" "180"])
                                        (fake-render calls)))
          (.then (fn [[code out]]
                   (let [png (second (re-find #":png \"([^\"]+)\"" out))
                         call (first @calls)]
                     (is (= 0 code) out)
                     (is (re-matches #"snapshots/snap-.*\.png" png))
                     (is (= "PNGDATA" (str (.readFileSync fs (.join path workspace png)))))
                     (is (str/includes? out "Crosshair: dirt at 0 64 -3"))
                     (is (= [320 180 64] ((juxt :width :height :maxDist) call)))
                     (is (close? (/ pi 2) (get-in call [:override :yaw])))
                     (is (= "B" (:agentName call)))
                     (is (= (.join path worlds) (get-in call [:stateDir :worldsDir])))
                     (is (= 0.5 (get-in call [:pose :eye :x])) "the checked pose is the one rendered"))))
          (.finally done)))))

(deftest main-keeps-the-snapshot-folder-bounded
  (async done
    (let [{:keys [worlds workspace] :as f} (world-fixture)
          dir (.join path workspace "snapshots")]
      (.mkdirSync fs dir #js {:recursive true})
      (doseq [i (range 25)] (.writeFileSync fs (.join path dir (snap/file-name (+ 1000 i))) "old"))
      (.writeFileSync fs (.join path dir "keep.txt") "mine")
      (write-pose! f (pose {:t (js/Date.now)}))
      (-> (capture-stdout #(snap/main! (into base ["--worlds" worlds "--workspace" workspace]) (fake-render (atom []))))
          (.then (fn [[code _]]
                   (let [files (set (array-seq (.readdirSync fs dir)))]
                     (is (= 0 code))
                     (is (= (inc snap/keep-count) (count files)))
                     (is (contains? files "keep.txt"))
                     (is (not (contains? files (snap/file-name 1000)))))))
          (.finally done)))))

(deftest main-refuses-a-symlinked-snapshots-folder-and-leaves-its-target-alone
  (async done
    (let [{:keys [worlds workspace] :as f} (world-fixture)
          target (.mkdtempSync fs (.join path (.tmpdir os) "snap-target-"))
          victim (.join path target (snap/file-name 1000))
          calls (atom [])]
      (.writeFileSync fs victim "old")
      (.mkdirSync fs workspace #js {:recursive true})
      (.symlinkSync fs target (.join path workspace "snapshots"))
      (write-pose! f (pose {:t (js/Date.now)}))
      (-> (capture-stdout #(snap/main! (into base ["--worlds" worlds "--workspace" workspace]) (fake-render calls)))
          (.then (fn [[code out]]
                   (is (= 1 code) out)
                   (is (str/includes? out ":reason :unsafe-snapshots-dir"))
                   (is (= ["old"] (map #(str (.readFileSync fs (.join path target %))) (array-seq (.readdirSync fs target)))))))
          (.finally (fn [] (.rmSync fs target #js {:recursive true :force true}) (done)))))))

(defn- swapping-render
  "A fake render that runs `swap!` (the folder swap) while the picture is being drawn."
  [swap!]
  (fn [_] (swap!) #js {:png (js/Buffer.from "PNGDATA") :ms 1 :columns 1 :center nil :seen #js []}))

(deftest main-refuses-a-snapshots-folder-swapped-for-a-link-during-the-render
  (async done
    (let [{:keys [worlds workspace] :as f} (world-fixture)
          target (.mkdtempSync fs (.join path (.tmpdir os) "snap-target-"))
          dir (.join path workspace "snapshots")]
      (.mkdirSync fs dir #js {:recursive true})
      (write-pose! f (pose {:t (js/Date.now)}))
      (-> (capture-stdout #(snap/main! (into base ["--worlds" worlds "--workspace" workspace])
                                        (swapping-render (fn [] (.rmSync fs dir #js {:recursive true}) (.symlinkSync fs target dir)))))
          (.then (fn [[code out]]
                   (is (= 1 code) out)
                   (is (str/includes? out ":reason :unsafe-snapshots-dir"))
                   (is (empty? (array-seq (.readdirSync fs target))))))
          (.finally (fn [] (.rmSync fs target #js {:recursive true :force true}) (done)))))))

(deftest main-refuses-a-workspace-swapped-for-a-link-during-the-render
  (async done
    (let [{:keys [worlds workspace] :as f} (world-fixture)
          target (.mkdtempSync fs (.join path (.tmpdir os) "snap-target-"))]
      (.mkdirSync fs workspace #js {:recursive true})
      (write-pose! f (pose {:t (js/Date.now)}))
      (-> (capture-stdout #(snap/main! (into base ["--worlds" worlds "--workspace" workspace])
                                        (swapping-render (fn [] (.rmSync fs workspace #js {:recursive true}) (.symlinkSync fs target workspace)))))
          (.then (fn [[code out]]
                   (is (= 1 code) out)
                   (is (str/includes? out ":reason :unsafe-snapshots-dir"))
                   (is (empty? (array-seq (.readdirSync fs target))))))
          (.finally (fn [] (.rmSync fs target #js {:recursive true :force true}) (done)))))))

(deftest main-refuses-an-ancestor-swapped-for-a-link-during-the-render
  (async done
    (let [{:keys [worlds dir] :as f} (world-fixture)
          parent (.join path dir "parent")
          workspace (.join path parent "ws")
          moved (.join path dir "moved")
          target (.mkdtempSync fs (.join path (.tmpdir os) "snap-target-"))]
      (.mkdirSync fs workspace #js {:recursive true})
      (write-pose! f (pose {:t (js/Date.now)}))
      (-> (capture-stdout #(snap/main! (into base ["--worlds" worlds "--workspace" workspace])
                                        (swapping-render (fn [] (.renameSync fs parent moved) (.symlinkSync fs target parent)))))
          (.then (fn [[code out]]
                   (is (= 1 code) out)
                   (is (str/includes? out ":reason :unsafe-snapshots-dir"))
                   (is (empty? (array-seq (.readdirSync fs target))))))
          (.finally (fn [] (.rmSync fs target #js {:recursive true :force true}) (done)))))))

(deftest main-refuses-a-symlinked-workspace
  (async done
    (let [{:keys [worlds workspace] :as f} (world-fixture)
          target (.mkdtempSync fs (.join path (.tmpdir os) "snap-target-"))
          calls (atom [])]
      (.symlinkSync fs target workspace)
      (write-pose! f (pose {:t (js/Date.now)}))
      (-> (capture-stdout #(snap/main! (into base ["--worlds" worlds "--workspace" workspace]) (fake-render calls)))
          (.then (fn [[code out]]
                   (is (= 1 code) out)
                   (is (empty? (array-seq (.readdirSync fs target))))))
          (.finally (fn [] (.rmSync fs target #js {:recursive true :force true}) (done)))))))

(deftest main-writes-through-a-symlinked-ancestor-that-was-there-from-the-start
  (async done
    (let [{:keys [worlds dir] :as f} (world-fixture)
          real (.join path dir "real")
          link (.join path dir "link")
          workspace (.join path link "ws")]
      (.mkdirSync fs real #js {:recursive true})
      (.symlinkSync fs real link)
      (write-pose! f (pose {:t (js/Date.now)}))
      (-> (capture-stdout #(snap/main! (into base ["--worlds" worlds "--workspace" workspace]) (fake-render (atom []))))
          (.then (fn [[code out]]
                   (is (= 0 code) out)
                   (is (= 1 (count (array-seq (.readdirSync fs (.join path real "ws" "snapshots"))))))))
          (.finally done)))))

(deftest main-refuses-an-offline-body-and-bad-arguments-without-rendering
  (async done
    (let [{:keys [worlds workspace] :as f} (world-fixture) calls (atom [])]
      (write-pose! f (pose {:status "offline" :t (js/Date.now)}))
      (-> (capture-stdout #(snap/main! (into base ["--worlds" worlds "--workspace" workspace]) (fake-render calls)))
          (.then (fn [[code out]]
                   (is (= 1 code))
                   (is (str/includes? out ":reason :body-offline"))
                   (capture-stdout #(snap/main! (into base ["--worlds" worlds "--width" "0"]) (fake-render calls)))))
          (.then (fn [[code out]]
                   (is (= 2 code))
                   (is (str/includes? out ":reason :invalid-option"))
                   (is (empty? @calls))
                   (is (not (.existsSync fs (.join path workspace "snapshots"))))))
          (.finally done)))))

(deftest read-pose-ignores-a-pose-file-over-one-megabyte
  (let [{:keys [dir worlds view]} (world-fixture)
        ctx {:world-dir (.join path worlds "w")}
        write! #(.writeFileSync fs (.join path view "pose.json") %)
        small (do (write! "{\"world\":\"w\"}") (snap/read-pose ctx "B"))
        large (do (write! (str "{\"pad\":\"" (apply str (repeat 1048577 "x")) "\"}")) (snap/read-pose ctx "B"))]
    (.rmSync fs dir #js {:recursive true :force true})
    (is (= {:world "w"} small))
    (is (nil? large))))

(deftest a-file-or-link-already-at-the-png-name-is-never-overwritten
  (async done
    (let [{:keys [worlds workspace] :as f} (world-fixture)
          dir (.join path workspace "snapshots")
          name (snap/file-name 1500)
          other (.join path workspace "other.png")
          opts (snap/options (into base ["--worlds" worlds "--workspace" workspace]))]
      (.mkdirSync fs dir #js {:recursive true})
      (.writeFileSync fs other "mine")
      (.writeFileSync fs (.join path dir name) "first")
      (write-pose! f (pose {:t 1000}))
      (-> (snap/execute! opts (fake-render (atom [])) 1500)
          (.then (fn [_] (is false "a second snapshot in the same millisecond must fail")) (fn [e] (is (= "EEXIST" (.-code e)))))
          (.then (fn [_]
                   (is (= "first" (str (.readFileSync fs (.join path dir name)))))
                   (.rmSync fs (.join path dir name))
                   (.symlinkSync fs other (.join path dir name))
                   (snap/execute! opts (fake-render (atom [])) 1500)))
          (.then (fn [_] (is false "a link at the png name must fail")) (fn [e] (is (= "EEXIST" (.-code e)))))
          (.then (fn [_] (is (= "mine" (str (.readFileSync fs other))) "the link target is untouched")))
          (.finally done)))))

(deftest main-fails-with-code-2-when-prune-meets-a-folder-at-an-old-png-name
  (async done
    (let [{:keys [worlds workspace] :as f} (world-fixture)
          dir (.join path workspace "snapshots")]
      (.mkdirSync fs dir #js {:recursive true})
      (doseq [i (range 1 25)] (.writeFileSync fs (.join path dir (snap/file-name (+ 1000 i))) "old"))
      (.mkdirSync fs (.join path dir (snap/file-name 1000)))
      (write-pose! f (pose {:t (js/Date.now)}))
      (-> (capture-stdout #(snap/main! (into base ["--worlds" worlds "--workspace" workspace]) (fake-render (atom []))))
          (.then (fn [[code _]]
                   (is (= 2 code))
                   (is (.isDirectory (.statSync fs (.join path dir (snap/file-name 1000)))) "the folder is left alone")))
          (.finally done)))))
