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
