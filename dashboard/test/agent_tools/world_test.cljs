(ns agent-tools.world-test
  (:require [cljs.test :refer [deftest is are async]]
            [agent-tools.fake-socket :as fake]
            [agent-tools.world :as world]
            [agent-tools.world-data :as data]
            ["node:fs" :as fs]
            ["node:os" :as os]
            ["node:path" :as path]))

(defn fails-with [pattern argv]
  (let [error (:error (world/request-for argv))]
    (and (string? error) (boolean (re-find pattern error)))))

(defn spec-of [& args]
  (let [r (world/request-for (into ["--world" "w" "Probe" "submit"] args))]
    (or (:error r) (get-in r [:request :spec]))))

(deftest every-action-is-a-job-submit-by-the-driver
  (are [args spec] (= spec (apply spec-of args))
    ["move-to" "1" "64" "-2"] '(jobs.movement.go-to {:pos {:x 1 :y 64 :z -2}})
    ["move-to" "1" "64" "-2" "--range" "2" "--doors" "leave-open" "--no-escalate"]
    '(jobs.movement.go-to {:pos {:x 1 :y 64 :z -2} :range 2 :doors :leave-open :escalate false})
    ["dig" "1" "64" "2"] '(jobs.blocks.dig {:pos {:x 1 :y 64 :z 2}})
    ["dig" "1" "64" "2" "--ignore-zones"] '(jobs.blocks.dig {:pos {:x 1 :y 64 :z 2} :ignore-zones? true})
    ["place" "1" "64" "2" "oak_planks"] '(jobs.blocks.place {:pos {:x 1 :y 64 :z 2} :item "oak_planks"})
    ["use-on" "1" "64" "2" "--item" "bread" "--face" "up"] '(jobs.blocks.use-on {:pos {:x 1 :y 64 :z 2} :item "bread" :face "up"})
    ["use-on" "1" "64" "2"] '(jobs.blocks.use-on {:pos {:x 1 :y 64 :z 2}})
    ["interact" "17" "--item" "lead"] '(jobs.items.interact {:id 17 :item "lead"})
    ["wear" "iron_helmet"] '(jobs.items.wear {:item "iron_helmet"})
    ["wear"] '(jobs.items.wear {})
    ["equip" "iron_pickaxe"] '(jobs.items.equip {:item "iron_pickaxe"})
    ["equip" "shield" "--hand" "off"] '(jobs.items.equip {:item "shield" :hand "off"})))

(deftest the-request-goes-to-the-jobs-api-as-the-driver
  (let [r (world/request-for ["--world" "w" "Probe" "--state" "/s" "submit" "dig" "1" "64" "2" "--who" "Wren" "--request-id" "r1"])]
    (is (nil? (:error r)))
    (is (= {:op :submit :request-id "r1" :by "Wren"} (select-keys (:request r) [:op :request-id :by])))
    (is (= "/jobs" (:path r)))
    (is (true? (:mutating r)))
    (is (re-find #"agents/Probe/engine/events\.sock$" (:socketPath r)))
    (is (re-matches #"[0-9a-f-]{36}" (get-in (world/request-for ["--world" "w" "Probe" "submit" "dig" "1" "64" "2"]) [:request :request-id])))
    (is (= "claude" (get-in (world/request-for ["--world" "w" "Probe" "submit" "dig" "1" "64" "2"]) [:request :by])))))

(deftest rejects-options-that-do-not-fit-the-action
  (doseq [[pattern argv] [[#"body name" ["--world" "w" "../etc" "submit" "dig" "1" "64" "2"]]
                          [#"not valid for dig" ["--world" "w" "Probe" "submit" "dig" "1" "64" "2" "--range" "1"]]
                          [#"not valid for move-to" ["--world" "w" "Probe" "submit" "move-to" "1" "64" "2" "--item" "bread"]]
                          [#"not valid for wear" ["--world" "w" "Probe" "submit" "wear" "--ignore-zones"]]
                          [#"not valid for wear" ["--world" "w" "Probe" "submit" "wear" "--hand" "off"]]
                          [#"--hand must be" ["--world" "w" "Probe" "submit" "equip" "shield" "--hand" "left"]]
                          [#"equip needs one item" ["--world" "w" "Probe" "submit" "equip"]]
                          [#"--doors must be" ["--world" "w" "Probe" "submit" "move-to" "1" "64" "2" "--doors" "smash"]]
                          [#"--request-id must be" ["--world" "w" "Probe" "submit" "dig" "1" "64" "2" "--request-id" "bad id"]]
                          [#"--who must be" ["--world" "w" "Probe" "submit" "dig" "1" "64" "2" "--who" ""]]]]
    (is (fails-with pattern argv) (pr-str argv))))

(deftest the-old-commands-point-to-the-job-tools
  (is (fails-with #"jobs.mjs show" ["--world" "w" "Probe" "status" "op-1"]))
  (is (fails-with #"jobs.mjs cancel" ["--world" "w" "Probe" "cancel" "op-1"]))
  (is (fails-with #"observe inventory" ["--world" "w" "Probe" "inventory"]))
  (is (fails-with #"--timeout-s|--max-distance|unknown" ["--world" "w" "Probe" "submit" "move-to" "1" "64" "2" "--timeout-s" "4"])))

(deftest rejects-malformed-commands
  (doseq [[pattern argv] [[#"unknown action delete-file" ["--world" "w" "Probe" "submit" "delete-file"]]
                          [#"unknown action undefined" ["--world" "w" "Probe" "submit"]]
                          [#"needs x y z" ["--world" "w" "Probe" "submit" "dig" "1" "64"]]
                          [#"needs x y z item" ["--world" "w" "Probe" "submit" "place" "1" "64" "2"]]
                          [#"x must be a finite number" ["--world" "w" "Probe" "submit" "dig" "x" "64" "2"]]
                          [#"--range must be a finite number" ["--world" "w" "Probe" "submit" "move-to" "1" "64" "2" "--range" " "]]
                          [#"unknown command" ["--world" "w" "Probe" "dance"]]]]
    (is (fails-with pattern argv) (pr-str argv))))

(deftest a-body-is-addressed-in-its-world
  (is (fails-with #"missing --world <world>" ["Probe" "submit" "dig" "1" "64" "2"]))
  (is (fails-with #"world" ["Probe" "submit" "dig" "1" "64" "2" "--world" "../x"]))
  (let [r (world/request-for ["Probe" "submit" "dig" "1" "64" "2" "--world" "w" "--state" "/s"])]
    (is (= ["w" "Probe" "/s"] [(:world r) (:body r) (:state r)]))))

(deftest wait-and-its-timeout-belong-to-submit
  (is (= {:timeout "2m"} (:wait (world/request-for ["--world" "w" "Probe" "submit" "dig" "1" "64" "2" "--wait" "--timeout" "2m"]))))
  (is (nil? (:wait (world/request-for ["--world" "w" "Probe" "submit" "dig" "1" "64" "2"]))))
  (doseq [[pattern argv] [[#"--timeout requires --wait" ["--world" "w" "Probe" "submit" "dig" "1" "64" "2" "--timeout" "5s"]]
                          [#"--timeout must be" ["--world" "w" "Probe" "submit" "dig" "1" "64" "2" "--wait" "--timeout" "x"]]]]
    (is (fails-with pattern argv) (pr-str argv))))

(defn run-main! [state argv handler]
  (let [[request-fn seen] (fake/request-fn handler)
        lines (atom [])]
    (-> (world/main! (into ["--world" "w" "Probe" "--state" state] argv) {:request-fn request-fn :output #(swap! lines conj %)})
        (.then (fn [code] {:code code :out (apply str @lines) :seen @seen})))))

(deftest submit-posts-the-spec-with-the-driver-as-by
  (let [state (.mkdtempSync fs (.join path (.tmpdir os) "world-cli-"))]
    (async done
      (-> (run-main! state ["submit" "dig" "1" "64" "2" "--who" "Wren" "--request-id" "dig-1"]
                     (fn [{:keys [path]}]
                       {:text (if (= "/snapshot" path) "{:generation-id \"g\"}" "{:ok true :job {:id \"j5\" :status :queued :manual true}}")}))
          (.then (fn [{:keys [code out seen]}]
                   (let [post (first (filter #(= "POST" (:method %)) seen))
                         sent (data/read-edn (:body post))]
                     (is (= 0 code))
                     (is (= "j5" (get-in (data/read-edn out) [:job :id])))
                     (is (= ["GET" "POST"] (take 2 (mapv :method seen))))
                     (is (re-find #"events\.sock$" (:socket-path post)))
                     (is (= {:op :submit :by "Wren" :spec '(jobs.blocks.dig {:pos {:x 1 :y 64 :z 2}})}
                            (select-keys sent [:op :by :spec]))))))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))
