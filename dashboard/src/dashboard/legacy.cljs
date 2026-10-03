(ns dashboard.legacy
  "Villages, villagers and the blueprint library: the logic already lives in the old JS modules, which are loaded
  read-only with require (Node 24 can require ESM). Ported from library/villageSnapshot/villagers in tools/dashboard.mjs.
  Everything returned is a JS object, to be JSON.stringify'd as is."
  (:require ["path" :as path]))

(def modules (atom nil))

(def module-files
  {:lib "tools/dashboard/lib.mjs"
   :villages "tools/dashboard/villages.mjs"
   :roster "src/villager/roster.mjs"
   :inspection "src/villager/inspection.mjs"
   :source "src/blueprint/source.mjs"
   :schema "src/blueprint/schema.mjs"
   :manifest "src/blueprint/manifest.mjs"
   :build "src/blueprint/build.mjs"})

(defn load-all [repo]
  (try
    {:ok (into {} (map (fn [[k file]] [k (js/require (.join path repo file))])) module-files)}
    (catch :default e {:error (str "legacy modules unavailable: " (ex-message e))})))

;; the first load decides: a failed load is retried on the next call (a missing dependency may be installed meanwhile)
(defn get-modules [repo]
  (or @modules
      (let [loaded (load-all repo)]
        (when (:ok loaded) (reset! modules loaded))
        loaded)))

(defn call [mods k f & args]
  (.apply (aget (get-in mods [:ok k]) f) nil (to-array args)))

(defn assign [& objs] (apply js/Object.assign #js {} objs))

(defn to-js [x] (clj->js x :keyword-fn #(subs (str %) 1)))

;; ---------------------------------------------------------------- villagers
(defn villagers [repo data-root]
  (let [mods (get-modules repo)]
    (if (:error mods)
      #js {:version 1 :villagers #js {} :error (:error mods)}
      (try
        (call mods :roster "readVillagerRoster" (.join path data-root "state" "villagers.json"))
        (catch :default e
          (assign (call mods :roster "emptyVillagerRoster")
                  #js {:error (str "villager roster unavailable: " (ex-message e))}))))))

;; ---------------------------------------------------------------- villages
(defn manifest-for [mods place]
  (try
    #js {:manifest (call mods :manifest "readBlueprintManifest" (.-note place))}
    (catch :default e
      #js {:error #js {:place (.-name place) :error (str "saved blueprint unavailable: " (ex-message e))}})))

(defn inspections [mods]
  (try
    #js {:inspections (call mods :inspection "listVillageInspections")}
    (catch :default e
      #js {:inspections #js [] :error (str "saved village inspections unavailable: " (ex-message e))})))

(defn bp2? [place] (.startsWith (str (.-note place)) "bp2:"))

;; places: JS array. Returns #js {:villages [...] :error str-or-null}, never polling a body or scanning blocks.
(defn village-snapshot [repo data-root places]
  (let [mods (get-modules repo)]
    (if (:error mods)
      #js {:villages #js [] :error (:error mods)}
      (let [results (map #(manifest-for mods %) (filter bp2? places))
            manifests (keep #(when (.-manifest %) (.-manifest %)) results)
            manifest-errors (keep #(.-error %) results)
            insp (inspections mods)]
        #js {:villages (call mods :villages "villageViews"
                             #js {:places places
                                  :manifests (to-array manifests)
                                  :inspections (.-inspections insp)
                                  :manifestErrors (to-array manifest-errors)
                                  :roster (villagers repo data-root)})
             :error (or (.-error insp) nil)}))))

(defn attach-village-status [repo places villages]
  (let [mods (get-modules repo)]
    (if (:error mods)
      places
      (call mods :villages "attachVillageStatus" places villages))))

;; ---------------------------------------------------------------- the blueprint library
;; parsing, costing and lint are kept per file hash; the marked places built from a blueprint change with the world
(def details (atom {}))

(defn blueprint-for [mods file]
  (let [cached (get @details (.-name file))]
    (if (and cached (= (.-hash cached) (.-hash file)))
      cached
      (let [detail (call mods :lib "blueprintDetail" file)]
        (swap! details assoc (.-name file) detail)
        detail))))

(defn library [repo places]
  (let [mods (get-modules repo)]
    (if (:error mods)
      (throw (js/Error. (:error mods)))
      (let [dir (.-BLUEPRINT_DIR (get-in mods [:ok :build]))
            files (call mods :source "loadBlueprintDocuments" dir)
            hashed (.map files (fn [file] (assign file #js {:hash (call mods :schema "semanticBlueprintHash" (.-document file))})))]
        #js {:at (js/Date.now)
             :blueprints (.map hashed (fn [file]
                                        (assign (blueprint-for mods file)
                                                #js {:builds (call mods :lib "blueprintBuilds" (.-name file) (.-hash file) places)})))}))))

(defn preview [repo plan stock]
  (let [mods (get-modules repo)]
    (when (:error mods) (throw (js/Error. (:error mods))))
    (call mods :lib "blueprintDocumentDetail" plan stock)))
