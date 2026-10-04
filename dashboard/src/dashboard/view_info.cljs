(ns dashboard.view-info
  "The small summary of a body's view (worlds/<world>/agents/<name>/view/pose.json and hud.json) that /api/state carries.")

(defn villagers
  "The villagers among a pose's entities, as {:id :x :y :z}; the rest of the entity list is dropped."
  [entities]
  (vec (for [{:keys [id name pos]} entities
             :when (and (= "villager" name) pos)]
         {:id id :x (:x pos) :y (:y pos) :z (:z pos)})))

(defn summarize
  "{:poseMtimeMs :poseT :status :pos :dimension :hud :villagers}, or nil when the body has neither file. :poseT is the
  time the body wrote the pose, :villagers the villagers it saw then."
  [pose hud pose-mtime-ms]
  (when (or pose hud)
    {:poseMtimeMs (when pose pose-mtime-ms)
     :poseT (:t pose)
     :status (:status pose)
     :pos (:pos pose)
     :dimension (:dimension pose)
     :villagers (:villagers pose)
     :hud hud}))
