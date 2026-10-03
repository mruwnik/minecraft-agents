(ns dashboard.view-info
  "The small summary of a body's view (state/agents/<name>/view/pose.json and hud.json) that /api/state carries.")

(defn summarize
  "{:poseMtimeMs :status :pos :dimension :hud}, or nil when the body has neither file. The pose's entity list is dropped."
  [pose hud pose-mtime-ms]
  (when (or pose hud)
    {:poseMtimeMs (when pose pose-mtime-ms)
     :status (:status pose)
     :pos (:pos pose)
     :dimension (:dimension pose)
     :hud hud}))
