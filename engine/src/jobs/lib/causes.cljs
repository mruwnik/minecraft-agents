(ns jobs.lib.causes
  "The family of an ending's cause (docs/end-statuses.md section 4): callers branch on the family, while the cause
  stays the keyword the job gave."
  (:require [engine.contract :as contract]))

(def families
  "Family -> the cause keywords it holds; one family may sit under several kinds (hazard: a fact, or a seal that failed)."
  {:chest-space [:base-full :chest_unusable :to-full]
   :danger [:danger :danger-remembered :died-near :hostile-near :sealed-hostile-near :spawner :goal-dangerous]
   :free-slots [:bag-full :full :inventory-full :no-free-slot]
   :hazard [:cave-below :crop :falling-block :far-below :flooded :fluid :fluid-adjacent :fluid-in-cut :hazard
            :headroom :lava :lava-exposed :no-base :no-exit :no-floor :no-floor-below :no-stand :no-support
            :no-way-back :open-shell :own-body :plugged :roofed :source-blocked :standing-in :stranding
            :too-deep :unbreakable :under-body :under-feet :undercuts-way :water :lava-unsealed :water-unplugged]
   :need [:chest-lacks :food-reserve :food-short :missing :need :needs-food :no-bed :no-blocks :no-bucket
          :no-food :no-item :no-items :no-rod :no-sapling :no-shears :no-source :no-torches :short :short-seed
          :too-few-blocks]
   :not-loaded [:goal-unloaded :no-path-world :not-loaded]
   :not-ready [:not-ready]
   :plan [:bad-plan :cells-foreign :cells-unplantable :plan :plan-trouble]
   :state [:aboard :away :bed-open :moved-since-submit :not-aboard]
   :target [:absent :bed-missing :gone :log-on-spot :no-block :no-boat :no-box :no-chest :no-color :no-composter
            :no-crop-seen :no-destination :no-far-shore :no-food-found :no-home :no-lava :no-mob :no-mob-or-box
            :no-shore :no-site :no-table :no-target :no-targets :no-tree :no-vehicle :no-water :not-a-table
            :not-found :not-water :nothing-found :nothing-in-range :nothing-to-do :nothing-to-feed
            :nothing-to-guard :nothing-worth-it :occupied :too-far :too-few :too-short]
   :time [:day :day-not-come :dusk-not-come :night :not-night :hold-timeout :leg :limit-reached :still-burning
          :time :timeout]
   :tool [:no-tool :tool-spent :unarmed]
   :unseen [:floor-unseen :out-of-sight :unseen :unseen-floor :unseen-floor-dark]
   :zones [:footprint :no-zones :not-allowed :refused :withdraw-declined :zone :zone-exit]
   :abandoned [:abandoned]
   :agent [:agent]
   :cut [:cut]
   :death [:death]
   :ended [:ended]
   :restart [:restart]
   :shutdown [:shutdown]
   :already [:already :already-clear :already-sealed :already-sound :at-target :enough-room :night-safe
             :no-hostile :no-ranged-danger :not-burning :not-hungry :not-mounted :not-underwater :not-wedged
             :nothing-to-restore :nothing-to-tidy]
   :bad-args [:bad-args :bad-name :bad-pos :bad-state :needs-redstone :no-pen :no-way :not-a-line :not-armour
              :not-smeltable :not-toggleable :offline-forbidden :stale-args :unknown-item :unsupported-species]
   :duplicate-call [:cycle :duplicate-call]
   :unsupported [:log-out-unsupported :unsupported]
   :food [:hungry]
   :health [:health :low-health]
   :gave-up [:gave-up]
   :least [:least]
   :no-path [:abilities :bed-unreachable :cut-again :detour :door-stuck :exhausted :explore-spent
             :leg-search-spent :moved-while-searching :needs-health :no-approach :not-back :not-home :one-way
             :out-of-reach :partial :plant-blocked :replan-limit :shut-in :tree-unreachable :unreachable
             :walk-failed :walk-in-failed :way-dives]
   :tried [:bake :barren :blocked :board-failed :cannot :cannot-break :cannot_escape :cart-failed :child-failed
           :cleanup-open :clear-failed :dig-failed :dismount-failed :dry :eat-failed :exposed :failed :futile
           :given-up :incomplete :leave-failed :left :left-open :legs :no-air :no-bite :no-drops :no-stock
           :no-stone-found :no-support-floor :no-way-out :no_response :no_way_out :none :none-collected
           :none-taken :not-broken :not-collected :not-landed :not-restored :not-set :not-solved
           :nothing-collected :off-stair :place-failed :pour-failed :recover-failed :refills
           :roofed-not-enclosed :search-waited :spent-on-mend :step-failed :still-hungry :still-wedged :stuck
           :too-many-passes :torch-failed :tree-blocked :trips-exhausted :unchanged :unsealed :wear-failed
           :wrong-side :wrong-way]})

(def family-of (into {} (for [[f rs] families r rs] [r f])))

(defn family
  "The family of x, an ending or a cause (a keyword or a map read as contract/cause-key reads it); nil when it has none."
  [x]
  (family-of (contract/cause-key (if (contract/ending? x) x {:cause x}))))
