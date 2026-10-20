(ns jobs.lib.access-tool
  "Facts about the stops of the digging access jobs (stair, tunnel).")

(def facts
  "The stops that name a fact barring the next cell (condition-failed); the others are attempts that failed."
  #{:refused :zone :claim :footprint :no-zones :not-loaded :fluid-in-cut :crop :base-block :undercuts-way :unbreakable
    :no-tool :tool-spent :no-free-slot :inventory-full :hazard :lava-exposed :plugged :floor-unseen :no-floor :cave-below :off-stair :no-way-back :y-not-beyond-feet})
