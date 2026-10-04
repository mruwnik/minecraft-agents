// An inventory that is still moving (card c13b704d). A craft through a crafting table hands the grid and the cursor
// back to the pockets AFTER the window closes, one set_slot at a time, so a count read the moment the batch resolves
// can show the ingredient stack gone and the result not yet there: `craft item=bread count=3` called all 39 wheat
// spent for one bread that way. The `eat` right after asked for bread from a slot the server had just moved, and the
// plugin's meal "never showed"; the second eat worked. So a body waits for its slots to go quiet before it counts,
// and a meal called missing is judged again once the pockets have settled.

export const SETTLE_QUIET_MS = 100
export const SETTLE_MAX_MS = 2000

// lastChangeAt: when a slot last changed (null: never since the wait began); the wait ends 'settled' after a quiet
// SETTLE_QUIET_MS, or 'timeout' when the slots are still moving SETTLE_MAX_MS after it began
export const settleVerdict = ({ lastChangeAt, startedAt, now, quietMs = SETTLE_QUIET_MS, maxMs = SETTLE_MAX_MS }) => {
  if (now - (lastChangeAt ?? startedAt) >= quietMs) return 'settled'
  return now - startedAt >= maxMs ? 'timeout' : 'wait'
}

// a meal the plugin called missing, judged again from the settled pockets and the food number: it was eaten after
// all ('ate'), the slot it asked from was stale so one more try is due ('retry'), or the failure stands ('failed')
export const lateMeal = ({ failure, before, after, retried = false }) => {
  if (!failure) return 'failed'
  if (after.food > before.food || after.carried < before.carried) return 'ate'
  return !retried && /never showed/.test(String(failure?.message ?? failure)) ? 'retry' : 'failed'
}
