// A led animal within PATH_RANGE of the body after a short walk paths: it walks by its own route (through an open
// gate if the straight way is blocked) until it is REST_GAP from the body, and does not move while closer. Measured
// live. Beyond the range the lead drags it in a straight line (fake-leash). `pin: true` on the spec keeps it still.
import { walkTo } from './fake-tempt.mjs'

const PATH_RANGE = 6
const REST_GAP = 3.3

// The new position, or null when the path regime does not apply (the drag runs instead).
export function pathFollow (s, e) {
  if (e.snaps || e.trail !== undefined) return null
  const d = Math.hypot(e.pos.x - s.self.pos.x, e.pos.z - s.self.pos.z)
  if (d > PATH_RANGE) return null
  if (e.pin || d <= REST_GAP) return e.pos
  return walkTo(s, e, REST_GAP)
}
