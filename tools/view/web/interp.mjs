// Pose interpolation for the browser view. Pure: time is passed in, there are no globals.
// Poses arrive a little late and unevenly; the camera plays them back `delay` ms behind the body clock, blended between
// the two poses around that moment. See docs/view-rendering-performance.md.
const TWO_PI = 2 * Math.PI
const OFFSET_RELAX = 0.002 // ms of offset per ms of browser time: the clock mapping follows skew without stepping
const OFFSET_FALL = 0.02 // ms of offset per ms: a lower skew is followed over time, not in one step
const JITTER_KEEP = 20
const JITTER_QUANTILE = 0.9
const DELAY_MARGIN = 10
const BAND_BELOW = 8 // the delay only moves when the target leaves [delay - BAND_BELOW, delay + BAND_ABOVE]: no wobble from the jitter estimate
const BAND_ABOVE = 20
const GAP_KEEP = 12
const MAX_INTERVAL_GAP = 500
const DEFAULT_INTERVAL = 100
const RISE_PER_SECOND = 0.2 // the delay rises slowly ...
const FALL_PER_SECOND = 1 // ... and falls fast, so playback catches up when walking resumes
const INTERVAL_QUANTILE = 0.25 // a moving body writes at its rate cap; longer gaps are pauses

const clamp = (v, lo, hi) => Math.min(hi, Math.max(lo, v))
const lerp = (a, b, f) => a + (b - a) * f
const lerp3 = (a, b, f) => ({ ...b, x: lerp(a.x, b.x, f), y: lerp(a.y, b.y, f), z: lerp(a.z, b.z, f) })
const dist3 = (a, b) => Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z)
const quantile = (xs, q) => [...xs].sort((a, b) => a - b)[Math.min(xs.length - 1, Math.floor(xs.length * q))]

// along the shortest arc, normalised to [0, 2π)
const lerpYaw = (a, b, f) => {
  const d = ((((b - a + Math.PI) % TWO_PI) + TWO_PI) % TWO_PI) - Math.PI
  return (((a + d * f) % TWO_PI) + TWO_PI) % TWO_PI
}

const blendEntity = (a, b, f, teleport) => {
  if (!a || !b.pos || !a.pos || dist3(a.pos, b.pos) > teleport) return b
  return { ...b, pos: lerp3(a.pos, b.pos, f), yaw: typeof a.yaw === 'number' && typeof b.yaw === 'number' ? lerpYaw(a.yaw, b.yaw, f) : b.yaw }
}

const blendEntities = (a, b, f, teleport) => {
  if (!b.entities) return b.entities
  const before = new Map((a.entities ?? []).filter(e => e.id !== undefined).map(e => [e.id, e]))
  return b.entities.map(e => blendEntity(before.get(e.id), e, f, teleport))
}

const blend = (a, b, f, teleport) => ({
  ...b,
  eye: lerp3(a.eye, b.eye, f),
  pos: a.pos && b.pos ? lerp3(a.pos, b.pos, f) : b.pos,
  yaw: lerpYaw(a.yaw, b.yaw, f),
  pitch: lerp(a.pitch, b.pitch, f),
  entities: blendEntities(a, b, f, teleport)
})

export const poseInterpolator = ({ minDelay = 50, maxDelay = 200, maxBuffer = 32, teleport = 8 } = {}) => {
  let buffer = []
  let skews = [] // arrival - pose.t of the last arrivals
  let base = null // running minimum of skew, relaxing upwards between arrivals
  let baseAt = 0
  let inUse = null // the offset in use at baseAt (it falls to `base` at OFFSET_FALL)
  let lastT = null // body time last sampled
  let held = 0 // frames held at the newest pose since the last arrival
  let underruns = 0
  let gaps = []
  let applied = null
  let lastNow = null
  let offline = false

  const floorAt = at => base + OFFSET_RELAX * Math.max(0, at - baseAt)
  const offset = (at = baseAt) => (base === null ? 0 : Math.max(floorAt(at), inUse - OFFSET_FALL * Math.max(0, at - baseAt)))
  const interval = () => (gaps.length ? quantile(gaps, INTERVAL_QUANTILE) : DEFAULT_INTERVAL)
  // never underrun: the interval plus the arrival jitter seen lately, plus a margin
  const target = () => {
    const jitter = skews.length ? quantile(skews, JITTER_QUANTILE) - offset() : 0
    return clamp(interval() + Math.max(0, jitter) + DELAY_MARGIN, minDelay, maxDelay)
  }

  const push = (pose, arrivalMs) => {
    offline = pose.status === 'offline'
    if (!pose.eye) return
    const newest = buffer.at(-1)
    if (newest && pose.t <= newest.t) return
    const gap = newest ? pose.t - newest.t : 0
    if (gap > 0 && gap <= MAX_INTERVAL_GAP) gaps = [...gaps, gap].slice(-GAP_KEEP)
    const skew = arrivalMs - pose.t
    const current = base === null ? skew : offset(arrivalMs)
    base = base === null ? skew : Math.min(floorAt(arrivalMs), skew)
    baseAt = arrivalMs
    inUse = current
    skews = [...skews, skew].slice(-JITTER_KEEP)
    if (lastT !== null && lastT >= newest?.t && gap <= MAX_INTERVAL_GAP) underruns += held // a stand-still hold is a long gap, not an underrun
    held = 0
    buffer.push(pose)
    if (buffer.length > maxBuffer) buffer = buffer.slice(-maxBuffer)
    if (applied === null) applied = target()
  }

  const slew = now => {
    const goal = target()
    const seconds = lastNow === null ? 0 : Math.max(0, now - lastNow) / 1000
    const desired = clamp(applied, goal - BAND_BELOW, goal + BAND_ABOVE)
    applied += clamp(desired - applied, -FALL_PER_SECOND * goal * seconds, RISE_PER_SECOND * goal * seconds)
    lastNow = now
  }

  const sample = nowMs => {
    if (!buffer.length) return null
    slew(nowMs)
    const newest = buffer.at(-1)
    if (offline) return newest
    const T = nowMs - offset(nowMs) - applied
    lastT = T
    if (T >= newest.t) {
      held++
      return newest
    }
    if (T < buffer[0].t) return buffer[0]
    const i = buffer.findIndex((p, k) => buffer[k + 1].t > T)
    buffer = buffer.slice(i)
    const [a, b] = buffer
    if (dist3(a.eye, b.eye) > teleport) return a
    const span = b.t - a.t
    const start = span > 2 * interval() ? Math.max(a.t, b.t - interval()) : a.t
    if (T < start) return a
    return blend(a, b, (T - start) / (b.t - start), teleport)
  }

  // body time being shown at a browser time
  const playhead = nowMs => nowMs - offset(nowMs) - (applied ?? target())

  return { push, sample, delay: () => applied ?? target(), interval, offset, playhead, underruns: () => underruns }
}
