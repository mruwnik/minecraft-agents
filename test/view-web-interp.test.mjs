import test from 'node:test'
import assert from 'node:assert/strict'
import { poseInterpolator } from '../tools/view/web/interp.mjs'

const T0 = 1_000_000
const SKEW = 5000
const TWO_PI = 2 * Math.PI
// deterministic arrival jitter in ms (a 50 ms poll gives at most ~50); always contains 0 so the windowed minimum is stable
const JITTER = [0, 30, 10, 50, 20, 0, 40, 15, 35, 5, 45, 25]
const jitterAt = k => JITTER[k % JITTER.length]

const mk = (t, x, extra = {}) => ({ t, status: 'online', eye: { x, y: 65.62, z: 0 }, pos: { x, y: 64, z: 0 }, yaw: 1, pitch: 0, world: 'w', ...extra })

// a walk along x at 4.3 blocks/s, poses every `dt` ms, from k = 0 to count - 1
const walk = (dt, count, jitter = jitterAt) => Array.from({ length: count }, (_, k) => ({
  pose: mk(T0 + k * dt, (0.43 * dt / 100) * k),
  arrival: T0 + k * dt + SKEW + jitter(k)
}))

// feeds arrivals in time order while sampling every frame; returns [{now, pose}] for frames in [from, to)
const run = (interp, arrivals, from, to, step = 1000 / 60) => {
  const queue = [...arrivals].sort((a, b) => a.arrival - b.arrival)
  const frames = []
  for (let now = from; now < to; now += step) {
    while (queue.length && queue[0].arrival <= now) {
      const { pose, arrival } = queue.shift()
      interp.push(pose, arrival)
    }
    frames.push({ now, pose: interp.sample(now) })
  }
  return frames
}

const steps = frames => frames.slice(1).map((f, i) => f.pose.eye.x - frames[i].pose.eye.x)
const mean = xs => xs.reduce((a, b) => a + b, 0) / xs.length

const rates = [
  { name: '10 Hz', dt: 100, warm: 4000, delay: 150, jitter: jitterAt },
  { name: '20 Hz', dt: 50, warm: 9000, delay: 75, jitter: k => jitterAt(k) % 20 }
]
for (const { name, dt, warm, delay, jitter } of rates) {
  test(`steady walk at ${name}: even steps, delay settles`, () => {
    const interp = poseInterpolator()
    const arrivals = walk(dt, Math.ceil(16000 / dt), jitter)
    const from = T0 + SKEW + warm
    const frames = run(interp, arrivals, T0 + SKEW + 200, from + 3000).filter(f => f.now >= from)
    const s = steps(frames)
    const m = mean(s)
    assert.ok(m > 0)
    for (const d of s) assert.ok(Math.abs(d - m) <= 0.05 * m, `step ${d} vs mean ${m}`)
    assert.ok(Math.abs(interp.delay() - delay) <= 20, `delay ${interp.delay()}`)
  })
}

test('rate change 10 Hz to 20 Hz: no jerk, delay slews down', () => {
  const slow = walk(100, 100)
  const fastStart = T0 + 10000
  const fast = Array.from({ length: 400 }, (_, k) => ({
    pose: mk(fastStart + k * 50, 0.43 * 100 + 0.215 * k),
    arrival: fastStart + k * 50 + SKEW + jitterAt(k) % 20
  }))
  const interp = poseInterpolator()
  const frames = run(interp, [...slow, ...fast], T0 + SKEW + 200, T0 + SKEW + 25000)
  const speed = 0.43 / 100 // blocks per ms
  const from = frames.findIndex(f => f.now >= T0 + SKEW + 5500)
  const s = steps(frames).slice(from)
  const dtMs = 1000 / 60
  for (const d of s) assert.ok(Math.abs(d - speed * dtMs) <= 0.25 * speed * dtMs, `step ${d}`)
  const delays = []
  const again = poseInterpolator()
  const queue = [...slow, ...fast].sort((a, b) => a.arrival - b.arrival)
  for (let now = T0 + SKEW + 200; now < T0 + SKEW + 25000; now += dtMs) {
    while (queue.length && queue[0].arrival <= now) again.push(queue[0].pose, queue.shift().arrival)
    again.sample(now)
    delays.push(again.delay())
  }
  const slew = 1.0 * 200 * dtMs / 1000 + 1e-6
  for (let i = 1; i < delays.length; i++) assert.ok(Math.abs(delays[i] - delays[i - 1]) <= slew, `delay step ${delays[i] - delays[i - 1]}`)
  assert.ok(delays.at(-1) < 100)
})

test('yaw across the wrap takes the short arc', () => {
  const interp = poseInterpolator()
  const poses = [mk(T0, 0, { yaw: 6.2 }), mk(T0 + 100, 0, { yaw: 0.1 }), mk(T0 + 200, 0, { yaw: 0.1 })]
  const frames = run(interp, poses.map(pose => ({ pose, arrival: pose.t + SKEW })), T0 + SKEW + 150, T0 + SKEW + 400, 4)
  const yaws = frames.map(f => f.pose.yaw)
  for (const y of yaws) assert.ok(y >= 0 && y < TWO_PI)
  for (const y of yaws) assert.ok(y > 6.2 - 1e-9 || y < 0.1 + 1e-9, `yaw ${y} outside the short arc`)
  assert.ok(yaws.some(y => y > 6.2 + 0.01 || (y < 0.1 - 0.01)), 'saw intermediate samples')
})

test('teleport: no sample between the two positions', () => {
  const interp = poseInterpolator()
  const poses = [mk(T0, 0), mk(T0 + 100, 0.43), mk(T0 + 200, 100.43), mk(T0 + 300, 100.86)]
  const frames = run(interp, poses.map(pose => ({ pose, arrival: pose.t + SKEW })), T0 + SKEW + 100, T0 + SKEW + 600, 5)
  for (const { pose } of frames) assert.ok(pose.eye.x <= 0.43 + 1e-9 || pose.eye.x >= 100.43 - 1e-9, `x ${pose.eye.x}`)
  assert.ok(frames.some(f => f.pose.eye.x >= 100.43 - 1e-9))
})

test('underrun holds the last pose exactly', () => {
  const interp = poseInterpolator()
  const arrivals = walk(100, 20)
  const last = arrivals.at(-1).pose
  const frames = run(interp, arrivals, T0 + SKEW + 3000, T0 + SKEW + 3000 + 5000)
  const tail = frames.filter(f => f.now > T0 + SKEW + 2100 + 600)
  assert.ok(tail.length > 100)
  for (const { pose } of tail) assert.deepEqual(pose, last)
})

test('heartbeat gap: stays put, then moves at normal speed', () => {
  const interp = poseInterpolator()
  const poses = [mk(T0, 0), mk(T0 + 2000, 0), mk(T0 + 2100, 0.43)]
  const frames = run(interp, poses.map(pose => ({ pose, arrival: pose.t + SKEW })), T0 + SKEW + 150, T0 + SKEW + 2400, 5)
  const bodyT = f => f.now - SKEW - interp.delay()
  const before = frames.filter(f => bodyT(f) < T0 + 2000)
  assert.ok(before.length > 100)
  for (const { pose } of before) assert.equal(pose.eye.x, 0)
})

test('stop then move: waits, then covers one step at normal speed', () => {
  const moving = Array.from({ length: 11 }, (_, k) => mk(T0 + k * 100, 0.43 * k))
  const poses = [...moving, mk(T0 + 2000, 0.43 * 11)]
  const interp = poseInterpolator()
  const frames = run(interp, poses.map(pose => ({ pose, arrival: pose.t + SKEW })), T0 + SKEW + 1000, T0 + SKEW + 3200, 5)
  const bodyT = f => f.now - SKEW - interp.delay()
  const atRest = frames.filter(f => bodyT(f) > T0 + 1000 && bodyT(f) < T0 + 1000 + 700)
  assert.ok(atRest.length > 20)
  for (const { pose } of atRest) assert.ok(Math.abs(pose.eye.x - 4.3) < 1e-9, `x ${pose.eye.x}`)
  const ramp = frames.filter(f => bodyT(f) >= T0 + 1900 && bodyT(f) < T0 + 2000)
  const slope = (ramp.at(-1).pose.eye.x - ramp[0].pose.eye.x) / (bodyT(ramp.at(-1)) - bodyT(ramp[0]))
  assert.ok(Math.abs(slope - 0.0043) < 0.0004, `slope ${slope}`)
})

test('offline freezes at the newest pose', () => {
  const interp = poseInterpolator()
  const poses = [mk(T0, 0), mk(T0 + 100, 0.43), mk(T0 + 150, 0.9, { status: 'offline' })]
  const frames = run(interp, poses.map(pose => ({ pose, arrival: pose.t + SKEW })), T0 + SKEW + 160, T0 + SKEW + 600, 10)
  for (const { pose } of frames) assert.deepEqual(pose, poses[2])
})

test('offline short record keeps the frozen eye pose', () => {
  const interp = poseInterpolator()
  const last = mk(T0, 3)
  interp.push(last, T0 + SKEW)
  interp.push({ t: T0 + 500, status: 'offline', world: 'w' }, T0 + 500 + SKEW)
  assert.deepEqual(interp.sample(T0 + SKEW + 900), last)
})

test('entities: both blend, A-only vanishes, B-only appears in place', () => {
  const ent = (id, x, extra = {}) => ({ id, pos: { x, y: 64, z: 0 }, yaw: 0, ...extra })
  const a = mk(T0, 0, { entities: [ent(1, 0), ent(2, 5)] })
  const b = mk(T0 + 100, 0.43, { entities: [ent(1, 1), ent(3, 9)] })
  const c = mk(T0 + 200, 0.86, { entities: [ent(1, 2), ent(3, 9)] })
  const interp = poseInterpolator()
  const frames = run(interp, [a, b, c].map(pose => ({ pose, arrival: pose.t + SKEW })), T0 + SKEW + 150, T0 + SKEW + 260, 5)
  const during = frames.filter(f => f.pose.eye.x > 0.001 && f.pose.eye.x < 0.429)
  assert.ok(during.length > 3)
  for (const { pose } of during) {
    assert.deepEqual(pose.entities.map(e => e.id).sort(), [1, 3])
    assert.equal(pose.entities.find(e => e.id === 3).pos.x, 9)
    const e1 = pose.entities.find(e => e.id === 1).pos.x
    assert.ok(e1 > 0 && e1 < 1)
    assert.ok(Math.abs(e1 - pose.eye.x / 0.43) < 1e-9)
  }
})

test('a late arrival leaves the clock mapping alone', () => {
  const interp = poseInterpolator()
  const early = walk(100, 40)
  for (const { pose, arrival } of early) interp.push(pose, arrival)
  const shown = interp.sample(T0 + SKEW + 4000)
  const before = interp.offset()
  assert.ok(Math.abs(before - SKEW) <= 3, `offset ${before}`)
  interp.push(mk(T0 + 4000, 17.2), T0 + 4000 + SKEW + 300)
  assert.ok(Math.abs(interp.offset() - before) <= 1, `offset moved ${interp.offset() - before}`)
  assert.ok(Math.abs(interp.sample(T0 + SKEW + 4000).eye.x - shown.eye.x) <= 0.01)
})

test('standing still on heartbeats, then walking: delay stays near 1.5 intervals and the first step shows promptly', () => {
  const heartbeats = Array.from({ length: 6 }, (_, k) => ({ pose: mk(T0 + k * 2000, 0), arrival: T0 + k * 2000 + SKEW + jitterAt(k) }))
  const H = T0 + 10000
  const stride = walk(100, 30).map(({ pose }, k) => ({ pose: mk(H + (k + 1) * 100, 0.43 * (k + 1)), arrival: H + (k + 1) * 100 + SKEW + jitterAt(k + 3) }))
  const interp = poseInterpolator()
  const frames = run(interp, [...heartbeats, ...stride], T0 + SKEW, H + SKEW + 3000)
  const delayAfterFive = (() => {
    const again = poseInterpolator()
    const queue = [...heartbeats, ...stride].sort((a, b) => a.arrival - b.arrival)
    let result = null
    for (let now = T0 + SKEW; now < H + SKEW + 1000; now += 1000 / 60) {
      while (queue.length && queue[0].arrival <= now) again.push(queue[0].pose, queue.shift().arrival)
      again.sample(now)
      result = again.delay()
      if (now > stride[4].arrival) return result
    }
    return result
  })()
  assert.ok(Math.abs(delayAfterFive - 150) <= 25, `delay ${delayAfterFive}`)
  const firstShown = frames.find(f => f.now > H + SKEW && f.pose.eye.x > 1e-6)
  assert.ok(firstShown.now <= stride[0].arrival + 200 + 1000 / 60, `shown at ${firstShown.now - stride[0].arrival} ms after arrival`)
})

test('slow irregular gaps keep the delay at or below 200', () => {
  const gaps = [300, 450, 350, 400, 320, 440]
  const poses = []
  for (let k = 0, t = T0; k < 40; t += gaps[k % gaps.length], k++) poses.push({ pose: mk(t, 0.1 * k), arrival: t + SKEW + jitterAt(k) })
  const interp = poseInterpolator()
  const queue = [...poses]
  for (let now = T0 + SKEW; now < T0 + SKEW + 14000; now += 1000 / 60) {
    while (queue.length && queue[0].arrival <= now) interp.push(queue[0].pose, queue.shift().arrival)
    interp.sample(now)
    assert.ok(interp.delay() <= 200, `delay ${interp.delay()}`)
  }
})

test('20 Hz: the delay is near 75 within 2 s', () => {
  const interp = poseInterpolator()
  run(interp, walk(50, 80, k => jitterAt(k) % 20), T0 + SKEW, T0 + SKEW + 2000)
  assert.ok(Math.abs(interp.delay() - 75) <= 15, `delay ${interp.delay()}`)
})

const hopCases = [
  { jump: 8.5, snaps: true },
  { jump: 12, snaps: true },
  { jump: 50, snaps: true },
  { jump: 5, snaps: false },
  { jump: 7, snaps: false }
]
for (const { jump, snaps } of hopCases) {
  test(`a server hop of ${jump} blocks ${snaps ? 'snaps' : 'glides'}`, () => {
    const poses = [mk(T0, 0), mk(T0 + 100, 0.1), mk(T0 + 200, 0.1 + jump), mk(T0 + 300, 0.2 + jump)]
    const frames = run(poseInterpolator(), poses.map(pose => ({ pose, arrival: pose.t + SKEW })), T0 + SKEW + 100, T0 + SKEW + 600, 5)
    const between = frames.filter(f => f.pose.eye.x > 0.1 + 1e-9 && f.pose.eye.x < 0.1 + jump - 1e-9)
    assert.equal(between.length > 0, !snaps)
  })
}

// every arrival in time order, sampling at 60 fps; collects the per-frame playhead and whether the sample held at the newest pose
const trace = (interp, arrivals, from, to) => {
  const queue = [...arrivals].sort((a, b) => a.arrival - b.arrival)
  const out = []
  const step = 1000 / 60
  for (let now = from; now < to; now += step) {
    while (queue.length && queue[0].arrival <= now) interp.push(queue[0].pose, queue.shift().arrival)
    interp.sample(now)
    out.push({ now, playhead: interp.playhead(now), delay: interp.delay(), underruns: interp.underruns() })
  }
  return out
}

const uniformJitter = k => ((k * 7919) % 51) // deterministic 0..50

test('playhead advances one frame per frame: no steps from the offset or the delay', () => {
  const frames = trace(poseInterpolator(), walk(100, 220, uniformJitter), T0 + SKEW, T0 + SKEW + 20000).filter(f => f.now > T0 + SKEW + 5000)
  const advances = frames.slice(1).map((f, i) => f.playhead - frames[i].playhead)
  for (const a of advances) assert.ok(Math.abs(a - 1000 / 60) <= 0.5, `advance ${a}`)
})

test('a late arrival never moves the playhead backwards', () => {
  const arrivals = walk(100, 220, uniformJitter).map((a, k) => (k === 120 ? { ...a, arrival: a.arrival + 300 } : a))
  const frames = trace(poseInterpolator(), arrivals, T0 + SKEW, T0 + SKEW + 20000).filter(f => f.now > T0 + SKEW + 5000)
  for (let i = 1; i < frames.length; i++) assert.ok(frames[i].playhead >= frames[i - 1].playhead, `backwards at ${frames[i].now}`)
})

const delayCases = [
  { name: '20 Hz, 0-50 ms jitter: no underruns after warm-up', dt: 50, jitter: uniformJitter, check: frames => assert.equal(frames.at(-1).underruns - frames[0].underruns, 0) },
  { name: '10 Hz, 0-20 ms jitter: delay about 130 or less', dt: 100, jitter: k => uniformJitter(k) % 21, check: frames => assert.ok(frames.at(-1).delay <= 135, `delay ${frames.at(-1).delay}`) }
]
for (const { name, dt, jitter, check } of delayCases) {
  test(`delay target: ${name}`, () => {
    const frames = trace(poseInterpolator(), walk(dt, Math.ceil(30000 / dt), jitter), T0 + SKEW, T0 + SKEW + 25000).filter(f => f.now > T0 + SKEW + 8000)
    check(frames)
  })
}

test('holding at the newest pose is counted as an underrun', () => {
  const interp = poseInterpolator()
  const arrivals = walk(100, 10, () => 0)
  trace(interp, arrivals, T0 + SKEW, T0 + SKEW + 1800)
  interp.push(mk(T0 + 1100, 5), T0 + 1100 + SKEW)
  assert.ok(interp.underruns() > 0)
})
