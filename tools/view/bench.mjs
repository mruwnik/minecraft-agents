// Why JavaScript: measured performance of the JS renderer; times frames per phase.
// Back-to-back frames with no sleep, for as long as asked (or `maxFrames`), summarised per phase.
import { columnStats, renderView } from './render.mjs'

const PHASES = ['pose', 'grid', 'raycast', 'png', 'total']
const round = v => Math.round(v * 100) / 100
const summary = values => {
  const sorted = [...values].sort((a, b) => a - b)
  const at = q => sorted[Math.min(sorted.length - 1, Math.floor(q * sorted.length))]
  return { mean: round(values.reduce((a, b) => a + b, 0) / values.length), p50: round(at(0.5)), p95: round(at(0.95)) }
}

export function runBench ({ world, agentName, seconds, maxFrames = Infinity, width = 320, height = 180, fov = 70, dist = 64, stateDir, noPng = false, onFrame = () => {} }) {
  const frames = []
  const started = performance.now()
  let last = null
  let poseChanges = 0
  let lastPng = null
  while (frames.length < maxFrames && (performance.now() - started) / 1000 < seconds) {
    onFrame(frames.length)
    const r = renderView({ world, agentName, width, height, fov, maxDist: dist, stateDir, noPng })
    if (last !== null && r.pose.t !== last) poseChanges++
    last = r.pose.t
    lastPng = r.png ?? lastPng
    frames.push(r.timings)
  }
  const elapsed = (performance.now() - started) / 1000
  const stats = columnStats()
  return {
    frames: frames.length,
    seconds: round(elapsed),
    fps: round(frames.length / elapsed),
    width,
    height,
    dist,
    columnsLoaded: stats.loaded,
    columnReloads: stats.reloads,
    ms: Object.fromEntries(PHASES.map(p => [p, summary(frames.map(f => f[p]))])),
    poseChanges,
    lastPng
  }
}
