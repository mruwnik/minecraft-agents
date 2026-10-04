// Draws what a body sees from the files it dumps, no server and no GPU:
//   node tools/view-render.mjs <AgentName> --world <world> [--out file.png] [--width 320] [--height 180] [--fov 70] [--dist 64] [--watch ms] [--state dir] [--bench seconds [--no-png]]
// Default out: worlds/<world>/agents/<Name>/view/frame.png. --bench <seconds> renders flat out and prints one JSON timing line (--no-png skips PNG encoding). --watch redraws whenever pose.json changes, writing atomically.
import fs from 'node:fs'
import path from 'node:path'
import { storageRoot, worldsDir } from '../engine/js/bodies.mjs'
import { parseArgs } from 'node:util'
import { runBench } from './view/bench.mjs'
import { DEFAULT_STATE_DIR, readPose, renderView, viewDir } from './view/render.mjs'
import { missingWorldError } from '../engine/js/bodies.mjs'

const { values, positionals } = parseArgs({
  allowPositionals: true,
  options: {
    out: { type: 'string' },
    width: { type: 'string', default: '320' },
    height: { type: 'string', default: '180' },
    fov: { type: 'string', default: '70' },
    dist: { type: 'string', default: '64' },
    watch: { type: 'string' },
    state: { type: 'string' }, worlds: { type: 'string' },
    world: { type: 'string' },
    bench: { type: 'string' },
    'no-png': { type: 'boolean', default: false }
  }
})
const [agentName] = positionals
if (!agentName) {
  console.error('usage: node tools/view-render.mjs <AgentName> --world <world> [--out file.png] [--width 320] [--height 180] [--fov 70] [--dist 64] [--watch ms] [--state dir] [--bench seconds [--no-png]]')
  process.exit(1)
}
const world = values.world
if (!world) {
  console.error(missingWorldError('--world'))
  process.exit(1)
}
const stateDir = storageRoot(values, path.resolve(import.meta.dirname, ".."))
const out = values.out ?? path.join(viewDir(world, agentName, stateDir), 'frame.png')
const poseFile = path.join(viewDir(world, agentName, stateDir), 'pose.json')

const writeAtomic = (file, buffer) => {
  fs.mkdirSync(path.dirname(file), { recursive: true })
  const tmp = `${file}.${process.pid}.tmp`
  fs.writeFileSync(tmp, buffer)
  fs.renameSync(tmp, file)
}

const draw = () => {
  const r = renderView({ world, agentName, width: Number(values.width), height: Number(values.height), fov: Number(values.fov), maxDist: Number(values.dist), stateDir })
  if (r.pose.status === 'offline') console.error(`warning: ${agentName} is offline, drawing the last known view`)
  writeAtomic(out, r.png)
  console.log(`${out}: ${r.columns} columns, ${Math.round(r.gridMs)} ms grid, ${Math.round(r.ms)} ms total${r.textured ? '' : ' (flat colours)'}`)
  return r
}

try {
  readPose(world, agentName, stateDir)
} catch (e) {
  console.error(e.message)
  process.exit(1)
}

if (values.bench) {
  const { lastPng, ...result } = runBench({ world, agentName, seconds: Number(values.bench), width: Number(values.width), height: Number(values.height), fov: Number(values.fov), dist: Number(values.dist), stateDir, noPng: values['no-png'] })
  if (values.out && lastPng) writeAtomic(values.out, lastPng)
  console.log(JSON.stringify(result))
  process.exit(0)
}

draw()
if (values.watch) {
  let seen = fs.statSync(poseFile).mtimeMs
  setInterval(() => {
    const mtime = fs.statSync(poseFile, { throwIfNoEntry: false })?.mtimeMs
    if (mtime === undefined || mtime === seen) return
    seen = mtime
    try { draw() } catch (e) { console.error(e.message) }
  }, Number(values.watch))
}
