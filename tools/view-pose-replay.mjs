// Replays a recorded pose stream (tools/view-pose-record.mjs) as body "Replay" of the recorded world (<world>/Replay) in a temp state dir, so the browser view
// can be measured on a deterministic input. Prints the state dir; serve it with `node tools/view-serve.mjs --state <dir>`.
//   node tools/view-pose-replay.mjs --in poses.jsonl [--hz 20] [--synthetic walk|teleport|sprint] [--state state] [--seconds N] [--touch-ms 2000]
// --synthetic ignores the recording except for its first pose (a bare --synthetic is walk):
//   walk: a straight walk at 4.317 blocks/s, 20 blocks each way, at --hz, in the recorded place with the world's chunks and biomes.json symlinked.
//   teleport: stands still and every 6 s jumps between two places >= 1000 blocks apart that both have column files.
//   sprint: 5.6 blocks/s along +x for 200 blocks and back, through the best-covered stretch of column files.
// teleport and sprint COPY the column files they need (within radius 9 of the path) into the temp state dir, so nothing in --state is
// ever written. --touch-ms rewrites (tmp + rename) the column file next to the eye at that period, to measure change-to-drawn latency.
// Poses are written atomically at the recorded spacing with t rewritten to now. The recording plays forward then backward
// so the loop has no jump. --hz resamples eye/pos/yaw/pitch by linear interpolation (changes only, 2 s heartbeat).
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { parseArgs } from 'node:util'
import { fileURLToPath } from 'node:url'
import { bodyDir } from '../engine/js/bodies.mjs'

const TWO_PI = 2 * Math.PI
const HEARTBEAT_MS = 2000
const round2 = v => Math.round(v * 100) / 100
const lerp = (a, b, f) => a + (b - a) * f
const lerp3 = (a, b, f) => ({ x: round2(lerp(a.x, b.x, f)), y: round2(lerp(a.y, b.y, f)), z: round2(lerp(a.z, b.z, f)) })
const lerpYaw = (a, b, f) => {
  const d = ((((b - a + Math.PI) % TWO_PI) + TWO_PI) % TWO_PI) - Math.PI
  return round2((((a + d * f) % TWO_PI) + TWO_PI) % TWO_PI)
}

// poses [{t, ...}] to poses every 1000/hz ms; unchanged ones (rounded) are skipped except a heartbeat
const resample = (poses, hz) => {
  const step = 1000 / hz
  const out = []
  let lastWritten = null
  for (let t = poses[0].t, i = 0; t <= poses.at(-1).t; t += step) {
    while (i < poses.length - 2 && poses[i + 1].t <= t) i++
    const a = poses[i]
    const b = poses[i + 1]
    const f = Math.min(1, Math.max(0, (t - a.t) / (b.t - a.t)))
    const pose = { ...a, t: Math.round(t), eye: lerp3(a.eye, b.eye, f), pos: lerp3(a.pos, b.pos, f), yaw: lerpYaw(a.yaw, b.yaw, f), pitch: round2(lerp(a.pitch, b.pitch, f)) }
    const key = JSON.stringify([pose.eye, pose.yaw, pose.pitch])
    if (lastWritten && lastWritten.key === key && pose.t - lastWritten.t < HEARTBEAT_MS) continue
    lastWritten = { key, t: pose.t }
    out.push(pose)
  }
  return out
}

const SYNTHETIC_SPEED = 4.317 // blocks per second
const SYNTHETIC_BLOCKS = 20
const SPRINT_SPEED = 5.6
const SPRINT_BLOCKS = 200
const TELEPORT_PERIOD_MS = 6000
const TELEPORT_MIN_DISTANCE = 1000
const COPY_RADIUS = 9
const MIN_COVERAGE = 0.7

// a straight walk along +x from a template pose at exact spacing (the play loop then walks it back)
const synthetic = (template, hz, speed = SYNTHETIC_SPEED, blocks = SYNTHETIC_BLOCKS) => Array.from({ length: Math.round(blocks / speed * hz) + 1 }, (_, k) => {
  const dx = speed * k / hz
  const shift = p => ({ ...p, x: round2(p.x + dx) })
  return { ...template, status: 'online', t: Math.round(k * 1000 / hz), eye: shift(template.eye), pos: shift(template.pos), entities: [] }
})

// ---- places with column files ----

const chunkKey = (cx, cz) => `${cx}.${cz}`
const listColumns = chunksDir => new Set(fs.readdirSync(chunksDir).filter(n => n.endsWith('.bin')).map(n => n.slice(0, -4)))
const rectCells = (x0, x1, z0, z1) => Array.from({ length: x1 - x0 + 1 }, (_, i) => Array.from({ length: z1 - z0 + 1 }, (_, j) => [x0 + i, z0 + j])).flat()
const coverage = (have, cells) => cells.filter(([cx, cz]) => have.has(chunkKey(cx, cz))).length / cells.length

// the column (cx, cz) whose window of `cellsAround(cx, cz)` is best covered by files, among those passing `accept`
const bestPlace = (have, cellsAround, accept = () => true) => {
  let best = null
  for (const key of have) {
    const [cx, cz] = key.split('.').map(Number)
    if (!accept(cx, cz)) continue
    const c = coverage(have, cellsAround(cx, cz))
    if (!best || c > best.coverage) best = { cx, cz, coverage: c }
  }
  if (!best || best.coverage < MIN_COVERAGE) throw new Error(`no place with ${MIN_COVERAGE} column coverage (best ${best?.coverage})`)
  return best
}

const around = (cx, cz) => rectCells(cx - COPY_RADIUS, cx + COPY_RADIUS, cz - COPY_RADIUS, cz + COPY_RADIUS)
const along = blocks => (cx, cz) => rectCells(cx - COPY_RADIUS, cx + Math.ceil(blocks / 16) + COPY_RADIUS, cz - COPY_RADIUS, cz + COPY_RADIUS)

// two well covered places at least TELEPORT_MIN_DISTANCE blocks apart; and a start for a straight stretch of `blocks` along +x
const pickTeleport = have => {
  const a = bestPlace(have, around)
  const far = (cx, cz) => Math.hypot(cx - a.cx, cz - a.cz) * 16 >= TELEPORT_MIN_DISTANCE
  return [a, bestPlace(have, around, far)]
}
const pickSprint = have => bestPlace(have, along(SPRINT_BLOCKS))

const blockCenter = c => c * 16 + 8
const placed = (template, x, z) => ({ ...template, status: 'online', eye: { ...template.eye, x, z }, pos: { ...template.pos, x, z }, entities: [] })

// the body stands still at each place for TELEPORT_PERIOD_MS, heartbeating every HEARTBEAT_MS, then jumps to the other
const teleportSequence = (template, [a, b]) => {
  const at = (place, rel) => ({ ...placed(template, blockCenter(place.cx), blockCenter(place.cz)), rel })
  const beats = Array.from({ length: TELEPORT_PERIOD_MS / HEARTBEAT_MS }, (_, k) => k * HEARTBEAT_MS)
  return [...beats.map(rel => at(a, rel)), ...beats.map(rel => at(b, TELEPORT_PERIOD_MS + rel))]
}

const sprintSequence = (template, start, hz) => {
  const origin = placed(template, blockCenter(start.cx), blockCenter(start.cz))
  return pingPong(synthetic(origin, hz, SPRINT_SPEED, SPRINT_BLOCKS))
}

// copies the column files of `cells` that exist; never links, so nothing in the source is written
const copyColumns = (from, to, cells, have) => {
  fs.mkdirSync(to, { recursive: true })
  const wanted = cells.filter(([cx, cz]) => have.has(chunkKey(cx, cz)))
  for (const [cx, cz] of wanted) fs.copyFileSync(path.join(from, `${chunkKey(cx, cz)}.bin`), path.join(to, `${chunkKey(cx, cz)}.bin`))
  return wanted.length
}

// rewrites the column file next to `eye`'s chunk with its own bytes: a new mtime, so the server sends a column event
const touchBeside = (chunksDir, eye) => {
  const file = path.join(chunksDir, `${chunkKey(Math.floor(eye.x / 16) + 1, Math.floor(eye.z / 16))}.bin`)
  if (!fs.existsSync(file)) return
  const tmp = `${file}.tmp.${process.pid}`
  fs.writeFileSync(tmp, fs.readFileSync(file))
  fs.renameSync(tmp, file)
}

// forward then backward, as relative times in ms
const pingPong = poses => {
  const t0 = poses[0].t
  const forward = poses.map(p => ({ ...p, rel: p.t - t0 }))
  const span = forward.at(-1).rel
  const back = forward.slice(1, -1).reverse().map(p => ({ ...p, rel: 2 * span - p.rel }))
  return [...forward, ...back]
}

const writeAtomic = (file, value) => {
  const tmp = `${file}.tmp.${process.pid}`
  fs.writeFileSync(tmp, JSON.stringify(value))
  fs.renameSync(tmp, file)
}

// a bare --synthetic means walk
const argvWithSyntheticMode = argv => argv.flatMap((a, i) => (a === '--synthetic' && (argv[i + 1] === undefined || argv[i + 1].startsWith('--')) ? [a, 'walk'] : [a]))

const main = () => {
  const repo = path.join(path.dirname(fileURLToPath(import.meta.url)), '..')
  const { values } = parseArgs({
    options: {
      in: { type: 'string' },
      hz: { type: 'string' },
      synthetic: { type: 'string' },
      'touch-ms': { type: 'string' },
      state: { type: 'string', default: path.join(repo, 'state') },
      seconds: { type: 'string' },
      dir: { type: 'string' }
    },
    args: argvWithSyntheticMode(process.argv.slice(2))
  })
  if (!values.in) {
    console.error('usage: node tools/view-pose-replay.mjs --in poses.jsonl [--hz 20] [--synthetic] [--state dir] [--seconds N] [--dir tmpdir]')
    process.exit(2)
  }
  const recorded = fs.readFileSync(values.in, 'utf8').trim().split('\n').map(l => JSON.parse(l).pose).filter(p => p.eye)
  const world = recorded[0].world
  const dir = values.dir ?? fs.mkdtempSync(path.join(os.tmpdir(), 'view-replay-'))
  const chunksDir = path.join(dir, 'worlds', world, 'chunks')
  const sourceChunks = path.join(values.state, 'worlds', world, 'chunks')
  const hz = Number(values.hz ?? 10)
  const mode = values.synthetic
  const copying = mode === 'teleport' || mode === 'sprint'
  const have = copying ? listColumns(sourceChunks) : null
  const picked = mode === 'teleport' ? pickTeleport(have) : mode === 'sprint' ? [pickSprint(have)] : []
  const sequence = {
    teleport: () => teleportSequence(recorded[0], picked),
    sprint: () => sprintSequence(recorded[0], picked[0], hz),
    walk: () => pingPong(synthetic(recorded[0], hz)),
    undefined: () => pingPong(values.hz ? resample(recorded, hz) : recorded)
  }[mode]?.()
  if (!sequence) throw new Error(`unknown --synthetic ${mode}`)
  const period = sequence.at(-1).rel + (sequence[1].rel - sequence[0].rel)

  const viewDir = path.join(bodyDir(dir, world, 'Replay'), 'view')
  fs.mkdirSync(viewDir, { recursive: true })
  fs.mkdirSync(path.join(dir, 'worlds', world), { recursive: true })
  if (copying) {
    const cells = picked.flatMap(p => (mode === 'sprint' ? along(SPRINT_BLOCKS) : around)(p.cx, p.cz))
    const copied = copyColumns(sourceChunks, chunksDir, cells, have)
    console.log(`places: ${JSON.stringify(picked.map(p => ({ ...p, coverage: Math.round(p.coverage * 100) / 100 })))}, copied ${copied} column files`)
  } else {
    // only the chunks and biomes are linked: the body folder lives inside the world folder, and nothing here may write into --state
    fs.symlinkSync(sourceChunks, chunksDir)
    const biomes = path.join(values.state, 'worlds', world, 'biomes.json')
    if (fs.existsSync(biomes)) fs.symlinkSync(biomes, path.join(dir, 'worlds', world, 'biomes.json'))
  }
  console.log(`replay state dir: ${dir}`)

  let lastEye = sequence[0].eye
  const started = Date.now()
  const play = (base, i) => {
    const wait = base + sequence[i].rel - Date.now()
    setTimeout(() => {
      writeAtomic(path.join(viewDir, 'pose.json'), { ...sequence[i], rel: undefined, t: base + sequence[i].rel })
      lastEye = sequence[i].eye
      const next = i + 1
      play(next < sequence.length ? base : base + period, next % sequence.length)
    }, Math.max(0, wait))
  }
  play(started, 0)
  if (values['touch-ms'] && copying) setInterval(() => touchBeside(chunksDir, lastEye), Number(values['touch-ms']))
  if (values.seconds) setTimeout(() => process.exit(0), Number(values.seconds) * 1000)
}
main()
