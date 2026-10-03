// Replays a recorded pose stream (tools/view-pose-record.mjs) as agent "Replay" in a temp state dir, so the browser view
// can be measured on a deterministic input. Prints the state dir; serve it with `node tools/view-serve.mjs --state <dir>`.
//   node tools/view-pose-replay.mjs --in poses.jsonl [--hz 20] [--synthetic] [--state state] [--seconds N]
// --synthetic ignores the recording except for its first pose: a straight walk at 4.317 blocks/s, 20 blocks each way, at --hz.
// Poses are written atomically at the recorded spacing with t rewritten to now. The recording plays forward then backward
// so the loop has no jump. --hz resamples eye/pos/yaw/pitch by linear interpolation (changes only, 2 s heartbeat).
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { parseArgs } from 'node:util'
import { fileURLToPath } from 'node:url'

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

// a straight walk along +x from a template pose at exact spacing (the play loop then walks it back)
const synthetic = (template, hz) => Array.from({ length: Math.round(SYNTHETIC_BLOCKS / SYNTHETIC_SPEED * hz) + 1 }, (_, k) => {
  const dx = SYNTHETIC_SPEED * k / hz
  const shift = p => ({ ...p, x: round2(p.x + dx) })
  return { ...template, status: 'online', t: Math.round(k * 1000 / hz), eye: shift(template.eye), pos: shift(template.pos), entities: [] }
})

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

const main = () => {
  const repo = path.join(path.dirname(fileURLToPath(import.meta.url)), '..')
  const { values } = parseArgs({
    options: {
      in: { type: 'string' },
      hz: { type: 'string' },
      synthetic: { type: 'boolean', default: false },
      state: { type: 'string', default: path.join(repo, 'state') },
      seconds: { type: 'string' },
      dir: { type: 'string' }
    }
  })
  if (!values.in) {
    console.error('usage: node tools/view-pose-replay.mjs --in poses.jsonl [--hz 20] [--synthetic] [--state dir] [--seconds N] [--dir tmpdir]')
    process.exit(2)
  }
  const recorded = fs.readFileSync(values.in, 'utf8').trim().split('\n').map(l => JSON.parse(l).pose).filter(p => p.eye)
  const poses = values.synthetic ? synthetic(recorded[0], Number(values.hz ?? 10)) : values.hz ? resample(recorded, Number(values.hz)) : recorded
  const sequence = pingPong(poses)
  const period = sequence.at(-1).rel + (sequence[1].rel - sequence[0].rel)

  const dir = values.dir ?? fs.mkdtempSync(path.join(os.tmpdir(), 'view-replay-'))
  const viewDir = path.join(dir, 'agents', 'Replay', 'view')
  fs.mkdirSync(viewDir, { recursive: true })
  fs.mkdirSync(path.join(dir, 'worlds'), { recursive: true })
  fs.symlinkSync(path.join(values.state, 'worlds', recorded[0].world), path.join(dir, 'worlds', recorded[0].world))
  console.log(`replay state dir: ${dir}`)

  const started = Date.now()
  const play = (base, i) => {
    const wait = base + sequence[i].rel - Date.now()
    setTimeout(() => {
      writeAtomic(path.join(viewDir, 'pose.json'), { ...sequence[i], rel: undefined, t: base + sequence[i].rel })
      const next = i + 1
      play(next < sequence.length ? base : base + period, next % sequence.length)
    }, Math.max(0, wait))
  }
  play(started, 0)
  if (values.seconds) setTimeout(() => process.exit(0), Number(values.seconds) * 1000)
}
main()
