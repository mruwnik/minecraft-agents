// Records an agent's pose.json changes to JSONL, one {mtime, pose} per change. Reads only.
//   node tools/view-pose-record.mjs --agent ProbeMove --seconds 45 --out poses.jsonl [--state state] [--poll 10]
import fs from 'node:fs'
import path from 'node:path'
import { parseArgs } from 'node:util'
import { fileURLToPath } from 'node:url'

const repo = path.join(path.dirname(fileURLToPath(import.meta.url)), '..')
const { values } = parseArgs({
  options: {
    agent: { type: 'string' },
    seconds: { type: 'string', default: '45' },
    out: { type: 'string' },
    state: { type: 'string', default: path.join(repo, 'state') },
    poll: { type: 'string', default: '10' }
  }
})
if (!values.agent || !values.out) {
  console.error('usage: node tools/view-pose-record.mjs --agent NAME --out file.jsonl [--seconds 45] [--state dir] [--poll ms]')
  process.exit(2)
}

const file = path.join(values.state, 'agents', values.agent, 'view', 'pose.json')
const mtimeOf = () => {
  try {
    return fs.statSync(file).mtimeMs
  } catch {
    return null
  }
}
const readPose = () => {
  try {
    return JSON.parse(fs.readFileSync(file, 'utf8'))
  } catch {
    return null // a read racing a rename: the next poll sees it
  }
}

const lines = []
let seen = null
const poll = () => {
  const mtime = mtimeOf()
  if (mtime === null || mtime === seen) return
  const pose = readPose()
  if (!pose) return
  seen = mtime
  lines.push(JSON.stringify({ mtime, pose }))
}

const timer = setInterval(poll, Number(values.poll))
setTimeout(() => {
  clearInterval(timer)
  fs.writeFileSync(values.out, lines.map(l => `${l}\n`).join(''))
  console.log(`recorded ${lines.length} poses to ${values.out}`)
}, Number(values.seconds) * 1000)
