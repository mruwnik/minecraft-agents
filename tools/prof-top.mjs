// Why JavaScript: reads V8 .cpuprofile JSON for tools/test-run.mjs (a Node launcher); also a CLI: node tools/prof-top.mjs <file.cpuprofile> [k]
import fs from 'node:fs'
import { fileURLToPath } from 'node:url'

// Self time per function (name + url:line), in ms, biggest first. A profile's samples[i] is a node id; timeDeltas[i] is the time since the previous sample.
export const topSelf = (profile, k = 10) => {
  const byId = new Map(profile.nodes.map((n) => [n.id, n]))
  const ms = new Map()
  profile.samples.forEach((id, i) => {
    const cf = byId.get(id).callFrame
    const key = `${cf.functionName || '(anonymous)'} ${cf.url.split('/').slice(-2).join('/')}:${cf.lineNumber + 1}`
    ms.set(key, (ms.get(key) ?? 0) + (profile.timeDeltas[i] ?? 0) / 1000)
  })
  return [...ms].sort((a, b) => b[1] - a[1]).slice(0, k).map(([key, t]) => `${String(Math.round(t)).padStart(8)} ms  ${key}`)
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const [file, k] = process.argv.slice(2)
  if (!file) { console.error('usage: node tools/prof-top.mjs <file.cpuprofile> [k]'); process.exit(2) }
  topSelf(JSON.parse(fs.readFileSync(file, 'utf8')), Number(k) || 10).forEach((l) => console.log(l))
}
