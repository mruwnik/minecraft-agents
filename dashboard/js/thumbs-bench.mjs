// node dashboard/js/thumbs-bench.mjs [stateDir]: render every body with a view once and print the cost.
import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { createRenderer } from './thumbs.mjs'

const repo = path.join(path.dirname(fileURLToPath(import.meta.url)), '..', '..')
const stateDir = path.resolve(process.argv[2] ?? path.join(repo, 'state'))
const names = fs.readdirSync(path.join(stateDir, 'agents')).filter(n => fs.existsSync(path.join(stateDir, 'agents', n, 'view', 'pose.json')))
const renderer = createRenderer({ stateDir })
const started = performance.now()
let bytes = 0
for (const name of names) {
  const t0 = performance.now()
  const result = await renderer.render(name).catch(err => ({ error: err.message }))
  const wall = performance.now() - t0
  if (!result?.png) {
    console.log(`${name.padEnd(20)} ${result?.error ?? 'no thumbnail'}`)
    continue
  }
  bytes += result.png.length
  console.log(`${name.padEnd(20)} ${wall.toFixed(0).padStart(5)} ms wall  ${String(result.png.length).padStart(7)} bytes  ${result.loaded} columns`)
}
console.log(`total ${(performance.now() - started).toFixed(0)} ms, ${bytes} bytes, ${names.length} bodies`)
renderer.close()
