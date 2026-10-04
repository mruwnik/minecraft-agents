// Why JavaScript: it builds the cljs, so it cannot be cljs; a Node launcher that starts the shadow-cljs JVM only when needed.
// Builds tools/view/web/cljs/viewer.mjs (dashboard/shadow-cljs.edn :viewer, `shadow-cljs release viewer`) when it is missing
// or older than a source it is built from. Run by the root `npm test` (pretest) and the dashboard launcher; by hand:
//   node tools/view/build-cljs.mjs [--force]
import { spawnSync } from 'node:child_process'
import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

const repo = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..', '..')
const dashboard = path.join(repo, 'dashboard')
export const output = path.join(repo, 'tools', 'view', 'web', 'cljs', 'viewer.mjs')
// what the :viewer build reads: its namespaces and the build config
const sources = [path.join(dashboard, 'src', 'view'), path.join(dashboard, 'src', 'drive'), path.join(dashboard, 'shadow-cljs.edn')]
const MIN_MEMORY_MB = 3500

// true when there is no output or a source is newer than it (mtimes in ms; output null when missing)
export const needsBuild = (outputMtime, sourceMtimes) => outputMtime === null || sourceMtimes.some(t => t > outputMtime)

const mtimeOrNull = file => fs.statSync(file, { throwIfNoEntry: false })?.mtimeMs ?? null
const filesUnder = p => fs.statSync(p).isDirectory() ? fs.readdirSync(p).flatMap(name => filesUnder(path.join(p, name))) : [p]
const availableMb = () => Number(/MemAvailable:\s+(\d+)/.exec(fs.readFileSync('/proc/meminfo', 'utf8'))?.[1] ?? 0) / 1024

const main = () => {
  const force = process.argv.includes('--force')
  if (!force && !needsBuild(mtimeOrNull(output), sources.flatMap(filesUnder).map(mtimeOrNull))) return 0
  const mb = Math.round(availableMb())
  if (mb < MIN_MEMORY_MB) {
    console.error(`viewer cljs build needed but refused: ${mb} MB available, ${MIN_MEMORY_MB} MB needed`)
    return 1
  }
  console.error('building the viewer cljs (shadow-cljs release viewer)')
  const run = spawnSync('flock', ['/tmp/mc-compile.lock', 'npx', 'shadow-cljs', 'release', 'viewer'], { cwd: dashboard, stdio: 'inherit' })
  return run.status ?? 1
}

if (process.argv[1] === fileURLToPath(import.meta.url)) process.exitCode = main()
