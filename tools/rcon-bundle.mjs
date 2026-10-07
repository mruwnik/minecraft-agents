// Why JavaScript: bootstrapping. Node must load the ahead-of-time compiled cljs bundle (dashboard/out/rcon-tools.cjs) before any cljs can run; this is the one place that does it and explains an unbuilt checkout.
import fs from 'node:fs'
import path from 'node:path'
import { createRequire } from 'node:module'

export const BUNDLE = path.join(import.meta.dirname, '..', 'dashboard', 'out', 'rcon-tools.cjs')
export const SOURCE_DIR = path.join(import.meta.dirname, '..', 'dashboard', 'src', 'dashboard')
export const BUILD_HINT = 'tools/compile dashboard rcon-tools --release'

// the newest modification time among the rcon*.cljs sources (the bundle's inputs)
const newestSource = dir => Math.max(0, ...fs.readdirSync(dir).filter(f => /^rcon.*\.cljs$/.test(f)).map(f => fs.statSync(path.join(dir, f)).mtimeMs))

export function loadRconTools (bundle = BUNDLE, sourceDir = SOURCE_DIR) {
  if (!fs.existsSync(bundle)) throw new Error(`the RCON tools bundle is not built (${bundle}); build it once with: ${BUILD_HINT}`)
  if (newestSource(sourceDir) > fs.statSync(bundle).mtimeMs) throw new Error(`the RCON tools bundle is stale (an rcon*.cljs source is newer than ${bundle}); rebuild it with: ${BUILD_HINT}`)
  return createRequire(import.meta.url)(bundle)
}

// runs one of the bundle's *Main entry points with argv and sets the process exit code
export async function runRconTool (entry, argv) {
  try {
    process.exitCode = await loadRconTools()[entry](argv)
  } catch (e) {
    console.error(e.message)
    process.exitCode = 1
  }
}
