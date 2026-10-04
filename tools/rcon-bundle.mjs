// Why JavaScript: bootstrapping. Node must load the ahead-of-time compiled cljs bundle (dashboard/out/rcon-tools.cjs) before any cljs can run; this is the one place that does it and explains an unbuilt checkout.
import fs from 'node:fs'
import path from 'node:path'
import { createRequire } from 'node:module'

export const BUNDLE = path.join(import.meta.dirname, '..', 'dashboard', 'out', 'rcon-tools.cjs')
export const BUILD_HINT = 'npm --prefix dashboard run build-rcon-tools'

export function loadRconTools (bundle = BUNDLE) {
  if (!fs.existsSync(bundle)) throw new Error(`the RCON tools bundle is not built (${bundle}); build it once with: ${BUILD_HINT}`)
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
