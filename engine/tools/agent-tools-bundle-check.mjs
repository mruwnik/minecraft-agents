// Why JavaScript: it checks the AOT cljs bundle from outside before a launcher calls into it; a stale bundle cannot
// report exports it does not yet have, so this check cannot live inside the bundle.
import fs from 'node:fs'
import path from 'node:path'

export const missingExports = (bundle, required) =>
  required.filter(name => bundle?.[name] === undefined)

export const buildRequiredMessage = missing =>
  `The compiled agent-tools bundle is stale; missing exports: ${missing.join(', ')}. Rebuild it once with: cd dashboard && npm run build-agent-tools`

export const buildRequiredEdn = missing =>
  `{:ok false :reason :build-required :missing-exports [${missing.map(name => JSON.stringify(name)).join(' ')}] :message ${JSON.stringify(buildRequiredMessage(missing))}}`

// The newest cljs/cljc source (under the given files or directories) that is newer than the bundle; null when the bundle is current.
export const newerSource = (bundle, roots) => {
  const bundleMs = fs.statSync(bundle).mtimeMs
  let newest = null
  const visit = (file, ms) => {
    if (ms > bundleMs && (!newest || ms > newest.ms)) newest = { file, ms }
  }
  const walk = file => {
    const stat = fs.statSync(file, { throwIfNoEntry: false })
    if (!stat) return
    if (!stat.isDirectory()) return /\.clj[sc]?$/.test(file) && visit(file, stat.mtimeMs)
    fs.readdirSync(file).forEach(name => walk(path.join(file, name)))
  }
  roots.forEach(walk)
  return newest?.file ?? null
}

export const staleBundleMessage = file =>
  `Warning: the compiled agent-tools bundle is older than ${file}; the tools may run old code. Rebuild it with: tools/compile dashboard agent-tools`

// The sources the agent-tools bundle is built from (dashboard/shadow-cljs.edn :agent-tools and the namespaces its exports require).
export const bundleSources = repoRoot => [
  'dashboard/src/agent_tools',
  'dashboard/src/dashboard/agent_plan_tools.cljs',
  'dashboard/src/dashboard/plan_compare.cljs',
  'engine/src/plan',
  'engine/src/engine/bodies.cljs',
].map(p => path.join(repoRoot, p))
