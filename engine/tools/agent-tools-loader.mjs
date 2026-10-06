// Why JavaScript: the one launcher over the AOT cljs bundle dashboard/out/agent-tools.cjs; it loads compiled code only, so there is no compiler or JVM start (500 ms startup budget).
import { createRequire } from 'node:module'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { bundleSources, buildRequiredEdn, buildRequiredMessage, missingExports, newerSource, staleBundleMessage } from './agent-tools-bundle-check.mjs'

const require = createRequire(import.meta.url)
// AGENT_TOOLS_BUNDLE points at another compiled bundle (a scratch one for trying a stale bundle).
const bundlePath = process.env.AGENT_TOOLS_BUNDLE || '../../dashboard/out/agent-tools.cjs'
// Only the bundle itself missing means "build it"; a missing module inside the bundle is a real error.
const resolveBundle = () => {
  try {
    return require.resolve(bundlePath)
  } catch (error) {
    if (error.code !== 'MODULE_NOT_FOUND') throw error
    process.stderr.write('Build the agent tools once: cd dashboard && npm run build-agent-tools\n')
    process.stdout.write('{:ok false :reason :build-required :message "Run cd dashboard && npm run build-agent-tools"}\n')
    process.exit(2)
  }
}
const bundleFile = resolveBundle()
const tools = require(bundleFile)

// A cheap mtime scan (no compile); stderr only, so the tool's stdout stays clean.
{
  const repoRoot = path.join(path.dirname(fileURLToPath(import.meta.url)), '..', '..')
  const newer = newerSource(bundleFile, bundleSources(repoRoot))
  if (newer) process.stderr.write(`${staleBundleMessage(path.relative(repoRoot, newer))}\n`)
}

// Each launcher names the exports it calls; only those are checked, so an unfinished tool breaks only itself.
export const loadTools = required => {
  const missing = missingExports(tools, required)
  if (!missing.length) return tools
  process.stderr.write(`${buildRequiredMessage(missing)}\n`)
  process.stdout.write(`${buildRequiredEdn(missing)}\n`)
  process.exit(2)
}

export default tools
