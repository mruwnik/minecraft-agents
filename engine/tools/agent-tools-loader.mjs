// Why JavaScript: the one launcher over the AOT cljs bundle dashboard/out/agent-tools.cjs; it loads compiled code only, so there is no compiler or JVM start (500 ms startup budget).
import { createRequire } from 'node:module'
import { buildRequiredEdn, buildRequiredMessage, missingExports } from './agent-tools-bundle-check.mjs'

const require = createRequire(import.meta.url)
// AGENT_TOOLS_BUNDLE points at another compiled bundle (a scratch one for trying a stale bundle).
const bundlePath = process.env.AGENT_TOOLS_BUNDLE || '../../dashboard/out/agent-tools.cjs'
let tools
try {
  tools = require(bundlePath)
} catch (error) {
  if (error.code !== 'MODULE_NOT_FOUND' || !error.message.includes('agent-tools.cjs')) throw error
  process.stderr.write('Build the agent tools once: cd dashboard && npm run build-agent-tools\n')
  process.stdout.write('{:ok false :reason :build-required :message "Run cd dashboard && npm run build-agent-tools"}\n')
  process.exit(2)
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
