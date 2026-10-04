// Stable Node entry point for the ahead-of-time compiled ClojureScript tools.
// Compilation is an explicit build step, never part of command startup.
import { createRequire } from 'node:module'

const require = createRequire(import.meta.url)
let tools
try {
  tools = require('../../dashboard/out/agent-tools.cjs')
} catch (error) {
  if (error.code !== 'MODULE_NOT_FOUND' || !error.message.includes('agent-tools.cjs')) throw error
  process.stderr.write('Build the agent tools once: cd dashboard && npm run build-agent-tools\n')
  process.stdout.write('{:ok false :reason :build-required :message "Run cd dashboard && npm run build-agent-tools"}\n')
  process.exit(2)
}
export default tools
