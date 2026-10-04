// Why JavaScript: stable entry point for the AOT cljs tools (compilation is a build step, never command startup); Thin Node launcher over the AOT cljs bundle dashboard/out/agent-tools.cjs; runs without starting a compiler or JVM (500 ms startup budget).
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
