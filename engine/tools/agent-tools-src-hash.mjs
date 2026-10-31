// Why JavaScript: tools/compile and tools/agent-tools-stale call it from bash; it shares the hash with the loader's stale check.
import { bundleSources, sourceHash } from './agent-tools-bundle-check.mjs'

console.log(sourceHash(bundleSources(process.argv[2])))
