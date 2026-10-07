// Why JavaScript: Node module-loader boundary; module.enableCompileCache must run before the modules it should cache are loaded.
// Import this first. Caches compiled code (mineflayer, minecraft-protocol, the cljs bundles) under the repo's ignored state/compile-cache.
import { enableCompileCache } from 'node:module'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

export const cacheDir = process.env.MC_COMPILE_CACHE_DIR || path.join(path.dirname(fileURLToPath(import.meta.url)), '..', '..', 'state', 'compile-cache')
export const result = process.env.MC_COMPILE_CACHE === 'off' ? null : enableCompileCache?.(cacheDir)
