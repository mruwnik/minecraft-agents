// node --experimental-vm-modules tools/link-check.mjs <file.mjs>...: links each file's whole import graph without running
// any of it, so a name imported from a module that does not export it is caught before a body starts on it (the body's
// own entry, src/bot.mjs, connects to the server the moment it is evaluated). Prints the first broken link, exit 1.
import fs from 'node:fs'
import path from 'node:path'
import vm from 'node:vm'
import { pathToFileURL } from 'node:url'

const cache = new Map()
const context = vm.createContext({})
async function load (specifier, from) {
  const local = specifier.startsWith('.') || specifier.startsWith('/')
  const key = local ? path.resolve(path.dirname(from), specifier) : specifier
  if (cache.has(key)) return cache.get(key)
  const mod = local
    ? new vm.SourceTextModule(fs.readFileSync(key, 'utf8'), { identifier: pathToFileURL(key).href, context, initializeImportMeta: () => {} })
    : await import(specifier).then(ns => new vm.SyntheticModule(Object.keys(ns), () => {}, { identifier: specifier, context }))
  cache.set(key, mod)
  return mod
}

let failed = 0
for (const file of process.argv.slice(2)) {
  const entry = await load(path.resolve(file), '/')
  try {
    await entry.link((specifier, referencing) => load(specifier, new URL(referencing.identifier).pathname))
  } catch (e) {
    console.log(`${file}: ${e.message}`)
    failed++
  }
}
process.exit(failed ? 1 : 0)
