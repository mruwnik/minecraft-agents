// Why JavaScript: binary file I/O of typed arrays at the Node boundary; the table itself is built in cljs.
// Keeps a table made of typed arrays and Maps of small arrays (the planner's block table, ~1 MB) on disk, so a body start reads it
// instead of walking every block state. The caller names the file whose build the table depends on; the data packages join the key.
import { createHash } from 'node:crypto'
import { createRequire } from 'node:module'
import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

const require = createRequire(import.meta.url)
const here = path.dirname(fileURLToPath(import.meta.url))
const dir = path.join(process.env.MC_COMPILE_CACHE_DIR || path.join(here, '..', '..', 'state', 'compile-cache'), 'tables')
const TYPES = { Uint8Array, Uint16Array, Uint32Array, Int32Array, Float32Array, Float64Array }
const DATA_PACKAGES = ['minecraft-data', 'prismarine-block', 'prismarine-registry']

const hashOf = (x) => createHash('sha1').update(JSON.stringify(x)).digest('hex').slice(0, 12)

// One file per build file path, so a rebuilt bundle replaces its table instead of piling up files.
function keyOf (name, buildFile) {
  const { mtimeMs, size } = fs.statSync(buildFile)
  const versions = DATA_PACKAGES.map(p => require(`${p}/package.json`).version)
  const prefix = `${name}-${hashOf(buildFile)}-`
  return { prefix, file: `${prefix}${hashOf([mtimeMs, size, versions])}.bin` }
}

export function encode (table) {
  const arrays = []
  const maps = {}
  const parts = []
  for (const [k, v] of Object.entries(table)) {
    if (v instanceof Map) { maps[k] = [...v]; continue }
    arrays.push([k, v.constructor.name, v.length])
    parts.push(Buffer.from(v.buffer, v.byteOffset, v.byteLength))
  }
  const header = Buffer.from(JSON.stringify({ arrays, maps }))
  const len = Buffer.alloc(4)
  len.writeUInt32LE(header.length)
  return Buffer.concat([len, header, ...parts])
}

export function decode (buf) {
  const headerEnd = 4 + buf.readUInt32LE(0)
  const { arrays, maps } = JSON.parse(buf.toString('utf8', 4, headerEnd))
  const table = {}
  let at = headerEnd
  for (const [k, type, length] of arrays) {
    const T = TYPES[type]
    const bytes = length * T.BYTES_PER_ELEMENT
    if (at + bytes > buf.length) throw new Error('truncated table')
    table[k] = new T(buf.buffer.slice(buf.byteOffset + at, buf.byteOffset + at + bytes))
    at += bytes
  }
  for (const [k, entries] of Object.entries(maps)) table[k] = new Map(entries)
  return table
}

// buildFile: the file whose change must drop the cached table (the bundle that holds the building code).
export function tableCached (name, buildFile, build, cacheDir = dir) {
  if (process.env.MC_COMPILE_CACHE === 'off' || !buildFile || !fs.existsSync(buildFile)) return build()
  const key = keyOf(name, buildFile)
  const file = path.join(cacheDir, key.file)
  if (fs.existsSync(file)) {
    try { return decode(fs.readFileSync(file)) } catch { /* a damaged file is rebuilt below */ }
  }
  const table = build()
  const tmp = `${file}.${process.pid}.tmp`
  try {
    fs.mkdirSync(cacheDir, { recursive: true })
    fs.writeFileSync(tmp, encode(table))
    fs.renameSync(tmp, file)
    for (const f of fs.readdirSync(cacheDir)) if (f.startsWith(key.prefix) && f !== key.file) fs.rmSync(path.join(cacheDir, f), { force: true })
  } catch { /* an unwritable cache only costs the build next time */ }
  return table
}
