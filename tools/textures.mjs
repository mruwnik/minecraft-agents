// textures/ is Mojang's art, so it is not checked in: this fills it (blocks, and items under item/) from a client jar the first time a body starts
// without it. tools/start-body runs it beside patch-deps.mjs. It never stops a body from starting: with no textures
// `./mc look` still draws, colouring every block by a hash of its name instead of its picture.
//   node tools/textures.mjs
// The jar it reads: $MC_CLIENT_JAR when set, else the newest plain release the launcher installed (<versions>/<v>/<v>.jar).
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import zlib from 'node:zlib'
import { clientVersions, versionsDirs, jarTextures } from '../src/lib.mjs'

const ROOT = path.join(import.meta.dirname, '..')
const TEXTURES = path.join(ROOT, 'textures')
const VERSIONS = versionsDirs(os.homedir(), process.platform)
const END_OF_CENTRAL_DIRECTORY = 0x06054b50

// A zip is read from the end: the central directory at its tail lists every entry and where its bytes begin.
function zipEntries (buf) {
  const eocd = (() => {
    // the record is 22 bytes, last in the file, after a comment of up to 64k
    for (let at = buf.length - 22; at >= 0; at--) if (buf.readUInt32LE(at) === END_OF_CENTRAL_DIRECTORY) return at
    return -1
  })()
  if (eocd < 0) throw new Error('no end-of-central-directory record: not a zip')
  const count = buf.readUInt16LE(eocd + 10)
  const entries = []
  let at = buf.readUInt32LE(eocd + 16)
  for (let i = 0; i < count; i++) {
    const [nameLength, extraLength, commentLength] = [28, 30, 32].map(o => buf.readUInt16LE(at + o))
    entries.push({
      name: buf.toString('utf8', at + 46, at + 46 + nameLength),
      method: buf.readUInt16LE(at + 10),
      size: buf.readUInt32LE(at + 20),
      offset: buf.readUInt32LE(at + 42)
    })
    at += 46 + nameLength + extraLength + commentLength
  }
  return entries
}

const contentOf = (buf, entry) => {
  const [nameLength, extraLength] = [26, 28].map(o => buf.readUInt16LE(entry.offset + o))
  const from = entry.offset + 30 + nameLength + extraLength
  const stored = buf.subarray(from, from + entry.size)
  return entry.method === 0 ? stored : zlib.inflateRawSync(stored)
}

const pngsIn = dir => fs.existsSync(dir) ? fs.readdirSync(dir).filter(f => f.endsWith('.png')) : []
const jarOf = (dir, version) => path.join(dir, version, `${version}.jar`)
const installedJar = () => VERSIONS.filter(fs.existsSync)
  .flatMap(dir => clientVersions(fs.readdirSync(dir)).map(v => jarOf(dir, v)))
  .find(fs.existsSync)

// blocks lie flat in textures/, where src/vision/eyes.mjs reads them; items go under item/ for the dashboard's inventory
// screen. Each kind is filled on its own, so a textures/ from before items were extracted still gets them
const KINDS = [['block', TEXTURES], ['item', path.join(TEXTURES, 'item')]]
const missing = KINDS.filter(([kind, dir]) => {
  const already = pngsIn(dir).length
  if (already) console.log(`[textures] ${kind}: already ${already}`)
  return !already
})
if (!missing.length) process.exit(0)

const jar = process.env.MC_CLIENT_JAR ?? installedJar()
if (!jar || !fs.existsSync(jar)) {
  console.log(`[textures] WARNING: no client jar found, so pictures will show every block as a colour hashed from its name.
[textures] Two ways to give it one: set MC_CLIENT_JAR=/path/to/<version>.jar in the environment a body starts in,
[textures] or install any plain release with the launcher so that one of these holds <version>/<version>.jar: ${VERSIONS.join(', ')}`)
  process.exit(0)
}

// a jar that is there but unreadable is still not a reason to leave the agent without a body
try {
  const buf = fs.readFileSync(jar)
  const entries = zipEntries(buf)
  for (const [kind, dir] of missing) {
    const wanted = new Set(jarTextures(entries.map(e => e.name), kind))
    const files = entries.filter(e => wanted.has(e.name))
    fs.mkdirSync(dir, { recursive: true })
    for (const entry of files) fs.writeFileSync(path.join(dir, path.basename(entry.name)), contentOf(buf, entry))
    console.log(`[textures] ${kind}: extracted ${files.length} from ${jar}`)
  }
} catch (e) {
  console.log(`[textures] WARNING: ${jar} could not be read (${e.message}); pictures will show every block as a colour hashed from its name`)
}
