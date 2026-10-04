// Reads files out of a client jar (a zip): the central directory at the tail lists every entry and where its bytes begin.
// Only what the view tools need: list the names, read one entry.
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import zlib from 'node:zlib'
import { clientVersions, versionsDirs } from '../jar.mjs'

const END_OF_CENTRAL_DIRECTORY = 0x06054b50

const findEnd = buf => {
  // the record is 22 bytes, last in the file, after a comment of up to 64k
  for (let at = buf.length - 22; at >= 0; at--) if (buf.readUInt32LE(at) === END_OF_CENTRAL_DIRECTORY) return at
  throw new Error('no end-of-central-directory record: not a zip')
}

// zipEntries and entryContent take the jar's BYTES (a Buffer, e.g. fs.readFileSync(path)), not a path; openJar(path) is the path version.
export function zipEntries (buf) {
  const eocd = findEnd(buf)
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

export const entryContent = (buf, entry) => {
  const [nameLength, extraLength] = [26, 28].map(o => buf.readUInt16LE(entry.offset + o))
  const from = entry.offset + 30 + nameLength + extraLength
  const stored = buf.subarray(from, from + entry.size)
  return entry.method === 0 ? stored : zlib.inflateRawSync(stored)
}

const RELEASE = /^\d+(\.\d+)*$/
// newest first, comparing the dotted numbers (26.1.2 is newer than 1.21.8)
const versionOrder = (a, b) => {
  const [x, y] = [a.split('.').map(Number), b.split('.').map(Number)]
  return [...Array(Math.max(x.length, y.length)).keys()].map(i => (y[i] ?? 0) - (x[i] ?? 0)).find(d => d !== 0) ?? 0
}

// the jars the agents' own cache holds (~/.cache/minecraft-agents/client/<version>.jar) as { version, file }
const cachedJars = home => {
  const dir = path.join(home, '.cache/minecraft-agents/client')
  if (!fs.existsSync(dir)) return []
  return fs.readdirSync(dir).filter(n => n.endsWith('.jar')).map(n => ({ version: n.slice(0, -4), file: path.join(dir, n) }))
}

const launcherJars = (home, platform) => versionsDirs(home, platform).filter(fs.existsSync)
  .flatMap(dir => clientVersions(fs.readdirSync(dir)).map(version => ({ version, file: path.join(dir, version, `${version}.jar`) })))

// $MC_CLIENT_JAR when set, else the newest plain release across the launcher's versions dirs and the agents' cache; null when there is none
export const findClientJar = ({ env = process.env, home = os.homedir(), platform = process.platform } = {}) => {
  if (env.MC_CLIENT_JAR) return fs.existsSync(env.MC_CLIENT_JAR) ? env.MC_CLIENT_JAR : null
  const found = [...launcherJars(home, platform), ...cachedJars(home)].filter(j => RELEASE.test(j.version) && fs.existsSync(j.file))
  return found.sort((a, b) => versionOrder(a.version, b.version))[0]?.file ?? null
}

// A jar by path: { names, has(name), read(name) -> Buffer | null } over one read of the file
export const openJar = jarPath => {
  const buf = fs.readFileSync(jarPath)
  const entries = new Map(zipEntries(buf).map(entry => [entry.name, entry]))
  return {
    names: [...entries.keys()],
    has: name => entries.has(name),
    read: name => entries.has(name) ? entryContent(buf, entries.get(name)) : null
  }
}
