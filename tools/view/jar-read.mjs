// Reads files out of a client jar (a zip): the central directory at the tail lists every entry and where its bytes begin.
// Only what the view tools need: list the names, read one entry.
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import zlib from 'node:zlib'
import { clientVersions, versionsDirs } from '../../src/lib/jar.mjs'

const END_OF_CENTRAL_DIRECTORY = 0x06054b50

const findEnd = buf => {
  // the record is 22 bytes, last in the file, after a comment of up to 64k
  for (let at = buf.length - 22; at >= 0; at--) if (buf.readUInt32LE(at) === END_OF_CENTRAL_DIRECTORY) return at
  throw new Error('no end-of-central-directory record: not a zip')
}

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

// $MC_CLIENT_JAR when set, else the newest plain release the launcher installed; null when there is none
export const findClientJar = ({ env = process.env, home = os.homedir(), platform = process.platform } = {}) => {
  if (env.MC_CLIENT_JAR) return fs.existsSync(env.MC_CLIENT_JAR) ? env.MC_CLIENT_JAR : null
  return versionsDirs(home, platform).filter(fs.existsSync)
    .flatMap(dir => clientVersions(fs.readdirSync(dir)).map(v => path.join(dir, v, `${v}.jar`)))
    .find(fs.existsSync) ?? null
}
