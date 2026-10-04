// Why JavaScript: worker/binary; builds the material, texture and element tables (about 150 MB of heap) in a worker.
// The view's material table, texture layers and element table for one Minecraft version. Building them costs ~150 MB of heap and several
// seconds, so the server builds them in a worker thread that exits afterwards (its heap goes back to the system) and keeps only the bytes.
import { Worker } from 'node:worker_threads'
import { textureBytes } from './materials.mjs'
import { columnFormat } from './web-format.mjs'

// One build gives the table, the texture bytes and the element table, so layer indices always agree.
export const buildAssets = (version, textureDir, jarPath) => {
  const { table, textures, elements } = textureBytes(version, textureDir, { jarPath })
  return {
    table: JSON.stringify({ ...table, format: columnFormat(version) }),
    textures: Buffer.from(textures.bytes),
    elements: elements ? Buffer.from(elements.buffer, elements.byteOffset, elements.byteLength) : Buffer.alloc(0)
  }
}

// A transfer moves a whole ArrayBuffer, so a view that covers only part of one (a pooled Buffer, a slice) is copied first.
export const transferable = bytes => bytes.byteOffset === 0 && bytes.byteLength === bytes.buffer.byteLength ? bytes : Uint8Array.from(bytes)

const fromTransferred = bytes => Buffer.from(bytes.buffer, bytes.byteOffset, bytes.byteLength)

export const buildAssetsInWorker = (version, textureDir, jarPath) => new Promise((resolve, reject) => {
  const worker = new Worker(new URL('./view-assets-worker.mjs', import.meta.url), { workerData: { version, textureDir, jarPath } })
  let settled = false
  const settle = (done, value) => {
    if (settled) return
    settled = true
    done(value)
  }
  worker.once('message', ({ error, table, textures, elements }) => {
    if (error) return settle(reject, new Error(error))
    settle(resolve, { table, textures: fromTransferred(textures), elements: fromTransferred(elements) })
  })
  worker.once('error', error => settle(reject, error))
  worker.once('exit', code => settle(reject, new Error(`view asset worker exited with code ${code}`)))
})

// version -> promise of its build: concurrent requests share one build, a failed one is forgotten so the next request retries.
export const createAssetCache = build => {
  const builds = new Map()
  return version => {
    if (builds.has(version)) return builds.get(version)
    const promise = build(version).catch(error => {
      builds.delete(version)
      throw error
    })
    builds.set(version, promise)
    return promise
  }
}
