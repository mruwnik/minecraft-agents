// Why JavaScript: worker thread; builds one version's view assets off the main thread and transfers the buffers.
// The build thread of view-assets.mjs: builds one version's assets, posts them (the buffers are transferred, not copied) and exits.
import { parentPort, workerData } from 'node:worker_threads'
import { buildAssets, transferable } from './view-assets.mjs'

const { version, textureDir, jarPath } = workerData
try {
  const { table, textures, elements } = buildAssets(version, textureDir, jarPath)
  const [t, e] = [transferable(textures), transferable(elements)]
  parentPort.postMessage({ table, textures: t, elements: e }, [t.buffer, e.buffer])
} catch (error) {
  parentPort.postMessage({ error: error?.message ?? String(error) })
}
