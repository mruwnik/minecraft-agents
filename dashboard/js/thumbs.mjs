// Renders one still of a body's view on request, in a worker thread (tools/view/render.mjs, software rendering, the fallback for
// browsers without WebGL2). No scheduling and no cache here: when to render, what to keep and when to replace the worker
// are decided in dashboard.thumbs (cljs). Plumbing only: the worker handle and the pending requests.
import { Worker } from 'node:worker_threads'

const workerFile = new URL('./thumbs-worker.mjs', import.meta.url)

// Heap caps of the worker thread (its column cache lives in it).
export const workerLimits = { maxOldGenerationSizeMb: 160, maxYoungGenerationSizeMb: 24 }

export class RenderError extends Error {
  constructor (message, name) {
    super(message)
    this.name = name
  }
}

// createRenderer({stateDir, width, height, maxDist}) -> {render(name), recycle(), close()}
//   render(name) -> Promise<{png: Buffer, ms, loaded} | null>   (null: the body has no usable pose; other failures reject)
//   recycle()     ends the worker; the next render starts a fresh one with an empty column cache
//   close()       ends the worker
// `loaded` is the number of columns the worker's cache holds after the render.
export function createRenderer ({ stateDir, width = 320, height = 180, maxDist = 48 }) {
  let worker = null
  let nextId = 0
  const pending = new Map()

  const fail = (w, err) => {
    if (worker !== w) return // a worker that was replaced
    pending.forEach(({ reject }) => reject(err))
    pending.clear()
    worker = null
  }
  const start = () => {
    const w = new Worker(workerFile, { resourceLimits: workerLimits })
    worker = w
    w.unref()
    w.on('message', ({ id, png, ms, loaded, error, errorName }) => {
      const { resolve, reject } = pending.get(id)
      pending.delete(id)
      if (errorName === 'PoseError') return resolve(null)
      if (error !== undefined) return reject(new RenderError(error, errorName))
      resolve({ png: Buffer.from(png.buffer, png.byteOffset, png.byteLength), ms, loaded })
    })
    w.on('error', err => fail(w, err))
    w.on('exit', () => fail(w, new Error('thumbnail worker exited')))
  }

  const render = name => new Promise((resolve, reject) => {
    if (!worker) start()
    const id = nextId++
    pending.set(id, { resolve, reject })
    worker.postMessage({ id, name, stateDir, width, height, maxDist })
  })
  const close = () => {
    const w = worker
    worker = null
    w?.terminate()
  }
  return { render, recycle: close, close }
}
