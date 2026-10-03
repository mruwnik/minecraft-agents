// Terrain drawn by a pool of worker threads, in bands of rows into shared memory; the caller draws entities after
// (raycaster.mjs drawEntities). render() is synchronous: it blocks the calling thread in Atomics.wait until every worker
// has reported done, which Node allows on the main thread and which leaves the workers' message delivery unaffected.
import { Worker, MessageChannel, receiveMessageOnPort } from 'node:worker_threads'

export const BAND = 4
// slots of the shared control array
export const NEXT_ROW = 0 // the next band's first row, claimed with Atomics.add
export const DONE = 1 // workers finished with the frame
export const ERROR = 2 // 1 when some worker failed; its message is on its error port
export const NEAR_PIXELS = 3 // pixels with terrain closer than NEAR, summed over workers
const SLOTS = 4
const TIMEOUT_MS = 30000

const shared = (Type, length) => new Type(new SharedArrayBuffer(length * Type.BYTES_PER_ELEMENT))

export function renderPool ({ threads, version, textureDir, timeoutMs = TIMEOUT_MS }) {
  const control = shared(Int32Array, SLOTS)
  const ports = []
  const workers = Array.from({ length: threads }, () => {
    const { port1, port2 } = new MessageChannel()
    ports.push(port1)
    const worker = new Worker(new URL('./raycast-worker.mjs', import.meta.url), { workerData: { version, textureDir, control, errors: port2 }, transferList: [port2] })
    worker.unref()
    return worker
  })
  let buffers = null
  let closed = false

  const bufferFor = (width, height) => {
    if (buffers?.width === width && buffers?.height === height) return buffers
    buffers = { width, height, rgba: shared(Uint8Array, width * height * 4), depth: shared(Float64Array, width * height) }
    return buffers
  }
  const workerError = () => ports.map(p => receiveMessageOnPort(p)?.message).find(Boolean) ?? 'a raycast worker failed'

  const waitDone = () => {
    const deadline = Date.now() + timeoutMs
    for (let seen = Atomics.load(control, DONE); seen < threads; seen = Atomics.load(control, DONE)) {
      const left = deadline - Date.now()
      if (left <= 0) { close(); throw new Error(`raycast pool timed out after ${timeoutMs} ms`) }
      Atomics.wait(control, DONE, seen, left)
    }
  }

  // The returned rgba and depth are views over the pool's shared buffers: the next render() overwrites them.
  function render ({ grid, info: _unused, entities: _entities, ...params }) {
    if (closed) throw new Error('the raycast pool is closed')
    const { rgba, depth } = bufferFor(params.width, params.height)
    Atomics.store(control, NEXT_ROW, 0)
    Atomics.store(control, DONE, 0)
    Atomics.store(control, ERROR, 0)
    Atomics.store(control, NEAR_PIXELS, 0)
    const frame = { grid: { origin: grid.origin, size: grid.size, data: grid.data, top: grid.top }, params, rgba, depth, band: BAND }
    for (const w of workers) w.postMessage(frame)
    waitDone()
    if (Atomics.load(control, ERROR)) throw new Error(workerError())
    return { rgba, depth, near: Atomics.load(control, NEAR_PIXELS) / (params.width * params.height) }
  }

  function close () {
    closed = true
    for (const w of workers) w.terminate()
    for (const p of ports) p.close()
  }

  return { render, close, get closed () { return closed } }
}

const pools = new Map()

export function poolFor ({ threads, version, textureDir }) {
  const key = `${version}|${textureDir}|${threads}`
  if (pools.get(key)?.closed === false) return pools.get(key)
  const pool = renderPool({ threads, version, textureDir })
  pools.set(key, pool)
  return pool
}

export function closePools () {
  for (const pool of pools.values()) pool.close()
  pools.clear()
}
