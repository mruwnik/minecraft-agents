// Decodes column files off the main thread. Jobs wait in a queue here, and whenever a worker is idle the queued job with
// the lowest priority(key) is sent to it, evaluated at that moment, so the camera's current position drives the order.
// Without workers (makeWorker throws, or ?worker=0 passes makeWorker: null) the same queue runs decodeColumn on the main
// thread, one job per macrotask.
import { decodeColumn as decodeOnMain } from './column-work.mjs'

const defaultMakeWorker = () => new Worker(new URL('./decode-worker.mjs', import.meta.url), { type: 'module' })
const defaultSize = () => Math.max(1, Math.min(3, (globalThis.navigator?.hardwareConcurrency ?? 2) - 1))
const nextTask = () => new Promise(resolve => setTimeout(resolve, 0))

const tryMake = makeWorker => {
  try {
    return makeWorker?.() ?? null
  } catch {
    return null
  }
}

export const createDecoder = ({ size = defaultSize(), makeWorker = defaultMakeWorker, priority, decodeColumn = decodeOnMain }) => {
  const queue = new Map() // key -> {key, bytes, resolve}
  const running = new Map() // job id -> {key, resolve, worker, cancelled}
  let table = null
  let nextId = 1
  let workers = null // [{worker, busy}], or [] for the main-thread fallback
  let mainBusy = false

  const startWorkers = () => {
    if (workers) return
    const made = Array.from({ length: size }, () => tryMake(makeWorker))
    workers = made.some(w => !w) ? [] : made.map(worker => ({ worker, busy: false }))
    for (const slot of workers) {
      slot.worker.postMessage({ type: 'table', ...table })
      slot.worker.onmessage = ({ data }) => finish(slot, data)
    }
  }

  const finish = (slot, { id, result, error, ms }) => {
    const job = running.get(id)
    running.delete(id)
    slot.busy = false
    if (error) console.error(`column ${job?.key}: ${error}`)
    job?.resolve(job.cancelled || error ? null : { ...result, ms, mainMs: 0 })
    pump()
  }

  const takeNearest = () => {
    let best = null
    let bestPriority = Infinity
    for (const job of queue.values()) {
      const p = priority(job.key)
      if (p >= bestPriority) continue
      best = job
      bestPriority = p
    }
    if (best) queue.delete(best.key)
    return best
  }

  // waits a macrotask first, so jobs queued meanwhile compete and the browser gets to draw between jobs
  const runOnMain = async () => {
    mainBusy = true
    await nextTask()
    const job = takeNearest()
    if (!job) {
      mainBusy = false
      return
    }
    const started = performance.now()
    const result = await decodeColumn(job.bytes, table).catch(error => {
      console.error(`column ${job.key}:`, error)
      return null
    })
    job.resolve(result && { ...result, ms: performance.now() - started, mainMs: result.syncMs }) // mainMs: main-thread busy time
    mainBusy = false
    pump()
  }

  const pump = () => {
    if (!table) return
    startWorkers()
    if (!workers.length) {
      if (!mainBusy && queue.size) runOnMain()
      return
    }
    for (const slot of workers) {
      if (slot.busy) continue
      const job = takeNearest()
      if (!job) return
      const id = nextId++
      slot.busy = true
      running.set(id, { key: job.key, resolve: job.resolve, cancelled: false })
      slot.worker.postMessage({ type: 'job', id, key: job.key, bytes: job.bytes }, [job.bytes.buffer])
    }
  }

  const cancel = key => {
    const job = queue.get(key)
    if (job) {
      queue.delete(key)
      job.resolve(null)
    }
    for (const entry of running.values()) if (entry.key === key) entry.cancelled = true
  }

  return {
    setTable: next => {
      table = next
      pump()
    },
    decode: (key, bytes) => new Promise(resolve => {
      cancel(key) // a newer file for the same column replaces the older job
      queue.set(key, { key, bytes, resolve })
      pump()
    }),
    cancel,
    pending: () => queue.size + running.size
  }
}
