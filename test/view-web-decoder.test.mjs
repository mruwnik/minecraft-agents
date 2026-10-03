import test from 'node:test'
import assert from 'node:assert/strict'
import { createDecoder } from '../tools/view/web/decoder.mjs'

const tick = () => new Promise(resolve => setTimeout(resolve, 0))
const settleAll = async () => { for (let i = 0; i < 5; i++) await tick() }

// a worker whose jobs finish only when the test calls finish(); every message is logged
const fakeWorkers = () => {
  const workers = []
  const makeWorker = () => {
    const worker = {
      log: [],
      onmessage: null,
      postMessage (message, transfer = []) { worker.log.push({ message, transfer }) },
      finish (id, result = { mats: new Uint16Array(2), flags: new Uint8Array(1), light: new Uint8Array(2) }) {
        const transfer = [result.mats.buffer, result.flags.buffer, result.light.buffer]
        worker.onmessage({ data: { id, result, ms: 1, transfer } })
      }
    }
    workers.push(worker)
    return worker
  }
  return { workers, makeWorker }
}
const jobsOf = worker => worker.log.filter(l => l.message.type === 'job')

test('the table goes to every worker before any job', async () => {
  const { workers, makeWorker } = fakeWorkers()
  const decoder = createDecoder({ size: 2, makeWorker, priority: () => 0 })
  decoder.decode('a', new Uint8Array(1))
  decoder.decode('b', new Uint8Array(1))
  decoder.setTable({ format: { f: 1 }, materialOf: new Uint16Array(3) })
  await settleAll()
  assert.equal(workers.length, 2)
  for (const worker of workers) assert.equal(worker.log[0].message.type, 'table')
  assert.equal(workers.flatMap(jobsOf).length, 2)
})

test('no job is dispatched before the table is set', async () => {
  const { workers, makeWorker } = fakeWorkers()
  const decoder = createDecoder({ size: 1, makeWorker, priority: () => 0 })
  decoder.decode('a', new Uint8Array(1))
  await settleAll()
  assert.equal(workers.flatMap(jobsOf).length, 0)
  assert.equal(decoder.pending(), 1)
})

test('dispatch follows the priority at dispatch time', async () => {
  const { workers, makeWorker } = fakeWorkers()
  const priorities = { a: 1, b: 2, c: 3, d: 4 }
  const decoder = createDecoder({ size: 1, makeWorker, priority: key => priorities[key] })
  decoder.setTable({ format: {}, materialOf: new Uint16Array(1) })
  const done = ['a', 'b', 'c', 'd'].map(key => decoder.decode(key, new Uint8Array(1)))
  await settleAll()
  const [worker] = workers
  assert.deepEqual(jobsOf(worker).map(l => l.message.id), [jobsOf(worker)[0].message.id]) // a is running
  Object.assign(priorities, { a: 9, b: 9, c: 9, d: 0 }) // the camera moved: d is now the nearest
  worker.finish(jobsOf(worker)[0].message.id)
  await settleAll()
  const order = jobsOf(worker).map(l => l.message.key)
  assert.deepEqual(order, ['a', 'd'])
  worker.finish(jobsOf(worker)[1].message.id)
  await settleAll()
  assert.equal(jobsOf(worker).length, 3)
  void done
})

test('the job transfers its bytes and the result buffers come back as results', async () => {
  const { workers, makeWorker } = fakeWorkers()
  const decoder = createDecoder({ size: 1, makeWorker, priority: () => 0 })
  decoder.setTable({ format: {}, materialOf: new Uint16Array(1) })
  const bytes = new Uint8Array([1, 2, 3])
  const promise = decoder.decode('a', bytes)
  await settleAll()
  const [job] = jobsOf(workers[0])
  assert.deepEqual(job.transfer, [bytes.buffer])
  workers[0].finish(job.message.id)
  const result = await promise
  assert.equal(result.mats.length, 2)
  assert.equal(result.ms, 1)
  assert.equal(decoder.pending(), 0)
})

test('cancel drops a queued job and resolves it with null', async () => {
  const { workers, makeWorker } = fakeWorkers()
  const decoder = createDecoder({ size: 1, makeWorker, priority: () => 0 })
  decoder.setTable({ format: {}, materialOf: new Uint16Array(1) })
  decoder.decode('a', new Uint8Array(1))
  const queued = decoder.decode('b', new Uint8Array(1))
  await settleAll()
  decoder.cancel('b')
  assert.equal(await queued, null)
  workers[0].finish(jobsOf(workers[0])[0].message.id)
  await settleAll()
  assert.deepEqual(jobsOf(workers[0]).map(l => l.message.key), ['a'])
})

test('cancel of a running job discards its result', async () => {
  const { workers, makeWorker } = fakeWorkers()
  const decoder = createDecoder({ size: 1, makeWorker, priority: () => 0 })
  decoder.setTable({ format: {}, materialOf: new Uint16Array(1) })
  const running = decoder.decode('a', new Uint8Array(1))
  await settleAll()
  decoder.cancel('a')
  workers[0].finish(jobsOf(workers[0])[0].message.id)
  assert.equal(await running, null)
  assert.equal(decoder.pending(), 0)
})

test('a worker error resolves the job with null and frees the worker', async () => {
  const { workers, makeWorker } = fakeWorkers()
  const decoder = createDecoder({ size: 1, makeWorker, priority: () => 0 })
  decoder.setTable({ format: {}, materialOf: new Uint16Array(1) })
  const first = decoder.decode('a', new Uint8Array(1))
  decoder.decode('b', new Uint8Array(1))
  await settleAll()
  workers[0].onmessage({ data: { id: jobsOf(workers[0])[0].message.id, error: 'boom' } })
  assert.equal(await first, null)
  await settleAll()
  assert.equal(jobsOf(workers[0]).length, 2)
})

test('the fallback runs on the main thread, one job per macrotask, in priority order', async () => {
  const priorities = { a: 3, b: 1, c: 2 }
  const ran = []
  const decoder = createDecoder({
    size: 1,
    makeWorker: () => { throw new Error('no workers') },
    priority: key => priorities[key],
    decodeColumn: async (bytes) => { ran.push(bytes[0]); return { mats: new Uint16Array(1), flags: new Uint8Array(1), light: new Uint8Array(1) } }
  })
  decoder.setTable({ format: {}, materialOf: new Uint16Array(1) })
  const results = [decoder.decode('a', new Uint8Array([1])), decoder.decode('b', new Uint8Array([2])), decoder.decode('c', new Uint8Array([3]))]
  await tick()
  assert.ok(ran.length <= 1, 'one job per macrotask')
  const all = await Promise.all(results)
  assert.deepEqual(ran, [2, 3, 1])
  for (const r of all) assert.equal(r.mats.length, 1)
})
