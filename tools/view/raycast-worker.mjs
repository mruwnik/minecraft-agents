// Why JavaScript: worker thread; draws bands of rows into shared memory.
// A raycast pool worker: draws bands of rows of each frame the pool posts it, into shared memory. Rows are claimed from a
// shared counter, so a thread that drew cheap sky rows comes back for more while another is still on the ground.
import { parentPort, workerData } from 'node:worker_threads'
import { makeChunkClass } from './columns.mjs'
import { makeBlockSource } from './blocks.mjs'
import { renderBand } from './raycaster.mjs'
import { NEXT_ROW, DONE, ERROR, NEAR_PIXELS } from './pool.mjs'

const { version, textureDir, control, errors } = workerData
const { info, texture } = makeBlockSource(makeChunkClass(version).registry, textureDir)

// claim bands until the frame has none left; the pixels near the eye this worker saw
const drawFrame = ({ grid, params, rgba, depth, band }) => {
  let near = 0
  for (let rowStart = Atomics.add(control, NEXT_ROW, band); rowStart < params.height; rowStart = Atomics.add(control, NEXT_ROW, band)) {
    near += renderBand({ ...params, grid, info, texture, rgba, depth, rowStart, rowEnd: Math.min(rowStart + band, params.height) })
  }
  return near
}

parentPort.on('message', frame => {
  try {
    Atomics.add(control, NEAR_PIXELS, drawFrame(frame))
  } catch (e) {
    errors.postMessage(e.message)
    Atomics.store(control, ERROR, 1)
  }
  Atomics.add(control, DONE, 1)
  Atomics.notify(control, DONE)
})
