// Why JavaScript: worker thread (worker_threads) that renders body previews with the JS view renderer off the main thread.
// Renders body previews off the main thread: {id, world, name, ...opts} in, {id, png, ms} or {id, error, errorName} out.
import { parentPort } from 'node:worker_threads'
import { renderView, columnStats } from '../../tools/view/render.mjs'

parentPort.on('message', ({ id, world, name, stateDir, width, height, maxDist }) => {
  try {
    const { png, ms } = renderView({ world, agentName: name, stateDir, width, height, maxDist })
    parentPort.postMessage({ id, png, ms, loaded: columnStats().loaded })
  } catch (err) {
    parentPort.postMessage({ id, error: err.message, errorName: err.name })
  }
})
