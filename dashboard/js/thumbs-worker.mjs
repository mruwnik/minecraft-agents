// Renders body previews off the main thread: {id, name, ...opts} in, {id, png, ms} or {id, error, errorName} out.
import { parentPort } from 'node:worker_threads'
import { renderView } from '../../tools/view/render.mjs'

parentPort.on('message', ({ id, name, stateDir, width, height, maxDist }) => {
  try {
    const { png, ms } = renderView({ agentName: name, stateDir, width, height, maxDist })
    parentPort.postMessage({ id, png, ms })
  } catch (err) {
    parentPort.postMessage({ id, error: err.message, errorName: err.name })
  }
})
